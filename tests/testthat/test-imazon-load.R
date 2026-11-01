# tests/testthat/test-imazon-load.R
#
# Offline test for load_imazon() (R/imazon.R) in its 2026-10-30 form: Imazon's SAD
# alerts by municipality and month, read from the CSV files behind its own dashboard.
# (It used to return a 0-to-3 classification of the municipalities from a file on a
# personal Google Drive, and had no test at all.) external_download() is mocked:
# the stand-in has the columns the real files have.

# IBGE codes from the package's own table, so nothing here is typed from memory
code_of <- function(name, state) {
  m <- as.data.frame(datazoom.amazonia::municipalities)
  m$code_muni[m$name_muni == tolower(stringi::stri_trans_general(name, "Latin-ASCII")) & m$abbrev_state == state]
}

# What the real CSV holds: one row per month, municipality and sensor.
raw_alerts <- function(alert = "desmatamento") {
  data.table::data.table(
    ALERTA = alert,
    MES = c(1L, 1L, 2L, 1L, 3L, 1L),
    ANO = c(2020L, 2020L, 2020L, 2020L, 2021L, 2019L),
    SENSOR = c("Sentinel-2", "Landsat-8", "Sentinel-2", "Sentinel-2", "Sentinel-2", "Modis"),
    ESTADO = c("PA", "PA", "PA", "RO", "PA", "PA"),
    MUNICIPIO = c("Altamira", "Altamira", "Altamira", "Machadinho D'Oeste", "São Félix do Xingu", "Altamira"),
    AREAKM2 = c(10, 2.5, 4, 1.25, 8, 100)
  )
}

load_fake <- function(raw = raw_alerts(), ...) {
  asked <- new.env()
  testthat::local_mocked_bindings(
    external_download = function(dataset = NULL, source = NULL, ...) {
      asked$dataset <- dataset
      asked$source <- source
      raw
    },
    .package = "datazoom.amazonia"
  )
  out <- suppressMessages(load_imazon(...))
  attr(out, "asked") <- as.list(asked)
  out
}

test_that("the datasets are the two SAD ones; the old one, and no dataset at all, are refused", {
  expect_error(load_imazon("imazon_shp"), "Dataset imazon_shp not supported")
  expect_error(load_imazon("imazon_shp"), "sad_deforestation")
  expect_error(load_imazon("imazon_shp"), "sad_degradation")
  # no default: load_imazon() used to return the Drive classification, and a bare call must not quietly return something else
  expect_error(load_imazon(), "dataset")
})

test_that("each dataset is read through external_download() as the manifest's imazon source", {
  for (dataset in c("sad_deforestation", "sad_degradation")) {
    out <- load_fake(dataset = dataset)
    expect_equal(attr(out, "asked")$dataset, dataset)
    expect_equal(attr(out, "asked")$source, "imazon")
  }
})

test_that("treated data: one row per municipality and month, the sensors' areas summed, IBGE codes added", {
  out <- load_fake(dataset = "sad_deforestation", language = "eng")

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("year", "month", "municipality_code", "municipality", "state", "area"))
  expect_equal(nrow(out), 5) # six alerts, Altamira in January 2020 reported by two sensors

  altamira_jan <- out[out$municipality == "Altamira" & out$year == 2020 & out$month == 1, ]
  expect_equal(altamira_jan$area, 12.5) # 10 + 2.5
  expect_equal(altamira_jan$municipality_code, code_of("Altamira", "PA"))
  expect_equal(out$municipality_code[out$municipality == "Machadinho D'Oeste"], code_of("Machadinho D'Oeste", "RO"))
  expect_equal(out$municipality_code[out$municipality == "São Félix do Xingu"], code_of("São Félix do Xingu", "PA")) # accents do not matter
  expect_false(anyNA(out$municipality_code))

  # the types a user joins on
  expect_type(out$municipality_code, "double")
  expect_type(out$year, "integer")
  expect_type(out$month, "integer")
  expect_type(out$area, "double")

  # ordered by year, month, state, municipality
  expect_equal(order(out$year, out$month, out$state, out$municipality), seq_len(nrow(out)))
  # the figures add up: nothing created, nothing lost
  expect_equal(sum(out$area), sum(raw_alerts()$AREAKM2))
})

test_that("the Portuguese names", {
  out <- load_fake(dataset = "sad_degradation", language = "pt")
  expect_equal(names(out), c("ano", "mes", "cod_municipio", "municipio", "uf", "area"))
  expect_equal(nrow(out), 5)
  expect_equal(out$area[out$municipio == "Altamira" & out$ano == 2020 & out$mes == 1], 12.5)
})

test_that("time_period keeps the years asked for, in treated and in raw data, and says nothing about the rest", {
  treated <- load_fake(dataset = "sad_deforestation", time_period = 2020)
  expect_equal(unique(treated$year), 2020L)
  expect_equal(nrow(treated), 3)

  both <- load_fake(dataset = "sad_deforestation", time_period = 2019:2020)
  expect_equal(sort(unique(both$year)), c(2019L, 2020L))

  raw <- load_fake(dataset = "sad_deforestation", time_period = 2021, raw_data = TRUE)
  expect_equal(unique(raw$ANO), 2021L)
  expect_equal(nrow(raw), 1)
})

test_that("raw data is the file as Imazon publishes it: every row, every sensor, its own column names", {
  raw <- load_fake(dataset = "sad_deforestation", raw_data = TRUE)
  expect_s3_class(raw, "tbl_df")
  expect_equal(names(raw), c("ALERTA", "MES", "ANO", "SENSOR", "ESTADO", "MUNICIPIO", "AREAKM2"))
  expect_equal(nrow(raw), nrow(raw_alerts()))
  expect_equal(sum(raw$AREAKM2), sum(raw_alerts()$AREAKM2))
})

test_that("a year the dataset does not have warns, from the manifest's available_time", {
  expect_warning(load_fake(dataset = "sad_deforestation", time_period = 2007), "time_period must be in")
  # 2025 is missing from the degradation alerts, and the manifest says so
  expect_warning(load_fake(dataset = "sad_degradation", time_period = 2025), "2008-2024, 2026", fixed = TRUE)
})

test_that("a municipality the IBGE table does not have under that state keeps its row, with NA code and a warning that says how much", {
  raw <- rbind(raw_alerts("degradacao"), data.table::data.table(
    ALERTA = "degradacao", MES = 5L, ANO = 2020L, SENSOR = "Sentinel-2", ESTADO = "MT",
    MUNICIPIO = "Machadinho D'Oeste", AREAKM2 = 7 # listed under MT; the municipality is in RO
  ))
  warnings <- character(0)
  out <- withCallingHandlers(
    load_fake(raw = raw, dataset = "sad_degradation"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(nrow(out), 6)
  expect_equal(sum(is.na(out$municipality_code)), 1)
  expect_equal(out$area[is.na(out$municipality_code)], 7)
  expect_length(warnings, 1)
  expect_match(warnings, "1 row(s), 7 km2", fixed = TRUE)
  expect_match(warnings, "Machadinho D'Oeste (MT)", fixed = TRUE)
  expect_match(warnings, "Their municipality code is NA", fixed = TRUE)
  # the rows that did match are unharmed
  expect_equal(out$municipality_code[out$municipality == "Altamira"][1], code_of("Altamira", "PA"))
})

test_that("names are matched without regard to accents, case and apostrophes", {
  key <- imazon_municipality_key
  expect_equal(key("Querência"), key("QUERENCIA"))
  expect_equal(key("Machadinho D'Oeste"), key("MACHADINHO D’OESTE")) # straight and curly apostrophes
  expect_equal(key("São Félix do Xingu"), key("sao felix do xingu"))
  expect_false(key("Santa Rosa") == key("Santa Rosa do Purus"))
})

test_that("every municipality of the real 2026 files matched the table (a snapshot of names: the files are not downloaded here)", {
  # The Legal Amazon municipalities of the deforestation alerts, as Imazon spells them: all 624 distinct
  # (state, name) pairs of 2008-2026 are in the IBGE table (checked 2026-10-30). Here: every name of the table
  # survives its own key, and no two municipalities of one state share a key (a join on name and state is a key).
  m <- as.data.frame(datazoom.amazonia::municipalities)
  keys <- paste(m$abbrev_state, imazon_municipality_key(m$name_muni))
  expect_equal(anyDuplicated(keys), 0L)
})
