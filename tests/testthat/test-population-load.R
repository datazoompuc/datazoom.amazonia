# tests/testthat/test-population-load.R
#
# Offline tests for what load_population() asks IBGE for, year by year, and what it calls
# the result (R/population.R). The series lives in four SIDRA tables: the estimates (6579)
# have no value for 2007, 2010, 2022 or 2023, and load_population() asks the Contagem da
# Populacao (793) for 2007, the Censo Demografico of 2010 (1378) and the Censo Demografico
# of 2022 (4714) for the others. Until 2026-11-12 that choice was an `if` inside the shared
# sidra_download() with no 2022 in it (so 2022 stopped with "Error in Download."), and
# the tables with no classification were sent with classific = "all", which sidrar answers
# by scraping an IBGE HTML page that is behind a bot challenge from some networks. There
# was no test of load_population() at all. sidra_download() is mocked (no network).

# ---- what load_population() asks sidra_download() for ---------------------------------

# sidra_download()'s answer for a table at a level, in sidrar's own column order (checked
# live 2026-11-12), with the variables each table reports: 6579 the estimate (9324), 793
# and 1378 the resident population (93), 4714 that and the area and the density
FAKE_VARIABLES <- c(
  `93` = "População residente", `9324` = "População residente estimada",
  `6318` = "Área da unidade territorial", `614` = "Densidade demográfica"
)
FAKE_TABLE_VARIABLES <- list(`6579` = "9324", `793` = "93", `1378` = "93", `4714` = c("93", "6318", "614"))
GEO_COLUMNS <- list(
  country = c("brasil_codigo", "brasil"),
  state = c("unidade_da_federacao_codigo", "unidade_da_federacao"),
  municipality = c("municipio_codigo", "municipio")
)
GEO_CODES <- list(country = "1", state = c("11", "12"), municipality = c("1100015", "1100023"))

fake_value <- function(table, variable, geo, year) {
  as.numeric(variable) * 1000 + as.numeric(geo) + as.numeric(year) / 10000 + as.numeric(table) / 1e8
}

fake_population <- function(table, year, geo_level, extra_variable = NULL) {
  variables <- c(FAKE_TABLE_VARIABLES[[as.character(table)]], extra_variable)
  names_of <- c(FAKE_VARIABLES, stats::setNames("Variavel nova", extra_variable))
  grid <- expand.grid(geo = GEO_CODES[[geo_level]], variavel_codigo = variables, stringsAsFactors = FALSE)
  out <- tibble::tibble(
    nivel_territorial_codigo = "3", nivel_territorial = "Unidade da Federação",
    unidade_de_medida_codigo = "1", unidade_de_medida = "Pessoas",
    valor = mapply(function(v, g) fake_value(table, v, g, year), grid$variavel_codigo, grid$geo)
  )
  out[[GEO_COLUMNS[[geo_level]][1]]] <- grid$geo
  out[[GEO_COLUMNS[[geo_level]][2]]] <- if (geo_level == "country") "Brasil" else paste("Lugar", grid$geo)
  out$ano_codigo <- as.character(year)
  out$ano <- as.character(year)
  out$variavel_codigo <- grid$variavel_codigo
  out$variavel <- unname(names_of[grid$variavel_codigo])
  out
}

# Runs load_population() with sidra_download() mocked; returns the result with the calls
# the mock saw in attr "calls" (one list per call: table, classific, category, year, geo_level).
load_fake <- function(..., extra_variable = NULL) {
  calls <- list()
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, classific = "all", category = "all", year, geo_level) {
      calls[[length(calls) + 1]] <<- list(table = sidra_code, classific = classific, category = category, year = year, geo_level = geo_level)
      fake_population(sidra_code, year, geo_level, extra_variable)
    },
    .package = "datazoom.amazonia"
  )
  out <- load_population(...)
  attr(out, "calls") <- calls
  out
}

test_that("the manifest says what these tests assume: one dataset, the bare estimates table", {
  committed <- link_table()[link_table()$survey == "population", ]
  expect_equal(nrow(committed), 1)
  expect_equal(committed$dataset, "population")
  expect_equal(committed$sidra_code, "6579")
})

test_that("load_population() asks, year by year, the table that has the year: 793 for 2007, 1378 for 2010, 4714 for 2022, 6579 for the rest", {
  calls <- attr(load_fake(geo_level = "state", time_period = c(2007, 2010, 2015, 2022, 2024), raw_data = TRUE), "calls")
  expect_length(calls, 5)
  tables <- vapply(calls, function(x) x$table, numeric(1))
  expect_equal(tables, c(793, 1378, 6579, 4714, 6579))
  expect_equal(vapply(calls, function(x) x$year, ""), c("2007", "2010", "2015", "2022", "2024"))
  expect_true(all(vapply(calls, function(x) x$geo_level == "state", NA)))

  # a table with no classification is sent with none (NULL, present in the call), never "all"
  for (i in c(1, 3, 4, 5)) {
    expect_true("classific" %in% names(calls[[i]]) && is.null(calls[[i]]$classific), info = calls[[i]]$table)
    expect_true("category" %in% names(calls[[i]]) && is.null(calls[[i]]$category), info = calls[[i]]$table)
  }
  # the Censo 2010 table has four classifications: the request names the Total of the first
  expect_equal(calls[[2]]$classific, "c1")
  expect_equal(calls[[2]]$category, list(0))
})

test_that("population_request() is the whole map: the estimates table's code, a special year's table, anything else as it is", {
  expect_equal(population_request(2015, 6579, NULL, NULL), list(table = 6579, classific = NULL, category = NULL))
  expect_equal(population_request(2007, 6579, NULL, NULL)$table, 793)
  expect_equal(population_request(2010, 6579, NULL, NULL)$table, 1378)
  expect_equal(population_request(2022, 6579, NULL, NULL)$table, 4714)
  expect_equal(population_request("2022", 6579, NULL, NULL)$table, 4714) # the loader passes the year as text
  # a table number the manifest does not know is never rerouted, whatever the year
  expect_equal(population_request(2007, 793, "all", "all"), list(table = 793, classific = "all", category = "all"))
  expect_equal(population_request(2022, 1612, "all", "all")$table, 1612)
  expect_setequal(names(population_special_tables), c("2007", "2010", "2022"))
})

test_that("a SIDRA table given as a number is the manifest dataset that reads it: the same requests as the dataset's name, special years included", {
  years <- c(2007, 2010, 2015, 2022)
  by_name <- load_fake(dataset = "population", geo_level = "state", time_period = years, raw_data = TRUE)
  by_number <- load_fake(dataset = 6579, geo_level = "state", time_period = years, raw_data = TRUE)
  expect_equal(attr(by_number, "calls"), attr(by_name, "calls"))
  expect_equal(sidra_dataset_of_table("population", 6579), "population")
  expect_identical(
    load_fake(dataset = 6579, geo_level = "state", time_period = years)[, c("year", "geo_id")],
    load_fake(dataset = "population", geo_level = "state", time_period = years)[, c("year", "geo_id")]
  )
})

test_that("a number the manifest does not know keeps the old request (classific = 'all'), and a bad dataset name still stops in check_params()", {
  calls <- attr(load_fake(dataset = 793, geo_level = "country", time_period = 2007, raw_data = TRUE), "calls")
  expect_equal(calls[[1]]$table, 793)
  expect_equal(calls[[1]]$classific, "all")
  expect_equal(calls[[1]]$category, "all")

  expect_error(load_population(dataset = "population_nonexistent", geo_level = "country", time_period = 2015), "not supported")
})

# ---- what load_population() calls the result ------------------------------------------------

test_that("2022 is the resident population of the census, like 2007 and 2010: variable 93, and no area or density column", {
  out <- load_fake(geo_level = "state", time_period = 2022)
  expect_equal(names(out), c("year", "geo_id", "population_v93"))
  expect_equal(out$geo_id, GEO_CODES$state)
  expect_equal(out$year, rep("2022", 2))
  expect_equal(out$population_v93, vapply(GEO_CODES$state, function(g) fake_value(4714, "93", g, 2022), numeric(1)), ignore_attr = TRUE)
  expect_false(any(grepl("area|densidade|v6318|v614", names(out))))
})

test_that("raw 2022 data is table 4714's rows for the resident population only: the area and the density are left out of raw data too", {
  raw <- load_fake(geo_level = "state", time_period = 2022, raw_data = TRUE)
  expect_equal(unique(raw$variavel_codigo), "93")
  expect_equal(nrow(raw), 2)
  # the other tables are untouched
  expect_equal(nrow(load_fake(geo_level = "state", time_period = 2007, raw_data = TRUE)), 2)
  expect_equal(unique(load_fake(geo_level = "state", time_period = 2015, raw_data = TRUE)$variavel_codigo), "9324")
})

test_that("a series across the tables has the census population in one column and the estimate in the other, one row per year and place", {
  out <- load_fake(geo_level = "state", time_period = c(2007, 2010, 2015, 2022, 2024))
  expect_equal(names(out), c("year", "geo_id", "population_v93", "estimated_population_v9324"))
  expect_equal(nrow(out), 10)
  expect_equal(sort(unique(out$year)), c("2007", "2010", "2015", "2022", "2024"))
  by_year <- split(out, out$year)
  for (year in c("2007", "2010", "2022")) {
    expect_false(anyNA(by_year[[year]]$population_v93), info = year)
    expect_true(all(is.na(by_year[[year]]$estimated_population_v9324)), info = year)
  }
  for (year in c("2015", "2024")) {
    expect_true(all(is.na(by_year[[year]]$population_v93)), info = year)
    expect_false(anyNA(by_year[[year]]$estimated_population_v9324), info = year)
  }
  # each value comes from the table of its year
  expect_equal(by_year[["2007"]]$population_v93[1], fake_value(793, "93", "11", 2007))
  expect_equal(by_year[["2010"]]$population_v93[1], fake_value(1378, "93", "11", 2010))
  expect_equal(by_year[["2015"]]$estimated_population_v9324[1], fake_value(6579, "9324", "11", 2015))
})

test_that("the Portuguese names are IBGE's own, cleaned, and the year is 'ano'", {
  out <- load_fake(geo_level = "country", time_period = c(2010, 2015), language = "pt")
  expect_equal(names(out), c("ano", "geo_id", "populacao_residente_v93", "populacao_residente_estimada_v9324"))
  expect_equal(out$geo_id, rep("Brasil", 2))
})

test_that("at every level the geography is geo_id: the code for state and municipality, IBGE's name for the country", {
  expect_equal(load_fake(geo_level = "state", time_period = 2022)$geo_id, GEO_CODES$state)
  expect_equal(load_fake(geo_level = "municipality", time_period = 2007)$geo_id, GEO_CODES$municipality)
  expect_equal(load_fake(geo_level = "country", time_period = 2015)$geo_id, "Brasil")
})

test_that("a variable another table reports, which nothing renames, passes through under IBGE's own name instead of vanishing or turning NA", {
  out <- load_fake(geo_level = "state", time_period = 2015, extra_variable = "9999")
  expect_true("variavel_nova_v9999" %in% names(out))
  expect_false(anyNA(out$variavel_nova_v9999))
  expect_true("estimated_population_v9324" %in% names(out))
  # (the census-2022 filter keeps variable 93 of table 4714 only: it is that table's own business)
  out_2022 <- load_fake(geo_level = "state", time_period = 2022, extra_variable = "9999")
  expect_false("variavel_nova_v9999" %in% names(out_2022))
})

# ---- time_period -----------------------------------------------------------------------------

test_that("time_period is checked against the manifest's available_time: 2007, 2010, 2022 and 2024-2026 are in it, 2023 and 2027 are not", {
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, classific = "all", category = "all", year, geo_level) fake_population(sidra_code, year, geo_level),
    .package = "datazoom.amazonia"
  )
  for (year in c(2001, 2007, 2010, 2021, 2022, 2024, 2026)) {
    expect_no_warning(load_population(geo_level = "country", time_period = year))
  }
  for (year in c(2000, 2023, 2027)) {
    expect_warning(
      load_population(geo_level = "country", time_period = year),
      "time_period must be in 2001-2022, 2024-2026",
      fixed = TRUE, info = year
    )
  }
})

# ---- the periodic check ---------------------------------------------------------------------------

test_that("check_all_loads.R checks population in years that reach all four tables, every one of them declared by the manifest", {
  script <- file.path(normalizePath(test_path("..", ".."), mustWork = FALSE), "actions", "scripts", "check_all_loads.R")
  skip_if_not(file.exists(script), "actions/ not present (stripped build)")

  exprs <- parse(script, keep.source = FALSE)
  settings <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("datasets_by_fn", "TIME_PERIOD_BY_FN", "TIME_PERIOD")) eval(e, settings)
  }
  expect_equal(settings$datasets_by_fn$load_population, "population")

  years <- settings$TIME_PERIOD_BY_FN$load_population
  expect_false(is.null(years))
  declared <- parse_years(link_table()[link_table()$survey == "population", ]$available_time)
  expect_true(all(years %in% declared))
  # the three special years, so that 793, 1378 and 4714 are each asked once, and one estimate year for 6579
  expect_true(all(as.numeric(names(population_special_tables)) %in% years))
  expect_true(any(!years %in% as.numeric(names(population_special_tables))))
})
