# tests/testthat/test-available-time-readers.R
#
# The pure readers behind available_time for MapBiomas, DETER, EPE and ANEEL
# (added 2026-11-01), against small files built here in the shapes those sources
# really publish -- each one was read live that day, see the resolvers' headers.
# No network.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("foreign")
skip_if_not_installed("openxlsx")
skip_if_not_installed("readxl")

source(file.path(actions_dir, "scripts", "sidra_api.R"), local = TRUE)
source(file.path(actions_dir, "scripts", "mapbiomas_resolver_cache.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_mapbiomas.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_deter.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_epe.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_aneel.R"), local = TRUE)

## ---- MapBiomas ----

test_that("plain year columns give their years, ignoring the rest of the header", {
  header <- c("biome", "geocode", "class_level_1", "1985", "1986", "2024")
  expect_equal(mapbiomas_years_from_header(header), c(1985L, 1986L, 2024L))
})

test_that("transition windows count both ends, and long spans don't invent years", {
  # the real file mixes one-year windows with longer ones, in no useful order
  header <- c("class_level_1_to", "p1985_1986", "p1986_1987", "p2023_2024", "p1985_2024", "p2010_2020")
  expect_equal(
    mapbiomas_years_from_header(header),
    c(1985L, 1986L, 1987L, 2010L, 2020L, 2023L, 2024L)
  )
})

test_that("names that merely contain digits are not years", {
  expect_length(mapbiomas_years_from_header(c("code", "level_2024", "19850", "p1985_", "198")), 0)
})

test_that("a long sheet's years come from its `year` column", {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  dat <- data.frame(code = 1:3, municipality = "a", year = c(1985, 2024, 2000), area_ha = 1)
  openxlsx::write.xlsx(list(WATER_CITY_ANNUAL = dat), path)
  header <- names(readxl::read_excel(path, sheet = "WATER_CITY_ANNUAL", n_max = 0))

  expect_equal(mapbiomas_sheet_years(path, "WATER_CITY_ANNUAL", header), c(1985L, 2000L, 2024L))
})

test_that("a sheet with neither shape has no years", {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  openxlsx::write.xlsx(list(s = data.frame(code = 1, name = "a")), path)
  header <- names(readxl::read_excel(path, sheet = "s", n_max = 0))

  expect_length(mapbiomas_sheet_years(path, "s", header), 0)
})

test_that("the cache keeps available_time, and an older file reads as blank", {
  cache <- read_resolver_cache(tempfile())
  cache <- cache_upsert(cache, "d", "g", 1, "sum", "fp", "pass", "S", available_time = "1985-2024", checked_at = "2026-11-01")
  expect_equal(cache_lookup(cache, "d", "g", 1, "sum", "fp")$available_time, "1985-2024")

  # replacing a row must not shift values across columns (the upsert is positional)
  cache <- cache_upsert(cache, "d", "g", 1, "sum2", "fp", "pass", "S", reason = "r", available_time = "1985-2025", checked_at = "2026-11-02")
  expect_equal(nrow(cache), 1)
  expect_equal(cache$reason, "r")
  expect_equal(cache$available_time, "1985-2025")
  expect_equal(cache$checked_at, "2026-11-02")

  old <- tempfile(fileext = ".csv")
  on.exit(unlink(old))
  writeLines(c("dataset,geo_level,file_id,checksum,rules_fingerprint,verdict,sheet,reason,checked_at", "d,g,1,sum,fp,pass,S,,2026-08-24"), old)
  read <- read_resolver_cache(old)
  expect_equal(names(read), RESOLVER_CACHE_COLS)
  expect_true(is.na(cache_lookup(read, "d", "g", 1, "sum", "fp")$available_time))
})

## ---- DETER ----

write_dbf <- function(dat) {
  path <- tempfile(fileext = ".dbf")
  foreign::write.dbf(dat, path)
  path
}

test_that("DETER's years come from VIEW_DATE, whatever its case", {
  path <- write_dbf(data.frame(VIEW_DATE = as.Date(c("2023-08-03", "2026-10-23", "2024-01-01")), CLASSNAME = "x"))
  on.exit(unlink(path))
  expect_equal(deter_years_from_dbf(path), c(2023L, 2024L, 2026L))

  lower <- write_dbf(data.frame(view_date = as.Date("2018-05-01")))
  on.exit(unlink(lower), add = TRUE)
  expect_equal(deter_years_from_dbf(lower), 2018L)
})

test_that("a DETER package without VIEW_DATE is an error, not an empty answer", {
  path <- write_dbf(data.frame(CLASSNAME = "x"))
  on.exit(unlink(path))
  expect_error(deter_years_from_dbf(path), "no VIEW_DATE field")
})

## ---- EPE ----

write_consumption <- function(sheets) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(sheets, path)
  path
}
monthly <- function(dates) {
  data.frame(Data = 1, DataExcel = as.Date(dates), Consumo = 1)
}

test_that("EPE's years come from DataExcel, and a partial current year counts", {
  path <- write_consumption(list(
    UF = monthly(c("2012-01-01", "2013-05-01", "2026-08-01")),
    RG = monthly(c("2012-02-01", "2013-01-01", "2026-09-01"))
  ))
  on.exit(unlink(path))
  # the two sheets end in different months of the same year, which is agreement
  expect_equal(epe_consumption_years(path, c("UF", "RG")), c(2012L, 2013L, 2026L))
})

test_that("EPE refuses to choose between sheets that disagree on years", {
  path <- write_consumption(list(
    UF = monthly(c("2012-01-01", "2026-08-01")),
    RG = monthly(c("2012-01-01", "2025-12-01"))
  ))
  on.exit(unlink(path))
  expect_error(epe_consumption_years(path, c("UF", "RG")), "disagree")
})

test_that("an EPE sheet without DataExcel is an error", {
  path <- write_consumption(list(UF = data.frame(Data = 1)))
  on.exit(unlink(path))
  expect_error(epe_consumption_years(path, "UF"), "no DataExcel column")
})

## ---- ANEEL ----

write_siga <- function(dates) {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "NomEmpreendimento;DatEntradaOperacao;DscFaseUsina",
    paste0("Usina;", dates, ";Opera", "ção")
  ), path, useBytes = TRUE)
  path
}

test_that("ANEEL's window skips the 1900 placeholder and blank dates", {
  path <- write_siga(c("1900-01-03", "1900-01-03", "1908-09-07", "", "2026-10-02", "1950-01-01"))
  on.exit(unlink(path))
  expect_equal(aneel_generation_available_time(path), "1908-2026")
})

test_that("ANEEL's window is a window: a year with no new plant is not a gap", {
  path <- write_siga(c("1908-09-07", "2026-10-02"))
  on.exit(unlink(path))
  expect_equal(aneel_generation_available_time(path), "1908-2026")
})

test_that("an ANEEL file with only placeholders is an error", {
  path <- write_siga(c("1900-01-03", ""))
  on.exit(unlink(path))
  expect_error(aneel_generation_available_time(path), "no usable")
})
