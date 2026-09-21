# tests/testthat/test-ips-sheets.R
#
# Tests for ips_match_sheets() (R/ips.R) -- the whitespace-tolerant year-tab
# matcher that replaced load_ips()'s hardcoded sheet_list. Pure function, no
# network: matches requested years against a workbook's own
# readxl::excel_sheets() output on the year DIGITS rather than the raw
# string, because IPS Amazônia's real "2018" tab is literally "2018 "
# (trailing space) -- verified live 2026-09-14 -- and readxl::read_xlsx()'s
# own sheet lookup is an exact string match, so this is the one place that
# quirk (or any future whitespace variant) gets absorbed.

test_that("ips_match_sheets() resolves a trailing-space tab name for its year", {
  sheet_names <- c("2023", "2021", "2018 ", "2014", "Definição dos indicadores")
  got <- ips_match_sheets(sheet_names, 2018)
  expect_equal(got, "2018 ")
})

test_that("ips_match_sheets() resolves a leading-space tab name too", {
  sheet_names <- c(" 2018", "2014")
  got <- ips_match_sheets(sheet_names, 2018)
  expect_equal(got, " 2018")
})

test_that("ips_match_sheets() returns verbatim upstream names (whitespace intact), not normalized ones", {
  sheet_names <- c("2023", "2018 ")
  got <- ips_match_sheets(sheet_names, 2018)
  expect_equal(nchar(got), 5) # "2018 " -- the trailing space must survive
})

test_that("ips_match_sheets() handles multiple years, preserving REQUEST order (not sheet order)", {
  sheet_names <- c("2023", "2021", "2018 ", "2014", "Definição dos indicadores")
  got <- ips_match_sheets(sheet_names, c(2023, 2014, 2018))
  expect_equal(got, c("2023", "2014", "2018 "))
})

test_that("ips_match_sheets() ignores non-year tabs (glossary sheet) without matching them by accident", {
  sheet_names <- c("2023", "Definição dos indicadores")
  got <- ips_match_sheets(sheet_names, 2023)
  expect_equal(got, "2023")
})

test_that("ips_match_sheets() errors, naming the missing year AND what tabs were found, when a year isn't present", {
  sheet_names <- c("2023", "2021", "2018 ", "2014")
  expect_error(
    ips_match_sheets(sheet_names, 2099),
    "2099"
  )
  err <- tryCatch(ips_match_sheets(sheet_names, 2099), error = function(e) conditionMessage(e))
  expect_match(err, "2023", fixed = TRUE)
})

test_that("ips_match_sheets() reports ALL missing years when more than one is absent", {
  sheet_names <- c("2023", "2014")
  err <- tryCatch(ips_match_sheets(sheet_names, c(2018, 2021)), error = function(e) conditionMessage(e))
  expect_match(err, "2018", fixed = TRUE)
  expect_match(err, "2021", fixed = TRUE)
})

test_that("ips_match_sheets() accepts character year input (e.g. from manifest parsing) the same as integer", {
  sheet_names <- c("2023", "2018 ")
  expect_equal(ips_match_sheets(sheet_names, "2018"), ips_match_sheets(sheet_names, 2018))
})
