# parse_years() is the one parser for the manifest's "available_time"
# grammar (comma-separated years and/or ranges). It replaces three
# independent ad-hoc implementations that used to live in check_params.R
# (comma+hyphen aware), epe.R and aneel.R (hyphen-only via
# eval(parse(text = ...)), which silently mishandled comma lists -- see
# R/manifest.R).

test_that("a single year parses to itself", {
  expect_equal(parse_years("2023"), 2023L)
})

test_that("a hyphen range expands to a sequence", {
  expect_equal(parse_years("2007-2016"), 2007:2016)
})

test_that("a comma list of single years parses to each of them", {
  expect_equal(parse_years("1970, 1975, 1980"), c(1970L, 1975L, 1980L))
})

test_that("a mix of single years and ranges in one comma list all parse", {
  expect_equal(parse_years("2003, 2007-2009, 2014"), c(2003L, 2007L, 2008L, 2009L, 2014L))
})

test_that("NA and NULL both parse to an empty integer vector", {
  expect_equal(parse_years(NA_character_), integer(0))
  expect_equal(parse_years(NULL), integer(0))
})

test_that("extra whitespace around tokens and range bounds is tolerated", {
  expect_equal(parse_years(" 2007 - 2009 , 2014 "), c(2007L, 2008L, 2009L, 2014L))
})
