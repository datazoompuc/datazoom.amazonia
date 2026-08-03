# check_params() never had a single unit test before this branch. It is pure
# and offline (it only reads datasets_link()), so it is fully testable without
# any network access.

test_that("an unsupported dataset raises an informative error", {
  expect_error(
    check_params(list(source = "prodes", dataset = "not_a_real_dataset")),
    "not supported"
  )
})

test_that("a numeric dataset short-circuits the dataset check", {
  expect_null(check_params(list(source = "pam", dataset = 123)))
})

test_that("a supported dataset passes silently", {
  expect_silent(
    check_params(list(source = "prodes", dataset = "deforestation"))
  )
})

test_that("time_period outside available_time warns but does not error", {
  expect_warning(
    check_params(list(
      source = "prodes", dataset = "deforestation",
      time_period = 1900
    )),
    "time_period"
  )
})

test_that("time_period inside available_time is silent", {
  expect_silent(
    check_params(list(
      source = "prodes", dataset = "deforestation",
      time_period = 2020
    ))
  )
})

test_that("an unsupported geo_level errors when available_geo is fully specified", {
  expect_error(
    check_params(list(
      source = "prodes", dataset = "deforestation",
      geo_level = "not_a_real_geo_level"
    )),
    "geo_level"
  )
})

test_that("geo_level is unchecked when available_geo is NA", {
  expect_silent(
    check_params(list(
      source = "degrad", dataset = "degrad",
      geo_level = "anything_goes"
    ))
  )
})
