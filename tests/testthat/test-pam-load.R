# tests/testthat/test-pam-load.R
#
# Offline tests for how load_pam() resolves what it is given to a SIDRA request
# (R/pam.R and the two helpers in R/download.R). load_pam() documents that a
# dataset can be given as a SIDRA table number ("You can also use SIDRA codes").
# That branch never worked: it set param$code and then read `sidra_info`, which
# only the dataset-name branch defines, so EVERY number stopped with
# "object 'sidra_info' not found" before a request was made -- the same family as
# load_ppm(dataset = 94), which hardcoded classific = "all" and, for treated
# data, stopped with "object 'harmonization_dat' not found". Now a table number
# the manifest knows is the dataset that reads that table whole.
#
# sidra_download() is mocked (no network).

# table number -> the dataset that reads it whole, and the request it must make
PAM_WHOLE_TABLES <- list(
  list(table = 1612, dataset = "temporary_crops", classific = "c81"),
  list(table = 1613, dataset = "permanent_crops", classific = "c82"),
  list(table = 5457, dataset = "all_crops", classific = "c782"),
  list(table = 839, dataset = "corn", classific = "c81"),
  list(table = 1000, dataset = "peanut", classific = "c81"),
  list(table = 1001, dataset = "potato", classific = "c81"),
  list(table = 1002, dataset = "beans", classific = "c81")
)

asked <- function(dataset, ...) {
  seen <- NULL
  testthat::local_mocked_bindings(
    sidra_download = function(...) {
      seen <<- list(...)
      stop("captured")
    },
    .package = "datazoom.amazonia"
  )
  expect_error(load_pam(dataset = dataset, geo_level = "state", time_period = 2023, ...), "captured")
  seen
}

test_that("the manifest says what these tests assume", {
  committed <- link_table()[link_table()$survey == "pam", ]
  for (w in PAM_WHOLE_TABLES) {
    expect_equal(committed$sidra_code[committed$dataset == w$dataset], sprintf("%s/%s/all", w$table, w$classific), info = w$dataset)
  }
})

test_that("a SIDRA table given as a number makes the request of the dataset that reads it whole", {
  for (w in PAM_WHOLE_TABLES) {
    by_number <- asked(w$table)
    expect_equal(by_number, asked(w$dataset), info = w$dataset)
    expect_equal(by_number$sidra_code, w$table, info = w$dataset)
    expect_equal(by_number$classific, w$classific, info = w$dataset)
    expect_equal(by_number$category, list("all"), info = w$dataset)
  }
})

test_that("a dataset name still makes the request its manifest row says, single crops included", {
  expect_equal(asked("soybean")[c("sidra_code", "classific", "category")], list(sidra_code = 1612, classific = "c81", category = list("2713")))
  expect_equal(asked("banana")[c("sidra_code", "classific", "category")], list(sidra_code = 1613, classific = "c82", category = list("2720")))
  expect_equal(asked("permanent_total")[c("sidra_code", "classific", "category")], list(sidra_code = 1613, classific = "c82", category = list("0")))
})

test_that("a number the manifest does not know keeps the old request (classific = 'all'), and a bad name or level still stops in check_params()", {
  seen <- asked(74, raw_data = TRUE) # a PPM table
  expect_equal(seen$sidra_code, 74)
  expect_equal(seen$classific, "all")
  expect_equal(seen$category, "all")

  expect_error(load_pam(dataset = "no_such_crop", geo_level = "state", time_period = 2023), "not supported")
  # the checks that applied to a name now apply to a number: they run after it is resolved
  expect_error(load_pam(dataset = 1612, geo_level = "galaxy", time_period = 2023), "geo_level must be one of")
  expect_warning(
    expect_error(
      testthat::with_mocked_bindings(load_pam(dataset = 839, geo_level = "state", time_period = 1990), sidra_download = function(...) stop("captured"), .package = "datazoom.amazonia"),
      "captured"
    ),
    "time_period must be in 2003-2025" # table 839 is the multi-harvest corn table: 2003 on
  )
})

test_that("before the fix every number stopped on `sidra_info`: no number reaches that error any more", {
  for (n in c(1612, 1613, 5457, 839, 1000, 1001, 1002, 74, 99999)) {
    seen <- NULL
    testthat::local_mocked_bindings(
      sidra_download = function(...) {
        seen <<- list(...)
        stop("captured")
      },
      .package = "datazoom.amazonia"
    )
    expect_error(suppressWarnings(load_pam(dataset = n, geo_level = "state", time_period = 2023, raw_data = TRUE)), "captured", info = n)
  }
})

# ---- sidra_dataset_of_table(), on a manifest made up for the purpose --------------

fake_manifest <- function(codes) {
  tibble::tibble(survey = "x", dataset = names(codes), sidra_code = unname(codes), available_time = NA_character_, available_geo = NA_character_, url = NA_character_)
}
dataset_of <- function(codes, table) {
  testthat::local_mocked_bindings(datasets_link = function(...) fake_manifest(codes), .package = "datazoom.amazonia")
  sidra_dataset_of_table("x", table)
}

test_that("sidra_dataset_of_table(): a table with one dataset is that dataset, whatever its category", {
  expect_equal(dataset_of(c(a = "10/c1/all", b = "20/c2/all"), 10), "a")
  expect_equal(dataset_of(c(a = "10/c1/all", b = "20/c2/all"), "20"), "b") # a number or its text
  expect_equal(dataset_of(c(a = "94", b = "95"), 94), "a") # a bare code: a table with no classification
  expect_equal(dataset_of(c(a = "10/c1/777", b = "20/c2/all"), 10), "a") # one single-category dataset is still the only one
})

test_that("sidra_dataset_of_table(): when several datasets read a table, the one that reads it whole", {
  codes <- c(whole = "10/c1/all", crop_a = "10/c1/5", crop_b = "10/c1/6", other = "20/c2/all")
  expect_equal(dataset_of(codes, 10), "whole")
  # a table's own total is a single category, not the whole table
  expect_equal(dataset_of(c(whole = "10/c1/all", total = "10/c1/0"), 10), "whole")
})

test_that("sidra_dataset_of_table(): no dataset, or no way to choose, is NULL", {
  expect_null(dataset_of(c(a = "10/c1/all"), 99))
  expect_null(dataset_of(c(a = "10/c1/all"), 1)) # 1 is not a prefix of 10
  expect_null(dataset_of(c(a = "10/c1/5", b = "10/c1/6"), 10)) # several, none whole
  expect_null(dataset_of(c(a = "10/c1/all", b = "10/c2/all"), 10)) # two whole-table datasets: ambiguous
  expect_null(dataset_of(c(a = NA_character_), 10)) # a dataset with no sidra_code
})

test_that("sidra_dataset_of_table() on the real manifest: PAM's seven tables, PPM's five, and nothing for a table of another survey", {
  expect_equal(vapply(PAM_WHOLE_TABLES, function(w) sidra_dataset_of_table("pam", w$table), ""), vapply(PAM_WHOLE_TABLES, function(w) w$dataset, ""))
  expect_null(sidra_dataset_of_table("pam", 74))
  expect_null(sidra_dataset_of_table("pam", 99999))
})

test_that("sidra_code_parts() splits a manifest code into what sidra_download() takes", {
  expect_equal(sidra_code_parts("1613/c82/45981"), list(table = 1613, classific = "c82", category = list("45981")))
  expect_equal(sidra_code_parts("5457/c782/all"), list(table = 5457, classific = "c782", category = list("all")))
  parts <- sidra_code_parts("94") # a bare code, a table with no classification (PPM)
  expect_equal(parts$table, 94)
  expect_null(parts$classific)
  expect_null(parts$category)
  expect_equal(names(parts), c("table", "classific", "category")) # NULL entries are kept, not dropped
})
