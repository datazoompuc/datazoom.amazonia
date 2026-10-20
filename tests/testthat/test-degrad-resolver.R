# tests/testthat/test-degrad-resolver.R
#
# Offline test for actions/scrapers/resolve_degrad.R. The resolver itself
# talks to www.obt.inpe.br (verified live, see that file's header: no Range
# support, HEAD reports Content-Length 0, so it reads the headers of a plain
# GET instead) -- what is testable without network is the pure verdict
# function, the guard clauses that fire before any request, and the
# consistency between the resolver's size table and the manifest's DEGRAD
# rows.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scrapers", "resolve_degrad.R"), local = TRUE)

test_that("a zip of the expected size passes", {
  expect_identical(
    degrad_header_problem(200L, "application/zip", 31212755, expected_bytes = 31212755),
    character(0)
  )
  # inside the +/-20% window, both sides
  expect_identical(degrad_header_problem(200L, "application/zip", 1.19e7, expected_bytes = 1e7), character(0))
  expect_identical(degrad_header_problem(200L, "application/zip", 0.81e7, expected_bytes = 1e7), character(0))
})

test_that("a size outside the tolerance is reported, with both numbers", {
  too_big <- degrad_header_problem(200L, "application/zip", 1.25e7, expected_bytes = 1e7)
  expect_length(too_big, 1)
  expect_match(too_big, "12500000")
  expect_match(too_big, "10000000")
  expect_length(degrad_header_problem(200L, "application/zip", 0.7e7, expected_bytes = 1e7), 1)
  # tolerance is a parameter, not a constant baked into the logic
  expect_length(degrad_header_problem(200L, "application/zip", 1.05e7, expected_bytes = 1e7, tolerance = 0.01), 1)
})

test_that("anything but HTTP 200 is a problem", {
  expect_match(degrad_header_problem(404L, "application/zip", 1e7, expected_bytes = 1e7), "HTTP 404")
  expect_match(degrad_header_problem(500L, "text/html", NA_real_, expected_bytes = 1e7), "HTTP 500")
  expect_length(degrad_header_problem(NA_integer_, "application/zip", 1e7, expected_bytes = 1e7), 1)
})

test_that("an HTML page served with HTTP 200 is caught by Content-Type", {
  # This is what www.obt.inpe.br actually returns for a moved/removed file's
  # parent page: 200, text/html, and no Content-Length at all (chunked) --
  # curl hands that back as numeric(0).
  html <- degrad_header_problem(200L, "text/html;charset=utf-8", numeric(0), expected_bytes = 1e7)
  expect_length(html, 1)
  expect_match(html, "not a zip")
  expect_match(degrad_header_problem(200L, NA_character_, 1e7, expected_bytes = 1e7), "missing")
  expect_length(degrad_header_problem(200L, NULL, 1e7, expected_bytes = 1e7), 1)
})

test_that("a missing or zero Content-Length is a problem, never a pass", {
  # HEAD on this server answers 200 + Content-Length: 0 (see the resolver's
  # header) -- a zero must not be mistaken for a valid size.
  expect_match(degrad_header_problem(200L, "application/zip", 0, expected_bytes = 1e7), "Content-Length")
  expect_match(degrad_header_problem(200L, "application/zip", numeric(0), expected_bytes = 1e7), "Content-Length")
  expect_match(degrad_header_problem(200L, "application/zip", NA_real_, expected_bytes = 1e7), "Content-Length")
})

test_that("DEGRAD_EXPECTED_BYTES covers exactly the manifest's degrad years", {
  manifest_years <- sort(link_table()$year[link_table()$survey == "degrad"])
  expect_equal(sort(names(DEGRAD_EXPECTED_BYTES)), manifest_years)
  expect_true(all(DEGRAD_EXPECTED_BYTES > 1e6)) # a few MB each, never a placeholder
})

test_that("the resolver refuses unverifiable input before touching the network", {
  skip_if_not_installed("curl")
  rows <- tibble::tibble(
    survey = "degrad", dataset = "degrad", geo_level = NA_character_, year = "2016",
    url = "http://example.invalid/degrad$year$_final_shp.zip", archive_file = "DEGRAD_2016_pol.shp"
  )

  expect_error(resolve_degrad(NULL), "no manifest rows")
  expect_error(resolve_degrad(rows[0, ]), "no manifest rows")
  expect_error(resolve_degrad(dplyr::mutate(rows, year = NA_character_)), "no year")
  expect_error(resolve_degrad(dplyr::mutate(rows, url = NA_character_)), "existing url")
  expect_error(resolve_degrad(dplyr::mutate(rows, archive_file = "")), "archive_file")
  # an 11th year can't be verified without a recorded size -- fail loudly
  expect_error(resolve_degrad(dplyr::mutate(rows, year = "2017")), "no expected file size.*2017")
})

test_that("resolve_degrad.R registers exactly one resolver in build_manifest.R's registry", {
  # build_manifest.R treats EVERY top-level binding named resolve_* as a
  # resolver, so this file's helpers must not use that prefix.
  # (ls() inside test_that() would list the test block's own frame, so look
  # at the environment resolve_degrad.R was source()'d into instead.)
  src_env <- environment(resolve_degrad)
  expect_identical(grep("^resolve_", ls(src_env), value = TRUE), "resolve_degrad")
  expect_identical(grep("^watch_", ls(src_env), value = TRUE), character(0))
})
