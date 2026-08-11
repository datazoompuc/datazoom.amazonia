# Behavior-preservation golden for the self-sufficient-rows migration (see
# R/manifest.R and data-raw/denormalize_manifest.R). The fixture
# (resolution_matrix_tiered.rds, captured by
# data-raw/capture_resolution_matrix.R) records what the OLD 5-tier
# dataset_field() answered for every (survey, dataset, geo_level, year,
# field) combination reachable from real R/ code, run against the manifest
# as it stood immediately BEFORE the migration.
#
# Unlike its predecessor (resolution_matrix_pre.rds / the old
# test-resolution-matrix.R), this fixture is captured from a script that
# actually exists and is re-runnable (data-raw/capture_resolution_matrix.R
# -- the previous fixture's cited generator, data-raw/
# capture_resolution_matrix.R, never actually existed in this repo despite
# being referenced by name).
#
# The assertion below has NO case_when patches. Every value captured here
# is expected to come back byte-identical from the new exact-key-match
# dataset_field() against the MIGRATED manifest -- that is the whole point
# of capturing it first: any mismatch is a real regression introduced by
# the migration, not a documented, deliberate difference.

matrix <- readRDS(test_path("fixtures", "resolution_matrix_tiered.rds"))

test_that("every captured pre-migration resolution result is reproduced exactly", {
  actual <- mapply(
    function(survey, dataset, geo_level, year, field) {
      dataset_field(survey, dataset, field, geo_level = geo_level, year = year)
    },
    matrix$survey, matrix$dataset, matrix$geo_level, matrix$year, matrix$field
  )

  expect_equal(unname(actual), matrix$value)
})

test_that("the resolution matrix fixture actually covers something (sanity check on the fixture itself)", {
  expect_true(nrow(matrix) > 1000)
  expect_true(all(c("url", "version") %in% matrix$field))
  # some non-NA values must exist for each renamed field, or the rename
  # itself would go untested
  expect_true(any(!is.na(matrix$value[matrix$field == "url"])))
  expect_true(any(!is.na(matrix$value[matrix$field == "version"])))
})
