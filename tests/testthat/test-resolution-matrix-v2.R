# Guard for the "delete manifest base rows" migration (see R/manifest.R).
# resolution_matrix_keyed.rds was captured by
# data-raw/capture_resolution_matrix_v2.R against the tree AS IT STOOD
# immediately BEFORE that migration. For the 9 datasets that had a base row
# at capture time, it holds that base row's own blank-key query. Every one
# of them must now stop() instead of returning a value: a blank-key query
# that still works means the guard has a gap.
#
# 2026-11-13: the two value-equivalence tests that used to live here (every
# real-key entry byte-identical to the fixture; datasets_link() differing
# from its pre-migration snapshot in exactly N cells) were removed. They
# proved the migration preserved every lookup, which it did. After that they
# only failed on every legitimate manifest change (23 hand-logged deltas,
# then a PRODES version bump in #369 broke master). test-datasets_link.R
# pins the manifest's current values.

matrix <- readRDS(test_path("fixtures", "resolution_matrix_keyed.rds"))

KNOWN_BASE_ROW_DATASETS <- list(
  c("aneel", "energy_development_budget"),
  c("degrad", "degrad"),
  c("epe", "consumer_energy_consumption"),
  c("epe", "industrial_energy_consumption"),
  c("mapbiomas", "mapbiomas_cover"),
  c("mapbiomas", "mapbiomas_irrigation"),
  c("mapbiomas", "mapbiomas_mining"),
  c("mapbiomas", "mapbiomas_transition"),
  c("mapbiomas", "mapbiomas_water")
)
is_known_base_row_dataset <- function(s, d) {
  any(vapply(KNOWN_BASE_ROW_DATASETS, function(k) identical(k[1], s) && identical(k[2], d), logical(1)))
}

test_that("every former base-row query now stops -- the guard has no gap", {
  base_row_entries <- matrix[is.na(matrix$geo_level) & is.na(matrix$year) & mapply(is_known_base_row_dataset, matrix$survey, matrix$dataset), ]
  # 9 datasets x 10 value columns
  expect_equal(nrow(base_row_entries), 90)

  still_works <- character(0)
  for (i in seq_len(nrow(base_row_entries))) {
    row <- base_row_entries[i, ]
    ok <- tryCatch({
      dataset_field(row$survey, row$dataset, row$field, geo_level = NULL, year = NULL)
      TRUE
    }, error = function(e) FALSE)
    if (ok) still_works <- c(still_works, paste(row$survey, row$dataset, row$field))
  }

  expect_equal(still_works, character(0))
})
