# Behavior-preservation golden for the "delete manifest base rows"
# migration (see R/manifest.R). Two fixtures, both captured by
# data-raw/capture_resolution_matrix_v2.R against the tree AS IT STOOD
# immediately BEFORE this migration (CSV, R/manifest.R, every call site,
# and every validator all still on the old base-row design):
#
#   - resolution_matrix_keyed.rds: dataset_field()'s answer for every
#     (survey, dataset, geo_level, year, field) combination that has a
#     REAL manifest row backing it -- one query per row that existed at
#     capture time, crossed with all 10 value columns. This DOES include,
#     for the 9 datasets that had a base row at capture time (aneel/
#     energy_development_budget, degrad/degrad, epe/consumer_energy_
#     consumption, epe/industrial_energy_consumption, mapbiomas_cover,
#     mapbiomas_irrigation, mapbiomas_mining, mapbiomas_transition,
#     mapbiomas_water), that base row's own blank-key query -- captured
#     faithfully because it was a real row at the time. See KNOWN_BASE_ROW_DATASETS
#     below for how this test tells those entries apart from real-key ones.
#   - datasets_link_pre_baserow_deletion.rds: a full snapshot of
#     datasets_link()'s output before the migration.
#
# Two claims, verified separately:
#   1. Every REAL-KEY entry in resolution_matrix_keyed.rds must come back
#      byte-identical from the migrated dataset_field() -- zero patches.
#      This is the "did the migration preserve every real lookup" proof.
#   2. Every FORMER-BASE-ROW entry (blank key, for one of the 9 datasets
#      above) must now stop() instead of returning a value -- this is the
#      "did the new guard actually replace the old fallback, everywhere it
#      used to apply" proof. A silently-still-working blank-key query here
#      would mean the guard has a gap, which would be a real regression
#      hiding as a pass.
#
# datasets_link()'s delta against its own pre-migration snapshot is
# EXPECTED to differ in exactly 5 cells (not patched away, not zero-diffed
# away either) -- see test-datasets_link.R's delta #8 for the full
# explanation of which 5 and why. This file only asserts the row/column
# shape stays consistent and that delta count; test-datasets_link.R is the
# canonical place the specific cells are pinned.

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

test_that("every real-key entry in resolution_matrix_keyed.rds resolves byte-identically after the migration", {
  real_rows <- matrix[!(is.na(matrix$geo_level) & is.na(matrix$year) & mapply(is_known_base_row_dataset, matrix$survey, matrix$dataset)), ]
  expect_gt(nrow(real_rows), 0)

  mismatches <- character(0)
  for (i in seq_len(nrow(real_rows))) {
    row <- real_rows[i, ]
    g <- if (is.na(row$geo_level)) NULL else row$geo_level
    y <- if (is.na(row$year)) NULL else row$year
    new_val <- dataset_field(row$survey, row$dataset, row$field, geo_level = g, year = y)
    same <- (is.na(row$value) && is.na(new_val)) || (!is.na(row$value) && !is.na(new_val) && row$value == new_val)
    if (!isTRUE(same)) {
      mismatches <- c(mismatches, paste(row$survey, row$dataset, row$geo_level, row$year, row$field, "old=", row$value, "new=", new_val))
    }
  }

  expect_equal(mismatches, character(0))
})

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

test_that("datasets_link() keeps the same rows/columns and differs in exactly 5 cells (see test-datasets_link.R delta #8)", {
  old_snap <- readRDS(test_path("fixtures", "datasets_link_pre_baserow_deletion.rds"))
  new_snap <- datasets_link()

  expect_equal(nrow(old_snap), nrow(new_snap))
  expect_setequal(names(old_snap), names(new_snap))

  key_old <- paste(old_snap$survey, old_snap$dataset, sep = "\r")
  key_new <- paste(new_snap$survey, new_snap$dataset, sep = "\r")
  expect_setequal(key_old, key_new)

  cols <- setdiff(names(old_snap), c("survey", "dataset"))
  delta <- 0
  for (k in unique(key_old)) {
    o <- old_snap[key_old == k, ]
    n <- new_snap[key_new == k, ]
    for (col in cols) {
      ov <- o[[col]][1]
      nv <- n[[col]][1]
      same <- (is.na(ov) && is.na(nv)) || (!is.na(ov) && !is.na(nv) && ov == nv)
      if (!isTRUE(same)) delta <- delta + 1
    }
  }

  expect_equal(delta, 5)
})
