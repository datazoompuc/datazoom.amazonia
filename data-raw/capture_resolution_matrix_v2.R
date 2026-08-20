# data-raw/capture_resolution_matrix_v2.R
#
# Captures dataset_field()'s answer for every (survey, dataset, geo_level,
# year, field) combination that has a REAL manifest row backing it, run
# against the tree AS IT STOOD before the base-row-deletion migration (see
# the plan at ok-claude-here-is-iterative-allen-baserows.md). This is the
# behavior-preservation golden for that migration:
# tests/testthat/test-resolution-matrix-v2.R asserts the post-migration
# dataset_field()/dataset_meta() reproduce every value here EXACTLY, zero
# patches -- any mismatch is a real regression.
#
# Unlike the ORIGINAL capture_resolution_matrix.R (kept for provenance, see
# its own header + test-resolution-matrix.R's retirement note), this script
# deliberately does NOT query the blank-key (geo_level=NULL, year=NULL)
# combination for a dataset that has real override rows -- that query is
# exactly what the migration makes illegal (dataset_field() now stop()s on
# it instead of silently falling through to a base row that no longer
# exists). Only every dataset's REAL keys are queried: for an unkeyed
# dataset (single manifest row) that's the blank key itself; for a keyed
# dataset it's each of its rows' actual (geo_level XOR year).
#
# Also captures a full snapshot of datasets_link()'s (effective_table()'s
# public-facing) output, separately -- the base-row deletion is EXPECTED to
# change some of its cells (see the plan's "real-world effect" section), so
# that snapshot is compared as an explicit, enumerated delta in the test,
# not a zero-diff.
#
# Run this ONCE, before touching R/manifest.R, the CSV, or any call site,
# from the package root:
#   Rscript data-raw/capture_resolution_matrix_v2.R
# Writes tests/testthat/fixtures/resolution_matrix_keyed.rds and
# tests/testthat/fixtures/datasets_link_pre_baserow_deletion.rds.

devtools::load_all(".", quiet = TRUE)

options(datazoom.amazonia.use_remote_manifest = FALSE)
datazoom.amazonia:::clear_manifest_cache()

tbl <- datazoom.amazonia:::link_table()

VALUE_COLS <- datazoom.amazonia:::MANIFEST_VALUE_COLS

ds_keys <- unique(tbl[!is.na(tbl$dataset), c("survey", "dataset")])

`%||%` <- function(x, y) if (is.null(x)) y else x

rows <- list()
idx <- 1

for (i in seq_len(nrow(ds_keys))) {
  survey <- ds_keys$survey[i]
  dataset <- ds_keys$dataset[i]

  ds_rows <- tbl[tbl$survey == survey & !is.na(tbl$dataset) & tbl$dataset == dataset, ]

  # One query per REAL row of this dataset -- its actual (geo_level, year),
  # which for an unkeyed (single-row) dataset is simply (NA, NA).
  for (r in seq_len(nrow(ds_rows))) {
    g <- ds_rows$geo_level[r]
    y <- ds_rows$year[r]
    for (field in VALUE_COLS) {
      val <- datazoom.amazonia:::dataset_field(
        survey, dataset, field,
        geo_level = if (is.na(g)) NULL else g,
        year = if (is.na(y)) NULL else y
      )
      rows[[idx]] <- data.frame(
        survey = survey, dataset = dataset,
        geo_level = g, year = y,
        field = field, value = val,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
}

matrix <- do.call(rbind, rows)
rownames(matrix) <- NULL

cat("Captured", nrow(matrix), "resolution results across",
    nrow(ds_keys), "(survey, dataset) pairs (", sum(vapply(seq_len(nrow(ds_keys)), function(i) {
      nrow(tbl[tbl$survey == ds_keys$survey[i] & !is.na(tbl$dataset) & tbl$dataset == ds_keys$dataset[i], ])
    }, integer(1))), "real rows).\n")

saveRDS(matrix, "tests/testthat/fixtures/resolution_matrix_keyed.rds")
cat("Wrote tests/testthat/fixtures/resolution_matrix_keyed.rds\n")

# ---- datasets_link() snapshot -----------------------------------------------

snapshot <- datazoom.amazonia:::datasets_link()
cat("Captured datasets_link() snapshot:", nrow(snapshot), "rows x", ncol(snapshot), "cols.\n")

saveRDS(snapshot, "tests/testthat/fixtures/datasets_link_pre_baserow_deletion.rds")
cat("Wrote tests/testthat/fixtures/datasets_link_pre_baserow_deletion.rds\n")
