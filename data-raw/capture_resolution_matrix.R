# data-raw/capture_resolution_matrix.R
#
# Captures the OLD 5-tier resolver's answer for every (survey, dataset,
# geo_level, year, field) combination reachable from real R/ code, run
# against the manifest AS IT STOOD before the "self-sufficient rows" +
# exact-key-match migration (see the plan at
# how-come-the-manifest-witty-nest.md). This is the behavior-preservation
# golden for that migration: tests/testthat/test-resolution-matrix.R asserts
# the new flat lookup reproduces every value captured here EXACTLY, with no
# case_when patches -- any mismatch is a real regression, not something to
# document away.
#
# Run this ONCE, before touching R/manifest.R or the CSV, from the package
# root:
#   Rscript data-raw/capture_resolution_matrix.R
# It writes tests/testthat/fixtures/resolution_matrix_tiered.rds.
#
# Query set: for every (survey, dataset) pair, every combination of
#   - geo_level in { NA, every geo_level value the dataset has an override
#     row for } -- PLUS three geo_level values that have no override row
#     today but are being added as real rows in the migration (see the plan's
#     "3 missing rows" table): epe/consumer_energy_consumption/region,
#     epe/industrial_energy_consumption/region, mapbiomas_cover/municipality.
#     These currently resolve via tier-2-miss fallthrough to the base row;
#     capturing them here proves the new explicit row (populated by copying
#     the base row's values, per the migration script) resolves identically.
#   - year in { NA, every year value the dataset has an override row for }
# crossed against every value column. geo_level and year are never combined
# (no current dataset has both kinds of override), matching how tiers 1-3
# are actually exercised today.

devtools::load_all(".", quiet = TRUE)

options(datazoom.amazonia.use_remote_manifest = FALSE)
datazoom.amazonia:::clear_manifest_cache()

tbl <- datazoom.amazonia:::link_table()

VALUE_COLS <- datazoom.amazonia:::MANIFEST_VALUE_COLS

ds_keys <- unique(tbl[!is.na(tbl$dataset), c("survey", "dataset")])

# Extra (survey, dataset, geo_level) triples with no override row today but
# that the migration adds as explicit rows -- see header comment above.
extra_geo <- data.frame(
  survey = c("epe", "epe", "mapbiomas"),
  dataset = c("consumer_energy_consumption", "industrial_energy_consumption", "mapbiomas_cover"),
  geo_level = c("region", "region", "municipality"),
  stringsAsFactors = FALSE
)

`%||%` <- function(x, y) if (is.null(x)) y else x

rows <- list()
idx <- 1

for (i in seq_len(nrow(ds_keys))) {
  survey <- ds_keys$survey[i]
  dataset <- ds_keys$dataset[i]

  ds_rows <- tbl[tbl$survey == survey & !is.na(tbl$dataset) & tbl$dataset == dataset, ]
  geo_vals <- unique(ds_rows$geo_level[!is.na(ds_rows$geo_level)])
  year_vals <- unique(ds_rows$year[!is.na(ds_rows$year)])

  extra <- extra_geo[extra_geo$survey == survey & extra_geo$dataset == dataset, "geo_level"]
  geo_vals <- unique(c(geo_vals, extra))

  # base (no geo_level, no year)
  queries <- list(list(geo_level = NULL, year = NULL))
  for (g in geo_vals) queries[[length(queries) + 1]] <- list(geo_level = g, year = NULL)
  for (y in year_vals) queries[[length(queries) + 1]] <- list(geo_level = NULL, year = y)

  for (q in queries) {
    for (field in VALUE_COLS) {
      val <- datazoom.amazonia:::dataset_field(
        survey, dataset, field,
        geo_level = q$geo_level, year = q$year
      )
      rows[[idx]] <- data.frame(
        survey = survey, dataset = dataset,
        geo_level = q$geo_level %||% NA_character_,
        year = q$year %||% NA_character_,
        field = field, value = val,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

matrix <- do.call(rbind, rows)
rownames(matrix) <- NULL

cat("Captured", nrow(matrix), "resolution results across",
    nrow(ds_keys), "(survey, dataset) pairs.\n")

saveRDS(matrix, "tests/testthat/fixtures/resolution_matrix_tiered.rds")
cat("Wrote tests/testthat/fixtures/resolution_matrix_tiered.rds\n")
