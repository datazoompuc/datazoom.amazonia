# One-off script: seeds inst/extdata/manifest/v1/datasets_link.csv from the
# CURRENT datasets_link() tribble in R/download.R, so that Fase 1 of the
# manifest migration is a pure no-op refactor: the manifest starts out
# containing exactly what the hardcoded table contained, plus the new
# (all-NA) columns that later phases will populate.
#
# This script is not part of the package build (data-raw/ is .Rbuildignore'd)
# and is meant to be run once, by hand, from the package root:
#
#   Rscript data-raw/seed_manifest.R
#
# Re-running it would blow away any manual edits made to the manifest after
# Fase 1, so it should not be wired into any automated pipeline.

devtools::load_all(".", quiet = TRUE)

base <- datazoom.amazonia:::datasets_link()

manifest <- base %>%
  dplyr::mutate(
    geo_level   = NA_character_,
    year        = NA_character_,
    archive_file = NA_character_,
    sheet       = NA_character_,
    collection  = NA_character_,
    layer_name  = NA_character_,
    resolver    = NA_character_
  ) %>%
  dplyr::select(
    survey, dataset, geo_level, year,
    sidra_code, available_time, available_geo, link,
    archive_file, sheet, collection, layer_name, resolver
  )

stopifnot(nrow(manifest) == nrow(base))

out_dir <- "inst/extdata/manifest/v1"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(manifest, file.path(out_dir, "datasets_link.csv"), na = "")

cat("Wrote", nrow(manifest), "rows to", file.path(out_dir, "datasets_link.csv"), "\n")
