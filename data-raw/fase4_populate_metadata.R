# One-off script: populates layer_name (PRODES), sheet + collection
# (MapBiomas), and sheet (EPE energy_state_panel) in the manifest, so that
# R/prodes.R, R/mapbiomas.R and R/epe.R can read these values instead of
# hardcoding them -- decoupling the code from the version tokens that
# actually churn (PRODES year, MapBiomas collection number).
#
# IMPORTANT gotcha this script takes care of: whenever a geo_level-specific
# override row already exists for a dataset (e.g. mapbiomas_cover /
# indigenous_land), that row's OWN `sheet` value is used in preference to
# the base row's -- dataset_row()'s tiered lookup picks the most specific
# match first. So every override row below also gets its `link` copied
# from the base row when the dataset's URL does not actually vary by
# geo_level (irrigation, mining, water): otherwise dataset_url() would
# return the override row's (NA) link instead of falling back correctly.
#
# Run once, by hand, from the package root:
#
#   Rscript data-raw/fase4_populate_metadata.R

library(dplyr)
library(readr)
library(tibble)

manifest_path <- "inst/extdata/manifest/v1/datasets_link.csv"

manifest <- read_csv(
  manifest_path,
  col_types = cols(.default = col_character()),
  na = c("", "NA")
)

MANIFEST_COLS <- c(
  "survey", "dataset", "geo_level", "year", "sidra_code",
  "available_time", "available_geo", "link",
  "archive_file", "sheet", "collection", "layer_name", "resolver"
)

new_row_like_base <- function(m, ds, geo, sheet_val) {
  base <- m %>%
    filter(survey == "mapbiomas", dataset == ds, is.na(geo_level), is.na(year))
  stopifnot(nrow(base) == 1)
  base$geo_level <- geo
  base$sheet <- sheet_val
  # link/available_time/available_geo/sidra_code copied from the base row
  # as-is (the URL and validity windows do not vary by geo_level here --
  # only the sheet name inside the workbook does).
  base
}

# ---- 1. PRODES layer_name -----------------------------------------------
# Was: R/prodes.R hardcoded "prodes_amazonia_legal_2023" 4 times (the
# raster band name terra::extract() gives its output column, which comes
# from the downloaded file's own name).

manifest <- manifest %>%
  mutate(layer_name = if_else(
    survey == "prodes" & is.na(geo_level) & is.na(year),
    "prodes_amazonia_legal_2023",
    layer_name
  ))

# ---- 2. MapBiomas sheet + collection -------------------------------------
# Was: R/mapbiomas.R's `sheets` tribble (dataset x geo_level -> sheet name)
# and 5 separate `message("Data from MapBiomas - Collection N")` blocks.

collection_by_dataset <- c(
  mapbiomas_cover = "9",
  mapbiomas_transition = "9",
  mapbiomas_deforestation_regeneration = "9",
  mapbiomas_mining = "8",
  mapbiomas_irrigation = "7",
  mapbiomas_fire = "3",
  mapbiomas_water = "2"
)

manifest <- manifest %>%
  mutate(collection = if_else(
    survey == "mapbiomas" & is.na(geo_level) & is.na(year) & dataset %in% names(collection_by_dataset),
    collection_by_dataset[dataset],
    collection
  ))

# sheet on base rows (covers the "only one geo_level really matters" cases,
# and acts as the fallback for any geo_level without its own override row)
manifest <- manifest %>%
  mutate(sheet = case_when(
    survey == "mapbiomas" & dataset == "mapbiomas_cover" & is.na(geo_level) & is.na(year) ~ "COVERAGE_9",
    survey == "mapbiomas" & dataset == "mapbiomas_transition" & is.na(geo_level) & is.na(year) ~ "TRANSITION_9",
    survey == "mapbiomas" & dataset == "mapbiomas_deforestation_regeneration" & is.na(geo_level) & is.na(year) ~ "DEF_SECVEG",
    survey == "mapbiomas" & dataset == "mapbiomas_fire" & is.na(geo_level) & is.na(year) ~ "a_ANNUAL",
    TRUE ~ sheet
  ))

# sheet on the EXISTING geo_level override rows (mapbiomas_cover/
# indigenous_land, mapbiomas_transition/biome, mapbiomas_transition/
# municipality) -- these already carry their own `link` from Fase 3, they
# just need `sheet` added so they don't shadow the base row's value.
manifest <- manifest %>%
  mutate(sheet = case_when(
    survey == "mapbiomas" & dataset == "mapbiomas_cover" & geo_level == "indigenous_land" ~ "COVERAGE_9",
    survey == "mapbiomas" & dataset == "mapbiomas_transition" & geo_level == "biome" ~ "TRANSITION_9",
    survey == "mapbiomas" & dataset == "mapbiomas_transition" & geo_level == "municipality" ~ "TRANSITION_9",
    TRUE ~ sheet
  ))

# brand-new override rows for datasets whose sheet DOES vary by geo_level
# but had no existing override row (their URL is geo_level-invariant, so
# link is simply copied from the base row)
mapbiomas_new_rows <- bind_rows(
  new_row_like_base(manifest, "mapbiomas_irrigation", "state", "UF"),
  new_row_like_base(manifest, "mapbiomas_irrigation", "biome", "BIOME"),
  new_row_like_base(manifest, "mapbiomas_mining", "municipality", "CITY_STATE_BIOME"),
  new_row_like_base(manifest, "mapbiomas_mining", "indigenous_land", "IL"),
  new_row_like_base(manifest, "mapbiomas_water", "state", "states_annual"),
  new_row_like_base(manifest, "mapbiomas_water", "biome", "biomes_annual"),
  new_row_like_base(manifest, "mapbiomas_water", "municipality", "mun_annual")
)

# ---- 3. EPE energy_state_panel sheet -------------------------------------
# Was: R/epe.R hardcoded `sheets <- "8.1 part 3"`.

manifest <- manifest %>%
  mutate(sheet = if_else(
    survey == "epe" & dataset == "energy_state_panel" & is.na(geo_level) & is.na(year),
    "8.1 part 3",
    sheet
  ))

# ---- Assemble and write ---------------------------------------------------

manifest <- bind_rows(manifest, mapbiomas_new_rows) %>%
  select(all_of(MANIFEST_COLS)) %>%
  arrange(survey, dataset, geo_level, year)

stopifnot(
  !any(duplicated(manifest[, c("survey", "dataset", "geo_level", "year")])),
  sum(manifest$survey == "prodes" & !is.na(manifest$layer_name)) == 6,
  sum(manifest$survey == "mapbiomas" & !is.na(manifest$sheet)) == 14,
  sum(manifest$survey == "mapbiomas" & is.na(manifest$geo_level) & !is.na(manifest$collection)) == 7
)

write_csv(manifest, manifest_path, na = "")
cat("Wrote", nrow(manifest), "rows to", manifest_path, "\n")
