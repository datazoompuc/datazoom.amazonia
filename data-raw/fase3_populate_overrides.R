# One-off script: populates the geo_level/year override rows and the
# archive_file column in inst/extdata/manifest/v1/datasets_link.csv,
# absorbing what used to be hardcoded branches inside external_download()
# (R/download.R): the MapBiomas geo_level overrides, the ANEEL CDE
# per-year sentinel/UUID map, the BACI inner-file version stamp (which used
# to be duplicated between the URL and a separate regex -- this script puts
# both in the SAME manifest row), the DETER per-dataset shapefile name, and
# the DEGRAD per-year shapefile name (previously a hardcoded table in
# R/degrad.R).
#
# Run once, by hand, from the package root:
#
#   Rscript data-raw/fase3_populate_overrides.R
#
# Not part of the package build (data-raw/ is .Rbuildignore'd).

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

blank_row <- function(...) {
  row <- as.list(rep(NA_character_, length(MANIFEST_COLS)))
  names(row) <- MANIFEST_COLS
  overrides <- list(...)
  row[names(overrides)] <- overrides
  as_tibble(row)
}

# ---- 1. MapBiomas geo_level overrides ---------------------------------
# Was: download.R "if (source == 'mapbiomas') { if (dataset == ...) { if
# (geo_level == ...) path <- <hardcoded literal> } }"

mapbiomas_overrides <- bind_rows(
  blank_row(
    survey = "mapbiomas", dataset = "mapbiomas_cover", geo_level = "indigenous_land",
    link = "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-INDIGENOUS_LANDS-1.xlsx"
  ),
  blank_row(
    survey = "mapbiomas", dataset = "mapbiomas_transition", geo_level = "biome",
    link = "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-BIOMES.xlsx"
  ),
  blank_row(
    survey = "mapbiomas", dataset = "mapbiomas_transition", geo_level = "municipality",
    link = "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_state_municipality.xlsx"
  )
)

# ---- 2. ANEEL CDE per-year overrides -----------------------------------
# Was: download.R "if (identical(path, 'aneel_cde_$year$')) { cde_urls <-
# c(...) ... }". The base row's link is cleared to NA since it is no
# longer a real (or sentinel) URL -- every valid year is now its own row.

cde_urls <- c(
  "2017" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/684a68fd-4278-4af1-bcf2-c02810dd7c0c/download/cde-beneficiarios-rede-basica-2017.csv",
  "2018" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/237f3f67-4795-4bd7-a4d2-5f6071ef39af/download/cde-beneficiarios-rede-basica-2018.csv",
  "2019" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/0bd1f129-39c5-4edc-b272-3f1d11181cca/download/cde-beneficiarios-rede-basica-2019.csv",
  "2020" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/f77bb713-cc06-406a-baa8-246acc357ff5/download/cde-beneficiarios-rede-basica-2020.csv",
  "2021" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/cdf1b068-7c76-462a-ab57-d6619ad290fa/download/cde-beneficiarios-rede-basica-2021.csv",
  "2022" = "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/e390baae-5304-4a94-854f-0905094b3357/download/cde-beneficiarios-rede-basica-2022.csv"
)

aneel_overrides <- bind_rows(lapply(names(cde_urls), function(y) {
  blank_row(
    survey = "aneel", dataset = "energy_development_budget", year = y,
    link = unname(cde_urls[y])
  )
}))

manifest <- manifest %>%
  mutate(link = if_else(
    survey == "aneel" & dataset == "energy_development_budget" &
      is.na(geo_level) & is.na(year),
    NA_character_,
    link
  ))

# ---- 3. BACI archive_file (on the existing base row) -------------------
# Was: download.R 'file_expression <- paste0("*", param$year,
# "_V202601.csv")' -- duplicated the URL's version stamp in a second place.
# Now both live in the same row: link keeps "_V202601.zip", archive_file
# keeps "_V202601.csv", so a resolver only ever has to update one row.

manifest <- manifest %>%
  mutate(archive_file = if_else(
    survey == "baci" & dataset == "HS92" & is.na(geo_level) & is.na(year),
    "*$year$_V202601.csv",
    archive_file
  ))

# ---- 4. DETER archive_file (on the existing base rows) ------------------
# Was: download.R "if (param$dataset == 'deter_amz') { ... } if (param$dataset
# == 'deter_cerrado') { ... }" picking the shapefile name inside the zip.

manifest <- manifest %>%
  mutate(archive_file = case_when(
    survey == "deter" & dataset == "deter_amz" & is.na(geo_level) & is.na(year) ~ "deter-amz-deter-public.shp",
    survey == "deter" & dataset == "deter_cerrado" & is.na(geo_level) & is.na(year) ~ "deter_public.shp",
    TRUE ~ archive_file
  ))

# ---- 5. DEGRAD archive_file, one row per year --------------------------
# Was: R/degrad.R's own hardcoded `file_list` table (dplyr::recode). Every
# new row repeats the base row's $year$-templated link so dataset_url()
# resolves to the identical URL whichever row (year-specific or base) is
# matched -- only archive_file actually varies by year.

degrad_base <- manifest %>%
  filter(survey == "degrad", dataset == "degrad", is.na(geo_level), is.na(year))
stopifnot(nrow(degrad_base) == 1)

degrad_files <- c(
  "2007" = "Degrad2007_Final_pol.shp",
  "2008" = "Degrad2008_Final_pol.shp",
  "2009" = "Degrad2009_Final_pol.shp",
  "2010" = "DEGRAD_2010_UF_pol.shp",
  "2011" = "DEGRAD_2011_INPE_pol.shp",
  "2012" = "DEGRAD_2012_INPE_pol.shp",
  "2013" = "DEGRAD_2013_INPE_pol.shp",
  "2014" = "DEGRAD_2014_pol.shp",
  "2015" = "DEGRAD_2015.shp",
  "2016" = "DEGRAD_2016_pol.shp"
)

degrad_overrides <- bind_rows(lapply(names(degrad_files), function(y) {
  row <- degrad_base
  row$year <- y
  row$archive_file <- unname(degrad_files[y])
  row
}))

# ---- Assemble and write --------------------------------------------------

manifest <- bind_rows(manifest, mapbiomas_overrides, aneel_overrides, degrad_overrides) %>%
  select(all_of(MANIFEST_COLS)) %>%
  arrange(survey, dataset, geo_level, year)

# sanity checks before writing
stopifnot(
  !any(duplicated(manifest[, c("survey", "dataset", "geo_level", "year")])),
  sum(manifest$survey == "mapbiomas" & is.na(manifest$geo_level) == FALSE) == 3,
  sum(manifest$survey == "aneel" & manifest$dataset == "energy_development_budget" & !is.na(manifest$year)) == 6,
  sum(manifest$survey == "degrad" & !is.na(manifest$year)) == 10
)

write_csv(manifest, manifest_path, na = "")
cat("Wrote", nrow(manifest), "rows to", manifest_path, "\n")
