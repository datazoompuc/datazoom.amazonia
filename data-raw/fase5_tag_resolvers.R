# One-off script: tags manifest rows with the `resolver` column, marking
# which scraper (if any) is meant to keep that row fresh. This column is
# read only by actions/scripts/build_manifest.R (to decide which rows a
# resolver's output is allowed to touch) -- it is never read by the
# package's runtime code.
#
# Run once, by hand, from the package root:
#
#   Rscript data-raw/fase5_tag_resolvers.R

library(dplyr)
library(readr)

manifest_path <- "inst/extdata/manifest/v1/datasets_link.csv"

manifest <- read_csv(
  manifest_path,
  col_types = cols(.default = col_character()),
  na = c("", "NA")
)

manifest <- manifest %>%
  mutate(resolver = case_when(
    # ANEEL: CDE (all 7 rows: 6 years + base) + energy_enterprises_distributed base.
    # energy_generation (SIGA) stays NA -- version-free, HTTP-check only.
    survey == "aneel" & dataset %in% c("energy_development_budget", "energy_enterprises_distributed") ~ "aneel",

    # BACI: single base row.
    survey == "baci" & dataset == "HS92" ~ "baci",

    # EPE: national_energy_balance + energy_state_panel base rows.
    # consumer/industrial_energy_consumption stay NA -- version-free file.
    survey == "epe" & dataset %in% c("national_energy_balance", "energy_state_panel") ~ "epe",

    # MapBiomas: cover (base + indigenous_land), transition (base + biome),
    # deforestation_regeneration (base), mining (all 3 rows). irrigation/
    # water/fire stay NA -- legacy S3 bucket or unchanged GCS collection,
    # not actively resolved by resolve_mapbiomas() (see its file header).
    survey == "mapbiomas" & dataset == "mapbiomas_cover" & geo_level %in% c(NA, "indigenous_land") ~ "mapbiomas",
    survey == "mapbiomas" & dataset == "mapbiomas_transition" & geo_level %in% c(NA, "biome") ~ "mapbiomas",
    survey == "mapbiomas" & dataset == "mapbiomas_deforestation_regeneration" ~ "mapbiomas",
    survey == "mapbiomas" & dataset == "mapbiomas_mining" ~ "mapbiomas",

    # PRODES: all 6 datasets share the one base link. Tagged even though
    # resolve_prodes() currently always fails (see its file header) -- this
    # makes the break visible on every run instead of silently doing nothing.
    survey == "prodes" ~ "prodes",

    TRUE ~ resolver
  ))

write_csv(manifest, manifest_path, na = "")

cat("Resolver tag counts:\n")
print(table(manifest$resolver, useNA = "ifany"))
