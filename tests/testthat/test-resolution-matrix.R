# Golden regression for the schema normalization itself (5-tier field
# coalescing -- see R/manifest.R and the "Normalizar o schema do manifest"
# plan). The fixture was captured from the OLD, pre-normalization manifest
# and OLD (row-based) dataset_field() -- for every (survey, dataset,
# geo_level, year) key that existed in the old manifest, and every value
# field, labelled with the field's NEW name (link -> url, collection ->
# version; see data-raw/capture_resolution_matrix.R).
#
# Only REACHABLE combinations are checked -- i.e. the ones real R/ code
# actually queries:
#   - url is queried with both geo_level and year (download.R).
#   - archive_file is queried with year (degrad.R) and with neither
#     (deter/baci, via download.R).
#   - sheet is queried with geo_level (mapbiomas.R, epe.R consumer/
#     industrial) and with neither (epe.R energy_state_panel).
#   - layer_name (prodes.R) and version (mapbiomas.R) are ALWAYS queried
#     with neither geo_level nor year -- unlike sheet/archive_file, no
#     caller ever asks for a per-override layer_name/version.
#   - sidra_code/available_time/available_geo/resolver are ONLY ever
#     queried at the base tier (geo_level = year = NA), via
#     datasets_link()/effective_table() -- never per-override.
#
# The excluded combinations are exactly where OLD and NEW are EXPECTED to
# differ: the old row-based dataset_row() returned the first matching row
# and plucked the field from it even when that field was NA on that row,
# with no further fallthrough. That inability to fall through past a
# "found a row, but this cell is blank" tier is the root cause section of
# the plan (R/manifest.R's dataset_row() docs) identifies as forcing
# DEGRAD/ANEEL/MapBiomas into inconsistent, duplicated encodings in the
# first place -- the new tiered dataset_field() fixes exactly that, so a
# hypothetical query no real caller makes (e.g. "available_time for
# ANEEL's 2022 override row") is SUPPOSED to resolve differently now.
#
# Further adjustments mirror deliberate, documented changes also covered in
# test-datasets_link.R:
#   1. available_geo is now stored lowercase.
#   2. SIDRA rows' landing page moved from url to the new docs_url column;
#      url is genuinely NA for them now.
#   3. epe/national_energy_balance's url/available_time/sheet were updated
#      2026-08-04 when its source moved to EPE's consolidated BEN table
#      (see R/epe.R's header and actions/scrapers/resolve_epe.R). Because
#      this dataset has no geo_level/year overrides, EVERY field on its
#      base row is included below via the `is.na(geo_level) & is.na(year)`
#      clause, not just the three OVERRIDE_QUERIED_FIELDS -- hence sheet
#      needs an adjustment here too, even though sheet is otherwise only
#      reachable at the base tier for this particular dataset.
#   4. epe/energy_state_panel gained a docs_url (pointing at the Anuario
#      publication page, after its old URL was found to 404 live) and lost
#      its resolver tag (no working replacement was found, so it reverted
#      to hand-maintained -- see resolve_epe.R's header for what was
#      searched). Same reasoning as #3: this dataset has no overrides
#      either, so both fields are in scope here.
#   5. epe/consumer_energy_consumption and industrial_energy_consumption
#      gained a resolver tag ("epe") now that resolve_epe.R actively
#      maintains their url. Their base rows (geo_level = year = NA) are
#      likewise always in scope regardless of OVERRIDE_QUERIED_FIELDS.
#   6. 2026-08-07: the manifest was regenerated for real (the previous
#      committed copy had never actually been refreshed since a09670c --
#      see build_manifest.R's OUT_DIR/RUNNER_TEMP fix and the "Stage Tier B
#      candidate" workflow step). PRODES's 6 dataset rows (url, version,
#      layer_name, available_time) all move together to TerraBrasilis' live
#      filename, now year + publish-date stamped (see resolve_prodes.R).
#   7. ANEEL: energy_development_budget's available_time widens to include
#      2023/2024 (published after the committed range was written); the
#      enterprises_distributed resource silently changed extension from
#      .csv to .zip on ANEEL's own server (this was already a known, live
#      finding from a09670c's own resolver testing -- see NEWS.md/the branch
#      report's bugs section -- just never landed in the committed manifest
#      until this regeneration actually staged it).
#   8. MapBiomas mining bumped from collection 8 to 9 (both override-row
#      urls and the base row's version); the base row's own url is
#      deliberately left at the old COL8.0 file, since resolve_mapbiomas.R
#      only ever writes url onto mining's geo_level-specific override rows,
#      never its base row (see that file's mining_base tibble).
#   9. BACI gained a version (its committed row never had one before this
#      resolver actually ran for real).

matrix <- readRDS(test_path("fixtures", "resolution_matrix_pre.rds"))

OVERRIDE_QUERIED_FIELDS <- c("url", "archive_file", "sheet")

sidra_code_by_key <- matrix %>%
  dplyr::filter(field == "sidra_code") %>%
  dplyr::transmute(survey, dataset, geo_level, year, sidra_code = value)

reachable <- matrix %>%
  dplyr::left_join(sidra_code_by_key, by = c("survey", "dataset", "geo_level", "year")) %>%
  dplyr::filter(field %in% OVERRIDE_QUERIED_FIELDS | (is.na(geo_level) & is.na(year))) %>%
  dplyr::mutate(
    value = dplyr::case_when(
      field == "available_geo" ~ tolower(value),
      field == "url" & !is.na(sidra_code) ~ NA_character_,
      survey == "epe" & dataset == "national_energy_balance" & field == "url" ~
        "https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/dados/tabela_balanco_energitico_consolidado.xlsx",
      survey == "epe" & dataset == "national_energy_balance" & field == "available_time" ~ "1970-2025",
      survey == "epe" & dataset == "national_energy_balance" & field == "sheet" ~ "Sheet1",
      survey == "epe" & dataset == "energy_state_panel" & field == "docs_url" ~
        "https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/anuario-estatistico-de-energia-eletrica",
      survey == "epe" & dataset == "energy_state_panel" & field == "resolver" ~ NA_character_,
      survey == "epe" & dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption") & field == "resolver" ~ "epe",
      survey == "prodes" & field == "url" ~
        "https://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/raster/prodes_amazonia_legal_2025_v20260408.zip",
      survey == "prodes" & field == "version" ~ "20260408",
      survey == "prodes" & field == "layer_name" ~ "prodes_amazonia_legal_2025_v20260408",
      survey == "prodes" & dataset == "deforestation" & field == "available_time" ~ "2007-2025",
      survey == "prodes" & dataset == "residual_deforestation" & field == "available_time" ~ "2010-2025",
      survey == "prodes" & field == "available_time" ~ "2025",
      survey == "aneel" & dataset == "energy_development_budget" & field == "available_time" ~ "2017-2024",
      survey == "aneel" & dataset == "energy_enterprises_distributed" & field == "url" ~
        "https://dadosabertos.aneel.gov.br/dataset/5e0fafd2-21b9-4d5b-b622-40438d40aba2/resource/b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/empreendimento-geracao-distribuida.zip",
      survey == "mapbiomas" & dataset == "mapbiomas_mining" & field == "url" & !is.na(geo_level) ~
        "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2025/03/TABELA-MINERACAO-MAPBIOMAS-COL9.0.xlsx",
      survey == "mapbiomas" & dataset == "mapbiomas_mining" & field == "version" ~ "9",
      survey == "baci" & dataset == "HS92" & field == "version" ~ "202601",
      TRUE ~ value
    )
  )

test_that("every reachable pre-normalization resolution result is reproduced exactly", {
  actual <- mapply(
    function(survey, dataset, geo_level, year, field) {
      dataset_field(survey, dataset, field, geo_level = geo_level, year = year)
    },
    reachable$survey, reachable$dataset, reachable$geo_level, reachable$year, reachable$field
  )

  expect_equal(unname(actual), reachable$value)
})

test_that("the resolution matrix fixture actually covers something (sanity check on the fixture itself)", {
  expect_equal(nrow(matrix), 1512)
  expect_true(nrow(reachable) > 0)
  expect_true(all(c("url", "version") %in% reachable$field))
  # some non-NA values must exist for each renamed field, or the rename
  # itself would go untested
  expect_true(any(!is.na(reachable$value[reachable$field == "url"])))
  expect_true(any(!is.na(reachable$value[reachable$field == "version"])))
})
