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
# Two further adjustments mirror the deliberate, documented changes also
# covered in test-datasets_link.R:
#   1. available_geo is now stored lowercase.
#   2. SIDRA rows' landing page moved from url to the new docs_url column;
#      url is genuinely NA for them now.

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
