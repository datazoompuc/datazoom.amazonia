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
# EXPECTED to differ in exactly 5 cells from the base-row-deletion migration
# itself (not patched away, not zero-diffed away either) -- see
# test-datasets_link.R's delta #8 for the full explanation of which 5 and
# why -- PLUS 1 more cell from a later, unrelated fix (delta #9, 2026-08-24:
# mapbiomas_deforestation_regeneration's available_time corrected), PLUS 2
# more from delta #10 (2026-09-08: epe/energy_state_panel's url and
# available_time, migrated to the BEN dashboard's Chapter 8 table -- see
# test-datasets_link.R), PLUS 8 more from delta #12 (2026-09-14: all 8 IPS
# rows' url, resolve_ips.R stopped being detect-only -- see
# test-datasets_link.R), PLUS 12 more from delta #13 (2026-09-15: all 6
# SEEG rows' url AND available_time, resolve_seeg.R stopped being
# detect-only -- see test-datasets_link.R). 28 total, also enumerated
# explicitly below rather than assumed. This file only asserts the
# row/column shape stays consistent and the delta count; test-datasets_link.R
# is the canonical place the specific cells are pinned.
#
# Two other things changed the same general period but are deliberately NOT
# part of the "16 cells" count below: (a) 2026-09-08 (PRODES/BACI resolver
# audit, delta #11 in test-datasets_link.R), a brand-new row, baci_dic/
# product_codes, was added to the manifest (and removed again 2026-10-28, when
# load_baci_dic() moved to comex_dic/hs_codes); (b) 2026-09-14 (delta #12), IPS's
# version field changed alongside its url, but version isn't one of
# datasets_link()'s 6 output columns. Neither fits "same rows, different
# values in an existing output column" the way the 16 do, so (a) is asserted
# separately in the datasets_link() test at the bottom of this file instead
# of folded into the delta count, and (b) simply isn't visible to this
# fixture at all.
#
# Note this file's OWN matrix (resolution_matrix_keyed.rds, captured
# per-field via dataset_field(), not just datasets_link()'s 6-column
# output) also picks up every docs_url/sheet/resolver change alongside
# url/available_time -- KNOWN_POST_CAPTURE_DELTAS below is a superset of
# the 8 cells above for exactly that reason.

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

# 2026-08-24 addendum: fixing R/mapbiomas.R's treatment of Collection 10's
# restructured workbooks (see NEWS.md) required 5 real manifest-cell changes
# on top of the base-row-deletion migration this fixture was captured to
# verify -- so these REAL-KEY entries are legitimately expected to differ
# from the fixture's (older) value. Enumerated explicitly, not exempted as a
# class or by dataset, per the skill's capture-first rule: any mismatch this
# list doesn't name is still a real regression to go fix, not to wave through.
# 3 more added the same day: the resolver's own newest-first walk (run for
# real, with the verification cache's timeout bug already fixed) found a
# genuinely newer Dataverse dataset for mapbiomas_cover/indigenous_land
# (file 523, Collection 10.1) than this session's earlier manual
# investigation had found (file 266, Collection 10) -- verified structurally
# compatible before accepting (see resolve_mapbiomas.R's header).
KNOWN_POST_CAPTURE_DELTAS <- c(
  "mapbiomas\rmapbiomas_cover\rmunicipality\rNA\rurl",
  "mapbiomas\rmapbiomas_cover\rmunicipality\rNA\rdocs_url",
  "mapbiomas\rmapbiomas_cover\rmunicipality\rNA\rsheet",
  "mapbiomas\rmapbiomas_cover\rmunicipality\rNA\rversion",
  "mapbiomas\rmapbiomas_deforestation_regeneration\rNA\rNA\ravailable_time",
  "mapbiomas\rmapbiomas_cover\rindigenous_land\rNA\rurl",
  "mapbiomas\rmapbiomas_cover\rindigenous_land\rNA\rdocs_url",
  "mapbiomas\rmapbiomas_cover\rindigenous_land\rNA\rversion",
  # 2026-09-08: two sessions of hand-edits/migrations on EPE, none of which
  # touch the base-row-deletion migration this fixture actually verifies --
  # (a) docs_url filled in by hand for 7 previously-blank rows (the "mother
  # source" landing page for each dataset), (b) energy_state_panel migrated
  # from its discontinued Chapter 8 workbook to the BEN dashboard's
  # consolidated generation table (url, available_time, sheet, resolver all
  # change -- see test-datasets_link.R's delta #10 and R/epe.R's header).
  "epe\rconsumer_energy_consumption\rregion\rNA\rdocs_url",
  "epe\rconsumer_energy_consumption\rstate\rNA\rdocs_url",
  "epe\rconsumer_energy_consumption\rsubsystem\rNA\rdocs_url",
  "epe\rindustrial_energy_consumption\rregion\rNA\rdocs_url",
  "epe\rindustrial_energy_consumption\rstate\rNA\rdocs_url",
  "epe\rindustrial_energy_consumption\rsubsystem\rNA\rdocs_url",
  "epe\rnational_energy_balance\rNA\rNA\rdocs_url",
  "epe\renergy_state_panel\rNA\rNA\rurl",
  "epe\renergy_state_panel\rNA\rNA\rdocs_url",
  "epe\renergy_state_panel\rNA\rNA\ravailable_time",
  "epe\renergy_state_panel\rNA\rNA\rsheet",
  "epe\renergy_state_panel\rNA\rNA\rresolver",
  # 2026-10-29: imazon_shp onboarded onto the resolver framework
  # (actions/scrapers/resolve_imazon.R's header). url and available_time are
  # unchanged (the resolver confirms them); the row is now owned by the resolver
  # (`resolver`) and records the md5 of the Drive file it verified (`version`).
  # Neither is a datasets_link() column, so only this per-field matrix sees them.
  "imazon\rimazon_shp\rNA\rNA\rresolver",
  "imazon\rimazon_shp\rNA\rNA\rversion",
  # 2026-09-08 (PRODES/BACI resolver audit): PRODES' docs_url was pointing
  # at a dashboard app, not the real downloads landing page -- fixed for
  # all 6 PRODES rows (see test-datasets_link.R delta #11).
  "prodes\rclouds\rNA\rNA\rdocs_url",
  "prodes\rdeforestation\rNA\rNA\rdocs_url",
  "prodes\rhydrography\rNA\rNA\rdocs_url",
  "prodes\rnative_vegetation\rNA\rNA\rdocs_url",
  "prodes\rnon_forest\rNA\rNA\rdocs_url",
  "prodes\rresidual_deforestation\rNA\rNA\rdocs_url",
  # 2026-09-14: resolve_ips.R stopped being detect-only -- all 8 IPS rows'
  # url moved off the manifest's old Google Drive link (confirmed owned by
  # a personal gmail account, not Datazoom -- see resolve_ips.R's header)
  # onto IPS Amazônia's own Strapi-hosted workbook (verified live to be a
  # drop-in replacement -- see resolve_ips.R's header), and version is now
  # populated. See test-datasets_link.R's delta #12.
  "ips\rall\rNA\rNA\rurl",
  "ips\rall\rNA\rNA\rversion",
  "ips\rcommunic\rNA\rNA\rurl",
  "ips\rcommunic\rNA\rNA\rversion",
  "ips\rdeforest\rNA\rNA\rurl",
  "ips\rdeforest\rNA\rNA\rversion",
  "ips\reduc\rNA\rNA\rurl",
  "ips\reduc\rNA\rNA\rversion",
  "ips\rlife_quality\rNA\rNA\rurl",
  "ips\rlife_quality\rNA\rNA\rversion",
  "ips\rmortality\rNA\rNA\rurl",
  "ips\rmortality\rNA\rNA\rversion",
  "ips\rsanit_habit\rNA\rNA\rurl",
  "ips\rsanit_habit\rNA\rNA\rversion",
  "ips\rviolence\rNA\rNA\rurl",
  "ips\rviolence\rNA\rNA\rversion",
  # 2026-09-15: aneel/energy_generation (SIGA) migrated from dead GitLab
  # URL (403 Forbidden) to official ANEEL CKAN Open Data CSV.
  "aneel\renergy_generation\rNA\rNA\rdocs_url",
  "aneel\renergy_generation\rNA\rNA\rurl",
  "aneel\renergy_generation\rNA\rNA\rresolver",
  # 2026-09-15: resolve_seeg.R stopped being detect-only -- all 6 SEEG rows'
  # url moved off the manifest's old Google Drive link onto seeg.eco.br's
  # own live v13.0 file (verified live: a genuine schema restructuring, not
  # just a version bump -- see R/seeg.R's header and test-datasets_link.R's
  # delta #13). available_time widens from 2000-2018 to 1970-2024, and
  # version/docs_url are now populated (both were NA before).
  "seeg\rseeg\rNA\rNA\rurl",
  "seeg\rseeg\rNA\rNA\rdocs_url",
  "seeg\rseeg\rNA\rNA\ravailable_time",
  "seeg\rseeg\rNA\rNA\rversion",
  "seeg\rseeg_energy\rNA\rNA\rurl",
  "seeg\rseeg_energy\rNA\rNA\rdocs_url",
  "seeg\rseeg_energy\rNA\rNA\ravailable_time",
  "seeg\rseeg_energy\rNA\rNA\rversion",
  "seeg\rseeg_farming\rNA\rNA\rurl",
  "seeg\rseeg_farming\rNA\rNA\rdocs_url",
  "seeg\rseeg_farming\rNA\rNA\ravailable_time",
  "seeg\rseeg_farming\rNA\rNA\rversion",
  "seeg\rseeg_industry\rNA\rNA\rurl",
  "seeg\rseeg_industry\rNA\rNA\rdocs_url",
  "seeg\rseeg_industry\rNA\rNA\ravailable_time",
  "seeg\rseeg_industry\rNA\rNA\rversion",
  "seeg\rseeg_land\rNA\rNA\rurl",
  "seeg\rseeg_land\rNA\rNA\rdocs_url",
  "seeg\rseeg_land\rNA\rNA\ravailable_time",
  "seeg\rseeg_land\rNA\rNA\rversion",
  "seeg\rseeg_residuals\rNA\rNA\rurl",
  "seeg\rseeg_residuals\rNA\rNA\rdocs_url",
  "seeg\rseeg_residuals\rNA\rNA\ravailable_time",
  "seeg\rseeg_residuals\rNA\rNA\rversion",
  # 2026-10-19: TerraClimate onboarded onto the resolver framework
  # (actions/scrapers/resolve_terraclimate.R). For each of the 14 variables
  # docs_url is filled in (the Climatology Lab's TerraClimate page), resolver
  # becomes "terraclimate", and available_time moves from 1958-2022 to the
  # real v1.1 span, 1950-2025. See test-datasets_link.R's delta #17.
  as.vector(outer(
    c(
      "max_temperature", "min_temperature", "wind_speed", "vapor_pressure_deficit",
      "vapor_pressure", "snow_water_equivalent", "shortwave_radiation_flux", "soil_moisture",
      "runoff", "precipitation", "potential_evaporation", "climatic_water_deficit",
      "water_evaporation", "palmer_drought_severity_index"
    ),
    c("docs_url", "available_time", "resolver"),
    function(d, f) paste("terraclimate", d, "NA", "NA", f, sep = "\r")
  )),
  # 2026-10: DEGRAD onboarded onto the manifest pipeline (see
  # actions/scrapers/resolve_degrad.R's header). A verify-only resolver: it
  # changes no url/archive_file/available_time -- the only things that moved
  # are that all 10 year rows now declare resolver = "degrad" (blank before)
  # and docs_url = the official INPE access page listing the 10 downloads
  # (blank before). No effect on datasets_link()'s 6 output columns (neither
  # is one of them), so test-datasets_link.R and the datasets_link()
  # cell-count test further down are untouched.
  "degrad\rdegrad\rNA\r2007\rresolver",
  "degrad\rdegrad\rNA\r2008\rresolver",
  "degrad\rdegrad\rNA\r2009\rresolver",
  "degrad\rdegrad\rNA\r2010\rresolver",
  "degrad\rdegrad\rNA\r2011\rresolver",
  "degrad\rdegrad\rNA\r2012\rresolver",
  "degrad\rdegrad\rNA\r2013\rresolver",
  "degrad\rdegrad\rNA\r2014\rresolver",
  "degrad\rdegrad\rNA\r2015\rresolver",
  "degrad\rdegrad\rNA\r2016\rresolver",
  "degrad\rdegrad\rNA\r2007\rdocs_url",
  "degrad\rdegrad\rNA\r2008\rdocs_url",
  "degrad\rdegrad\rNA\r2009\rdocs_url",
  "degrad\rdegrad\rNA\r2010\rdocs_url",
  "degrad\rdegrad\rNA\r2011\rdocs_url",
  "degrad\rdegrad\rNA\r2012\rdocs_url",
  "degrad\rdegrad\rNA\r2013\rdocs_url",
  "degrad\rdegrad\rNA\r2014\rdocs_url",
  "degrad\rdegrad\rNA\r2015\rdocs_url",
  "degrad\rdegrad\rNA\r2016\rdocs_url",
  # 2026-10: sigmine_active now declares available_geo "state, municipality"
  # (it was NA); see test-datasets_link.R delta #14.
  "sigmine\rsigmine_active\rNA\rNA\ravailable_geo",
  # 2026-10: sigmine_active now declares its parent page in docs_url (it was NA).
  "sigmine\rsigmine_active\rNA\rNA\rdocs_url",
  # 2026-10: DETER migration (see test-datasets_link.R's delta #14 and
  # resolve_deter.R's header). deter_cerrado moved to the live
  # "deter-cerrado-nb" slug (url + the shapefile name inside the package);
  # resolve_deter.R emits https links, so deter_amz's url changes scheme;
  # both rows are now owned by the deter resolver; available_time of both
  # was re-derived from the real data (see test-datasets_link.R's delta #14);
  # docs_url of both was filled in with TerraBrasilis' downloads landing page
  # ("Adding parent url" -- docs_url isn't a datasets_link() column, so only
  # this per-field matrix sees it).
  "deter\rdeter_amz\rNA\rNA\rurl",
  "deter\rdeter_amz\rNA\rNA\ravailable_time",
  "deter\rdeter_amz\rNA\rNA\rdocs_url",
  "deter\rdeter_amz\rNA\rNA\rresolver",
  "deter\rdeter_cerrado\rNA\rNA\rurl",
  "deter\rdeter_cerrado\rNA\rNA\ravailable_time",
  "deter\rdeter_cerrado\rNA\rNA\rarchive_file",
  "deter\rdeter_cerrado\rNA\rNA\rdocs_url",
  "deter\rdeter_cerrado\rNA\rNA\rresolver",
  # 2026-10-17: COMEX onboarding (see test-datasets_link.R's delta #15 and
  # resolve_comex.R's header). All 4 rows' url moved from
  # balanca.economia.gov.br (TLS certificate expired 2026-10-03) to the
  # ministry's current host balanca.mdic.gov.br; available_time was
  # re-derived from the real files (1997-2023 -> 1997-2026); docs_url was
  # filled in with the ministry's open-data page and the rows are now owned
  # by the comex resolver (docs_url/resolver aren't datasets_link() columns,
  # so only this per-field matrix sees them).
  "comex\rexport_mun\rNA\rNA\rurl",
  "comex\rexport_mun\rNA\rNA\ravailable_time",
  "comex\rexport_mun\rNA\rNA\rdocs_url",
  "comex\rexport_mun\rNA\rNA\rresolver",
  "comex\rexport_prod\rNA\rNA\rurl",
  "comex\rexport_prod\rNA\rNA\ravailable_time",
  "comex\rexport_prod\rNA\rNA\rdocs_url",
  "comex\rexport_prod\rNA\rNA\rresolver",
  "comex\rimport_mun\rNA\rNA\rurl",
  "comex\rimport_mun\rNA\rNA\ravailable_time",
  "comex\rimport_mun\rNA\rNA\rdocs_url",
  "comex\rimport_mun\rNA\rNA\rresolver",
  "comex\rimport_prod\rNA\rNA\rurl",
  "comex\rimport_prod\rNA\rNA\ravailable_time",
  "comex\rimport_prod\rNA\rNA\rdocs_url",
  "comex\rimport_prod\rNA\rNA\rresolver"
)
post_capture_key <- function(s, d, g, y, f) {
  paste(s, d, ifelse(is.na(g), "NA", g), ifelse(is.na(y), "NA", y), f, sep = "\r")
}

# 2026-10-21: PAM onboarding (see test-datasets_link.R's delta #18 and
# resolve_pam.R's header). Built from the captured matrix itself rather than
# listed key by key: 161 cells on 79 unkeyed rows -- resolver on every row,
# sidra_code on the 7 whole-table rows (classific "all" -> the explicit
# classification id), and available_time on every row except the 4 series that
# ended in 1987, which IBGE's coverage left exactly as they were. The SET of
# changed cells does not move when the resolver refreshes available_time again.
# 2026-10-22: + docs_url on every row (79 more cells) -- the survey page
# https://sidra.ibge.gov.br/pesquisa/pam/tabelas, bot-challenged (HTTP 403) so
# nothing could verify it, became each row's own table page
# https://sidra.ibge.gov.br/tabela/<table> (7 pages for 79 rows, HTTP 200, taken
# from the metadata by resolve_pam.R). 240 cells in all. docs_url is not a
# datasets_link() column, so only this per-field matrix sees it.
pam_datasets <- unique(matrix$dataset[matrix$survey == "pam"])
pam_whole_table <- c("all_crops", "temporary_crops", "permanent_crops", "beans", "corn", "peanut", "potato")
pam_unchanged_time <- c("alfafa", "cashew", "forage_cane", "rubber_liquid_latex")
KNOWN_POST_CAPTURE_DELTAS <- c(
  KNOWN_POST_CAPTURE_DELTAS,
  post_capture_key("pam", pam_datasets, NA, NA, "resolver"),
  post_capture_key("pam", pam_whole_table, NA, NA, "sidra_code"),
  post_capture_key("pam", setdiff(pam_datasets, pam_unchanged_time), NA, NA, "available_time"),
  post_capture_key("pam", pam_datasets, NA, NA, "docs_url")
)

# 2026-10-22: PPM onboarding (see test-datasets_link.R's delta #19 and
# resolve_ppm.R's header). Built from the captured matrix itself rather than
# listed key by key: 18 cells on 5 unkeyed rows -- resolver, available_time and
# docs_url on every row (the survey-wide page became each row's own table page),
# sidra_code on the 3 tables that have a classification. The SET of changed cells
# does not move when the resolver refreshes available_time again.
ppm_datasets <- unique(matrix$dataset[matrix$survey == "ppm"])
ppm_classified <- c("ppm_animal_origin_production", "ppm_aquaculture", "ppm_livestock_inventory")
KNOWN_POST_CAPTURE_DELTAS <- c(
  KNOWN_POST_CAPTURE_DELTAS,
  post_capture_key("ppm", ppm_datasets, NA, NA, "resolver"),
  post_capture_key("ppm", ppm_datasets, NA, NA, "available_time"),
  post_capture_key("ppm", ppm_datasets, NA, NA, "docs_url"),
  post_capture_key("ppm", ppm_classified, NA, NA, "sidra_code")
)

test_that("every real-key entry in resolution_matrix_keyed.rds resolves byte-identically after the migration, except 5 known post-capture deltas", {
  real_rows <- matrix[!(is.na(matrix$geo_level) & is.na(matrix$year) & mapply(is_known_base_row_dataset, matrix$survey, matrix$dataset)), ]
  expect_gt(nrow(real_rows), 0)

  mismatches <- character(0)
  known_delta_hits <- character(0)
  for (i in seq_len(nrow(real_rows))) {
    row <- real_rows[i, ]
    g <- if (is.na(row$geo_level)) NULL else row$geo_level
    y <- if (is.na(row$year)) NULL else row$year
    new_val <- dataset_field(row$survey, row$dataset, row$field, geo_level = g, year = y)
    same <- (is.na(row$value) && is.na(new_val)) || (!is.na(row$value) && !is.na(new_val) && row$value == new_val)
    if (!isTRUE(same)) {
      key <- post_capture_key(row$survey, row$dataset, row$geo_level, row$year, row$field)
      if (key %in% KNOWN_POST_CAPTURE_DELTAS) {
        known_delta_hits <- c(known_delta_hits, key)
      } else {
        mismatches <- c(mismatches, paste(row$survey, row$dataset, row$geo_level, row$year, row$field, "old=", row$value, "new=", new_val))
      }
    }
  }

  expect_equal(mismatches, character(0))
  expect_setequal(known_delta_hits, KNOWN_POST_CAPTURE_DELTAS)
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

test_that("datasets_link() keeps the same rows/columns and differs in exactly 42 cells (see test-datasets_link.R deltas #8-#15)", {
  # 33 as of delta #14, + 1 for sigmine_active's available_geo (2026-10,
  # NA -> "state, municipality", same delta #14 -- the assertion was bumped to
  # 34 for it but this comment wasn't, so #354's merge rebuilt 41 off the 33
  # and dropped it), + 8 from delta #15 (2026-10-17, COMEX: all 4 rows'
  # url AND available_time -- docs_url/resolver aren't datasets_link() columns)
  # = 42.
  # Cell count, enumerated by replay (not assumed): 5 (delta #8) + 1 (#9) +
  # 2 (#10) + 8 (#12) + 12 (#13) = 28, + 1 for aneel/energy_generation's url
  # (2026-09-15, same migration as the KNOWN_POST_CAPTURE_DELTAS entry above;
  # it was previously counted in the assertion but not in this comment)
  # + 4 from delta #14 (2026-10, DETER: deter_amz's and deter_cerrado's
  # url, plus both of their available_time cells) = 33.
  # 5 cells from delta #8 (the base-row-deletion migration this fixture was
  # captured to verify) + 1 more from delta #9 (2026-08-24,
  # mapbiomas_deforestation_regeneration's available_time corrected from
  # "1985-2024" to "1987-2024") + 2 more from delta #10 (2026-09-08,
  # epe/energy_state_panel's url and available_time -- see NEWS.md and
  # test-datasets_link.R) + 8 more from delta #12 (2026-09-14, all 8 IPS
  # rows' url -- see test-datasets_link.R) + 12 more from delta #13
  # (2026-09-15, all 6 SEEG rows' url AND available_time -- see
  # test-datasets_link.R). cover's url stays NA either way (its rows still
  # disagree, just on a different pair of URLs now), so that doesn't add
  # another cell here -- and the docs_url-only EPE/PRODES changes (delta
  # #10's sibling hand-edits, delta #11's PRODES fix) don't count in THIS
  # test either, since docs_url isn't one of datasets_link()'s 6 output
  # columns (nor is version, IPS's and SEEG's other changed field).
  old_snap <- readRDS(test_path("fixtures", "datasets_link_pre_baserow_deletion.rds"))
  new_snap <- datasets_link()

  # terraclimate: the 14 rows' available_time cells (delta #17, 2026-10-19,
  # test-datasets_link.R) are asserted on their own and the rows set aside
  # before the comparison, so the shared cell count below keeps meaning what
  # its comment says. Everything else about them must be unchanged.
  tc_new <- new_snap[new_snap$survey == "terraclimate", ]
  tc_old <- old_snap[old_snap$survey == "terraclimate", ]
  expect_equal(nrow(tc_new), 14)
  expect_setequal(tc_new$dataset, tc_old$dataset)
  expect_equal(unique(tc_old$available_time), "1958-2022")
  expect_equal(unique(tc_new$available_time), "1950-2025")
  tc_new <- tc_new[order(tc_new$dataset), ]
  tc_old <- tc_old[order(tc_old$dataset), ]
  for (col in setdiff(names(tc_old), "available_time")) {
    expect_equal(tc_new[[col]], tc_old[[col]], info = paste("terraclimate column", col))
  }
  new_snap <- new_snap[new_snap$survey != "terraclimate", ]
  old_snap <- old_snap[old_snap$survey != "terraclimate", ]

  # baci_dic/product_codes was a genuinely NEW row for a while (delta #11,
  # test-datasets_link.R) and was removed again 2026-10-28: load_baci_dic()
  # reads comex_dic/hs_codes, the same file (set aside below with the other
  # comex_dic row). Nothing to set aside any more -- assert it stays gone.
  expect_false("baci_dic" %in% new_snap$survey)

  # pam: 7 sidra_code cells and 75 available_time cells (delta #18, 2026-10-21,
  # test-datasets_link.R) are asserted there and the rows set aside before the
  # comparison, so the shared cell count below keeps meaning what its comment
  # says. Everything else about them must be unchanged.
  pam_new <- new_snap[new_snap$survey == "pam", ]
  pam_old <- old_snap[old_snap$survey == "pam", ]
  expect_equal(nrow(pam_new), 79)
  expect_setequal(pam_new$dataset, pam_old$dataset)
  pam_new <- pam_new[order(pam_new$dataset), ]
  pam_old <- pam_old[order(pam_old$dataset), ]
  for (col in setdiff(names(pam_old), c("sidra_code", "available_time"))) {
    expect_equal(pam_new[[col]], pam_old[[col]], info = paste("pam column", col))
  }
  expect_equal(sum(pam_new$sidra_code != pam_old$sidra_code), 7)
  new_snap <- new_snap[new_snap$survey != "pam", ]
  old_snap <- old_snap[old_snap$survey != "pam", ]

  # ppm: 3 sidra_code cells and 5 available_time cells (delta #19, 2026-10-22,
  # test-datasets_link.R) are asserted there and the rows set aside before the
  # comparison, so the shared cell count below keeps meaning what its comment
  # says. Everything else about them must be unchanged.
  ppm_new <- new_snap[new_snap$survey == "ppm", ]
  ppm_old <- old_snap[old_snap$survey == "ppm", ]
  expect_equal(nrow(ppm_new), 5)
  expect_setequal(ppm_new$dataset, ppm_old$dataset)
  ppm_new <- ppm_new[order(ppm_new$dataset), ]
  ppm_old <- ppm_old[order(ppm_old$dataset), ]
  for (col in setdiff(names(ppm_old), c("sidra_code", "available_time"))) {
    expect_equal(ppm_new[[col]], ppm_old[[col]], info = paste("ppm column", col))
  }
  expect_equal(sum(ppm_new$sidra_code != ppm_old$sidra_code), 3)
  new_snap <- new_snap[new_snap$survey != "ppm", ]
  old_snap <- old_snap[old_snap$survey != "ppm", ]

  # sigmine/sigmine_inactive is a genuinely NEW row (delta #14,
  # test-datasets_link.R): assert it separately, then exclude it from the
  # row-for-row comparison.
  new_sig_inactive <- new_snap[new_snap$survey == "sigmine" & new_snap$dataset == "sigmine_inactive", ]
  expect_equal(nrow(new_sig_inactive), 1)
  expect_equal(new_sig_inactive$url, "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/PROCESSOS_INATIVOS.zip")
  new_snap <- new_snap[!(new_snap$survey == "sigmine" & new_snap$dataset == "sigmine_inactive"), ]
  # deter_pantanal and deter_non_forest are genuinely NEW rows too (delta
  # #14, test-datasets_link.R) -- same treatment as sigmine_inactive above.
  new_deter <- new_snap[new_snap$survey == "deter" & new_snap$dataset %in% c("deter_pantanal", "deter_non_forest"), ]
  expect_setequal(new_deter$dataset, c("deter_pantanal", "deter_non_forest"))
  expect_equal(
    sort(new_deter$url),
    sort(c(
      "https://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-pantanal/shape",
      "https://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-nf/shape"
    ))
  )
  new_snap <- new_snap[!(new_snap$survey == "deter" & new_snap$dataset %in% c("deter_pantanal", "deter_non_forest")), ]

  # comex_dic/hs_codes and comex_dic/ncm_codes are genuinely NEW rows as well
  # (delta #16, 2026-10-19, test-datasets_link.R): the two product
  # dictionaries load_br_trade() reads, moved from literals in R/comex.R into
  # the manifest. A new row adds no changed cell, so the count below stays 42.
  new_comex_dic <- new_snap[new_snap$survey == "comex_dic", ]
  expect_setequal(new_comex_dic$dataset, c("hs_codes", "ncm_codes"))
  expect_equal(
    sort(new_comex_dic$url),
    sort(c(
      "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv",
      "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM.csv"
    ))
  )
  new_snap <- new_snap[new_snap$survey != "comex_dic", ]

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

  expect_equal(delta, 42)
})
