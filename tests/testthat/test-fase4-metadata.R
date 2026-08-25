# Fase 4 regression: version tokens that used to be hardcoded inside R code
# (PRODES raster layer name, MapBiomas sheet names + collection numbers, EPE
# sheet/year ranges, ANEEL valid years) now live in the manifest. These
# tests check the manifest-reading side (dataset_field()/dataset_url()) --
# the exact same values the R code now derives instead of hardcoding.

test_that("PRODES layer_name matches the manifest's current resolved value", {
  # Updated 2026-08-07 when the prodes resolver was fixed (a tibble()
  # column-shadowing bug meant available_time -- not layer_name -- was
  # wrong; see resolve_prodes.R) and re-run for real, picking up TerraBrasilis'
  # live filename (year + publish-date stamp). This value moves every time
  # the manifest is refreshed -- that's expected, not a regression.
  #
  # Updated again 2026-08-19 (base-row-deletion migration): reads via
  # dataset_meta(), matching what R/prodes.R itself now calls (layer_name
  # is dataset-wide, not per-row -- see R/manifest.R).
  for (ds in c("deforestation", "residual_deforestation", "native_vegetation",
               "non_forest", "hydrography", "clouds")) {
    expect_equal(
      dataset_meta("prodes", ds, "layer_name"),
      "prodes_amazonia_legal_2025_v20260408"
    )
  }
})

test_that("MapBiomas sheet matches the current Dataverse-verified value, per dataset/geo_level", {
  # Updated 2026-08-19: mapbiomas moved to the MapBiomas Dataverse archive
  # (data.mapbiomas.org) -- see resolve_mapbiomas.R's header for exactly
  # what was investigated (every sheet name here was confirmed by actually
  # downloading the candidate file and reading readxl::excel_sheets(), not
  # just trusting a URL that returns 200). cover/municipality and
  # cover/indigenous_land now come from two DIFFERENT Dataverse files with
  # different sheet names -- no longer the same "COVERAGE_9" literal both
  # used to share. mapbiomas_transition/municipality and
  # mapbiomas_water/state are the two rows deliberately left untouched:
  # no Dataverse substitute exists for either (verified live).
  # Updated 2026-08-24: cover/municipality moved off Dataverse's
  # "Collection 10.1" dataset -- its COVERAGE_10.1 sheet matches by name
  # but has no municipality code column at all (verified live). Lands on
  # the "Collection 10" dataset's COVERAGE_10 sheet instead, which does --
  # see resolve_mapbiomas.R's dv_file_layout() and required_col_pattern.
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "sheet", geo_level = "municipality"), "COVERAGE_10")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "sheet", geo_level = "indigenous_land"), "COVERAGE_INDIGENOUS_TERRITORIES")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "sheet", geo_level = "biome"), "TRANSITION_10")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "sheet", geo_level = "municipality"), "TRANSITION_9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_deforestation_regeneration", "sheet", geo_level = "municipality"), "DEFORESTATION")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_secondary_vegetation", "sheet", geo_level = "municipality"), "SECONDARY_VEGETATION")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "sheet", geo_level = "state"), "UF")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "sheet", geo_level = "biome"), "BIOME")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "sheet", geo_level = "municipality"), "CITY_STATE_BIOME")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "sheet", geo_level = "indigenous_land"), "IL")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "state"), "states_annual")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "biome"), "WATER_BIOME_ANNUAL")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "municipality"), "WATER_CITY_ANNUAL")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_fire", "sheet", geo_level = "state"), "a_ANNUAL")
})

test_that("MapBiomas version (collection) number matches the current Dataverse-verified value, per geo_level", {
  # Updated 2026-08-19 -- see the sheet test above for what changed and why.
  #
  # Updated again the same day (base-row-deletion migration): version is
  # NOT dataset-wide for MapBiomas -- several datasets deliberately pin one
  # geo_level to an older Dataverse collection than its siblings (see
  # R/mapbiomas.R and MANIFEST_META_COLS's comment in R/manifest.R), so
  # every keyed dataset below is read with its real geo_level now, not a
  # single bare dataset_field() call (which would stop() -- there's no
  # base row left to answer a no-key query for a keyed dataset). Only the
  # genuinely unkeyed datasets (deforestation_regeneration,
  # secondary_vegetation, fire) still resolve without a key.
  # Updated 2026-08-24: 10.1 -> 10, same reason as the sheet test above.
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "version", geo_level = "municipality"), "10")
  # Updated 2026-08-24: 10 -> 10.1, see test-dataset_url.R for why.
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "version", geo_level = "indigenous_land"), "10.1")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "version", geo_level = "biome"), "10")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "version", geo_level = "municipality"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_deforestation_regeneration", "version"), "10")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_secondary_vegetation", "version"), "10")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "version", geo_level = "municipality"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "version", geo_level = "indigenous_land"), "8")
  # irrigation's rows AGREE on version (both 7) -- but it is still a keyed
  # dataset (real biome/state override rows), so a no-key call would stop()
  # regardless of whether the values happen to agree.
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "version", geo_level = "biome"), "7")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "version", geo_level = "state"), "7")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_fire", "version"), "3")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "version", geo_level = "biome"), "4")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "version", geo_level = "municipality"), "4")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "version", geo_level = "state"), "2")
})

test_that("mapbiomas_mining/indigenous_land is deliberately pinned one collection behind its siblings", {
  # Collection 9's Dataverse mining file dropped the IL sheet entirely
  # (verified on both Dataverse's copy and the current WordPress-hosted
  # COL9.0 upload) -- Collection 8 is the newest one that still has it.
  # resolve_mapbiomas.R discovers this by walking newest-to-oldest and
  # checking each candidate's real sheet list, not by hardcoding "8".
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "version", geo_level = "indigenous_land"), "8")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "version", geo_level = "municipality"), "9")
})

test_that("MapBiomas cover/mining/transition/water no longer share a single URL across all geo_levels", {
  # Before the base-row-deletion migration, this test compared each
  # dataset's "municipality" (or "biome") row against its BASE row's url --
  # and they agreed, because the base row was a stale copy of that one
  # override's value (mapbiomas_mining's base row was pinned to Collection
  # 8 for weeks after both its real override rows had already moved to 9 --
  # see NEWS.md). Now that the base row is gone, there is nothing left to
  # duplicate that agreement: each of these datasets has exactly one
  # geo_level DELIBERATELY pinned to a different Dataverse collection than
  # its siblings (see R/mapbiomas.R and resolve_mapbiomas.R), so the two
  # real rows genuinely disagree -- confirmed here directly, and reflected
  # in datasets_link()'s collapsed url going to NA for all four (see
  # test-datasets_link.R).
  expect_false(identical(
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "municipality"),
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "indigenous_land")
  ))
  expect_false(identical(
    dataset_url("mapbiomas", "mapbiomas_mining", geo_level = "municipality"),
    dataset_url("mapbiomas", "mapbiomas_mining", geo_level = "indigenous_land")
  ))
  expect_false(identical(
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "biome"),
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "municipality")
  ))
  expect_false(identical(
    dataset_url("mapbiomas", "mapbiomas_water", geo_level = "state"),
    dataset_url("mapbiomas", "mapbiomas_water", geo_level = "municipality")
  ))
})

test_that("MapBiomas irrigation's override rows still all share one URL", {
  # Irrigation has no Dataverse substitute at all (verified live) -- every
  # geo_level row still points at the same S3 url as before the migration,
  # unlike cover/mining/transition/water above. Compared directly between
  # its two real rows now (there is no base row left to compare against).
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_irrigation", geo_level = "biome"),
    dataset_url("mapbiomas", "mapbiomas_irrigation", geo_level = "state")
  )
})

test_that("MapBiomas water/state deliberately does NOT share the other geo_levels' URL", {
  # Collection 4 (Dataverse) has no state-level sheet at all (verified by
  # downloading the file and checking readxl::excel_sheets()) -- municipality
  # and biome moved to Collection 4 and share one url; state stays on its
  # original Collection 2 url. Compared between real rows now (municipality
  # as the anchor, since there is no base row left).
  anchor <- dataset_url("mapbiomas", "mapbiomas_water", geo_level = "municipality")
  expect_equal(dataset_url("mapbiomas", "mapbiomas_water", geo_level = "biome"), anchor)
  expect_false(identical(dataset_url("mapbiomas", "mapbiomas_water", geo_level = "state"), anchor))
})

test_that("EPE energy_state_panel sheet matches the old hardcoded literal", {
  expect_equal(dataset_field("epe", "energy_state_panel", "sheet"), "8.1 part 3")
})

test_that("EPE national_energy_balance available_time still parses into the expected year range", {
  # Updated 2026-08-04 when national_energy_balance's source moved from the
  # old one-sheet-per-year SharePoint workbook (2003-2023) to EPE's
  # consolidated BEN table (1970-2025) -- see R/epe.R and
  # actions/scrapers/resolve_epe.R's headers.
  available <- dataset_field("epe", "national_energy_balance", "available_time")
  years <- eval(parse(text = stringr::str_replace(available, "-", ":")))
  expect_equal(years, 1970:2025)
})

test_that("ANEEL energy_development_budget available_time still parses into 2017:2024", {
  # Updated 2026-08-07: the aneel resolver found 2023 and 2024 CDE resources
  # published after the manifest's committed range (2017:2022) was written.
  #
  # Updated again 2026-08-19 (base-row-deletion migration): reads via
  # dataset_meta(), matching what R/aneel.R itself now calls (available_time
  # is dataset-wide here, fanned across all 8 year rows -- see
  # resolve_aneel.R -- not read off a base row that no longer exists).
  available <- dataset_meta("aneel", "energy_development_budget", "available_time")
  years <- eval(parse(text = stringr::str_replace(available, "-", ":")))
  expect_equal(years, 2017:2024)
})
