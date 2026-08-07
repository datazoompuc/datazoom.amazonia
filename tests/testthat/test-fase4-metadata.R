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
  for (ds in c("deforestation", "residual_deforestation", "native_vegetation",
               "non_forest", "hydrography", "clouds")) {
    expect_equal(
      dataset_field("prodes", ds, "layer_name"),
      "prodes_amazonia_legal_2025_v20260408"
    )
  }
})

test_that("MapBiomas sheet matches the old hardcoded tribble, per dataset/geo_level", {
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "sheet", geo_level = "municipality"), "COVERAGE_9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "sheet", geo_level = "indigenous_land"), "COVERAGE_9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "sheet", geo_level = "biome"), "TRANSITION_9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "sheet", geo_level = "municipality"), "TRANSITION_9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_deforestation_regeneration", "sheet", geo_level = "municipality"), "DEF_SECVEG")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "sheet", geo_level = "state"), "UF")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "sheet", geo_level = "biome"), "BIOME")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "sheet", geo_level = "municipality"), "CITY_STATE_BIOME")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "sheet", geo_level = "indigenous_land"), "IL")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "state"), "states_annual")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "biome"), "biomes_annual")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "sheet", geo_level = "municipality"), "mun_annual")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_fire", "sheet", geo_level = "state"), "a_ANNUAL")
})

test_that("MapBiomas version (collection) number matches the old hardcoded messages, regardless of geo_level", {
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "version"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "version"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_deforestation_regeneration", "version"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "version"), "9") # bumped 8->9 by the live resolver run, 2026-08-07
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "version"), "7")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_fire", "version"), "3")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "version"), "2")
})

test_that("new MapBiomas override rows (irrigation/water) still resolve the correct URL", {
  # These rows exist only to carry `sheet` -- they carry no `url` of their
  # own at all, so dataset_url() must fall through to the dataset's base
  # row for every geo_level, or the download would silently break.
  # mapbiomas_mining is deliberately excluded here since 2026-08-07: the live
  # resolver now writes a real per-geo_level `url` directly onto its
  # indigenous_land/municipality override rows (see resolve_mapbiomas.R), so
  # those rows no longer fall through to the base row at all -- the premise
  # this test checks no longer applies to mining specifically.
  for (ds in c("mapbiomas_irrigation", "mapbiomas_water")) {
    base_url <- dataset_url("mapbiomas", ds)
    for (geo in strsplit(datasets_link(source = "mapbiomas", dataset = ds)$available_geo, ", ")[[1]]) {
      expect_equal(dataset_url("mapbiomas", ds, geo_level = tolower(geo)), base_url)
    }
  }
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
  available <- dataset_field("aneel", "energy_development_budget", "available_time")
  years <- eval(parse(text = stringr::str_replace(available, "-", ":")))
  expect_equal(years, 2017:2024)
})
