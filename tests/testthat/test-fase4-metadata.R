# Fase 4 regression: version tokens that used to be hardcoded inside R code
# (PRODES raster layer name, MapBiomas sheet names + collection numbers, EPE
# sheet/year ranges, ANEEL valid years) now live in the manifest. These
# tests check the manifest-reading side (dataset_field()/dataset_url()) --
# the exact same values the R code now derives instead of hardcoding.

test_that("PRODES layer_name matches the old hardcoded raster band name", {
  for (ds in c("deforestation", "residual_deforestation", "native_vegetation",
               "non_forest", "hydrography", "clouds")) {
    expect_equal(
      dataset_field("prodes", ds, "layer_name"),
      "prodes_amazonia_legal_2023"
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

test_that("MapBiomas collection number matches the old hardcoded messages, regardless of geo_level", {
  expect_equal(dataset_field("mapbiomas", "mapbiomas_cover", "collection"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_transition", "collection"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_deforestation_regeneration", "collection"), "9")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_mining", "collection"), "8")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_irrigation", "collection"), "7")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_fire", "collection"), "3")
  expect_equal(dataset_field("mapbiomas", "mapbiomas_water", "collection"), "2")
})

test_that("new MapBiomas override rows (irrigation/mining/water) still resolve the correct URL", {
  # These rows exist only to carry `sheet` -- their link must be identical
  # to the base row's, or dataset_url() would silently break the download.
  for (ds in c("mapbiomas_irrigation", "mapbiomas_mining", "mapbiomas_water")) {
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
  available <- dataset_field("epe", "national_energy_balance", "available_time")
  years <- eval(parse(text = stringr::str_replace(available, "-", ":")))
  expect_equal(years, 2003:2023)
})

test_that("ANEEL energy_development_budget available_time still parses into 2017:2022", {
  available <- dataset_field("aneel", "energy_development_budget", "available_time")
  years <- eval(parse(text = stringr::str_replace(available, "-", ":")))
  expect_equal(years, 2017:2022)
})
