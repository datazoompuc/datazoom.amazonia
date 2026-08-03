# Fase 3 regression: the geo_level/year overrides that used to be hardcoded
# branches inside external_download() (MapBiomas by geo_level, ANEEL CDE by
# year) are now rows in the manifest, resolved by dataset_url(). Every value
# here is the exact literal the old hardcoded branch used to return -- this
# proves the migration changed WHERE the value lives, not what it is.

test_that("MapBiomas geo_level overrides resolve to the old hardcoded literals", {
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "indigenous_land"),
    "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-INDIGENOUS_LANDS-1.xlsx"
  )
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "biome"),
    "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-BIOMES.xlsx"
  )
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "municipality"),
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_state_municipality.xlsx"
  )
})

test_that("MapBiomas falls back to the base row when geo_level has no override", {
  # mapbiomas_cover + "municipality" was never overridden -- always used the
  # base row's link, both before and after the migration.
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "municipality"),
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/statistics/mapbiomas_brazil_col_coverage_biome_state_municipality.xlsx"
  )
})

test_that("ANEEL CDE per-year overrides resolve to the old hardcoded UUID URLs", {
  expect_equal(
    dataset_url("aneel", "energy_development_budget", year = 2019),
    "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/0bd1f129-39c5-4edc-b272-3f1d11181cca/download/cde-beneficiarios-rede-basica-2019.csv"
  )
  expect_equal(
    dataset_url("aneel", "energy_development_budget", year = 2022),
    "https://dadosabertos.aneel.gov.br/dataset/a7191647-b187-4893-b20a-8954d57ff89c/resource/e390baae-5304-4a94-854f-0905094b3357/download/cde-beneficiarios-rede-basica-2022.csv"
  )
})

test_that("an out-of-range ANEEL CDE year resolves to NA, not a wrong URL", {
  expect_true(is.na(dataset_url("aneel", "energy_development_budget", year = 2030)))
})

test_that("external_download() stops with an informative error for the out-of-range year", {
  # This must fail BEFORE any network access -- dataset_url() returns NA,
  # and external_download()'s guard stops immediately.
  expect_error(
    external_download(source = "aneel", dataset = "energy_development_budget", year = 2030),
    "No download URL found"
  )
})

test_that("BACI archive_file carries the same version stamp as the URL", {
  url <- dataset_url("baci", "HS92")
  archive_file <- dataset_field("baci", "HS92", "archive_file")

  expect_match(url, "V202601\\.zip$")
  expect_equal(archive_file, "*$year$_V202601.csv")
})

test_that("DETER archive_file matches the old hardcoded shapefile names", {
  expect_equal(dataset_field("deter", "deter_amz", "archive_file"), "deter-amz-deter-public.shp")
  expect_equal(dataset_field("deter", "deter_cerrado", "archive_file"), "deter_public.shp")
})

test_that("DEGRAD archive_file is resolved per year and the link stays $year$-templated", {
  expect_equal(dataset_field("degrad", "degrad", "archive_file", year = 2010), "DEGRAD_2010_UF_pol.shp")
  expect_equal(dataset_field("degrad", "degrad", "archive_file", year = 2015), "DEGRAD_2015.shp")

  url_2010 <- dataset_url("degrad", "degrad", year = 2010)
  url_2015 <- dataset_url("degrad", "degrad", year = 2015)
  expect_equal(url_2010, url_2015) # same template, substituted later in external_download()
  expect_match(url_2010, "\\$year\\$")
})

test_that("datasets_link() (base rows only) is unaffected by the new override rows", {
  # The override rows added in Fase 3 must never leak into the 6-column,
  # base-row-only view that check_params() and the 8 SIDRA loaders rely on.
  expect_equal(nrow(datasets_link(source = "mapbiomas", dataset = "mapbiomas_cover")), 1)
  expect_equal(nrow(datasets_link(source = "aneel", dataset = "energy_development_budget")), 1)
  expect_equal(nrow(datasets_link(source = "degrad", dataset = "degrad")), 1)
  expect_true(is.na(datasets_link(source = "aneel", dataset = "energy_development_budget", url = TRUE)))
})
