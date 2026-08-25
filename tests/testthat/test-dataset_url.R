# Fase 3 regression: the geo_level/year overrides that used to be hardcoded
# branches inside external_download() (MapBiomas by geo_level, ANEEL CDE by
# year) are now rows in the manifest, resolved by dataset_url(). Every value
# here is the exact literal the old hardcoded branch used to return -- this
# proves the migration changed WHERE the value lives, not what it is.

test_that("MapBiomas geo_level overrides resolve to their Dataverse-hosted urls", {
  # Updated 2026-08-19 when mapbiomas moved from a mix of GCS/WordPress
  # links to the MapBiomas Dataverse archive (data.mapbiomas.org) -- see
  # resolve_mapbiomas.R's header for what was investigated and why each
  # row landed where it did. mapbiomas_transition/municipality is the one
  # row deliberately left untouched: no Collection 10+ substitute exists
  # on Dataverse for it (verified live), so it still points at its
  # original GCS url.
  # Updated 2026-08-24: 266 (Collection 10) -> 523 (Collection 10.1) -- the
  # live resolver found a newer "Indigenous Territories" coverage dataset
  # on Dataverse that didn't turn up in this session's earlier manual
  # investigation. Verified structurally compatible before accepting it
  # (state/state_acronym collision already handled by mapbiomas_treat();
  # indigenous_territories column matches the name-based territory lookup;
  # it even adds a geocode column 266 didn't have).
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "indigenous_land"),
    "https://data.mapbiomas.org/api/access/datafile/523?format=original"
  )
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "biome"),
    "https://data.mapbiomas.org/api/access/datafile/457?format=original"
  )
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_transition", geo_level = "municipality"),
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_state_municipality.xlsx"
  )
})

test_that("MapBiomas cover/municipality resolves to its own explicit override row", {
  # mapbiomas_cover has a real, explicit "municipality" row -- there is no
  # base row left to fall back to at all (see R/manifest.R). Its url
  # genuinely differs from the "indigenous_land" row's (see
  # test-datasets_link.R: this is exactly why the dataset's collapsed
  # datasets_link() url is NA -- the rows disagree, on purpose).
  #
  # Updated 2026-08-24: file 535 (Dataverse's "Collection 10.1" dataset)
  # was replaced with file 254 ("Collection 10", same dataset MapBiomas
  # also mirrors on GCS) -- 535's sheet has no municipality code column at
  # all, verified live. See resolve_mapbiomas.R's header and
  # R/mapbiomas.R's mapbiomas_treat() for the full story.
  expect_equal(
    dataset_url("mapbiomas", "mapbiomas_cover", geo_level = "municipality"),
    "https://data.mapbiomas.org/api/access/datafile/254?format=original"
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

test_that("datasets_link() (one synthesized row per dataset) is unaffected by override rows", {
  # The override rows added in Fase 3 must never leak into the 6-column
  # view that check_params() and the 8 SIDRA loaders rely on -- updated
  # 2026-08-19 when the base-row-per-dataset concept was removed (see
  # R/manifest.R): effective_table() now synthesizes exactly one display
  # row per (survey, dataset) by collapsing each keyed dataset's rows to
  # their agreed value (or NA if they genuinely disagree -- see
  # test-datasets_link.R for the enumerated cases where they do), rather
  # than reading a real base row that always existed as its own row.
  expect_equal(nrow(datasets_link(source = "mapbiomas", dataset = "mapbiomas_cover")), 1)
  expect_equal(nrow(datasets_link(source = "aneel", dataset = "energy_development_budget")), 1)
  expect_equal(nrow(datasets_link(source = "degrad", dataset = "degrad")), 1)
  # aneel's per-year urls always disagree (each year is a different CSV) --
  # NA here for the same reason before and after this migration.
  expect_true(is.na(datasets_link(source = "aneel", dataset = "energy_development_budget", url = TRUE)))
})
