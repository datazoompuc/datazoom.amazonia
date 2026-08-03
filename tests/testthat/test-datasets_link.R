# Golden regression: datasets_link() must keep returning exactly the same
# table (same columns, same rows, same values) across the manifest migration.
# The fixture was captured right after the Fase 0 cleanup (duplicate
# "pam"/"permanent_crops" row removed, dead SEEG override removed, the
# raw.github.com -> raw.githubusercontent.com fix applied) and BEFORE any
# manifest code existed, so it locks in the "no-op refactor" promise of
# Fase 1. Row order is no longer significant after Fase 3 added new override
# rows to the manifest (sorted survey/dataset/geo_level/year), so both sides
# are sorted by (survey, dataset) before comparing.

fixture <- readRDS(test_path("fixtures", "datasets_link_pre.rds"))

test_that("datasets_link() returns the exact pre-migration column set and order", {
  expect_equal(
    names(datasets_link()),
    c("survey", "dataset", "sidra_code", "available_time", "available_geo", "link")
  )
})

test_that("datasets_link() matches the golden fixture except for the one Fase 3 change", {
  actual <- datasets_link() %>% dplyr::arrange(survey, dataset)
  expected <- fixture %>% dplyr::arrange(survey, dataset)

  # ANEEL's base-row link intentionally became NA in Fase 3: the six CDE
  # years (2017-2022) are now individual override rows resolved by
  # dataset_url() (see test-dataset_url.R) instead of a sentinel string
  # ("aneel_cde_$year$") resolved by a hardcoded UUID map in download.R.
  expected$link[expected$survey == "aneel" & expected$dataset == "energy_development_budget"] <- NA_character_

  expect_equal(actual, expected)
})

test_that("the duplicated pam/permanent_crops row stays fixed", {
  expect_equal(nrow(datasets_link(dataset = "permanent_crops")), 1)
})

test_that("filtering by source narrows correctly", {
  prodes_rows <- datasets_link(source = "prodes")
  expect_true(nrow(prodes_rows) > 0)
  expect_true(all(prodes_rows$survey == "prodes"))
})

test_that("filtering by source + dataset + url = TRUE returns a single string", {
  url <- datasets_link(source = "prodes", dataset = "deforestation", url = TRUE)
  expect_type(url, "character")
  expect_length(url, 1)
  expect_match(url, "^https://terrabrasilis\\.dpi\\.inpe\\.br/")
})

test_that("the internal geo_municipalities link uses raw.githubusercontent.com", {
  url <- datasets_link(source = "internal", dataset = "geo_municipalities", url = TRUE)
  expect_match(url, "^https://raw\\.githubusercontent\\.com/")
  expect_false(grepl("raw\\.github\\.com", url, fixed = FALSE))
})

test_that("a representative sample of url = TRUE values resolve to non-NA strings", {
  samples <- list(
    c("mapbiomas", "mapbiomas_transition"),
    c("baci", "HS92"),
    c("comex", "export_prod"),
    c("ibama", "distributed_fines"),
    c("aneel", "energy_generation"), # energy_development_budget's base link is intentionally NA -- see test-dataset_url.R
    c("epe", "national_energy_balance"),
    c("degrad", "degrad"),
    c("sigmine", "sigmine_active"),
    c("seeg", "seeg"),
    c("deter", "deter_amz")
  )

  for (s in samples) {
    url <- datasets_link(source = s[1], dataset = s[2], url = TRUE)
    expect_type(url, "character")
    expect_false(is.na(url))
    expect_true(nchar(url) > 0)
  }
})
