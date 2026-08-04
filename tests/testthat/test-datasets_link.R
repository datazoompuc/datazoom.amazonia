# Golden regression: datasets_link() must keep returning exactly the same
# table across the manifest schema normalization (5-tier field coalescing,
# see R/manifest.R and the "Normalizar o schema do manifest" plan), except
# for three DELIBERATE changes made by that normalization:
#
#   1. The column "link" is renamed "url" -- no external contract ever
#      depended on the literal name "link" (see R/manifest.R's header).
#   2. For the ~100 SIDRA-sourced rows (sidra_code not NA), the value that
#      used to sit in "link" was a documentation landing page, never
#      actually downloaded (SIDRA data comes from sidra_code via the
#      sidrar API) -- it now lives in the separate "docs_url" column, and
#      "url" is genuinely NA for these rows. Nothing in R/ ever read
#      datasets_link()'s link/url field for a SIDRA-sourced dataset (they
#      all call sidra_download() directly), so this is a safe behavior
#      change, verified explicitly below and in test-dataset_url.R.
#   3. available_geo is now stored lowercase. check_params.R already
#      lowercased it at read time before comparing against a user-supplied
#      geo_level, so this is a storage-only change with no behavior effect.
#
# The fixture was captured right before normalization. Row order is not
# significant (both sides sorted by (survey, dataset) before comparing).

fixture <- readRDS(test_path("fixtures", "datasets_link_pre_normalize.rds")) %>%
  dplyr::rename(url = link) %>%
  dplyr::mutate(
    url = ifelse(!is.na(sidra_code), NA_character_, url),
    available_geo = tolower(available_geo)
  )

test_that("datasets_link() returns the exact pre-normalization column set and order", {
  expect_equal(
    names(datasets_link()),
    c("survey", "dataset", "sidra_code", "available_time", "available_geo", "url")
  )
})

test_that("datasets_link() matches the golden fixture (schema normalization is a no-op beyond the two documented changes)", {
  actual <- datasets_link() %>% dplyr::arrange(survey, dataset)
  expected <- fixture %>% dplyr::arrange(survey, dataset)

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
    c("aneel", "energy_generation"), # energy_development_budget's base url is intentionally NA -- see test-dataset_url.R
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

test_that("SIDRA-sourced datasets have NA url -- their data comes from sidra_code, not a download link", {
  for (s in list(c("pam", "acai"), c("cempre", "cempre"), c("population", "population"))) {
    url <- datasets_link(source = s[1], dataset = s[2], url = TRUE)
    expect_true(is.na(url))
  }
})
