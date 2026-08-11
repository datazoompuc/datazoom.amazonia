# Golden regression: datasets_link() must keep returning exactly the same
# table across the manifest schema normalization (5-tier field coalescing,
# see R/manifest.R and the "Normalizar o schema do manifest" plan), except
# for a handful of DELIBERATE changes:
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
#   4. epe/national_energy_balance's url and available_time were updated
#      2026-08-04 (see R/epe.R's header and actions/scrapers/resolve_epe.R):
#      the old SharePoint workbook (2003-2023) was replaced by EPE's
#      consolidated BEN table (1970-2025), verified live to be a strict
#      superset covering the same accounts.
#   5. All 6 PRODES dataset rows' url now resolves to TerraBrasilis' live,
#      year + publish-date stamped filename (see resolve_prodes.R and
#      test-resolution-matrix.R item 6) instead of the stale committed
#      2023 file -- the manifest was regenerated for real 2026-08-07. Their
#      available_time moves the same way (2023 -> 2025 release year).
#   6. ANEEL: energy_development_budget's available_time widens to
#      2017-2024 (new years published), and energy_enterprises_distributed's
#      resource changed extension from .csv to .zip on ANEEL's own server
#      -- see test-resolution-matrix.R item 7 for the same two changes.
#
# The fixture was captured right before normalization. Row order is not
# significant (both sides sorted by (survey, dataset) before comparing).
#
# The manifest was later denormalized again (survey-default rows removed,
# every row made self-sufficient -- see data-raw/denormalize_manifest.R);
# that migration only changed STORAGE, not any value datasets_link() (or
# anything else in R/) resolves, so none of the 6 deltas above needed a 7th.

fixture <- readRDS(test_path("fixtures", "datasets_link_pre_normalize.rds")) %>%
  dplyr::rename(url = link) %>%
  dplyr::mutate(
    url = ifelse(!is.na(sidra_code), NA_character_, url),
    available_geo = tolower(available_geo),
    url = ifelse(
      survey == "epe" & dataset == "national_energy_balance",
      "https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/dados/tabela_balanco_energitico_consolidado.xlsx",
      url
    ),
    available_time = ifelse(
      survey == "epe" & dataset == "national_energy_balance",
      "1970-2025",
      available_time
    ),
    url = ifelse(
      survey == "prodes",
      "https://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/raster/prodes_amazonia_legal_2025_v20260408.zip",
      url
    ),
    url = ifelse(
      survey == "aneel" & dataset == "energy_enterprises_distributed",
      "https://dadosabertos.aneel.gov.br/dataset/5e0fafd2-21b9-4d5b-b622-40438d40aba2/resource/b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/empreendimento-geracao-distribuida.zip",
      url
    ),
    available_time = ifelse(
      survey == "prodes" & dataset == "deforestation", "2007-2025",
      ifelse(
        survey == "prodes" & dataset == "residual_deforestation", "2010-2025",
        ifelse(survey == "prodes", "2025", available_time)
      )
    ),
    available_time = ifelse(
      survey == "aneel" & dataset == "energy_development_budget",
      "2017-2024",
      available_time
    )
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
