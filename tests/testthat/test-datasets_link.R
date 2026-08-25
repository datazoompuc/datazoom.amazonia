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
#   7. MapBiomas moved from a mix of GCS/WordPress links to the MapBiomas
#      Dataverse archive (data.mapbiomas.org), 2026-08-19 -- see
#      resolve_mapbiomas.R's header for what was investigated and why.
#      mapbiomas_cover, mapbiomas_deforestation_regeneration,
#      mapbiomas_fire, mapbiomas_mining, mapbiomas_transition, and
#      mapbiomas_water all got new base-row urls; the first three also
#      widened to available_time 1985-2024 (Collection 10/10.1's actual
#      span, confirmed via Dataverse's own dataset descriptions). A new
#      dataset, mapbiomas_secondary_vegetation, was added -- Collection 10
#      split what used to be one combined deforestation+regeneration file
#      into two separate ones; this is the regeneration half. mapbiomas
#      rows NOT in this list (mapbiomas_irrigation entirely,
#      mapbiomas_water's own state-level override, mapbiomas_transition's
#      own municipality-level override) are deliberately unchanged --
#      no Dataverse substitute exists for any of them, confirmed live.
#   8. The manifest's base rows were deleted entirely, 2026-08-19 (see
#      R/manifest.R and the "delete manifest base rows" migration):
#      effective_table() now synthesizes ONE display row per dataset by
#      collapsing each dataset's real rows to the value they all AGREE on
#      -- a column any two of a dataset's rows genuinely disagree on
#      collapses to NA instead of keeping whatever the (now deleted) base
#      row happened to say. Delta #7 above patched cover/mining/transition/
#      water's url to a specific value (the base row's) -- that value no
#      longer exists to read, because those four datasets each have
#      exactly one geo_level deliberately pinned to an older Dataverse
#      collection than its siblings (see resolve_mapbiomas.R), so their
#      real rows disagree on url for real, on purpose. This capture-and-
#      replay step (data-raw/capture_resolution_matrix_v2.R,
#      tests/testthat/fixtures/resolution_matrix_keyed.rds and
#      datasets_link_pre_baserow_deletion.rds) also measured ONE more cell
#      neither this migration's own plan nor delta #7 above anticipated:
#      mapbiomas_transition's available_time ALSO disagrees (its
#      municipality row is still sourced from the older, shorter-coverage
#      Collection 9 GCS file) -- 5 cells differ in total, not 3, verified
#      by direct replay rather than assumed.
#   9. 2026-08-24: fixed R/mapbiomas.R's treatment code for Collection 10's
#      restructured workbooks (see NEWS.md and R/mapbiomas.R's
#      mapbiomas_treat()). Two manifest cells changed as part of that fix,
#      both superseding delta #7's values: mapbiomas_cover/municipality's
#      url moved from Dataverse file 535 ("Collection 10.1", which turned
#      out to have no municipality code column at all, verified live) to
#      file 254 ("Collection 10", the same file MapBiomas also serves off
#      GCS) -- doesn't change the fixture comparison below since delta #8
#      already collapses mapbiomas_cover's url to NA regardless. And
#      mapbiomas_deforestation_regeneration's available_time was corrected
#      from "1985-2024" to "1987-2024" -- the DEFORESTATION sheet's first
#      year column is actually 1987, verified live; leaving it at 1985
#      would have made check_params() silently accept time_period = 1985.
#
# The fixture was captured right before normalization. Row order is not
# significant (both sides sorted by (survey, dataset) before comparing).
#
# The manifest was later denormalized again (survey-default rows removed,
# every row made self-sufficient -- see data-raw/denormalize_manifest.R);
# that migration only changed STORAGE, not any value datasets_link() (or
# anything else in R/) resolves, so none of the first 6 deltas above
# needed a 7th until the MapBiomas Dataverse migration actually changed
# real content. The base-row deletion (delta #8) is the first migration
# since to change VALUES again, not just storage.

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
    ),
    url = dplyr::case_when(
      survey == "mapbiomas" & dataset == "mapbiomas_cover" ~ "https://data.mapbiomas.org/api/access/datafile/254?format=original",
      survey == "mapbiomas" & dataset == "mapbiomas_deforestation_regeneration" ~ "https://data.mapbiomas.org/api/access/datafile/485?format=original",
      survey == "mapbiomas" & dataset == "mapbiomas_fire" ~ "https://data.mapbiomas.org/api/access/datafile/230?format=original",
      survey == "mapbiomas" & dataset == "mapbiomas_mining" ~ "https://data.mapbiomas.org/api/access/datafile/336?format=original",
      survey == "mapbiomas" & dataset == "mapbiomas_transition" ~ "https://data.mapbiomas.org/api/access/datafile/457?format=original",
      survey == "mapbiomas" & dataset == "mapbiomas_water" ~ "https://data.mapbiomas.org/api/access/datafile/344?format=original",
      TRUE ~ url
    ),
    available_time = dplyr::case_when(
      survey == "mapbiomas" & dataset %in% c("mapbiomas_cover", "mapbiomas_transition") ~ "1985-2024",
      # delta #9 -- corrected from 1985-2024, see file header
      survey == "mapbiomas" & dataset == "mapbiomas_deforestation_regeneration" ~ "1987-2024",
      TRUE ~ available_time
    )
  ) %>%
  dplyr::bind_rows(
    # mapbiomas_secondary_vegetation is genuinely new -- Collection 10 split
    # what used to be one combined deforestation+regeneration file into two;
    # this is the regeneration half. See resolve_mapbiomas.R's header.
    tibble::tibble(
      survey = "mapbiomas", dataset = "mapbiomas_secondary_vegetation",
      sidra_code = NA_character_, available_time = "1985-2024",
      available_geo = "municipality",
      url = "https://data.mapbiomas.org/api/access/datafile/310?format=original"
    )
  ) %>%
  dplyr::mutate(
    # Delta #8 -- see the file header. The 5 cells this migration actually
    # changed, enumerated explicitly (not a case_when fudge over a whole
    # column): 4 datasets' url disagrees across their real rows now that
    # there's no base row to silently keep agreeing with one of them, plus
    # mapbiomas_transition's available_time.
    url = dplyr::case_when(
      survey == "mapbiomas" & dataset %in% c(
        "mapbiomas_cover", "mapbiomas_mining", "mapbiomas_transition", "mapbiomas_water"
      ) ~ NA_character_,
      TRUE ~ url
    ),
    available_time = dplyr::case_when(
      survey == "mapbiomas" & dataset == "mapbiomas_transition" ~ NA_character_,
      TRUE ~ available_time
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
    # mapbiomas_irrigation, not mapbiomas_transition -- transition's
    # collapsed url is NA now (its real rows disagree, see delta #8 in
    # this file's header); irrigation's two real rows still genuinely
    # agree on one url.
    c("mapbiomas", "mapbiomas_irrigation"),
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

test_that("exactly the 4 MapBiomas datasets with genuinely disagreeing rows have a NA catalog url", {
  # Pins delta #8 (see file header) explicitly: these 4 -- and ONLY these
  # 4 -- have one geo_level deliberately pinned to a different Dataverse
  # collection than its siblings (see resolve_mapbiomas.R), so their real
  # manifest rows disagree on url and datasets_link()'s collapsed value is
  # NA. A NA url here is not "we don't know the url" -- it's "ask for a
  # specific geo_level with dataset_url()" (see test-fase4-metadata.R).
  na_url_datasets <- c("mapbiomas_cover", "mapbiomas_mining", "mapbiomas_transition", "mapbiomas_water")
  for (d in na_url_datasets) {
    expect_true(
      is.na(datasets_link(source = "mapbiomas", dataset = d, url = TRUE)),
      info = paste(d, "expected NA url")
    )
  }

  # Every OTHER mapbiomas dataset must still have a real, non-NA url --
  # confirming the NA-ness above is specific to those 4, not a blanket
  # regression across the whole survey.
  other_datasets <- setdiff(
    datasets_link(source = "mapbiomas")$dataset,
    na_url_datasets
  )
  for (d in other_datasets) {
    expect_false(
      is.na(datasets_link(source = "mapbiomas", dataset = d, url = TRUE)),
      label = paste(d, "url")
    )
  }
})
