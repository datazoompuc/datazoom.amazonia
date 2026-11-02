# actions/scrapers/resolve_aneel.R
#
# Resolves ANEEL's three CKAN-backed datasets through the CKAN JSON API at
# dadosabertos.aneel.gov.br -- NOT HTML scraping. CKAN's `package_show`
# action returns every resource (file) attached to a dataset as JSON; we
# just filter and pattern-match on resource metadata, never parse a page.
#
# Contract (see actions/scripts/build_manifest.R): resolve_aneel(rows)
# receives the manifest rows currently tagged resolver == "aneel" (not used
# here, since every row this resolver owns is looked up fresh from ANEEL's
# API rather than derived from the existing rows) and must return a tibble
# with key columns survey/dataset/geo_level/year plus any changed value
# columns. It must stop() on any failure -- never return a partial result.
#
# available_time: energy_development_budget reads it off the year in each
# resource's URL; energy_generation reads it from the CSV itself (see
# aneel_generation_available_time()). energy_enterprises_distributed is a current
# register with no time dimension, so its cell stays blank and it is listed in
# manifest_validate.R's AVAILABLE_TIME_EXEMPT.

# The first and last real years of DatEntradaOperacao in the SIGA CSV at `path`.
# A window, not a year list: this is a register of plants by the date each entered
# operation, and a year with no new plant is not missing data. Year 1900 is left
# out because it is a placeholder, not a date (VERIFIED LIVE 2026-11-01: 2,371 of
# 25,046 rows read 1900-01-03). The first year still reports what the file says,
# typos included: that day it was 1901, from a plant called "North Shopping
# Caruaru" dated 1901-12-04, and nothing here can tell a typo from an old plant.
aneel_generation_available_time <- function(path) {
  dat <- readr::read_delim(
    path,
    delim = ";", col_select = "DatEntradaOperacao",
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  )
  years <- suppressWarnings(as.integer(substr(dat$DatEntradaOperacao, 1, 4)))
  years <- years[!is.na(years) & years > 1900L]
  if (length(years) == 0) {
    stop("no usable DatEntradaOperacao values")
  }
  paste(min(years), max(years), sep = "-")
}

resolve_aneel <- function(rows) {
  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_aneel() needs the 'jsonlite' and 'curl' packages (CI-only; not package Imports).")
  }

  ckan_package_show <- function(package_id) {
    url <- paste0("https://dadosabertos.aneel.gov.br/api/3/action/package_show?id=", package_id)
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 30)),
      error = function(e) stop("resolve_aneel(): request failed for package ", package_id, ": ", conditionMessage(e))
    )
    if (resp$status_code != 200) {
      stop("resolve_aneel(): ANEEL CKAN API returned HTTP ", resp$status_code, " for package ", package_id)
    }
    body <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    if (!isTRUE(body$success)) {
      stop("resolve_aneel(): CKAN API reported success = false for package ", package_id)
    }
    body$result$resources
  }

  resource_url <- function(resource) if (is.null(resource$url)) NA_character_ else resource$url
  resource_format <- function(resource) if (is.null(resource$format)) NA_character_ else resource$format

  out <- list()

  ## -- energy_development_budget (CDE): one row per year found ------------

  resources <- ckan_package_show("a7191647-b187-4893-b20a-8954d57ff89c")
  urls <- vapply(resources, resource_url, character(1))
  formats <- vapply(resources, resource_format, character(1))
  csv_urls <- urls[!is.na(formats) & toupper(formats) == "CSV" & !is.na(urls)]

  year_match <- stringr::str_match(csv_urls, "cde-beneficiarios-rede-basica-(\\d{4})")
  found_years <- year_match[!is.na(year_match[, 2]), 2]
  found_urls <- csv_urls[!is.na(year_match[, 2])]

  if (length(found_years) == 0) {
    stop("resolve_aneel(): no year-tagged CDE CSV resources found -- ANEEL package layout may have changed.")
  }

  # available_time is dataset-wide (read via dataset_meta(), see
  # R/aneel.R) but there is no base row left to write it onto -- every one
  # of this dataset's rows must carry the SAME copy of it explicitly (see
  # R/manifest.R's self-sufficient-rows schema), so it's fanned across
  # every year row instead of written once.
  out$cde_years <- tibble::tibble(
    survey = "aneel", dataset = "energy_development_budget",
    geo_level = NA_character_, year = found_years,
    url = found_urls,
    available_time = sidra_api_years_to_available_time(found_years)
  )

  ## -- energy_enterprises_distributed: single resource, no year in URL ----

  resources2 <- ckan_package_show("5e0fafd2-21b9-4d5b-b622-40438d40aba2")
  urls2 <- vapply(resources2, resource_url, character(1))
  match_idx <- which(!is.na(urls2) & grepl("empreendimento-geracao-distribuida", urls2, fixed = TRUE))

  if (length(match_idx) == 0) {
    stop("resolve_aneel(): 'empreendimento-geracao-distribuida' resource not found in ANEEL package.")
  }

  out$enterprises <- tibble::tibble(
    survey = "aneel", dataset = "energy_enterprises_distributed",
    geo_level = NA_character_, year = NA_character_,
    url = urls2[match_idx[1]]
  )

  ## -- energy_generation (SIGA): single CSV resource, no year in URL -----

  resources3 <- ckan_package_show("6d90b77c-c5f5-4d81-bdec-7bc619494bb9")
  urls3 <- vapply(resources3, resource_url, character(1))
  match_idx3 <- which(!is.na(urls3) & grepl("siga-empreendimentos-geracao.csv", urls3, fixed = TRUE))

  if (length(match_idx3) == 0) {
    stop("resolve_aneel(): 'siga-empreendimentos-geracao.csv' resource not found in ANEEL SIGA package.")
  }

  generation_url <- urls3[match_idx3[1]]
  generation_file <- tempfile(fileext = ".csv")
  on.exit(unlink(generation_file), add = TRUE)
  generation_resp <- tryCatch(
    curl::curl_fetch_disk(generation_url, generation_file, handle = curl::new_handle(timeout = 1000, followlocation = TRUE)),
    error = function(e) stop("resolve_aneel(): download failed for ", generation_url, ": ", conditionMessage(e))
  )
  if (generation_resp$status_code != 200) {
    stop("resolve_aneel(): HTTP ", generation_resp$status_code, " for ", generation_url)
  }

  out$generation <- tibble::tibble(
    survey = "aneel", dataset = "energy_generation",
    geo_level = NA_character_, year = NA_character_,
    url = generation_url,
    available_time = aneel_generation_available_time(generation_file)
  )

  dplyr::bind_rows(out)
}
