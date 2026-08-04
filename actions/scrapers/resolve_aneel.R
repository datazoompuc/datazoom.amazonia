# actions/scrapers/resolve_aneel.R
#
# Resolves ANEEL's two CKAN-backed datasets through the CKAN JSON API at
# dadosabertos.aneel.gov.br -- NOT HTML scraping. CKAN's `package_show`
# action returns every resource (file) attached to a dataset as JSON; we
# just filter and pattern-match on resource metadata, never parse a page.
#
# `energy_generation` (SIGA, served from a GitLab raw URL) is version-free
# and is intentionally NOT resolved here -- it keeps resolver = NA in the
# manifest and is only covered by the HTTP-check in the validation gate.
#
# Contract (see actions/scripts/build_manifest.R): resolve_aneel(rows)
# receives the manifest rows currently tagged resolver == "aneel" (not used
# here, since every row this resolver owns is looked up fresh from ANEEL's
# API rather than derived from the existing rows) and must return a tibble
# with key columns survey/dataset/geo_level/year plus any changed value
# columns. It must stop() on any failure -- never return a partial result.

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

  out$cde_years <- tibble::tibble(
    survey = "aneel", dataset = "energy_development_budget",
    geo_level = NA_character_, year = found_years,
    url = found_urls
  )
  out$cde_base <- tibble::tibble(
    survey = "aneel", dataset = "energy_development_budget",
    geo_level = NA_character_, year = NA_character_,
    available_time = paste(min(as.integer(found_years)), max(as.integer(found_years)), sep = "-")
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

  dplyr::bind_rows(out)
}
