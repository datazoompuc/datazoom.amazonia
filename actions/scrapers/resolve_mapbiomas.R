# actions/scrapers/resolve_mapbiomas.R
#
# MapBiomas is resolved through a single mechanism: the MapBiomas Dataverse
# archive (data.mapbiomas.org) -- a real Harvard Dataverse installation with
# a documented REST API (Search API + Data Access API), permanent DOIs, and
# versioned files. VERIFIED LIVE (2026-08-19): downloadable with no
# authentication via GET /api/access/datafile/{id}?format=original -- the
# ?format=original param recovers the real multi-sheet xlsx even for files
# Dataverse "ingested" into a flattened .tab. This replaced the old
# GCS-bucket-listing mechanism, which lagged MapBiomas's own releases by
# months (confirmed: the bucket still had no statistics/ folder for
# Collection 10/11 when Dataverse already had Collection 10.1).
#
# This file used to ALSO watch brasil.mapbiomas.org/downloads/estatisticas/
# (the WordPress statistics page) as a freshness check on mapbiomas_cover's
# docs_url. That mechanism is gone -- it never spoke for any dataset besides
# mapbiomas_cover, couldn't see a link disappearing (no candidate found was
# indistinguishable from "nothing new"), and compared collection numbers
# like "10.1" vs "11" as a guess about a labeling convention MapBiomas
# doesn't actually owe anyone. It's been replaced by a proper site-link
# INVENTORY (actions/scripts/site_inventory.R + actions/scrapers/
# watch_mapbiomas.R) that records every download link that page advertises
# and diffs it run over run -- see watch_mapbiomas.R's header for the full
# writeup, including the same June->August 2026 template-rebuild evidence
# that used to live in this file. The site no longer writes anything to the
# manifest at all: manifest = what we download, inventory = what the site
# advertises. docs_url below stays the stable Dataverse DOI.
#
# Every row this resolver emits carries every value column it can
# determine for that row -- self-sufficient rows, no inheritance (see
# R/manifest.R). Fields it cannot determine are simply left out of the
# row it returns.
#
# ---- What was investigated and where each row landed (2026-08-19) -----
#
# For every (dataset, geo_level), the Dataverse Search API was queried,
# candidates sorted newest-collection-first, and EACH candidate's actual
# file was downloaded and its sheet names inspected (via readxl::
# excel_sheets()) before trusting it -- a URL returning 200 is not enough,
# the sheet R/mapbiomas.R expects has to actually be there. Findings:
#
#   - mapbiomas_cover (base+municipality): Collection 10.1, sheet
#     COVERAGE_10.1. indigenous_land: a DIFFERENT Dataverse dataset
#     (special-territories one), Collection 10, sheet
#     COVERAGE_INDIGENOUS_TERRITORIES.
#   - mapbiomas_transition (base+biome): Collection 10, sheet
#     TRANSITION_10 (same file as that collection's coverage sheet).
#     municipality: NO SUBSTITUTE EXISTS on Dataverse at any collection --
#     its "Collection 9 by states and municipalities" dataset is
#     mislabeled and only actually contains a coverage sheet, verified by
#     downloading it. Deliberately not queried below; that row's existing
#     GCS url (mapbiomas_brasil_col9_state_municipality.xlsx) stays
#     untouched -- confirmed still working this session (483,946 rows).
#   - mapbiomas_deforestation_regeneration (municipality): Collection 10
#     split what used to be one combined DEF_SECVEG sheet into two
#     separate files. This row now points at the deforestation-only file
#     (sheet DEFORESTATION); the regeneration/secondary-vegetation half
#     is the new mapbiomas_secondary_vegetation dataset below.
#   - mapbiomas_mining (base+municipality): Collection 9, sheet
#     CITY_STATE_BIOME (fixes a dead link -- the old COL8.0 WordPress
#     upload 404s). indigenous_land: Collection 9's file DROPPED the IL
#     sheet entirely (verified on both Dataverse's copy and the current
#     WordPress-hosted COL9.0 upload) -- the newest-first walk below
#     naturally lands on Collection 8, which still has it. Not hardcoded;
#     if Dataverse ever re-adds an IL sheet to a newer collection this
#     will pick it back up automatically.
#   - mapbiomas_fire (state): only Collection 3 exists on Dataverse at
#     all (no Collection 5 stats file yet, only its ATBD handbook) --
#     same collection as before, but now via a working host (fixes a
#     dead link).
#   - mapbiomas_water (base+municipality, biome separately): Collection 4,
#     sheets WATER_CITY_ANNUAL / WATER_BIOME_ANNUAL (fixes a dead link).
#     state: Collection 4's file has NO state-level sheet at all
#     (WATER_CITY_*/WATER_BIOME_*/WATER_SUBBASIN_* only) -- not queried
#     below; that row's existing Collection-2 S3 url stays untouched.
#   - mapbiomas_irrigation: no irrigation statistics dataset exists on
#     Dataverse at all (only ATBD handbooks + an unrelated "Agriculture
#     statistics" dataset). Not queried below; stays exactly as-is,
#     resolver-free, same posture as epe/energy_state_panel.
#
# Re-verify all of this at implementation/maintenance time, not just
# trust this comment -- Dataverse content changes over time same as
# anything else.

`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_mapbiomas <- function(rows) {
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("curl", quietly = TRUE) ||
      !requireNamespace("readxl", quietly = TRUE)) {
    stop("resolve_mapbiomas() needs 'jsonlite', 'curl', and 'readxl' (CI-only; not package Imports).")
  }

  out <- list()

  ## ============================================================ ##
  ## Dataverse: search -> verify sheet -> emit                    ##
  ## ============================================================ ##

  dv_base <- "https://data.mapbiomas.org"

  dv_get_json <- function(url, timeout_s = 30) {
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout_s)),
      error = function(e) NULL
    )
    if (is.null(resp) || resp$status_code != 200) return(NULL)
    tryCatch(jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE), error = function(e) NULL)
  }

  # Dataverse's Search API -- documented, not scraped. subtree restricts to
  # the MapBiomas brazil-landcover Dataverse specifically.
  dv_search <- function(query) {
    url <- paste0(
      dv_base, "/api/search?q=", utils::URLencode(query, reserved = TRUE),
      "&subtree=brazil-landcover&type=dataset&per_page=20"
    )
    j <- dv_get_json(url)
    if (is.null(j) || is.null(j$data) || is.null(j$data$items)) return(list())
    j$data$items
  }

  dv_dataset_files <- function(global_id) {
    url <- paste0(dv_base, "/api/datasets/:persistentId/?persistentId=", global_id)
    j <- dv_get_json(url)
    if (is.null(j) || is.null(j$data$latestVersion$files)) return(list())
    lapply(j$data$latestVersion$files, function(f) f$dataFile)
  }

  dv_access_url <- function(file_id) {
    paste0(dv_base, "/api/access/datafile/", file_id, "?format=original")
  }

  dv_doi_url <- function(global_id) {
    paste0("https://doi.org/", sub("^doi:", "", global_id))
  }

  # A hit's actual sheet layout is the only thing that matters -- a 200
  # response is not enough (see file header: Dataverse's "Collection 9 by
  # municipality" dataset is mislabeled and doesn't contain what its own
  # title claims). Downloads the real file (?format=original) to inspect
  # it; skips anything implausibly large rather than hanging CI on it --
  # every real target file observed this session was well under this.
  dv_file_sheets <- function(file_id, max_bytes = 2e8) {
    head_resp <- tryCatch(
      curl::curl_fetch_memory(
        dv_access_url(file_id),
        handle = curl::new_handle(nobody = TRUE, timeout = 20)
      ),
      error = function(e) NULL
    )
    if (!is.null(head_resp)) {
      len <- suppressWarnings(as.numeric(curl::parse_headers_list(head_resp$headers)[["content-length"]][1]))
      if (!is.na(len) && len > max_bytes) return(NULL)
    }

    temp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp), add = TRUE)
    ok <- tryCatch({
      curl::curl_download(dv_access_url(file_id), temp, quiet = TRUE, handle = curl::new_handle(timeout = 120))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) return(NULL)

    tryCatch(readxl::excel_sheets(temp), error = function(e) NULL)
  }

  # "Collection 10.1" / "Coleção 11" -- Dataverse titles this dataset's own
  # collection number consistently; ATBD handbooks/factsheets also say
  # "Collection N" so is_real_stats_hit() below filters those out
  # separately rather than relying on this regex alone.
  extract_collection <- function(name) {
    m <- regmatches(name, regexpr("Collection\\s+([0-9]+(?:\\.[0-9]+)?)", name, ignore.case = TRUE))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    sub(".*?([0-9]+(?:\\.[0-9]+)?)$", "\\1", m)
  }

  is_real_stats_hit <- function(name) {
    grepl("statistics", name, ignore.case = TRUE) &&
      !grepl("ATBD|Handbook|Destaques|Factsheet|Fact_|Fact-", name, ignore.case = TRUE)
  }

  # Walk candidates newest-collection-first; take the FIRST whose actual
  # file has a sheet matching sheet_pattern. This is what makes
  # mapbiomas_mining/indigenous_land land on Collection 8 (Collection 9's
  # file dropped the IL sheet) without hardcoding "8" anywhere -- if a
  # future collection re-adds a matching sheet, this picks it up on its
  # own, no code change needed.
  resolve_via_dataverse <- function(dataset, geo_levels, query, sheet_pattern) {
    items <- tryCatch(dv_search(query), error = function(e) NULL)
    if (is.null(items) || length(items) == 0) return(NULL)

    names_ <- vapply(items, function(it) it$name %||% "", character(1))
    keep <- vapply(names_, is_real_stats_hit, logical(1))
    items <- items[keep]
    names_ <- names_[keep]
    if (length(items) == 0) return(NULL)

    cols <- vapply(names_, extract_collection, character(1))
    ord <- order(suppressWarnings(as.numeric(cols)), decreasing = TRUE, na.last = TRUE)
    items <- items[ord]
    cols <- cols[ord]

    for (i in seq_along(items)) {
      if (is.na(cols[i])) next
      gid <- items[[i]]$global_id
      if (is.null(gid)) next

      files <- tryCatch(dv_dataset_files(gid), error = function(e) list())
      for (f in files) {
        if (is.null(f$id)) next
        sheets <- dv_file_sheets(f$id)
        if (is.null(sheets)) next
        hit <- grep(sheet_pattern, sheets, ignore.case = TRUE, value = TRUE)
        if (length(hit) >= 1) {
          return(tibble::tibble(
            survey = "mapbiomas", dataset = dataset,
            geo_level = geo_levels, year = NA_character_,
            url = dv_access_url(f$id), version = cols[i],
            sheet = hit[1], docs_url = dv_doi_url(gid)
          ))
        }
      }
    }
    NULL
  }

  # See the file header for what's deliberately NOT in this list
  # (mapbiomas_transition/municipality, mapbiomas_water/state,
  # mapbiomas_irrigation -- all investigated, none have a Dataverse
  # substitute).
  #
  # geo_levels values here match the manifest's ACTUAL row shape exactly --
  # there is no base row anymore for any dataset that has real geo_level
  # overrides (see R/manifest.R and NEWS.md: a base row could silently
  # drift stale relative to its own overrides, which is exactly what had
  # happened to mapbiomas_mining's before this migration). A dataset with
  # real overrides (cover, transition, mining, water below) lists ONLY its
  # real geo_level values -- never NA_character_ alongside them, or the
  # emitted row would never match anything in the manifest and would get
  # bind_rows()'d as a brand-new, duplicate row instead of updating the
  # right one. A dataset with NO overrides at all (deforestation_regeneration,
  # secondary_vegetation, fire) is base-row-only in the OLD sense but really
  # just "unkeyed, one row, geo_level blank" now -- those still use
  # geo_levels = NA_character_, because that IS their one real row's key.
  configs <- list(
    list(dataset = "mapbiomas_cover", geo_levels = "municipality",
         query = "Coverage statistics by biomes, states and municipalities",
         sheet_pattern = "^COVERAGE"),
    list(dataset = "mapbiomas_cover", geo_levels = "indigenous_land",
         query = "Coverage and transitions statistics by special territories - Indigenous Territories",
         sheet_pattern = "^COVERAGE"),
    list(dataset = "mapbiomas_transition", geo_levels = "biome",
         query = "Coverage and transitions statistics by biomes and states",
         sheet_pattern = "^TRANSITION"),
    # Unkeyed (single row, geo_level blank) -- deforestation_regeneration
    # and secondary_vegetation have no override rows at all in the
    # manifest, same shape as mapbiomas_fire below. geo_levels = NA here IS
    # that one row's real key, not a "base row" fallback.
    list(dataset = "mapbiomas_deforestation_regeneration", geo_levels = NA_character_,
         query = "Deforestation statistics by biomes, states and municipalities",
         sheet_pattern = "^DEFORESTATION"),
    list(dataset = "mapbiomas_secondary_vegetation", geo_levels = NA_character_,
         query = "Secondary vegetation statistics by biomes, states and municipalities",
         sheet_pattern = "^SECONDARY_VEGETATION"),
    list(dataset = "mapbiomas_mining", geo_levels = "municipality",
         query = "Mining statistics",
         sheet_pattern = "^CITY_STATE_BIOME"),
    list(dataset = "mapbiomas_mining", geo_levels = "indigenous_land",
         query = "Mining statistics",
         sheet_pattern = "^IL$"),
    # Also unkeyed, like deforestation_regeneration/secondary_vegetation --
    # mapbiomas_fire has no override rows in the manifest (available_geo
    # says "state" descriptively, but that is not a key). This previously
    # read geo_levels = "state" literally, which would have emitted a row
    # that could never match the manifest's real (blank-key) row -- a
    # latent version of the exact duplication bug already fixed above for
    # deforestation_regeneration/secondary_vegetation. It never actually
    # fired because MapBiomas's "Fire scar statistics" Dataverse search has
    # been returning no hits (checked live, 2026-08-19) -- fixed anyway,
    # correctness here shouldn't depend on that staying true.
    list(dataset = "mapbiomas_fire", geo_levels = NA_character_,
         query = "Fire scar statistics",
         sheet_pattern = "^a_ANNUAL"),
    list(dataset = "mapbiomas_water", geo_levels = "municipality",
         query = "Water surface statistics",
         sheet_pattern = "^WATER_CITY_ANNUAL"),
    list(dataset = "mapbiomas_water", geo_levels = "biome",
         query = "Water surface statistics",
         sheet_pattern = "^WATER_BIOME_ANNUAL")
  )

  for (cfg in configs) {
    res <- tryCatch(
      resolve_via_dataverse(cfg$dataset, cfg$geo_levels, cfg$query, cfg$sheet_pattern),
      error = function(e) {
        message("resolve_mapbiomas(): Dataverse lookup failed for ", cfg$dataset, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res)) {
      key <- paste(cfg$dataset, paste(cfg$geo_levels, collapse = "+"), sep = "__")
      out[[key]] <- res
    }
  }

  if (length(out) == 0) {
    stop(
      "resolve_mapbiomas(): found no confidently-matched update from ",
      "Dataverse this run. This can legitimately mean 'nothing changed' -- ",
      "check manually before treating it as a scraper break. (The WordPress ",
      "statistics page is watched separately -- see watch_mapbiomas.R -- and ",
      "no longer affects this resolver's own success/failure.)"
    )
  }

  dplyr::bind_rows(out)
}
