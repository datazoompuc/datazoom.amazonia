# actions/scrapers/resolve_prodes.R
#
# PRODES is served through a public JSON API that TerraBrasilis' own
# /downloads/ page calls client-side -- confirmed by reading that page's own
# JS (`base_url+'/business/api/v1/download/all'`), NOT by scraping HTML.
# `GET .../business/api/v1/download/all` returns a flat JSON array, one
# entry per published file, each with `name`, `link` (relative to the API
# host), `category` and `enabled`.
#
# VERIFIED LIVE (2026-08-04): the entry for the Legal Amazon deforestation
# raster has
#   link = "/download/dataset/legal-amz-prodes/raster/prodes_amazonia_legal_2025_v20260408.zip"
# -- confirming the filename now carries BOTH the data year (2025) and a
# publish-date stamp (v20260408, YYYYMMDD), a naming change made after the
# manifest's committed URL ("..._2023.zip", no date stamp) was written; that
# committed URL already returns 404 live. The zip's central directory was
# inspected directly (no assumption): it contains exactly one .tif, named
# "prodes_amazonia_legal_2025_v20260408.tif" -- i.e. the raster's own
# basename matches the zip's basename byte-for-byte, which is what
# `layer_name` below relies on (terra::rast() names an unlabeled
# single-band GeoTIFF's layer after its own file basename -- see
# R/prodes.R's `layer` variable and R/download.R's prodes branch).
#
# 2026-09-08 (audit follow-up, alongside the same pass that hardened
# resolve_epe.R/resolve_mapbiomas.R): that "inspected directly" check above
# was a one-time, by-hand thing -- nothing re-verified it on later runs, so a
# renamed inner .tif would have passed this resolver silently and only
# broken at a user's load_prodes() call. Every run now re-verifies it via
# zip_remote_listing.R's zip_remote_entries() (a ranged GET of the zip's
# tail, no 133MB download) -- see that file's header for how, verified live
# against this exact zip this session.
#
# Also added: the zip's .qml sidecar (a QGIS style file INPE ships alongside
# the .tif, listing every raster code's meaning as <paletteEntry value=".."
# label="..">) gets extracted the same way (zip_remote_extract_entry(), one
# more small ranged GET) and cross-checked against R/prodes.R's hardcoded
# raster legend (R/prodes.R:130-135) -- that legend used to be "verified by
# hand once" per that file's own comment; VERIFIED LIVE this session that all
# 5 checkable codes still match exactly (deforestation = release_year - 2000
# labelled "d<year>"; residual_deforestation = release_year - 1960 labelled
# "r<year>"; hydrography = 91 "Hidrografia"; native_vegetation = 100
# "Vegetação nativa florestal"; non_forest = 101 "Vegetação nativa não
# florestal"). clouds = 99 genuinely has NO entry in the live .qml -- this
# check independently reproduces what R/prodes.R's own comment already said
# by hand ("could not be re-verified against 2025"); treated as
# expected-absent (a message, not a failure) below, since forcing a match on
# a code that legitimately isn't there would just make the resolver stop()
# every single run.
#
# All 6 PRODES dataset rows (clouds/deforestation/hydrography/
# native_vegetation/non_forest/residual_deforestation) share one url,
# layer_name and version -- under the self-sufficient-rows schema
# (R/manifest.R) each of those 6 rows must carry its own copy explicitly, so
# this resolver fans out across every real row in `rows` (the manifest rows
# currently tagged resolver == "prodes"), echoing each row's own
# geo_level/year rather than hardcoding NA -- a no-op today (every PRODES row
# is unkeyed) but the correct pattern if PRODES ever gains a keyed row (see
# resolve_epe.R's consumer/industrial block for the same echo-don't-hardcode
# pattern already applied there).

resolve_prodes <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_prodes(): no manifest rows tagged resolver == 'prodes' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_prodes() needs the 'jsonlite' and 'curl' packages (CI-only; not package Imports).")
  }

  api_url <- "https://terrabrasilis.dpi.inpe.br/business/api/v1/download/all"
  resp <- tryCatch(
    curl::curl_fetch_memory(api_url, handle = curl::new_handle(timeout = 30)),
    error = function(e) stop("resolve_prodes(): request failed for ", api_url, ": ", conditionMessage(e))
  )
  if (resp$status_code != 200) {
    stop("resolve_prodes(): TerraBrasilis download API returned HTTP ", resp$status_code)
  }

  entries <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  links <- vapply(entries, function(e) if (is.null(e$link)) NA_character_ else e$link, character(1))
  enabled <- vapply(entries, function(e) isTRUE(e$enabled), logical(1))

  pattern <- "^/download/dataset/legal-amz-prodes/raster/(prodes_amazonia_legal_([0-9]{4})_v([0-9]{8}))\\.zip$"
  matches <- regmatches(links, regexec(pattern, links))
  hit_idx <- which(enabled & vapply(matches, length, integer(1)) > 0)

  if (length(hit_idx) == 0) {
    stop(
      "resolve_prodes(): no enabled entry matching ",
      "'prodes_amazonia_legal_<year>_v<YYYYMMDD>.zip' found in the ",
      "TerraBrasilis download API response. The raster naming scheme or ",
      "API shape may have changed -- see this file's header for the ",
      "pattern last verified live."
    )
  }

  # more than one edition can be listed at once (e.g. while a new one is
  # being published alongside the outgoing one) -- keep the newest stamp.
  hits <- matches[hit_idx]
  stamps <- vapply(hits, function(m) m[4], character(1))
  best <- hit_idx[order(stamps, decreasing = TRUE)][1]
  m <- matches[[best]]

  layer_name <- m[2]
  # Named release_year, not year: tibble() evaluates arguments left-to-right
  # and lets a later argument reference an earlier one BY COLUMN NAME, so
  # `available_time = year` below would silently resolve to the `year =
  # NA_character_` column just defined in the same tibble() call instead of
  # this variable -- confirmed live (see resolve_prodes(NULL) output showing
  # available_time == "2007-NA"/"2010-NA" before this rename).
  release_year <- m[3]
  stamp <- m[4]
  url <- paste0("https://terrabrasilis.dpi.inpe.br", links[best])

  ## -- resolver alert cache: an informational Slack signal for "a new -------
  ## release showed up but failed content verification" -- see
  ## actions/scripts/resolver_alert_cache.R's header (generalizes the same
  ## idea resolve_mapbiomas.R's cache already covers for Dataverse
  ## candidates). item_key = this release's stamp, so a NEW stamp failing is
  ## reported once; the SAME stamp failing again on a later run (the site
  ## stays broken) is already in the committed cache and won't re-alert --
  ## the stop() calls below are UNCHANGED (still open a GitHub issue every
  ## run via resolver_failed); this is a purely additive side channel.
  ## repo_root is a build_manifest.R global already in scope here (this file
  ## is source()d into that environment, same as resolve_mapbiomas.R's own
  ## cache read) -- falls back to an in-memory-only cache if somehow
  ## missing, so a standalone/test invocation degrades gracefully.
  alert_cache_path <- tryCatch(
    file.path(repo_root, "actions", "cache", "resolver_alerts.csv"),
    error = function(e) NA_character_
  )
  alert_cache <- if (!is.na(alert_cache_path)) {
    read_resolver_alert_cache(alert_cache_path)
  } else {
    read_resolver_alert_cache(tempfile())
  }
  record_prodes_verdict <- function(verdict, reason = NA_character_) {
    alert_cache <<- alert_cache_upsert(
      alert_cache,
      source = "prodes", item_key = stamp, dataset = "(all)", geo_level = NA_character_,
      verdict = verdict, reason = reason
    )
    out_path <- tryCatch(
      file.path(OUT_DIR, "prodes_alert_cache_candidate.csv"),
      error = function(e) tempfile(fileext = ".csv")
    )
    write_resolver_alert_cache(alert_cache, out_path)
    assign("prodes_alert_cache_candidate_path", out_path, envir = .GlobalEnv)
  }

  ## -- verify the zip's actual contents, not just the API's filename --------
  # See this file's header. zip_remote_listing.R is sourced by
  # build_manifest.R alongside every other actions/scripts/*.R helper.

  zip_entries <- tryCatch(
    zip_remote_entries(url),
    error = function(e) stop("resolve_prodes(): zip content verification failed for ", url, ": ", conditionMessage(e))
  )

  tif_name <- grep("\\.tif$", names(zip_entries), value = TRUE)
  if (length(tif_name) != 1 || sub("\\.tif$", "", tif_name) != layer_name) {
    tif_reason <- paste0(
      "the zip's inner .tif (found: ", paste(tif_name, collapse = ", "),
      ") doesn't match the expected layer_name '", layer_name, "' derived from the zip's ",
      "own filename -- R/prodes.R's terra::rast() call relies on these matching. The ",
      "zip's internal structure may have changed."
    )
    record_prodes_verdict("fail", tif_reason)
    stop("resolve_prodes(): ", tif_reason)
  }

  qml_name <- grep("\\.qml$", names(zip_entries), value = TRUE)
  if (length(qml_name) == 1) {
    qml_txt <- tryCatch(
      rawToChar(zip_remote_extract_entry(url, zip_entries[[qml_name]])),
      error = function(e) stop("resolve_prodes(): could not read the .qml legend: ", conditionMessage(e))
    )
    pal_entries <- regmatches(qml_txt, gregexpr('<paletteEntry[^/]*/>', qml_txt))[[1]]
    pal_values <- as.integer(sub('.*value="([0-9]+)".*', "\\1", pal_entries))
    pal_labels <- sub('.*label="([^"]*)".*', "\\1", pal_entries)
    pal_map <- setNames(pal_labels, as.character(pal_values))

    release_year_int <- as.integer(release_year)
    d_code <- release_year_int - 2000L
    r_code <- release_year_int - 1960L
    # Mirrors R/prodes.R:130-135's hardcoded raster_codes exactly -- if that
    # map ever changes, this list needs updating alongside it.
    expected_legend <- list(
      list(code = d_code, label = sprintf("%d d%d", d_code, release_year_int)),
      list(code = r_code, label = sprintf("%d r%d", r_code, release_year_int)),
      list(code = 91L, label = "91 Hidrografia"),
      list(code = 100L, label = "100 Vegetação nativa florestal"),
      list(code = 101L, label = "101 Vegetação nativa não florestal")
    )

    mismatches <- character(0)
    for (item in expected_legend) {
      actual <- pal_map[[as.character(item$code)]]
      if (is.null(actual)) {
        mismatches <- c(mismatches, sprintf("code %d: expected label '%s', not present in the .qml", item$code, item$label))
      } else if (!identical(trimws(actual), trimws(item$label))) {
        mismatches <- c(mismatches, sprintf("code %d: expected label '%s', got '%s'", item$code, item$label, actual))
      }
    }
    if (length(mismatches) > 0) {
      qml_reason <- paste0(
        "the .qml legend no longer matches R/prodes.R's hardcoded raster codes ",
        "(R/prodes.R:130-135) -- update that map by hand before trusting this release:\n- ",
        paste(mismatches, collapse = "\n- ")
      )
      record_prodes_verdict("fail", qml_reason)
      stop("resolve_prodes(): ", qml_reason)
    }
    if (!(99L %in% pal_values)) {
      message(
        "resolve_prodes(): clouds (code 99) still has no entry in this release's .qml ",
        "legend -- matches the documented, currently-unverifiable state noted in R/prodes.R."
      )
    }
  } else {
    message("resolve_prodes(): expected exactly one .qml sidecar in the zip, found ", length(qml_name), " -- skipping the raster-legend cross-check this run.")
  }

  # deforestation/residual_deforestation are PRODES' two multi-year series;
  # their available_time keeps its documented start year and extends to
  # whatever year the API just reported. The other four datasets
  # (clouds/hydrography/native_vegetation/non_forest) only ever cover the
  # single most recent release year.
  available_time <- dplyr::case_when(
    rows$dataset == "deforestation" ~ paste("2007", release_year, sep = "-"),
    rows$dataset == "residual_deforestation" ~ paste("2010", release_year, sep = "-"),
    TRUE ~ release_year
  )

  record_prodes_verdict("pass")

  tibble::tibble(
    survey = "prodes", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = url, layer_name = layer_name, version = stamp,
    available_time = available_time, resolver = "prodes"
  )
}
