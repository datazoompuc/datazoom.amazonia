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
# All 6 PRODES dataset rows (clouds/deforestation/hydrography/
# native_vegetation/non_forest/residual_deforestation) share one url,
# layer_name and version -- under the self-sufficient-rows schema
# (R/manifest.R) each of those 6 rows must carry its own copy explicitly,
# so this resolver fans them out across every dataset in `rows` (the
# manifest rows currently tagged resolver == "prodes") instead of writing
# a single shared row the others used to inherit from.

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

  # deforestation/residual_deforestation are PRODES' two multi-year series;
  # their available_time keeps its documented start year and extends to
  # whatever year the API just reported. The other four datasets
  # (clouds/hydrography/native_vegetation/non_forest) only ever cover the
  # single most recent release year.
  datasets <- unique(rows$dataset)
  available_time <- dplyr::case_when(
    datasets == "deforestation" ~ paste("2007", release_year, sep = "-"),
    datasets == "residual_deforestation" ~ paste("2010", release_year, sep = "-"),
    TRUE ~ release_year
  )

  tibble::tibble(
    survey = "prodes", dataset = datasets,
    geo_level = NA_character_, year = NA_character_,
    url = url, layer_name = layer_name, version = stamp,
    available_time = available_time, resolver = "prodes"
  )
}
