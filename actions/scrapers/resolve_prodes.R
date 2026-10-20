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
# raster legend (prodes_raster_code()) -- that legend used to be "verified by
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
# 2026-10-20: the check moved out of resolve_prodes() into file-scope
# functions (prodes_legend_mismatches() and friends, below) so
# tests/testthat/test-prodes-legend.R can hold them to R/prodes.R's own copy
# -- the "mirrors R/prodes.R exactly" promise used to be a comment only.
# Also: a zip with no (or several) .qml now fails the run instead of skipping
# the check with a message; every d<year>/r<year> entry in the legend is
# checked, not only the release year's; and clouds fails if code 99 ever
# appears meaning something else (absent is still just a message).
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

# ---- raster legend check ----------------------------------------------------
#
# A copy of R/prodes.R's prodes_raster_code()/prodes_legend_label()/
# prodes_parse_qml(): this script runs without the package loaded, so it
# can't call them. tests/testthat/test-prodes-legend.R fails if the two
# copies ever disagree -- change both together. Kept at file scope (not
# inside resolve_prodes()) so that test can reach them.

# The 5 codes checkable against a release's .qml; clouds (99) is handled
# apart in prodes_legend_mismatches(), since no release so far lists it.
prodes_expected_legend <- function(release_year) {
  d_code <- release_year - 2000L
  r_code <- release_year - 1960L
  list(
    list(dataset = "deforestation", code = d_code, label = sprintf("%d d%d", d_code, release_year)),
    list(dataset = "residual_deforestation", code = r_code, label = sprintf("%d r%d", r_code, release_year)),
    list(dataset = "hydrography", code = 91L, label = "91 Hidrografia"),
    list(dataset = "native_vegetation", code = 100L, label = "100 Vegeta\u00e7\u00e3o nativa florestal"),
    list(dataset = "non_forest", code = 101L, label = "101 Vegeta\u00e7\u00e3o nativa n\u00e3o florestal")
  )
}

# .qml text -> named character vector, label by code. Same parsing as
# R/prodes.R's prodes_parse_qml() (see there for why a regex, `[^>]*` and the
# unescaping).
prodes_qml_palette <- function(qml_txt) {
  entries <- regmatches(qml_txt, gregexpr("<paletteEntry\\b[^>]*>", qml_txt, perl = TRUE))[[1]]
  attr_of <- function(name) {
    m <- regmatches(entries, regexec(sprintf("\\b%s=\"([^\"]*)\"", name), entries, perl = TRUE))
    vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  }
  values <- suppressWarnings(as.integer(attr_of("value")))
  labels <- prodes_qml_unescape(attr_of("label"))
  keep <- !is.na(values) & !is.na(labels)
  setNames(labels[keep], values[keep])
}

prodes_qml_unescape <- function(x) {
  num <- gregexpr("&#(x[0-9A-Fa-f]+|[0-9]+);", x, perl = TRUE)
  regmatches(x, num) <- lapply(regmatches(x, num), function(ents) {
    vapply(ents, function(e) {
      code <- sub("^&#(.*);$", "\\1", e)
      n <- if (startsWith(code, "x")) strtoi(substring(code, 2), 16L) else as.integer(code)
      intToUtf8(n)
    }, character(1), USE.NAMES = FALSE)
  })
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Everything in `pal_map` that contradicts the codes; character(0) if none.
prodes_legend_mismatches <- function(pal_map, release_year) {
  mismatches <- character(0)
  for (item in prodes_expected_legend(release_year)) {
    actual <- unname(pal_map[as.character(item$code)])
    if (is.na(actual)) {
      mismatches <- c(mismatches, sprintf("code %d: expected label '%s', not present in the .qml", item$code, item$label))
    } else if (!identical(trimws(actual), item$label)) {
      mismatches <- c(mismatches, sprintf("code %d: expected label '%s', got '%s'", item$code, item$label, actual))
    }
  }

  # Every d<year>/r<year> entry, not just release_year's: load_prodes()
  # accepts every year in available_time, all through the same offsets.
  labels <- trimws(unname(pal_map))
  parts <- regmatches(labels, regexec("^([0-9]+) ([dr])([0-9]{4})$", labels))
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (length(p) != 4) next
    yr <- as.integer(p[4])
    want <- if (p[3] == "d") yr - 2000L else yr - 1960L
    code <- as.integer(names(pal_map)[i])
    if (code != want) {
      mismatches <- c(mismatches, sprintf("code %d is labelled '%s', but %s%d should be code %d", code, labels[i], p[3], yr, want))
    }
  }

  # clouds: absent is the known state; present but meaning something else
  # is not
  clouds <- unname(pal_map["99"])
  if (!is.na(clouds) && !grepl("nuvem", clouds, ignore.case = TRUE)) {
    mismatches <- c(mismatches, sprintf("code 99 (clouds) is labelled '%s', which is not clouds", clouds))
  }

  unique(mismatches)
}

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
  ## alert_recorder() owns the read/upsert/write/global-stash boilerplate
  ## (shared with resolve_epe.R/resolve_ips.R) -- see its header.
  alert_rec <- alert_recorder("prodes")
  record_prodes_verdict <- function(verdict, reason = NA_character_) {
    alert_rec(item_key = stamp, dataset = "(all)", verdict = verdict, reason = reason)
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

  # A missing legend fails the run like a wrong .tif does: without it nothing
  # tells us the codes still mean what load_prodes() assumes, and a wrong
  # code doesn't error, it silently counts the wrong pixels for every user.
  qml_name <- grep("\\.qml$", names(zip_entries), value = TRUE)
  if (length(qml_name) != 1) {
    qml_reason <- paste0(
      "expected exactly one .qml legend in the zip, found ", length(qml_name),
      " (", paste(names(zip_entries), collapse = ", "), ") -- the raster codes in ",
      "R/prodes.R's prodes_raster_code() can't be checked against this release. ",
      "Open the zip and check them by hand before trusting it."
    )
    record_prodes_verdict("fail", qml_reason)
    stop("resolve_prodes(): ", qml_reason)
  }
  qml_txt <- tryCatch(
    rawToChar(zip_remote_extract_entry(url, zip_entries[[qml_name]])),
    error = function(e) stop("resolve_prodes(): could not read the .qml legend: ", conditionMessage(e))
  )
  Encoding(qml_txt) <- "UTF-8"
  pal_map <- prodes_qml_palette(qml_txt)

  mismatches <- prodes_legend_mismatches(pal_map, as.integer(release_year))
  if (length(mismatches) > 0) {
    qml_reason <- paste0(
      "the .qml legend no longer matches R/prodes.R's hardcoded raster codes ",
      "(prodes_raster_code()) -- update them, and this file's copy, by hand ",
      "before trusting this release:\n- ",
      paste(mismatches, collapse = "\n- ")
    )
    record_prodes_verdict("fail", qml_reason)
    stop("resolve_prodes(): ", qml_reason)
  }
  if (!("99" %in% names(pal_map))) {
    message(
      "resolve_prodes(): clouds (code 99) still has no entry in this release's .qml ",
      "legend -- matches the documented, currently-unverifiable state noted in R/prodes.R."
    )
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
