# actions/scrapers/resolve_ips.R
#
# ipsamazonia.org.br is a React single-page app -- its HTML shell (~1.3KB)
# has no data links at all. VERIFIED LIVE (2026-08-04): the app's own JS
# bundle (linked from the shell as /static/js/main.<hash>.js) hardcodes the
# current workbook's URL as a literal string:
#   painel.ipsamazonia.org.br/uploads/IPS_Amazonia_<year>_<hash>.xlsx
# (painel.ipsamazonia.org.br is the Strapi CMS backing the site). Two
# static GETs -- the HTML shell, then the JS bundle it points at -- no
# browser/headless rendering needed to reach it. RE-VERIFIED LIVE
# 2026-09-14: the bundle hardcodes EXACTLY ONE /uploads/*.xlsx reference (it
# also links one /uploads/*.pdf, excluded by the \.xlsx$ anchor below) -- no
# ambiguity, no candidate walk needed.
#
# 2026-09-14: this resolver used to be DETECT-ONLY (emitting only `version`/
# `docs_url`, never `url`), on the assumption that the Strapi workbook was
# "a single year's workbook", a different shape than load_ips()'s (R/ips.R)
# four-year-sheet read of the manifest's Google-Drive-hosted workbook. That
# assumption was never actually checked, and turned out to be WRONG --
# downloaded and compared both workbooks directly:
#
#              Strapi (this resolver's find)   Drive (manifest, until now)
#   sheets     2023, 2021, "2018 ", 2014,       identical (including the
#              "Definição dos indicadores"      "2018 " trailing space)
#   rows/sheet 772 (every year sheet)           772 (every year sheet)
#   colnames   71, janitor::clean_names()-      identical on all 4 year
#              identical on all 4 year sheets   sheets (setdiff both ways
#                                                is empty)
#
# i.e. a drop-in replacement, and the PUBLISHER's own copy rather than the
# manifest's old Google Drive link -- CONFIRMED (2026-09-14, via Drive's
# own "Proprietário" field) to be owned by a personal gmail account
# (breno.avidos@gmail.com), not Datazoom and not in Datazoom's shared
# Drive folder -- a one-off personal upload, not an org-maintained mirror.
# It was NOT kept in sync with anything upstream -- this resolver now emits a
# real `url`, re-verified every run (see below) rather than trusted from a
# one-time-by-hand comparison.
#
# All 8 IPS dataset rows share one url/version/available_time -- under the
# self-sufficient-rows schema (R/manifest.R) each row must carry its own
# copy explicitly, so this resolver fans out across every real row in
# `rows` (the manifest rows tagged resolver == "ips"), echoing each row's
# own geo_level/year rather than hardcoding NA -- a no-op today (every IPS
# row is unkeyed) but the correct pattern if IPS ever gains a keyed row
# (see resolve_epe.R's consumer/industrial block for the same
# echo-don't-hardcode pattern already applied there).

resolve_ips <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_ips(): no manifest rows tagged resolver == 'ips' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_ips() needs the 'curl' package (CI-only; not a package Import).")
  }

  fetch <- function(url, timeout_s = 30) {
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout_s, followlocation = TRUE)),
      error = function(e) NULL
    )
    if (is.null(resp) || resp$status_code != 200) {
      return(NULL)
    }
    txt <- rawToChar(resp$content)
    Encoding(txt) <- "UTF-8"
    txt
  }

  shell <- fetch("https://ipsamazonia.org.br/")
  if (is.null(shell)) {
    stop("resolve_ips(): could not fetch https://ipsamazonia.org.br/.")
  }

  bundle_hit <- regmatches(shell, regexpr('src="(/static/js/main\\.[a-f0-9]+\\.js)"', shell))
  if (length(bundle_hit) == 0 || !nzchar(bundle_hit)) {
    stop(
      "resolve_ips(): could not find a /static/js/main.<hash>.js bundle ",
      "reference in ipsamazonia.org.br's HTML shell. Site build may have ",
      "changed."
    )
  }
  bundle_path <- sub('^src="', "", bundle_hit)
  bundle_path <- sub('"$', "", bundle_path)

  bundle <- fetch(paste0("https://ipsamazonia.org.br", bundle_path))
  if (is.null(bundle)) {
    stop("resolve_ips(): could not fetch the JS bundle at ", bundle_path, ".")
  }

  hit <- regmatches(
    bundle,
    regexpr("painel\\.ipsamazonia\\.org\\.br/uploads/IPS_Amazonia_([0-9]{4})_([a-f0-9]+)\\.xlsx", bundle)
  )
  if (length(hit) == 0 || !nzchar(hit)) {
    stop(
      "resolve_ips(): no 'IPS_Amazonia_<year>_<hash>.xlsx' reference found ",
      "in the JS bundle. Site build may have changed."
    )
  }

  year <- sub(".*IPS_Amazonia_([0-9]{4})_([a-f0-9]+)\\.xlsx.*", "\\1", hit)
  hash <- sub(".*IPS_Amazonia_([0-9]{4})_([a-f0-9]+)\\.xlsx.*", "\\2", hit)
  stamp <- paste(year, hash, sep = "_")
  url <- paste0("https://", hit) # `hit` already carries the painel.../uploads/... host+path

  ## -- resolver alert cache: an informational Slack signal for "a new ------
  ## upload showed up but failed content verification" -- see
  ## actions/scripts/resolver_alert_cache.R's header. item_key = this
  ## upload's stamp (year_hash), so a NEW upload failing is reported once;
  ## the SAME upload failing again on a later run is already in the
  ## committed cache and won't re-alert -- the stop() below is UNCHANGED
  ## (still opens a GitHub issue every run via resolver_failed); this is a
  ## purely additive side channel, same relationship resolve_prodes.R's
  ## cache already has to its own stop() calls.
  alert_rec <- alert_recorder("ips")

  ## -- verify the workbook's actual tabs, not just the JS bundle's filename -
  # painel.ipsamazonia.org.br serves Accept-Ranges: bytes (verified live) --
  # an xlsx IS a zip, so zip_remote_listing.R applies. Pull xl/workbook.xml
  # (~1.1KB, verified live) via one ranged GET instead of downloading the
  # whole ~2.5MB file, and check every year load_ips() (R/ips.R) currently
  # promises via this dataset's manifest available_time is present as a tab.
  zip_entries <- tryCatch(
    zip_remote_entries(url),
    error = function(e) stop("resolve_ips(): zip content verification failed for ", url, ": ", conditionMessage(e))
  )
  wb_entry <- zip_entries[["xl/workbook.xml"]]
  if (is.null(wb_entry)) {
    stop("resolve_ips(): no xl/workbook.xml found in ", url, " -- not a valid xlsx, or the internal layout changed.")
  }
  wb_xml <- tryCatch(
    rawToChar(zip_remote_extract_entry(url, wb_entry)),
    error = function(e) stop("resolve_ips(): could not read xl/workbook.xml: ", conditionMessage(e))
  )
  sheet_names <- regmatches(wb_xml, gregexpr('<sheet name="[^"]*"', wb_xml))[[1]]
  sheet_names <- sub('<sheet name="', "", sub('"$', "", sheet_names))
  if (length(sheet_names) == 0) {
    stop("resolve_ips(): could not parse any <sheet name=\"...\"> entries out of xl/workbook.xml -- workbook.xml format may have changed.")
  }

  # Year tabs only, matched on the digits (not the raw string) -- the SAME
  # whitespace-tolerant rule ips_match_sheets() (R/ips.R) applies at read
  # time, because the "2018 " tab (trailing space, verified live) is not a
  # one-off: this check would otherwise reject a perfectly good workbook.
  year_tabs <- suppressWarnings(as.integer(trimws(sheet_names)))
  years_found <- sort(unique(year_tabs[!is.na(year_tabs)]))

  # rows$year is NA for IPS's unkeyed rows -- the years this dataset
  # actually promises live in available_time, not `year`/`geo_level`.
  expected_years <- tryCatch(
    parse_years(dataset_meta("ips", unique(rows$dataset)[1], "available_time")),
    error = function(e) integer(0)
  )
  missing_years <- setdiff(expected_years, years_found)

  if (length(missing_years) > 0) {
    ips_reason <- paste0(
      "the workbook at ", url, " is missing year tab(s) ",
      paste(missing_years, collapse = ", "), " that the manifest's available_time ",
      "promises -- tabs found: ", paste(sheet_names, collapse = ", ")
    )
    alert_rec(item_key = stamp, dataset = "(all)", verdict = "fail", reason = ips_reason)
    stop("resolve_ips(): ", ips_reason)
  }

  alert_rec(item_key = stamp, dataset = "(all)", verdict = "pass")

  available_time <- paste(years_found, collapse = ", ")

  tibble::tibble(
    survey = "ips", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = url, version = stamp,
    available_time = available_time, docs_url = "https://ipsamazonia.org.br/",
    resolver = "ips"
  )
}
