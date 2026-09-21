# actions/scrapers/resolve_seeg.R
#
# SEEG's own dados/ page (seeg.eco.br/dados/) links a versioned XLSX
# directly in static HTML -- no API, nothing rendered client-side. VERIFIED
# LIVE: a plain GET returns HTTP 403 without a browser-like User-Agent; WITH
# one it returns 200 and links the full absolute URL, e.g.
# "https://seeg.eco.br/wp-content/uploads/2025/12/Dados-municipais-resumido-
# CO2e-GWP-AR5-13.0.xlsx" -- the year/month path segment is NOT guessable
# from the filename alone (confirmed live it does not always match the
# current calendar date), so this resolver captures the href verbatim
# rather than reconstructing a path.
#
# 2026-09-15 (hardened): the href match used to require the ENTIRE current
# filename convention verbatim ("Dados-municipais-resumido-CO2e-GWP-AR5-
# <version>.xlsx"), which meant a purely cosmetic rename by SEEG (dropping
# "resumido", a future GWP-AR6 methodology revision, different casing)
# would break discovery even though the actual data is fine. Loosened to
# require only the one semantic anchor that actually matters -- "municipais"
# in the filename, case-insensitive -- because the page also links a
# SEPARATE national-level workbook (confirmed live) that must never be
# picked up by mistake. Everything else about the filename is now the
# structural checks' job, not the regex's: if a rename lands on the wrong
# file, the "Dados" sheet name check or the pivot-cache field check below
# will still catch it, and they check the actual downloaded content, not a
# string in the URL. `stop()`s if zero or more than one municipal-looking
# candidate is found, so an unexpectedly ambiguous page still fails loudly
# instead of silently guessing.
#
# 2026-09-15: this used to be a DETECT-ONLY resolver (emitted only
# `version`/`docs_url`, never `url`) on the assumption that "the live file
# has a different shape" than what R/seeg.R read -- an assumption that was
# never actually checked. It was checked this session, live, by downloading
# both the manifest's then-committed 2021 snapshot and the real current
# v13.0 file and comparing each workbook's own
# xl/pivotCache/pivotCacheDefinition1.xml. The assumption was TRUE and worse
# than guessed -- not a version bump, a genuine schema restructuring (see
# R/seeg.R's header for the full old-to-new column mapping). R/seeg.R was
# rewritten against the new schema in the same round of work that graduated
# this resolver, so it is now safe to emit a real `url`.
#
# Structural verification mirrors resolve_prodes.R's/resolve_ips.R's
# live-verification methodology: an xlsx IS a zip, so zip_remote_listing.R
# applies. Two small ranged GETs -- xl/workbook.xml (~2.4KB, confirmed live)
# to check the data sheet is still named "Dados", and
# xl/pivotCache/pivotCacheDefinition1.xml (~163KB, confirmed live -- much
# smaller than PRODES' whole zip) to confirm the exact <cacheField name="...">
# set R/seeg.R's rewrite depends on is still present and the year-field
# range still covers what it reads -- neither GET ever touches the 900MB+
# data sheet itself. seeg.eco.br 403s these ranged GETs too without the same
# browser User-Agent used for the /dados/ page fetch (confirmed live), hence
# zip_remote_listing.R's `useragent` parameter.
#
# All 6 SEEG dataset rows share one url/version/available_time -- under the
# self-sufficient-rows schema (R/manifest.R) each row must carry its own
# copy explicitly, so this resolver fans out across every real row in
# `rows` (the manifest rows tagged resolver == "seeg"), echoing each row's
# own geo_level/year rather than hardcoding NA, same pattern resolve_ips.R
# and resolve_epe.R already use.

SEEG_EXPECTED_CACHE_FIELDS <- c(
  "Setor de emissão", "Categoria emissora", "Sub-categoria emissora",
  "Atividade geral", "Recorte", "Estado", "Município", "ID Território"
)

resolve_seeg <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_seeg(): no manifest rows tagged resolver == 'seeg' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_seeg() needs the 'curl' package (CI-only; not a package Import).")
  }

  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
  )

  h <- curl::new_handle(timeout = 30, useragent = ua)
  resp <- tryCatch(curl::curl_fetch_memory("https://seeg.eco.br/dados/", handle = h), error = function(e) NULL)

  if (is.null(resp) || resp$status_code != 200) {
    stop(
      "resolve_seeg(): could not fetch https://seeg.eco.br/dados/ (",
      if (is.null(resp)) "request failed" else paste("HTTP", resp$status_code), "). ",
      "This page returns 403 to requests without a browser User-Agent -- ",
      "confirm the User-Agent set in this resolver still works before ",
      "assuming the site itself changed."
    )
  }

  html <- rawToChar(resp$content)
  Encoding(html) <- "UTF-8"

  # Captures the full absolute href, not just the filename -- the
  # /wp-content/uploads/<year>/<month>/ path segment moves with each
  # upload and is not derivable from the filename alone. Deliberately loose
  # beyond the domain/path and the ".xlsx" extension -- see this file's
  # header for why the filename convention itself isn't load-bearing here.
  hrefs <- regmatches(
    html,
    gregexpr('href="https://seeg\\.eco\\.br/wp-content/uploads/[^"]+\\.xlsx"', html)
  )[[1]]
  hrefs <- unique(sub('"$', "", sub('^href="', "", hrefs)))

  # The one semantic anchor that actually matters: this must be the
  # MUNICIPAL-level workbook, not the separate national-level one SEEG also
  # publishes on the same page (confirmed live both appear).
  candidates <- hrefs[grepl("municipa[il]s?", hrefs, ignore.case = TRUE)]

  if (length(candidates) == 0) {
    stop(
      "resolve_seeg(): no municipal-level '.xlsx' link found on ",
      "seeg.eco.br/dados/ (looked for any /wp-content/uploads/*.xlsx href ",
      "with 'municipa(l/is)' in its filename). Site layout may have changed."
    )
  }
  if (length(candidates) > 1) {
    stop(
      "resolve_seeg(): found ", length(candidates), " candidate municipal ",
      ".xlsx links on seeg.eco.br/dados/, expected exactly 1 -- ambiguous, ",
      "needs a human to pick the right one: ", paste(candidates, collapse = " | ")
    )
  }

  url <- candidates
  # Version = the last run of digits/dots immediately before ".xlsx",
  # wherever it falls in the filename -- not tied to a specific prefix, so
  # a renamed prefix doesn't also break version extraction.
  version <- regmatches(url, regexpr("[0-9]+(?:\\.[0-9]+)*(?=\\.xlsx$)", url, perl = TRUE))
  if (length(version) == 0 || !nzchar(version)) {
    stop(
      "resolve_seeg(): could not extract a version number from the ",
      "filename of ", url, " -- expected a trailing '<digits>[.digits...]' ",
      "just before '.xlsx'."
    )
  }

  ## -- resolver alert cache: same informational Slack signal PRODES/EPE/IPS -
  ## already have for "a new upload showed up but failed content
  ## verification" -- see actions/scripts/resolver_alert_cache.R's header.
  ## item_key = version, so a NEW SEEG version failing is reported once; the
  ## SAME version failing again on a later run is already in the committed
  ## cache and won't re-alert. Purely additive: the stop() calls below are
  ## unchanged and still fail the run / open an issue every time.
  alert_rec <- alert_recorder("seeg")

  ## -- verify the workbook's actual structure, not just the /dados/ page's --
  ## filename -- see this file's header for why these two small ranged GETs
  ## are enough and never touch the 900MB+ data sheet.
  zip_entries <- tryCatch(
    zip_remote_entries(url, useragent = ua),
    error = function(e) {
      seeg_reason <- paste0("zip content verification failed for ", url, ": ", conditionMessage(e))
      alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
      stop("resolve_seeg(): ", seeg_reason)
    }
  )

  wb_entry <- zip_entries[["xl/workbook.xml"]]
  if (is.null(wb_entry)) {
    seeg_reason <- paste0("no xl/workbook.xml found in ", url, " -- not a valid xlsx, or the internal layout changed")
    alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
    stop("resolve_seeg(): ", seeg_reason)
  }
  wb_xml <- tryCatch(
    rawToChar(zip_remote_extract_entry(url, wb_entry, useragent = ua)),
    error = function(e) {
      seeg_reason <- paste0("could not read xl/workbook.xml: ", conditionMessage(e))
      alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
      stop("resolve_seeg(): ", seeg_reason)
    }
  )
  sheet_names <- regmatches(wb_xml, gregexpr('<sheet name="[^"]*"', wb_xml))[[1]]
  sheet_names <- sub('<sheet name="', "", sub('"$', "", sheet_names))
  if (!"Dados" %in% sheet_names) {
    seeg_reason <- paste0(
      "no sheet named 'Dados' found in ", url, " -- tabs found: ",
      paste(sheet_names, collapse = ", ")
    )
    alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
    stop("resolve_seeg(): ", seeg_reason)
  }

  pivot_entry <- zip_entries[["xl/pivotCache/pivotCacheDefinition1.xml"]]
  if (is.null(pivot_entry)) {
    seeg_reason <- paste0("no xl/pivotCache/pivotCacheDefinition1.xml found in ", url)
    alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
    stop("resolve_seeg(): ", seeg_reason)
  }
  pivot_xml <- tryCatch(
    rawToChar(zip_remote_extract_entry(url, pivot_entry, useragent = ua)),
    error = function(e) {
      seeg_reason <- paste0("could not read xl/pivotCache/pivotCacheDefinition1.xml: ", conditionMessage(e))
      alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
      stop("resolve_seeg(): ", seeg_reason)
    }
  )
  cache_fields <- regmatches(pivot_xml, gregexpr('<cacheField name="[^"]*"', pivot_xml))[[1]]
  cache_fields <- sub('<cacheField name="', "", sub('"$', "", cache_fields))

  missing_fields <- setdiff(SEEG_EXPECTED_CACHE_FIELDS, cache_fields)
  if (length(missing_fields) > 0) {
    seeg_reason <- paste0(
      "the workbook at ", url, " is missing pivot-cache field(s) ",
      paste(missing_fields, collapse = ", "), " that R/seeg.R's rewrite depends on -- ",
      "fields found: ", paste(cache_fields, collapse = ", ")
    )
    alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
    stop("resolve_seeg(): ", seeg_reason)
  }

  # Year-field range: every cache field name that is purely digits is a
  # year column (see the confirmed 1970..2024 field list) -- confirm the
  # range still covers what R/seeg.R's pivot_longer()/across() calls need.
  year_fields <- suppressWarnings(as.integer(cache_fields))
  years_found <- sort(unique(year_fields[!is.na(year_fields)]))
  expected_years <- tryCatch(
    parse_years(dataset_meta("seeg", unique(rows$dataset)[1], "available_time")),
    error = function(e) integer(0)
  )
  missing_years <- setdiff(expected_years, years_found)
  if (length(missing_years) > 0) {
    seeg_reason <- paste0(
      "the workbook at ", url, " is missing year field(s) ",
      paste(missing_years, collapse = ", "), " that the manifest's available_time promises -- ",
      "year range found: ", paste(range(years_found), collapse = "-")
    )
    alert_rec(item_key = version, dataset = "(all)", verdict = "fail", reason = seeg_reason)
    stop("resolve_seeg(): ", seeg_reason)
  }

  alert_rec(item_key = version, dataset = "(all)", verdict = "pass")

  available_time <- paste(range(years_found), collapse = "-")

  tibble::tibble(
    survey = "seeg", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = url, version = version,
    available_time = available_time, docs_url = "https://seeg.eco.br/dados/",
    resolver = "seeg"
  )
}
