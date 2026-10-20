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
#
# 2026-10-17 (diagnostics for the CI-only 403): this resolver fails with HTTP
# 403 from GitHub Actions runners (issue #344) but succeeds from an ordinary
# connection -- with the very same browser User-Agent already set below, and
# no other header (re-verified from a home connection 2026-10-17). So a
# missing User-Agent or Accept header is not what blocks it. The working
# hypothesis is that SEEG's edge refuses GitHub's datacenter IP ranges, which
# a headless browser could not fix since it would run from the same IPs --
# but that is a hypothesis: no response from the blocked side (CI) has ever
# been captured.
#
# What IS known, from one live request made 2026-10-17 WITHOUT the browser
# User-Agent (the documented bare-UA 403, kept as a real fixture in
# tests/testthat/fixtures/seeg_cloudfront_403_*): seeg.eco.br is served
# through Amazon CloudFront (`server: CloudFront`, `via: ... (CloudFront)`,
# `x-amz-cf-*`), NOT Cloudflare, and the refusal is CloudFront's own
# "ERROR: The request could not be satisfied ... Request blocked." page with
# `x-cache: Error from cloudfront` -- as far as AWS's documentation goes, the
# page CloudFront serves when a rule of an AWS WAF web ACL attached to the
# distribution blocks the request (not confirmed with SEEG). That page does
# not name the rule, so it cannot by itself separate an IP/hosting-provider
# rule from a bot/User-Agent rule; what the CI response can add is whether it
# is the SAME page. A bare "HTTP 403" cannot tell these apart:
#   * the same CloudFront "Request blocked" page, no further headers: WAF
#     block, consistent with an IP/ASN rule (an allowlist request to SEEG or
#     a non-datacenter runner are the options; a headless browser is not);
#   * an `x-amzn-waf-action` header or a challenge/CAPTCHA body: a WAF
#     challenge action -- the one case where a real browser engine might get
#     through;
#   * 429/503 or a retry-after header: rate limiting, not an IP block;
#   * no HTTP response at all (reset, timeout, TLS failure): reported as "no
#     HTTP response", with curl's own error text.
# seeg_failed_fetch_evidence() below puts that evidence into the stop()
# message, so the next failed CI run (the failure comment on the open
# "Manifest refresh" issue, or the `Run build_manifest.R` step log) carries it
# instead of just a status code. When a real CI sample is in hand, record what
# it showed in the shared SKILL.md's "Known limitations".

SEEG_EXPECTED_CACHE_FIELDS <- c(
  "Setor de emissão", "Categoria emissora", "Sub-categoria emissora",
  "Atividade geral", "Recorte", "Estado", "Município", "ID Território"
)

# Raw response bytes -> ONE valid UTF-8 string. Capping at `max_bytes` can
# split a multibyte character and a WAF page is not guaranteed to be UTF-8 at
# all; iconv(sub = "?") turns every invalid byte into "?", so the result is
# always valid. An invalid string would make jsonlite::write_json() fail on
# manifest_report.json -- on exactly the run the evidence is wanted from.
# NUL bytes are dropped because rawToChar() errors on them.
seeg_raw_to_text <- function(x, max_bytes) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  x <- x[x != as.raw(0)]
  x <- x[seq_len(min(length(x), max_bytes))]
  iconv(rawToChar(x), from = "UTF-8", to = "UTF-8", sub = "?")
}

# Plain-text evidence about a failed GET of seeg.eco.br/dados/, appended to
# resolve_seeg()'s stop() message (see this file's header, "diagnostics").
# `resp` is curl::curl_fetch_memory()'s result (or NULL when curl itself
# errored, in which case `fetch_error` carries curl's message). Returns one
# multi-line string. Kept to what separates the candidate causes -- it does
# NOT dump every header (Set-Cookie values are noise) or the whole body.
# build_manifest.R stores the stop() message verbatim in resolvers_failed, so
# it reaches the step log as-is and the GitHub issue through update-manifest
# .yaml's fmtFailures(), which puts everything under the first line in a code
# block precisely so an HTML excerpt is shown, not rendered.
seeg_failed_fetch_evidence <- function(resp, fetch_error = NULL, max_body_chars = 600L) {
  if (is.null(resp)) {
    return(paste0(
      "no HTTP response was received -- curl error: ",
      if (is.null(fetch_error) || !nzchar(fetch_error)) "(none captured)" else fetch_error
    ))
  }

  # curl returns one header block per redirect hop; only the last one is the
  # response that actually refused us.
  header_text <- seeg_raw_to_text(resp$headers, 16384L)
  header_blocks <- strsplit(header_text, "\r?\n\r?\n")[[1]]
  header_blocks <- header_blocks[nzchar(trimws(header_blocks))]
  header_lines <- if (length(header_blocks) > 0) {
    trimws(strsplit(header_blocks[length(header_blocks)], "\r?\n")[[1]])
  } else {
    character(0)
  }
  fields <- header_lines[grepl("^[^:[:space:]]+:", header_lines)]
  field_names <- tolower(sub(":.*$", "", fields))
  field_values <- trimws(sub("^[^:]+:", "", fields))

  keep <- grepl("^(server|cf-|x-|content-type|content-length|retry-after|location|via)", field_names)
  # sprintf (not paste0): with nothing kept, paste0("  ", character(0), ": ")
  # is the one-element "  : ", sprintf() gives character(0) as wanted.
  kept <- sprintf("  %s: %s", field_names[keep], substr(field_values[keep], 1, 200))
  cookie_names <- unique(sub("=.*$", "", field_values[field_names == "set-cookie"]))

  # One line, no markup noise; the first 4096 bytes always include <title>.
  squish <- function(s) trimws(gsub("[[:space:]]+", " ", gsub("[[:cntrl:]]", " ", s)))
  body <- squish(seeg_raw_to_text(resp$content, 4096L))
  title <- regmatches(body, regexpr("<title[^>]*>[^<]*</title>", body, ignore.case = TRUE))
  title <- if (length(title) > 0) squish(gsub("<[^>]*>", "", title)) else ""
  shown <- substr(body, 1, max_body_chars)

  paste(c(
    sprintf(
      "HTTP %s from %s", resp$status_code,
      if (is.null(resp$url)) "(unknown url)" else resp$url
    ),
    "response headers (only server, cf-*, x-*, content-type/length, retry-after, location, via):",
    if (length(kept) > 0) kept else "  (none of those headers present)",
    if (length(cookie_names) > 0) paste0("  set-cookie (names only): ", paste(cookie_names, collapse = ", ")),
    if (nzchar(title)) paste0("html <title>: ", title),
    if (length(resp$content) == 0 || !nzchar(shown)) {
      "body: (empty)"
    } else {
      sprintf(
        "body (%d bytes; first %d chars, whitespace collapsed): %s",
        length(resp$content), nchar(shown), shown
      )
    }
  ), collapse = "\n")
}

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
  fetch_error <- NULL
  resp <- tryCatch(
    curl::curl_fetch_memory("https://seeg.eco.br/dados/", handle = h),
    error = function(e) {
      fetch_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(resp) || resp$status_code != 200) {
    # The first line stays a one-line summary (it becomes the list item in
    # the GitHub issue); everything after the first "\n" is the evidence.
    stop(
      "resolve_seeg(): could not fetch https://seeg.eco.br/dados/ (",
      if (is.null(resp)) "request failed" else paste("HTTP", resp$status_code), "). ",
      "A browser User-Agent is already sent (a missing one WAS the cause of the ",
      "403 first seen here, but not of the CI-only one), so read the response ",
      "evidence below before assuming the site itself changed.\n",
      seeg_failed_fetch_evidence(resp, fetch_error)
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
