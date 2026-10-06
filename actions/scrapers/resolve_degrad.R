# actions/scrapers/resolve_degrad.R
#
# DEGRAD is a closed, historical-only product -- discontinued by INPE in
# December 2016, replaced by DETER-B (confirmed live, 2026-10-06, on
# http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad: "O projeto
# DEGRAD foi descontinuado em dezembro de 2016"). There are exactly 10
# manifest rows (years 2007-2016) and there will never be an 11th -- unlike
# every other resolver in this codebase, this one has nothing new to
# DISCOVER. Its job is pure VERIFICATION: confirm each of the 10 already-
# committed URLs still serves a zip of the expected size -- i.e. catch INPE
# silently moving or restructuring these files the way deter_cerrado's URL
# went dead, unnoticed, for five years (see resolve_deter.R's header for that
# story).
#
# VERIFIED LIVE (2026-10-06), in this order:
#
#   1. The official access page (.../degrad/acesso-ao-dados-do-degrad) lists
#      10 download links that match the manifest's URL template byte-for-byte
#      (http://www.obt.inpe.br/OBT/.../arquivos/degrad<year>_final_shp.zip).
#
#   2. www.obt.inpe.br DOES NOT SUPPORT RANGE REQUESTS. Every response carries
#      `Accept-Ranges: none`, and a ranged GET (`Range: bytes=0-99`, or the
#      last 8192 bytes) comes back as HTTP 200 with the WHOLE file, not 206.
#      zip_remote_entries() (zip_remote_listing.R), which PRODES/BACI use to
#      read a zip's central directory without downloading it, therefore cannot
#      work here: it fails on its first ranged GET with "expected HTTP 206
#      (Partial Content) ... got 200". This was the one open risk in the first
#      draft of this resolver; it is now a confirmed fact, not a guess.
#
#   3. HEAD IS USELESS FOR SIZE. Every year's HEAD answers 200 with
#      `Content-Length: 0` and `Content-Type: text/plain` (the real values
#      appear only on a GET), so a "HEAD + Content-Length within +/-20%" check
#      -- the obvious fallback -- would reject all 10 years as empty files.
#
#   4. What DOES work, and is what this resolver does: open a plain GET as a
#      curl connection, read only the RESPONSE HEADERS (status, Content-Type,
#      Content-Length), and close the connection without reading the body.
#      Measured live for all 10 years: HTTP 200, `Content-Type:
#      application/zip`, a Content-Length identical to the bytes a full
#      download then returned (table below), in 0.06-0.16 s per year with no
#      body transferred.
#
#   5. A one-time FULL download of all 10 zips (~130MB, 11 s total) confirmed
#      the thing this resolver can no longer check on every run: each
#      manifest `archive_file` is, byte-for-byte (case included), a file
#      inside its year's zip. The shapefile names are NOT uniform across
#      years (Degrad2007_Final_pol.shp ... DEGRAD_2010_UF_pol.shp,
#      DEGRAD_2011_INPE_pol.shp, ..., DEGRAD_2015.shp, DEGRAD_2016_pol.shp),
#      which is exactly why the manifest keeps one archive_file per year.
#      Re-running that check is a manual job (download + unzip -l), see
#      the bottom of this header.
#
# WHAT THIS CHECK IS, AND IS NOT. It catches a URL that is gone (404), moved,
# or replaced by an HTML error/landing page served with HTTP 200 (wrong
# Content-Type), or a file that is empty/truncated/wildly different in size.
# It does NOT confirm that `archive_file` is still inside the zip (impossible
# without Range support or a full download), and it cannot notice a file
# silently replaced by one of similar size. DEGRAD is frozen, so the exact
# bytes observed on 2026-10-06 are recorded below; the +/-20% tolerance is
# deliberately wide (it only has to tell "the file" from "not the file") --
# tightening DEGRAD_SIZE_TOLERANCE is a one-line change if a stricter
# frozen-file check is ever wanted. If a run ever FAILS on size for a year
# whose URL still returns a zip, re-do the full-download check (step 5)
# before touching anything: the likely cause is INPE re-publishing a
# corrected file, which could also rename the shapefile inside it.
#
# Deliberately does NOT try to discover a new year or a new URL -- there is
# none. If INPE ever resurrected this program or published an 11th year,
# that would need a manifest row added by hand first (see .claude/SKILL.md's
# onboarding section), AND an entry in DEGRAD_EXPECTED_BYTES below.
#
# The manifest stores ONE shared url template across all 10 rows, with a
# literal "$year$" placeholder -- R/download.R only substitutes this at
# actual download time (external_download()'s path-resolution step), NOT
# when the manifest CSV is read. rows$url here therefore still contains the
# literal "$year$" string, never a real per-year URL -- this resolver has to
# redo that same substitution itself before it can fetch anything, or every
# request below would 404 against a URL containing a literal dollar sign.
# The rows it returns echo that same template back unchanged, so a clean run
# changes nothing in the manifest (it only claims ownership, resolver =
# "degrad").
#
# To redo the one-time content check by hand (step 5), from the repo root:
#   for y in 2007 ... 2016: curl -O .../arquivos/degrad${y}_final_shp.zip
#   then, in R: utils::unzip(zip, list = TRUE)$Name must contain the row's
#   archive_file exactly.

# Exact Content-Length (bytes) of each year's zip, measured live 2026-10-06
# (GET headers; each value also equals the size of the file a full download
# returned). The product is frozen, so these are not expected to move.
DEGRAD_EXPECTED_BYTES <- c(
  "2007" = 16887044,
  "2008" = 18995689,
  "2009" = 7888629,
  "2010" = 11338838,
  "2011" = 14452171,
  "2012" = 5851407,
  "2013" = 4819285,
  "2014" = 4324408,
  "2015" = 21966056,
  "2016" = 31212755
)
DEGRAD_SIZE_TOLERANCE <- 0.20

# Pure decision function (no network) so the verdict logic can be unit-tested
# -- see tests/testthat/test-degrad-resolver.R. Returns character(0) when the
# response looks like the expected zip, otherwise ONE short reason string.
# (Named degrad_*, not resolve_*, on purpose: build_manifest.R registers every
# top-level `resolve_*` binding as a resolver.)
degrad_header_problem <- function(status, content_type, content_length, expected_bytes,
                                  tolerance = DEGRAD_SIZE_TOLERANCE) {
  if (length(status) != 1 || is.na(status) || status != 200) {
    return(sprintf("HTTP %s instead of 200", if (length(status) == 1) status else "NA"))
  }
  if (length(content_type) != 1 || is.na(content_type) || !grepl("zip", content_type, ignore.case = TRUE)) {
    return(sprintf(
      "Content-Type is '%s', not a zip (an HTML error page served with HTTP 200 looks like this)",
      if (length(content_type) == 1 && !is.na(content_type)) content_type else "missing"
    ))
  }
  if (length(content_length) != 1 || is.na(content_length) || content_length <= 0) {
    return("no usable Content-Length header (missing, or 0)")
  }
  ratio <- content_length / expected_bytes
  if (abs(ratio - 1) > tolerance) {
    return(sprintf(
      "Content-Length %.0f bytes is %.0f%% of the expected %.0f bytes (tolerance +/-%.0f%%)",
      content_length, 100 * ratio, expected_bytes, 100 * tolerance
    ))
  }
  character(0)
}

# Opens a GET as a curl connection and reads ONLY the response headers, then
# closes it without consuming the body (see header, step 4). `timeout` is a
# whole-transfer ceiling; headers arrive in well under a second, so 30s is
# generous. Throws on any transport failure -- "couldn't check" must never be
# conflated with "checked and fine".
degrad_peek_headers <- function(url, timeout_s = 30) {
  h <- curl::new_handle(timeout = timeout_s, followlocation = TRUE)
  con <- curl::curl(url, handle = h)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  open(con, "rb") # performs the request up to the response headers
  d <- curl::handle_data(h)
  hdrs <- curl::parse_headers_list(d$headers)
  list(
    status = d$status_code,
    content_type = d$type,
    content_length = suppressWarnings(as.numeric(hdrs[["content-length"]]))
  )
}

resolve_degrad <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_degrad(): no manifest rows tagged resolver == 'degrad' -- ",
      "cannot tell which years to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_degrad() needs the 'curl' package (CI-only; not a package Import).")
  }
  if (any(is.na(rows$year))) {
    stop(
      "resolve_degrad(): every DEGRAD row is keyed by year -- found a row with no year. ",
      "This resolver doesn't know how to handle an unkeyed degrad row."
    )
  }
  if (any(is.na(rows$url) | rows$url == "")) {
    stop("resolve_degrad(): every row needs an existing url to verify -- this resolver doesn't invent one.")
  }
  if (any(is.na(rows$archive_file) | rows$archive_file == "")) {
    stop(
      "resolve_degrad(): every row needs an archive_file (load_degrad() reads that shapefile out of the zip). ",
      "This resolver cannot verify it is inside the zip (see its header), only that it is non-empty."
    )
  }
  unknown_years <- setdiff(as.character(rows$year), names(DEGRAD_EXPECTED_BYTES))
  if (length(unknown_years) > 0) {
    stop(
      "resolve_degrad(): no expected file size on record for year(s) ",
      paste(unknown_years, collapse = ", "),
      " -- DEGRAD is discontinued, so this means a manifest row was added by hand. ",
      "Add its Content-Length to DEGRAD_EXPECTED_BYTES (measure it live first)."
    )
  }

  problems <- character(0)

  for (i in seq_len(nrow(rows))) {
    year <- as.character(rows$year[i])
    # rows$url is the raw manifest template, literal "$year$" and all -- see
    # this file's header. Same substitution R/download.R does at download time.
    url <- sub("$year$", year, rows$url[i], fixed = TRUE)

    peek <- tryCatch(degrad_peek_headers(url), error = function(e) e)
    if (inherits(peek, "error")) {
      problems <- c(problems, sprintf("year %s: could not read %s (%s)", year, url, conditionMessage(peek)))
      next
    }

    reason <- degrad_header_problem(
      peek$status, peek$content_type, peek$content_length,
      expected_bytes = DEGRAD_EXPECTED_BYTES[[year]]
    )
    if (length(reason) > 0) {
      problems <- c(problems, sprintf("year %s: %s -- %s", year, reason, url))
    }
  }

  if (length(problems) > 0) {
    stop(
      "resolve_degrad(): ", length(problems), " of ", nrow(rows), " year(s) failed verification:\n- ",
      paste(problems, collapse = "\n- ")
    )
  }

  # Nothing to change -- every row verified clean against what's already
  # committed. Still return every row (echoing its own existing url) rather
  # than an empty tibble, so build_manifest.R records this as a successful
  # run (owning these rows) rather than a failure.
  tibble::tibble(
    survey = "degrad", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = rows$url, resolver = "degrad"
  )
}
