# actions/scrapers/resolve_baci.R
#
# BACI (CEPII) is pure URL probing -- no HTML parsed, no API called. The
# host publishes zip files under a predictable "V<YYYYMM>" version stamp
# (https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V<YYYYMM>.zip);
# we generate candidate stamps and HEAD-probe them (a full GET would
# download a multi-gigabyte file -- verified live while writing this: the
# zip is >2GB, HEAD returns instantly, a GET does not).
#
# The whole point of this resolver: it emits `url`, `archive_file`, AND
# `version` TOGETHER, using the same stamp, in the same manifest row --
# structurally eliminating the old bug where the URL's version stamp
# (download.R:820, pre-migration) and the inner-file regex
# (download.R:609, pre-migration) had to be updated in two separate places
# and could silently drift apart.
#
# 2026-09-08 (audit follow-up): two things hardened, both confirmed live
# against the real archive this session.
#
# 1. `archive_file` and `available_time` used to be asserted purely by
#    string construction, never actually observed -- the resolver advanced
#    `url`/`version` on every refresh but NEVER `available_time`, so once
#    CEPII ships a year past 2024 the manifest would stay frozen at
#    "1995-2024" and check_params() would reject a `time_period` the file
#    genuinely contains. Every candidate that HEAD-probes clean now also gets
#    its file listing read via zip_remote_listing.R's zip_remote_entries()
#    (one more small ranged GET of the zip's tail, not a download of the
#    2.4GB archive) to confirm at least one inner file matches the
#    `archive_file` pattern, and `available_time` is derived from the real
#    `BACI_HS92_Y<year>_V<stamp>.csv` filenames found -- VERIFIED LIVE this
#    session: V202601's zip lists Y1995 through Y2024, so available_time
#    comes out "1995-2024", matching what was previously just hand-typed.
#
# 2. `probe_head()` used to treat a transport-level failure (DNS, connection
#    refused, timeout) exactly the same as a clean HTTP 404 -- both just
#    meant "keep trying the next stamp". Measured live this session: a real
#    miss answers in ~1-2s (CEPII returns 404 fast), so the 36-candidate
#    range finishing "nothing found" because the naming scheme genuinely
#    changed costs well under a minute -- the resolver's old worst-case
#    (36 x 20s timeout ~= 12 minutes) only happens if the SERVER itself is
#    unreachable, in which case every one of those 36 candidates burns its
#    full timeout to reach the same "nothing found" conclusion a human could
#    already tell from the 1st failure. probe_head() now distinguishes the
#    two: a real HTTP response (200 or 404, we reached the server) resets
#    the consecutive-transport-failure counter; a transport-level error
#    (the tryCatch around curl_fetch_memory() itself firing) increments it,
#    and the walk aborts early with a distinct "CEPII unreachable" error once
#    that streak hits CONSECUTIVE_TRANSPORT_FAILURE_LIMIT, instead of
#    grinding through the full 36-candidate range one 20s timeout at a time.
#    The per-probe timeout was also dropped from 20s to 8s (still generous
#    headroom over the ~1-2s measured live) so even the worst case shrinks
#    further without touching the success path's cost.
#
# 2026-10-19: this resolver also owns the manifest row baci_dic/product_codes,
# the product-code dictionary R/baci.R's load_baci_dic() joins BACI's product
# codes with. That table is not CEPII's: it is Brazil's COMEX dictionary
# NCM_SH.csv, on the ministry's balanca host. Its row used to point at
# balanca.economia.gov.br with an EMPTY resolver, so nothing watched it, and
# when that host's TLS certificate expired (2026-10-03) load_baci() began to
# stop -- after downloading CEPII's 2.4 GB zip -- with a certificate error
# (issue #353). The same file is served, byte-identical (same MD5), by
# balanca.mdic.gov.br, the host the ministry's open-data page links; that is
# where the row points now. Everything about that host (the 200/206
# text/plain Joomla placeholder a wrong path gets, the incomplete TLS chain,
# the dictionary's columns and Latin-1 encoding) is measured and explained in
# actions/scrapers/resolve_comex.R's header on the update-comex branch; this
# resolver only checks what load_baci_dic() needs of it, once per run:
# one ranged GET that must come back `text/csv` (a status code proves
# nothing there) with the raw columns CO_SH6, NO_SH6_POR, NO_SH6_ING.
#
# A dictionary problem is reported as a PARTIAL failure (see resolve_epe.R /
# build_manifest.R): the HS92 row above still gets merged, because a broken
# dictionary must not freeze CEPII's own version stamp, and the baci_dic row is
# simply left as it was. Known and accepted: until a CI step makes the runner
# trust balanca.mdic.gov.br's missing intermediate CA (added on the
# update-comex branch), this part reports "cannot verify the TLS chain" on an
# ubuntu runner every week. NOTE on duplication: the host below is a second
# copy of resolve_comex.R's COMEX_HOST, and the dictionary check a smaller copy
# of its own -- kept separate on purpose so this fix does not depend on that
# unmerged branch. Once both are on master, fold the baci_dic row into
# resolve_comex.R's dictionary logic (comex_dic/hs_codes is this same file) and
# delete this block.

BACI_DIC_URL <- "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv"
BACI_DIC_DOCS_URL <- "https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta"
# Raw columns R/baci.R's load_baci_dic() selects (before clean_names()).
BACI_DIC_REQUIRED <- c("CO_SH6", "NO_SH6_POR", "NO_SH6_ING")

# Verify the dictionary and return its manifest row, or stop() with the reason.
# `rows` = the manifest rows with survey == "baci_dic" this resolver owns.
resolve_baci_dictionary <- function(rows) {
  # Redirects are not followed: a 3xx would be the host saying the layout moved.
  h <- curl::new_handle(timeout = 30, connecttimeout = 15, followlocation = FALSE)
  curl::handle_setheaders(h, Range = "bytes=0-4095")

  resp <- tryCatch(curl::curl_fetch_memory(BACI_DIC_URL, handle = h), error = function(e) e)
  if (inherits(resp, "error")) {
    msg <- conditionMessage(resp)
    reason <- if (grepl("certificate has expired|CERT_EXPIRED|certificate expired", msg, ignore.case = TRUE)) {
      "the server's TLS certificate has expired"
    } else if (grepl("issuer certificate|UNTRUSTED|unable to verify|incomplete", msg, ignore.case = TRUE)) {
      paste0(
        "this runner cannot verify the server's TLS chain: the server omits its intermediate CA ",
        "('AC SERPRO AR46 OV TLS CA 2025', http://secure.globalsign.com/cacert/acserproar46ovtlsca2025.crt), ",
        "which Windows and browsers fetch on their own but OpenSSL does not -- add it to the runner's ",
        "trust store, never turn verification off"
      )
    } else {
      "no HTTP response (DNS, connection, timeout or TLS failure)"
    }
    stop("could not reach ", BACI_DIC_URL, " -- ", reason, ". curl said: ", msg)
  }

  headers <- tryCatch(curl::parse_headers_list(resp$headers), error = function(e) list())
  ctype <- headers[["content-type"]]
  ctype <- if (is.null(ctype)) "" else ctype
  if (!(resp$status_code %in% c(200L, 206L)) || !grepl("^text/csv", ctype, ignore.case = TRUE)) {
    stop(
      "no real CSV (text/csv) at ", BACI_DIC_URL, " -- HTTP ", resp$status_code, ", content-type '", ctype,
      "' (a missing path on that host answers with a text/plain placeholder, not a 404)."
    )
  }

  # First line of the (partial) body: the quoted, ';'-separated header row.
  body <- iconv(rawToChar(resp$content[resp$content != as.raw(0)]), from = "UTF-8", to = "UTF-8", sub = "?")
  columns <- toupper(trimws(gsub('"', "", strsplit(sub("[\r\n].*$", "", body), ";", fixed = TRUE)[[1]], fixed = TRUE)))
  missing_cols <- setdiff(BACI_DIC_REQUIRED, columns)
  if (length(missing_cols) > 0) {
    stop(
      "the dictionary at ", BACI_DIC_URL, " no longer carries the columns R/baci.R's load_baci_dic() ",
      "selects -- missing ", paste(missing_cols, collapse = ", "), ". Fix R/baci.R before publishing it."
    )
  }

  tibble::tibble(
    survey = rows$survey, dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = BACI_DIC_URL, docs_url = BACI_DIC_DOCS_URL
  )
}

resolve_baci <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_baci() needs the 'curl' package (CI-only; not a package Import).")
  }

  CONSECUTIVE_TRANSPORT_FAILURE_LIMIT <- 5L

  probe_head <- function(url, timeout_s = 8) {
    h <- curl::new_handle(nobody = TRUE, timeout = timeout_s, followlocation = TRUE)
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(resp, "error")) {
      return(list(reached_server = FALSE, ok = FALSE, error = conditionMessage(resp)))
    }
    list(reached_server = TRUE, ok = resp$status_code == 200, status_code = resp$status_code)
  }

  # last 30 months plus 6 months ahead (CEPII sometimes stages a release
  # under next month's stamp before announcing it)
  today <- Sys.Date()
  months_back <- seq(as.Date(paste0(format(today, "%Y-%m"), "-01")), by = "-1 month", length.out = 30)
  months_fwd <- seq(as.Date(paste0(format(today, "%Y-%m"), "-01")), by = "1 month", length.out = 7)[-1]
  stamps <- unique(format(c(months_fwd, months_back), "%Y%m"))
  stamps <- sort(stamps, decreasing = TRUE) # newest first

  url_for <- function(stamp) sprintf("https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V%s.zip", stamp)

  found_stamp <- NULL
  consecutive_transport_failures <- 0L
  for (stamp in stamps) {
    probe <- probe_head(url_for(stamp))
    if (!probe$reached_server) {
      consecutive_transport_failures <- consecutive_transport_failures + 1L
      if (consecutive_transport_failures >= CONSECUTIVE_TRANSPORT_FAILURE_LIMIT) {
        stop(
          "resolve_baci(): ", consecutive_transport_failures, " consecutive transport-level ",
          "failures probing cepii.fr (most recent: ", probe$error, ") -- this looks like ",
          "CEPII is unreachable, not a naming-scheme change. Aborted early instead of ",
          "burning through the full candidate range. Try again later; if this persists, ",
          "check https://www.cepii.fr manually."
        )
      }
      next
    }
    consecutive_transport_failures <- 0L
    if (probe$ok) {
      found_stamp <- stamp
      break
    }
  }

  if (is.null(found_stamp)) {
    stop("resolve_baci(): no valid BACI_HS92_V<YYYYMM>.zip found in the probed range -- CEPII's URL scheme may have changed.")
  }

  url <- url_for(found_stamp)

  ## -- verify the zip's actual contents, not just the HEAD probe ------------
  # zip_remote_listing.R is sourced by build_manifest.R alongside every other
  # actions/scripts/*.R helper.

  zip_entries <- tryCatch(
    zip_remote_entries(url),
    error = function(e) stop("resolve_baci(): zip content verification failed for ", url, ": ", conditionMessage(e))
  )

  archive_file <- paste0("*$year$_V", found_stamp, ".csv")
  archive_pattern <- sprintf("^BACI_HS92_Y[0-9]{4}_V%s\\.csv$", found_stamp)
  data_files <- grep(archive_pattern, names(zip_entries), value = TRUE)
  if (length(data_files) == 0) {
    stop(
      "resolve_baci(): ", url, " HEAD-probed clean, but its file listing contains no ",
      "file matching '", archive_pattern, "' -- archive_file's assumed naming pattern ",
      "no longer matches what's actually inside the zip."
    )
  }

  years <- as.integer(sub(".*_Y([0-9]{4})_V.*", "\\1", data_files))
  available_time <- paste(min(years), max(years), sep = "-")

  result <- tibble::tibble(
    survey = "baci", dataset = "HS92",
    geo_level = NA_character_, year = NA_character_,
    url = url,
    archive_file = archive_file,
    version = found_stamp,
    available_time = available_time
  )

  ## -- the product-code dictionary row (see this file's 2026-10-19 note) ----
  # Only if the manifest hands us that row. A failure here is partial: HS92
  # above is still returned, the baci_dic row is left untouched, and the reason
  # reaches the report/issue via the `partial_failures` attribute.
  dic_rows <- if (is.null(rows)) rows else rows[rows$survey == "baci_dic", ]
  if (!is.null(dic_rows) && nrow(dic_rows) > 0) {
    dic <- tryCatch(resolve_baci_dictionary(dic_rows), error = function(e) e)
    if (inherits(dic, "error")) {
      attr(result, "partial_failures") <- paste0("baci_dic/product_codes: ", conditionMessage(dic))
    } else {
      # bind_rows() NA-pads each side's missing columns; NA means "leave the
      # cell alone" downstream, so HS92's docs_url and the dictionary's
      # archive_file/version/available_time stay as they are.
      result <- dplyr::bind_rows(result, dic)
    }
  }

  result
}
