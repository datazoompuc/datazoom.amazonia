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

  tibble::tibble(
    survey = "baci", dataset = "HS92",
    geo_level = NA_character_, year = NA_character_,
    url = url,
    archive_file = archive_file,
    version = found_stamp,
    available_time = available_time
  )
}
