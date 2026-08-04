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

resolve_baci <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_baci() needs the 'curl' package (CI-only; not a package Import).")
  }

  probe_head <- function(url, timeout_s = 20) {
    h <- curl::new_handle(nobody = TRUE, timeout = timeout_s, followlocation = TRUE)
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
    !is.null(resp) && resp$status_code == 200
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
  for (stamp in stamps) {
    if (probe_head(url_for(stamp))) {
      found_stamp <- stamp
      break
    }
  }

  if (is.null(found_stamp)) {
    stop("resolve_baci(): no valid BACI_HS92_V<YYYYMM>.zip found in the probed range -- CEPII's URL scheme may have changed.")
  }

  tibble::tibble(
    survey = "baci", dataset = "HS92",
    geo_level = NA_character_, year = NA_character_,
    url = url_for(found_stamp),
    archive_file = paste0("*$year$_V", found_stamp, ".csv"),
    version = found_stamp
  )
}
