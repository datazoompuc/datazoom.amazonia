# actions/scrapers/resolve_prodes.R
#
# PRODES is the hardest of the five sources, and is documented here as a
# KNOWN-INCOMPLETE resolver rather than a working one -- verified live
# while writing this (2026-08-03):
#
#   - The raster directory does not support autoindex listing:
#     https://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/raster/
#     returns 403, exactly like it did before this branch existed.
#
#   - The filename pattern hardcoded in the manifest today
#     ("prodes_amazonia_legal_2023.zip") ALSO now returns 403 live. The
#     real, currently-working URL (confirmed live by hand) is:
#       prodes_amazonia_legal_2025_v20260408.zip
#     i.e. the naming scheme gained a SECOND version component -- a
#     publish-date stamp ("_v20260408", presumably an 8-digit YYYYMMDD) on
#     top of the data year ("2025") -- that did not exist when the
#     hardcoded URL was written. This means the manifest's PRODES row is
#     ALREADY stale as of this exercise: a real, live-discovered example of
#     exactly the problem this whole migration exists to solve.
#
#   - https://terrabrasilis.dpi.inpe.br/downloads/ (suggested as a possible
#     scrape target) is a general news/marketing slider page, not a file
#     listing -- it links to blog posts about PRODES updates, not to the
#     zip file itself, so it cannot be regex-matched into a working URL
#     without an extra hop into whichever blog post is current (and blog
#     post URLs are themselves not predictable).
#
# Given the date-stamp component is NOT derivable from the year alone (it
# is set whenever INPE republishes a file, not on a fixed schedule), URL
# probing (works for BACI, which only varies by year+month) and directory
# autoindex (works for nothing here, blocked) both fail. A working resolver
# would need to follow the data.inpe.br/biomasbr news feed (or a similar
# announcement channel) to find whichever post announces the latest
# release and extract the link/date from there -- deliberately deferred:
# this needs its own investigation, not a guess baked into CI.
#
# Until that follow-up lands, this resolver documents the finding and
# fails cleanly every time it runs: build_manifest.R records it as a
# failed resolver, the PRODES rows in the manifest stay exactly as they
# are (hand-maintained), and the validation gate's plain HTTP check (which
# runs independently of any resolver) is what will actually notice the
# 403 on every scheduled run until a human fixes the row by hand or a
# future resolver ships. PRODES is "always Tier B" in this design
# regardless (a year/collection change here alters the raster legend
# codes in R/prodes.R:110-115, which no automated check can validate), so
# nothing here would have been auto-committed even if it worked.

resolve_prodes <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_prodes() needs the 'curl' package (CI-only; not a package Import).")
  }

  probe <- function(url, timeout_s = 20) {
    h <- curl::new_handle(nobody = TRUE, timeout = timeout_s, followlocation = TRUE)
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (is.null(resp)) NA_integer_ else resp$status_code
  }

  dir_status <- probe("https://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/raster/")

  # NOTE: `rows` is the set of dataset rows tagged resolver == "prodes". All
  # six of them share the same url (and layer_name), so after the schema
  # normalization that value lives on the "prodes" survey-default row
  # instead of being repeated six times here -- rows$url[1] will typically
  # be NA. current_status stays NA in that case, which is fine: it's a
  # diagnostic aside in the stop() message below, not load-bearing.
  current_link <- if (nrow(rows) > 0) rows$url[1] else NA_character_
  current_status <- if (!is.na(current_link)) probe(current_link) else NA_integer_

  stop(
    "resolve_prodes() is a documented no-op (see file header): PRODES' real ",
    "download URL now includes an unpredictable publish-date stamp ",
    "(e.g. 'prodes_amazonia_legal_2025_v20260408.zip') that cannot be ",
    "derived from the year alone, directory autoindex is blocked (HTTP ",
    dir_status, " on the raster/ directory), and the currently manifested ",
    "URL itself returns HTTP ", current_status, " live. A working resolver ",
    "needs to follow INPE's announcement channel (data.inpe.br/biomasbr) ",
    "instead of guessing a filename -- deferred pending that investigation. ",
    "PRODES rows are left untouched (hand-maintained); this source is ",
    "'always Tier B' by design even when a resolver exists, since a ",
    "year/collection change here also affects the raster legend codes in ",
    "R/prodes.R, which no automated check can validate."
  )
}
