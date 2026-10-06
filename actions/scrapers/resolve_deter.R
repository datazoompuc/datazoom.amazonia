# actions/scrapers/resolve_deter.R
#
# DETER is served through the same TerraBrasilis download API that
# resolve_prodes.R already uses (`GET .../business/api/v1/download/all`,
# the JSON endpoint TerraBrasilis' own /downloads/ page calls client-side).
# Each entry is `{name, link, category, enabled}`; DETER's own entries are
# static per-biome download endpoints (no per-release date stamp embedded
# in the link, unlike PRODES) -- e.g. deter_amz's link never changes, it's
# always "/file-delivery/download/deter-amz/shape".
#
# VERIFIED LIVE (2026-09-28): the manifest's committed deter_cerrado URL
# (".../download/deter-cerrado/shape", hardcoded since 2021-06-19, carried
# unchanged through the manifest migration) no longer appears ANYWHERE in
# this API's response -- not even as a disabled entry. The live Cerrado
# entry is now at the slug "deter-cerrado-nb" instead. This resolver exists
# specifically so a future rename like this gets caught automatically
# instead of silently going stale for years again.
#
# Also discovered live on the same date: the API lists two DETER products
# datazoom.amazonia never supported -- "deter-pantanal" (Pantanal biome)
# and "deter-nf" (Amazonia, non-forest areas). Manifest rows for both
# (deter_pantanal, deter_non_forest) were added by hand in commit 34bd4b5,
# with resolver left blank per the onboarding playbook (SKILL.md sec. 10)
# -- this resolver is what fills that column in once it runs clean.
#
# Each dataset is identified by a fixed substring expected inside its
# entry's `link` field -- NOT by `category`/`name`, which come in both
# Portuguese and English duplicate entries for the same link (confirmed
# live: Cerrado appears twice, "Cerrado Biome - DETER (Notices)" and
# "Bioma Cerrado - DETER (Avisos)", identical link). Matching on the link
# itself sidesteps the language duplication entirely.
#
# UNLIKE resolve_prodes.R, this resolver does NOT attempt to verify the
# downloaded archive's internal contents (zip_remote_entries() etc.) --
# DETER's `archive_file` (the shapefile name inside the delivered package)
# cannot be derived from the API response, and verifying it would require
# actually downloading the (potentially large) shapefile package, which
# this resolver does not do. archive_file is therefore never touched here
# -- it is simply absent from the returned tibble, which per the manifest
# schema means "leave the existing value alone" (same convention
# resolve_prodes.R already uses for the columns it doesn't set). For
# deter_pantanal and deter_non_forest, archive_file is still blank in the
# manifest as of this writing; filling it in requires downloading the real
# package by hand once and reading off the shapefile's name -- see
# R/deter.R and the onboarding playbook, step 5.
#
# available_time is for the same reason left untouched -- DETER's download
# links carry no date/version stamp to read a coverage window off of,
# unlike PRODES's year+stamp filenames. If TerraBrasilis ever exposes a
# per-product "last updated" field in this API or another one, this
# resolver should start reading available_time from it instead of leaving
# it as a manually-maintained value.

resolve_deter <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_deter(): no manifest rows tagged resolver == 'deter' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_deter() needs the 'jsonlite' and 'curl' packages (CI-only; not package Imports).")
  }

  api_url <- "https://terrabrasilis.dpi.inpe.br/business/api/v1/download/all"
  resp <- tryCatch(
    curl::curl_fetch_memory(api_url, handle = curl::new_handle(timeout = 30)),
    error = function(e) stop("resolve_deter(): request failed for ", api_url, ": ", conditionMessage(e))
  )
  if (resp$status_code != 200) {
    stop("resolve_deter(): TerraBrasilis download API returned HTTP ", resp$status_code)
  }

  entries <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  links <- vapply(entries, function(e) if (is.null(e$link)) NA_character_ else e$link, character(1))
  enabled <- vapply(entries, function(e) isTRUE(e$enabled), logical(1))

  # dataset -> fixed substring expected in `link`. Update this table (not
  # the matching logic below) if TerraBrasilis renames a slug again -- that
  # is precisely the failure mode this resolver is meant to surface loudly
  # via stop(), not silently keep serving a dead URL for years like before.
  slug_by_dataset <- c(
    deter_amz         = "/file-delivery/download/deter-amz/shape",
    deter_cerrado     = "/file-delivery/download/deter-cerrado-nb/shape",
    deter_pantanal    = "/file-delivery/download/deter-pantanal/shape",
    deter_non_forest  = "/file-delivery/download/deter-nf/shape"
  )

  unknown <- setdiff(rows$dataset, names(slug_by_dataset))
  if (length(unknown) > 0) {
    stop(
      "resolve_deter(): manifest has dataset(s) this resolver doesn't know ",
      "how to resolve: ", paste(unknown, collapse = ", "),
      " -- add them to slug_by_dataset above (and confirm the expected ",
      "link substring live against the API first)."
    )
  }

  resolved_url <- vapply(rows$dataset, function(ds) {
    slug <- slug_by_dataset[[ds]]
    hit <- which(enabled & grepl(slug, links, fixed = TRUE))
    if (length(hit) == 0) {
      stop(
        "resolve_deter(): no enabled entry with link containing '", slug,
        "' found in the TerraBrasilis download API response for dataset '",
        ds, "' -- the slug may have changed again. Check the API response ",
        "by hand before updating slug_by_dataset."
      )
    }
    # Cerrado (and possibly others) list the same link twice, once per
    # language -- that's fine, any match gives the identical link.
    paste0("https://terrabrasilis.dpi.inpe.br", links[[hit[1]]])
  }, character(1), USE.NAMES = FALSE)

  tibble::tibble(
    survey = "deter", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = resolved_url, resolver = "deter"
  )
}
