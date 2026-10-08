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

# 2026-10-08 (hardening, requested by Antonio): the primary slug match above
# is exactly what let deter_cerrado's 2021 hardcoded URL go stale silently
# for years -- it's precise when the slug is still what we last saw, but by
# itself it can only ever stop() loudly on a NEW rename, not recover from
# one. Added a keyword fallback (see keyword_by_dataset + the resolution
# loop below): when the known slug isn't found, look for a single
# unambiguous DETER-namespaced entry whose human-readable `name` field
# contains the dataset's biome keyword, and trust it if (and only if)
# exactly one distinct link matches. This turns a future rename into a
# normal manifest PR for a human to review (same as any other resolver
# success) instead of just another silent-forever failure -- it still
# never guesses when the result would be ambiguous.
#
# KEYWORDS VERIFIED LIVE (2026-10-08, all 231 entries of the API response; the
# 8 entries whose link contains "/deter" are exactly the 4 products x 2
# languages, all enabled). Real `name` text (PT / EN; the PT originals carry
# accents -- Amazonia, areas, Nao -- dropped here only to keep this file ASCII):
#   deter_amz         "Avisos no Bioma Amazonia" / "Amazon Biome notices"
#   deter_cerrado     "Deter Cerrado" / "Cerrado Deter"
#   deter_pantanal    "Avisos DETER no Pantanal" / "Pantanal Notices of vegetal suppression"
#   deter_non_forest  "Avisos em areas de Nao Floresta" / "Amazon Biome notices for non-forest areas"
# Every keyword in keyword_by_dataset matches its own entries (the accented
# "Nao Floresta" included, which the regex handles). BUT "amaz" alone is NOT
# unique: the English non-forest entry is called "Amazon Biome notices for
# non-forest areas", so it also matches deter_amz's keyword and drags the
# deter-nf link into deter_amz's candidate set. Left as is, a renamed
# deter_amz slug would fail the fallback (2 candidates) -- and, worse, if the
# Amazon entries were ever REMOVED instead of renamed, deter-nf would be the
# only "amaz" candidate left and be accepted as deter_amz's URL, silently
# giving two datasets the same download (reproduced with the real data).
# Hence two rules in the fallback below: (1) a link that ANOTHER dataset's
# primary slug still matches is that dataset's, never a candidate here;
# (2) two datasets may never resolve to the same URL. The category field is
# deliberately not used: it is "... - DETER (Avisos)" for every product.

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
  names_field <- vapply(entries, function(e) if (is.null(e$name)) NA_character_ else e$name, character(1))

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

  # dataset -> keyword expected (case-insensitive) in a /deter-namespaced
  # entry's `name` field, used only as the fallback below when the slug
  # above isn't found. All four verified live against the real `name` text
  # on 2026-10-08 -- see the header note (and why "amaz" alone isn't unique).
  keyword_by_dataset <- c(
    deter_amz         = "amaz",
    deter_cerrado     = "cerrado",
    deter_pantanal    = "pantanal",
    deter_non_forest  = "n[a\u00e3]o.?florest|non-?forest"
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

  # Collected (via <<- from inside the closure below, same pattern already
  # used elsewhere in this codebase for resolver-internal bookkeeping -- see
  # the Slack-notification section of the shared SKILL.md) whenever the
  # keyword fallback actually fires. Purely informational: attached to the
  # returned tibble as a "fallback_notes" attribute so build_manifest.R can
  # surface it in the PR body for a human reviewer -- unlike
  # "partial_failures" (a sibling attribute some other resolvers use), this
  # never counts as a failure and never affects the exit code, because
  # nothing actually failed: the fallback succeeding IS the resolver working
  # as designed. Added 2026-10-08 per Antonio's request: without this, a
  # fallback-sourced URL change looks identical to any other URL change in
  # the PR diff -- the message() below only ever reached the CI log, never
  # the PR a human actually reviews.
  fallback_notes <- character(0)

  resolved_url <- vapply(rows$dataset, function(ds) {
    slug <- slug_by_dataset[[ds]]
    hit <- which(enabled & grepl(slug, links, fixed = TRUE))
    if (length(hit) > 0) {
      # Cerrado (and possibly others) list the same link twice, once per
      # language -- that's fine, any match gives the identical link.
      return(paste0("https://terrabrasilis.dpi.inpe.br", links[[hit[1]]]))
    }

    # Primary slug match failed -- the slug may have been renamed again
    # (exactly what happened to deter_cerrado in the past). Try the keyword
    # fallback before giving up -- see the 2026-10-08 header note above.
    keyword <- keyword_by_dataset[[ds]]
    in_deter_namespace <- enabled & grepl("/deter", links, fixed = TRUE)

    # Links that ANOTHER dataset's known slug still matches belong to that
    # dataset -- never candidates for this one (see the header note: the
    # non-forest entry's English name contains "Amazon").
    claimed_links <- unique(unlist(lapply(
      setdiff(names(slug_by_dataset), ds),
      function(other) links[enabled & grepl(slug_by_dataset[[other]], links, fixed = TRUE)]
    )))
    keyword_hit <- which(
      in_deter_namespace &
        grepl(keyword, names_field, ignore.case = TRUE) &
        !(links %in% claimed_links)
    )
    distinct_links <- unique(links[keyword_hit])

    if (length(distinct_links) == 1) {
      note <- sprintf(
        paste(
          "%s: primary slug '%s' not found -- matched instead via the",
          "keyword fallback ('%s' in the entry name) to a single",
          "unambiguous candidate: %s. This likely means TerraBrasilis",
          "renamed the slug again -- once confirmed, update slug_by_dataset",
          "in actions/scrapers/resolve_deter.R."
        ),
        ds, slug, keyword, distinct_links
      )
      message("resolve_deter(): ", note)
      fallback_notes <<- c(fallback_notes, note)
      return(paste0("https://terrabrasilis.dpi.inpe.br", distinct_links))
    }

    stop(
      "resolve_deter(): no enabled entry with link containing '", slug,
      "' found for dataset '", ds, "', and the keyword fallback ('", keyword,
      "' in the entry name, scoped to /deter links, ignoring links another ",
      "dataset's known slug still matches) found ",
      length(distinct_links), " distinct candidate(s) instead of exactly 1 -- ",
      if (length(distinct_links) == 0) "nothing matches" else paste(distinct_links, collapse = " | "),
      ". The slug may have changed to something this resolver can't guess ",
      "safely. Check the API response by hand before updating ",
      "slug_by_dataset or keyword_by_dataset above."
    )
  }, character(1), USE.NAMES = FALSE)

  # Last line of defence, whichever path produced the URLs: each DETER dataset
  # is a different download, so two of them sharing one URL means something
  # above resolved wrongly (rows of the same dataset legitimately share it).
  per_dataset <- unique(data.frame(dataset = rows$dataset, url = resolved_url, stringsAsFactors = FALSE))
  dup_urls <- unique(per_dataset$url[duplicated(per_dataset$url)])
  if (length(dup_urls) > 0) {
    stop(
      "resolve_deter(): more than one dataset resolved to the same URL (",
      paste(dup_urls, collapse = ", "), ": ",
      paste(per_dataset$dataset[per_dataset$url %in% dup_urls], collapse = ", "),
      ") -- refusing to write that. Check the API response by hand."
    )
  }

  out <- tibble::tibble(
    survey = "deter", dataset = rows$dataset,
    geo_level = rows$geo_level, year = rows$year,
    url = resolved_url, resolver = "deter"
  )
  if (length(fallback_notes) > 0) {
    attr(out, "fallback_notes") <- fallback_notes
  }
  out
}
