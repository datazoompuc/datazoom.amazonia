# actions/scripts/mapbiomas_resolver_cache.R
#
# Pure helpers, no top-level execution -- split out the same way
# mapbiomas_fragility.R is, so this can be sourced on its own by
# resolve_mapbiomas.R AND by tests, without pulling in build_manifest.R's
# full pipeline (which is NOT safe to source() directly -- it runs the
# whole resolver pipeline over the network at the top level).
#
# ---- Why this exists -------------------------------------------------------
#
# resolve_via_dataverse() (resolve_mapbiomas.R) walks Dataverse candidates
# newest-collection-first and downloads the FULL FILE for each one just to
# check its sheet name and required column -- there is no way to learn
# either from Dataverse's metadata APIs, only by opening the actual xlsx
# (see dv_file_layout()'s header). That download is unavoidable the FIRST
# time a given file is checked. What's wasteful is that every later
# scheduled run repeats it for files whose verdict we already know, because
# the resolver has no memory between runs.
#
# This is that memory: a committed CSV (actions/cache/
# mapbiomas_resolver_cache.csv) recording, per Dataverse file actually
# inspected, whether it passed and (if so) which sheet matched. A verdict is
# only ever trusted when BOTH of these are unchanged since it was recorded:
#
#   - the file's own MD5 checksum (Dataverse exposes this inline on the SAME
#     dv_dataset_files() JSON call the walk already makes -- verified live
#     2026-08-24, zero extra network cost). If Dataverse ever replaces a
#     file's content under the same id, the checksum changes and the cached
#     verdict is no longer trusted.
#   - a fingerprint of the CODE's own matching rules (sheet_pattern +
#     required_col_pattern) for that config. If a future maintainer tightens
#     either pattern, every verdict computed under the old rule is
#     invalidated automatically -- nobody has to remember to clear a cache
#     by hand.
#
# Anything short of checking BOTH would risk silently trusting a verdict
# that's no longer true -- see this file's test suite
# (test-mapbiomas-resolver-cache.R) for the direct proof of that property.

# Defined locally (redundant with build_manifest.R's own copy, harmless to
# redefine) so this file is standalone-sourceable by tests without also
# needing build_manifest.R -- same convention resolve_mapbiomas.R follows.
`%||%` <- function(x, y) if (is.null(x)) y else x

## ============================================================ ##
## Schema                                                       ##
## ============================================================ ##

RESOLVER_CACHE_COLS <- c(
  "dataset", "geo_level", "file_id", "checksum",
  "rules_fingerprint", "verdict", "sheet", "checked_at"
)

read_resolver_cache <- function(path) {
  if (!file.exists(path)) {
    empty <- setNames(
      lapply(RESOLVER_CACHE_COLS, function(x) character(0)),
      RESOLVER_CACHE_COLS
    )
    return(tibble::as_tibble(empty))
  }
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

write_resolver_cache <- function(df, path) {
  df <- df[, RESOLVER_CACHE_COLS]
  df <- dplyr::arrange(df, dataset, dplyr::coalesce(geo_level, ""), file_id)
  readr::write_csv(df, path, na = "")
}

## ============================================================ ##
## Rules fingerprint                                            ##
## ============================================================ ##

# A verdict is only meaningful under the exact matching rules that produced
# it. Hashing both pattern strings means a rule change invalidates every
# verdict computed under the old rule automatically, without anyone having
# to remember to clear the cache -- a stale-but-uninvalidated verdict is
# exactly the failure mode this whole mechanism exists to avoid.
rules_fingerprint <- function(sheet_pattern, required_col_pattern = NULL) {
  key <- paste(
    sheet_pattern %||% "",
    required_col_pattern %||% "",
    sep = "\r"
  )
  # Base-R only (avoids adding a CI-only 'digest' dependency for one short
  # hash): sum of character codes is not cryptographically meaningful, but
  # this only needs to detect "did either pattern string change", not resist
  # adversarial collision -- both patterns are short, hand-written regexes
  # living in this same repo, not untrusted input.
  sprintf("%x", sum(utf8ToInt(key)) * 31L + nchar(key))
}

## ============================================================ ##
## Lookup / upsert                                              ##
## ============================================================ ##

# Requires ALL FIVE fields to match -- dataset, geo_level, file_id (the
# candidate actually being considered), checksum (has the file's content
# changed?), and fingerprint (have the rules that judged it changed?). A
# mismatch on ANY of these is a cache MISS, not a stale hit -- see this
# file's header for why that's the safety property that matters here.
cache_lookup <- function(cache, dataset, geo_level, file_id, checksum, fingerprint) {
  if (nrow(cache) == 0) return(NULL)
  g <- geo_level %||% NA_character_
  hit <- cache[
    cache$dataset == dataset &
      (is.na(cache$geo_level) & is.na(g) | !is.na(cache$geo_level) & !is.na(g) & cache$geo_level == g) &
      cache$file_id == as.character(file_id) &
      cache$checksum == checksum &
      cache$rules_fingerprint == fingerprint,
  ]
  if (nrow(hit) == 0) return(NULL)
  as.list(hit[1, ])
}

# Replaces the existing row for this (dataset, geo_level, file_id) key if
# one exists (a file's checksum or the rules can change without its id
# changing -- this keeps exactly one row per file_id, not an ever-growing
# history), or appends a new one.
cache_upsert <- function(cache, dataset, geo_level, file_id, checksum,
                          fingerprint, verdict, sheet,
                          checked_at = format(Sys.Date())) {
  g <- geo_level %||% NA_character_
  key_match <- cache$dataset == dataset &
    (is.na(cache$geo_level) & is.na(g) | !is.na(cache$geo_level) & !is.na(g) & cache$geo_level == g) &
    cache$file_id == as.character(file_id)

  new_row <- tibble::tibble(
    dataset = dataset, geo_level = g, file_id = as.character(file_id),
    checksum = checksum, rules_fingerprint = fingerprint,
    verdict = verdict, sheet = sheet %||% NA_character_, checked_at = checked_at
  )

  if (any(key_match)) {
    cache[key_match, ] <- new_row[rep(1, sum(key_match)), ]
    cache
  } else {
    dplyr::bind_rows(cache, new_row)
  }
}
