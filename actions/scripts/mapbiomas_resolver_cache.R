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
  "rules_fingerprint", "verdict", "sheet", "reason", "checked_at"
)

read_resolver_cache <- function(path) {
  if (!file.exists(path)) {
    empty <- setNames(
      lapply(RESOLVER_CACHE_COLS, function(x) character(0)),
      RESOLVER_CACHE_COLS
    )
    return(tibble::as_tibble(empty))
  }
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
  # Schema tolerance: a cache written under an OLDER column set (e.g. the
  # 183-row file committed before `reason` existed) is still perfectly
  # valid -- the columns that DO exist are the ones lookup trusts. Fill any
  # column THIS version knows about but the file lacks with NA, and drop
  # anything unknown, so every consumer downstream (build_manifest.R's
  # cache diff, cache_upsert()'s positional row replacement) can assume the
  # canonical shape unconditionally. Keep this permanently, not just as a
  # one-time migration shim -- it's cheap defense for the next column too.
  for (col in setdiff(RESOLVER_CACHE_COLS, names(df))) df[[col]] <- NA_character_
  df[, RESOLVER_CACHE_COLS]
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
                          reason = NA_character_,
                          checked_at = format(Sys.Date())) {
  g <- geo_level %||% NA_character_
  key_match <- cache$dataset == dataset &
    (is.na(cache$geo_level) & is.na(g) | !is.na(cache$geo_level) & !is.na(g) & cache$geo_level == g) &
    cache$file_id == as.character(file_id)

  # Column order here MUST match RESOLVER_CACHE_COLS: the replace branch
  # below assigns `cache[key_match, ] <- new_row[...]` POSITIONALLY, not by
  # name. A drifted order would silently write values into the wrong
  # columns with no error -- see test-mapbiomas-resolver-cache.R's
  # old-schema-upsert test for the direct proof this stays safe.
  new_row <- tibble::tibble(
    dataset = dataset, geo_level = g, file_id = as.character(file_id),
    checksum = checksum, rules_fingerprint = fingerprint,
    verdict = verdict, sheet = sheet %||% NA_character_,
    reason = reason %||% NA_character_, checked_at = checked_at
  )

  if (any(key_match)) {
    cache[key_match, ] <- new_row[rep(1, sum(key_match)), ]
    cache
  } else {
    dplyr::bind_rows(cache, new_row)
  }
}

## ============================================================ ##
## New-candidate detection (informational side-channel only)    ##
## ============================================================ ##

# Rows in `new` whose FILE CONTENT the resolver had never inspected before
# (per `old`) and which came back "fail" this run. This exists purely to
# feed an informational Slack ping -- it is read-only with respect to the
# manifest, never feeds cache_diff_n/total_changed, and never influences an
# exit code. See build_manifest.R's call site.
#
# "Never inspected before" is keyed on (file_id, checksum), NOT on the full
# cache key, for three separate reasons:
#   - dedupe: the SAME physical Dataverse file can back up to 5 different
#     (dataset, geo_level) config rows, so a full-key comparison would
#     report one new file up to five times.
#   - content replacement: cache_upsert() is keyed on
#     (dataset, geo_level, file_id), so MapBiomas swapping a file's content
#     under an unchanged id reads as a "changed" row, not an "added" one --
#     but it is genuinely content nobody has ever checked, so it counts.
#   - rules changes: if only rules_fingerprint moved (a maintainer tightened
#     sheet_pattern/required_col_pattern), the FILE is not new -- our rule
#     is. Keying on (file_id, checksum) correctly stays silent there.
#
# Deliberately NOT reported: a re-confirmed known fail (same file_id, same
# checksum, already fail in `old`); and a new file that PASSES (that already
# produces a real manifest change and opens a PR through the normal path --
# a second ping would be redundant). Nothing about a re-confirmed fail
# changed, and a weekly ping about unchanging state is exactly the noise
# this project rejected a `last_seen` inventory column over.
#
# Coverage caveat, worth knowing rather than assuming away: this only sees
# files the walk actually reached this run. resolve_via_dataverse() returns
# on the first PASS, so candidates older than the winning one are never
# inspected/cached in that run -- fine, since those are not "new" anyway.
# And enumeration itself is gated by dv_search() + is_real_stats_hit() + a
# parseable "Collection N" in the title, so a genuinely new Dataverse
# DATASET that misses those filters is invisible to this mechanism too.
new_failed_files <- function(old, new) {
  empty <- new[0, , drop = FALSE]
  if (nrow(new) == 0) return(empty)
  # A cold/empty committed cache is a BOOTSTRAP, not news -- reporting all
  # ~176 historical fails as "new" on a first seeding run would be pure
  # noise. Stay silent; the seed PR itself is the review artifact.
  if (nrow(old) == 0) return(empty)

  content_key <- function(df) {
    paste(df$file_id, dplyr::coalesce(df$checksum, ""), sep = "\r")
  }
  is_fail <- !is.na(new$verdict) & new$verdict == "fail"
  fresh   <- !(content_key(new) %in% content_key(old))
  hits    <- new[is_fail & fresh, , drop = FALSE]
  if (nrow(hits) == 0) return(empty)
  # One line per physical file, not per config row that happened to reach it.
  hits[!duplicated(hits$file_id), , drop = FALSE]
}
