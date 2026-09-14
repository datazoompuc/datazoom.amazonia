# actions/scripts/resolver_alert_cache.R
#
# Pure helpers, no top-level execution -- split out the same way
# mapbiomas_resolver_cache.R and fragility_notes.R are, so this can be
# sourced on its own by resolve_prodes.R/resolve_epe.R AND by tests, without
# pulling in build_manifest.R's full pipeline.
#
# ---- Why this exists -------------------------------------------------------
#
# mapbiomas_resolver_cache.R's new_failed_files() gives resolve_mapbiomas.R's
# newest-first Dataverse walk a Slack ping for "a candidate file I've never
# checked before turned out not to qualify" -- informational, since the
# manifest itself never changes on a rejected candidate (an older, still-good
# one may still be in use), so nothing else in the pipeline would otherwise
# surface it. resolve_prodes.R's .qml legend cross-check and resolve_epe.R's
# per-block verify_table_columns() checks (both added earlier this session)
# are the same kind of moment for those two sources -- this file generalizes
# the mechanism instead of copying MapBiomas's file-checksum-specific cache a
# second and third time. A `source` column is the only real difference: one
# committed cache (actions/cache/resolver_alerts.csv) serves every source
# that plugs in, keyed on (source, item_key) so no two sources' rows can
# collide.
#
# `item_key` is "the identity of the thing being checked", matching
# mapbiomas_resolver_cache.R's (file_id, checksum) role:
#   - PRODES: the release stamp (e.g. "20260910") -- a new INPE release is
#     inherently a new key; the SAME stamp failing again on a later run is
#     already in the committed cache and won't re-alert.
#   - EPE: block label + reason text -- a DIFFERENT new failure mode on the
#     same block still alerts, the same recurring reason doesn't.
#
# Every verdict actually checked (pass AND fail) gets a row, for audit parity
# with the MapBiomas cache, but new_resolver_alerts() only ever surfaces
# verdict == "fail" rows that are genuinely new -- exactly mirroring
# new_failed_files()'s own is_fail/fresh logic.

`%||%` <- function(x, y) if (is.null(x)) y else x

## ============================================================ ##
## Which sources plug into the shared cache -- table-driven the  ##
## same way FRAGILITY_SOURCES is (fragility_notes.R): a new      ##
## source is one line here, not a new copy of build_manifest.R's ##
## merge loop.                                                   ##
## ============================================================ ##

RESOLVER_ALERT_SOURCES <- c("prodes", "epe", "ips")

## ============================================================ ##
## Schema                                                       ##
## ============================================================ ##

RESOLVER_ALERT_COLS <- c("source", "item_key", "dataset", "geo_level", "verdict", "reason", "checked_at")

read_resolver_alert_cache <- function(path) {
  if (!file.exists(path)) {
    empty <- setNames(
      lapply(RESOLVER_ALERT_COLS, function(x) character(0)),
      RESOLVER_ALERT_COLS
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
  # Schema tolerance, same convention as read_resolver_cache(): fill any
  # column this version knows about but the file lacks, drop anything
  # unknown -- cheap defense for the next column too, not just a one-time
  # migration shim.
  for (col in setdiff(RESOLVER_ALERT_COLS, names(df))) df[[col]] <- NA_character_
  df[, RESOLVER_ALERT_COLS]
}

write_resolver_alert_cache <- function(df, path) {
  readr::write_csv(df[, RESOLVER_ALERT_COLS], path, na = "")
}

## ============================================================ ##
## Upsert                                                       ##
## ============================================================ ##

# Replaces the row keyed on (source, item_key) if it exists, appends
# otherwise. checked_at defaults to now (UTC), matching mapbiomas_resolver_
# cache.R's cache_upsert() timestamp convention.
alert_cache_upsert <- function(cache, source, item_key, dataset = NA_character_,
                                geo_level = NA_character_, verdict, reason = NA_character_,
                                checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  key <- paste(cache$source, cache$item_key, sep = "\r")
  this_key <- paste(source, item_key, sep = "\r")
  match_idx <- which(key == this_key)

  new_row <- tibble::tibble(
    source = source, item_key = item_key, dataset = dataset,
    geo_level = geo_level, verdict = verdict, reason = reason, checked_at = checked_at
  )

  if (length(match_idx) == 0) {
    dplyr::bind_rows(cache, new_row)
  } else {
    cache[match_idx[1], RESOLVER_ALERT_COLS] <- new_row[1, RESOLVER_ALERT_COLS]
    cache
  }
}

## ============================================================ ##
## Recorder (per-resolver convenience wrapper)                  ##
## ============================================================ ##

# resolve_prodes.R, resolve_epe.R and resolve_ips.R each need the same four
# steps every time they check a candidate: read the committed cache once,
# upsert a verdict row, write the accumulated result to a candidate file
# under OUT_DIR, and assign("<source>_alert_cache_candidate_path", ...,
# envir = .GlobalEnv) so build_manifest.R's merge loop can find it even if
# the resolver itself later stop()s (confirmed safe: the tryCatch() wrapping
# each resolver's call in build_manifest.R catches the error but never rolls
# back a global assignment made before it). alert_recorder() is that
# boilerplate, extracted once instead of copied a third time.
#
# repo_root/OUT_DIR are build_manifest.R globals every resolver file is
# source()d into (same convention resolve_mapbiomas.R's own cache read
# already relies on) -- cache_path/out_dir let a standalone/test invocation
# override them explicitly instead of silently depending on globals that
# may not exist there, degrading to an in-memory-only cache if neither is
# available.
alert_recorder <- function(source, cache_path = NULL, out_dir = NULL) {
  if (is.null(cache_path)) {
    cache_path <- tryCatch(
      file.path(get("repo_root", envir = .GlobalEnv), "actions", "cache", "resolver_alerts.csv"),
      error = function(e) NA_character_
    )
  }
  cache <- if (!is.na(cache_path)) read_resolver_alert_cache(cache_path) else read_resolver_alert_cache(tempfile())

  function(item_key, verdict, reason = NA_character_, dataset = NA_character_, geo_level = NA_character_) {
    cache <<- alert_cache_upsert(
      cache,
      source = source, item_key = item_key, dataset = dataset, geo_level = geo_level,
      verdict = verdict, reason = reason
    )
    if (is.null(out_dir)) {
      out_dir <- tryCatch(get("OUT_DIR", envir = .GlobalEnv), error = function(e) NA_character_)
    }
    candidate_path <- if (!is.na(out_dir)) {
      file.path(out_dir, paste0(source, "_alert_cache_candidate.csv"))
    } else {
      tempfile(fileext = ".csv")
    }
    write_resolver_alert_cache(cache, candidate_path)
    assign(paste0(source, "_alert_cache_candidate_path"), candidate_path, envir = .GlobalEnv)
    invisible(cache)
  }
}

## ============================================================ ##
## Merge (multi-source cooperation without a shared mutable      ##
## object -- see resolve_prodes.R's/resolve_epe.R's headers for  ##
## why each resolver only ever writes its OWN source's partition)##
## ============================================================ ##

# Replaces `committed`'s rows for `source` with `source_rows` (which is
# itself a full RESOLVER_ALERT_COLS-shaped table, already upserted by that
# source's own resolver on top of a copy of `committed`) -- every OTHER
# source's rows in `committed` pass through untouched. Safe to call once per
# source that produced a candidate this run; the result after all calls is
# the correct final merged table even when two sources both changed
# something in the same run.
merge_resolver_alert_source <- function(committed, source, source_rows) {
  kept <- committed[committed$source != source, , drop = FALSE]
  added <- source_rows[source_rows$source == source, , drop = FALSE]
  dplyr::bind_rows(kept, added)
}

## ============================================================ ##
## Diff -- the actual Slack-worthy signal                       ##
## ============================================================ ##

new_resolver_alerts <- function(old, new) {
  empty <- new[0, , drop = FALSE]
  if (nrow(new) == 0) return(empty)
  # A cold/empty committed cache is a BOOTSTRAP, not news -- same rule
  # new_failed_files() applies, for the same reason (reporting every
  # historical fail as "new" on a first seeding run would be pure noise;
  # the seed PR itself is the review artifact).
  if (nrow(old) == 0) return(empty)

  key <- function(df) paste(df$source, df$item_key, sep = "\r")
  is_fail <- !is.na(new$verdict) & new$verdict == "fail"

  # "fresh" here means "this (source, item_key)'s LAST recorded verdict, if
  # any, was not already a fail" -- not just "the key never existed before".
  # A key that flips from pass to fail (e.g. the SAME PRODES stamp -- an
  # unlikely but real INPE-republished-under-the-same-name scenario -- or
  # more realistically an EPE block whose reason text happens to repeat
  # verbatim after a different one in between) is a genuine regression and
  # deserves an alert every bit as much as a brand-new key failing for the
  # first time; a repeat of the SAME already-known fail must not re-alert
  # (the anti-noise property new_failed_files() has for MapBiomas). Found
  # live: keying "fresh" on item_key alone missed exactly this pass->fail
  # case during this feature's own verification.
  old_verdict_by_key <- stats::setNames(old$verdict, key(old))
  last_verdict <- old_verdict_by_key[key(new)]
  was_already_fail <- !is.na(last_verdict) & last_verdict == "fail"
  fresh <- !was_already_fail

  new[is_fail & fresh, , drop = FALSE]
}
