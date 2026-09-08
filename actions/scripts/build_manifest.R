# actions/scripts/build_manifest.R
#
# Driver for the manifest refresh pipeline. This is the exact script that
# .github/workflows/update-manifest.yaml runs on its weekly schedule -- and,
# right now, the exact script exercised BY HAND with --dry-run while that
# workflow is written but not yet activated (nothing has been pushed to
# GitHub, so the Action itself never runs; see the plan / PDF report for the
# activation checklist).
#
# What it does:
#   1. Reads the committed manifest (inst/extdata/manifest/v1/datasets_link.csv)
#      AND the committed site-link inventory (actions/watch/site_links.csv --
#      see site_inventory.R's header for what that file is and why it exists).
#   2. Runs every registered resolver (functions named resolve_<source>() in
#      actions/scrapers/resolve_*.R) inside tryCatch, collecting successes
#      and failures separately. A resolver failure never touches that
#      source's rows -- they are simply left as they were.
#   3. Merges each successful resolver's output over the rows it owns
#      (identified by the manifest's `resolver` column). A brand-new row is
#      seeded from its dataset's existing base row (not blank) before the
#      resolver's own values are overlaid -- every row must be
#      self-sufficient at read time (R/manifest.R does a single exact-key
#      lookup, no fallthrough), so a new row born all-NA would resolve every
#      field the resolver didn't just set, even when a sensible value is
#      sitting right there on the sibling base row.
#   4. Runs every registered WATCHER (functions named watch_<source>() in
#      actions/scrapers/watch_*.R) the same way -- tryCatch-isolated, one
#      failure never touches another source. Unlike a resolver, a watcher
#      reports the FULL current set of links it saw on its page(s), so its
#      output wholesale-replaces that source's rows in the candidate
#      inventory rather than being merged field-by-field.
#   5. Runs the full validation gate from manifest_validate.R against the
#      merged manifest candidate, including validate_key_completeness() --
#      every geo_level/year the base row declares (available_geo/
#      available_time) must have an actual row backing it, since there is no
#      more inheritance to fall back on for one that's missing -- and the
#      structural gate from site_inventory.R (validate_inventory()) against
#      the merged inventory candidate.
#   6. Writes a JSON report and a plain-text summary to OUT_DIR, and always
#      writes the candidate manifest AND candidate inventory to OUT_DIR too.
#      OUT_DIR is RUNNER_TEMP when set (GitHub Actions' job-scoped temp dir,
#      which survives across steps/processes within the same job) or R's own
#      tempdir() otherwise (local/interactive runs) -- plain tempdir() would
#      NOT work for the CI case, because R deletes its own session tempdir()
#      when the Rscript process exits, and each workflow step is a separate
#      process. Neither committed path (the manifest or the inventory) is
#      EVER written by this script -- every change, however small, is left
#      for the calling workflow to stage onto a PR branch for human review
#      (see update-manifest.yaml's "Open PR" step). All resolved OUT_DIR
#      paths are exposed to the workflow as candidate_path/inventory_path/
#      report_path step outputs via GITHUB_OUTPUT (same pattern as
#      GITHUB_STEP_SUMMARY below), since a later step's shell has no other
#      way to know them.
#
# Usage:
#   Rscript actions/scripts/build_manifest.R --dry-run
#   Rscript actions/scripts/build_manifest.R --dry-run --only aneel
#   Rscript actions/scripts/build_manifest.R --dry-run --only aneel --only baci
#   Rscript actions/scripts/build_manifest.R --dry-run --skip seeg
#   Rscript actions/scripts/build_manifest.R --dry-run --check-all
#   Rscript actions/scripts/build_manifest.R --dry-run --no-watch
#
# --check-all additionally probes every non-exempt `url` in the candidate
# manifest (not just the rows a resolver just changed) and REPORTS which
# ones are broken -- it never adds to `errors` or changes the exit code, so
# a long-standing, documented break (e.g. epe/energy_state_panel, 404 since
# before this flag existed) stays visible on every run instead of only
# being noticed the next time that row happens to change.
#
# --skip <source> excludes one resolver AND/OR watcher of that name from
# this run entirely (repeatable, same shape as --only) -- --only/--skip
# select by SOURCE NAME across both registries at once, since one source
# (e.g. mapbiomas) can have both a resolver and a watcher registered under
# the same name. This is NOT the same as blanking a row's `resolver` column
# in the manifest -- that column only decides which rows an already-
# discovered resolver owns, so a blanked-but-still-discovered resolver would
# run, get zero owned rows, and be recorded as a failure below (see the
# empty-result check after "Run resolvers"). --skip removes it from the
# registry before it ever runs, so its rows are simply left untouched and no
# failure is recorded. Added to let a resolver that is known-broken in one
# environment (e.g. seeg returns HTTP 403 specifically from GitHub Actions
# runners, not locally -- see WEBSCRAPING_BETA_REPORT) stop failing the
# scheduled run without silently dropping every OTHER future resolver the
# way a hardcoded --only whitelist in the workflow file would.
#
# --no-watch disables the entire watcher pass (every watch_<source>()),
# independent of --only/--skip -- useful when iterating on resolver logic
# without re-scraping every watched page each time.
#
# Exit codes (unchanged in meaning since resolvers alone -- "changes" and
# "failures" below now each mean "from a resolver OR a watcher"):
#   0  candidate validated; no changes -- nothing to do
#   10 candidate validated; changes present (manifest and/or inventory) --
#      open a PR for human review
#   11 candidate validated; changes present, AND one or more resolvers/
#      watchers also errored -- open a PR for the real changes (from
#      whichever succeeded) AND raise an issue for the failure, instead of
#      the failure silently blocking the PR. Before this code existed, a
#      single broken resolver (e.g. mapbiomas throwing because MapBiomas
#      restructured its site) blocked a PR for every OTHER resolver's real
#      changes that same run too -- see project_mapbiomas_resolver_incident.
#   20 structural or HTTP validation failed (manifest or inventory) --
#      candidate is NOT trustworthy
#   30 one or more resolvers/watchers errored AND there were no changes to
#      report -- nothing for a human to review yet, just the failure to fix

suppressPackageStartupMessages({
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Find this script's own path so it can be run from any working directory
# (Rscript sets a --file= argument; devtools/RStudio "Source" leaves nothing,
# in which case we fall back to assuming the current directory is the repo root).
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
script_path <- if (length(this_file) > 0) this_file[1] else "actions/scripts/build_manifest.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
source(file.path(repo_root, "actions", "scripts", "manifest_validate.R"))
source(file.path(repo_root, "actions", "scripts", "site_inventory.R"))
source(file.path(repo_root, "actions", "scripts", "mapbiomas_fragility.R"))
source(file.path(repo_root, "actions", "scripts", "mapbiomas_resolver_cache.R"))

MANIFEST_PATH <- file.path(repo_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
INVENTORY_PATH <- file.path(repo_root, "actions", "watch", "site_links.csv")
SCRAPERS_DIR <- file.path(repo_root, "actions", "scrapers")

# R deletes its own tempdir() when the Rscript process exits, but the report
# and candidate CSV this script writes need to survive into LATER, separate
# workflow steps (a fresh process each) that read them back via GITHUB_OUTPUT.
# RUNNER_TEMP is a directory GitHub Actions provisions for the whole job (not
# tied to any one step/process) and cleans up itself at job end -- use it when
# present; fall back to tempdir() for local/interactive runs, where everything
# happens inside this one process anyway.
OUT_DIR <- {
  d <- Sys.getenv("RUNNER_TEMP", "")
  if (nzchar(d)) d else tempdir()
}

# ---- CLI args ---------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
check_all <- "--check-all" %in% args
no_watch <- "--no-watch" %in% args
only_sources <- character(0)
i <- which(args == "--only")
for (idx in i) {
  if (idx < length(args)) only_sources <- c(only_sources, args[idx + 1])
}
skip_sources <- character(0)
i <- which(args == "--skip")
for (idx in i) {
  if (idx < length(args)) skip_sources <- c(skip_sources, args[idx + 1])
}

# ---- Load resolvers + watchers ------------------------------------------------
# actions/scrapers/resolve_<source>.R defines resolve_<source>(rows) (manifest
# rows) and actions/scrapers/watch_<source>.R defines watch_<source>(rows)
# (inventory rows for that source) -- both picked up automatically by
# filename convention, no registry file to keep in sync. --only/--skip
# select by SOURCE NAME across both registries at once (see header comment).

resolver_files <- list.files(SCRAPERS_DIR, pattern = "^resolve_.*\\.R$", full.names = TRUE)
for (f in resolver_files) source(f)
watcher_files <- list.files(SCRAPERS_DIR, pattern = "^watch_.*\\.R$", full.names = TRUE)
for (f in watcher_files) source(f)

resolver_names <- grep("^resolve_", ls(), value = TRUE)
registry <- setNames(
  lapply(resolver_names, get),
  sub("^resolve_", "", resolver_names)
)

watcher_fn_names <- grep("^watch_", ls(), value = TRUE)
watch_registry <- setNames(
  lapply(watcher_fn_names, get),
  sub("^watch_", "", watcher_fn_names)
)

all_known_sources <- union(names(registry), names(watch_registry))

if (length(only_sources) > 0) {
  unknown <- setdiff(only_sources, all_known_sources)
  if (length(unknown) > 0) {
    stop(
      "--only referenced source(s) registered in neither a resolver nor a watcher: ",
      paste(unknown, collapse = ", "),
      if (length(all_known_sources) == 0) " (none registered yet)" else ""
    )
  }
  registry <- registry[intersect(names(registry), only_sources)]
  watch_registry <- watch_registry[intersect(names(watch_registry), only_sources)]
}

if (length(skip_sources) > 0) {
  unknown <- setdiff(skip_sources, all_known_sources)
  if (length(unknown) > 0) {
    stop(
      "--skip referenced source(s) registered in neither a resolver nor a ",
      "watcher (or already excluded by --only): ", paste(unknown, collapse = ", ")
    )
  }
  registry <- registry[setdiff(names(registry), skip_sources)]
  watch_registry <- watch_registry[setdiff(names(watch_registry), skip_sources)]
}

if (no_watch) watch_registry <- watch_registry[character(0)]

cat(sprintf(
  "build_manifest.R: %d resolver(s) registered, %d selected to run; %d watcher(s) registered, %d selected to run.\n",
  length(resolver_names), length(registry), length(watcher_fn_names), length(watch_registry)
))
if (length(skip_sources) > 0) {
  cat("Skipped by --skip:", paste(skip_sources, collapse = ", "), "\n")
}
if (no_watch) {
  cat("Watcher pass disabled by --no-watch.\n")
}

# ---- Read the current manifest ----------------------------------------------

read_manifest_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

old <- read_manifest_csv(MANIFEST_PATH)
candidate <- old

old_inventory <- read_inventory(INVENTORY_PATH) # empty tibble if the file doesn't exist yet (pre-seeding)
candidate_inventory <- old_inventory

# ---- Run resolvers and merge their output -----------------------------------

# Seeds a brand-new manifest row with the AGREED value across its dataset's
# EXISTING sibling rows for every value column (KEY_COLS get overwritten by
# the caller right after this returns, so their placeholder here is never
# actually used) -- mirrors R/manifest.R's agreed_value()/effective_table()
# collapse exactly, kept as its own standalone copy for the same reason
# manifest_validate.R keeps its own MANIFEST_ALL_COLS/VALIDATOR_META_COLS
# rather than importing R/manifest.R's (this script never loads the actual
# package -- see this file's own header). A dataset with zero existing
# siblings (its very first row) seeds everything NA, which is correct: the
# resolver's own values (applied by the caller) are 100% of what's known.
seed_new_row <- function(candidate, survey, dataset) {
  siblings <- candidate[candidate$survey == survey & candidate$dataset == dataset, ]
  value_cols <- setdiff(MANIFEST_ALL_COLS, KEY_COLS)
  vals <- lapply(value_cols, function(col) {
    v <- unique(siblings[[col]][!is.na(siblings[[col]])])
    if (length(v) == 1) v else NA_character_
  })
  names(vals) <- value_cols
  c(list(survey = survey, dataset = dataset, geo_level = NA_character_, year = NA_character_), vals)
}

resolver_ok <- character(0)
resolver_failed <- list()
# 2026-09-08: a resolver covering several independent sub-sources in one
# function (e.g. resolve_epe.R's 3 blocks) can have ONE of them fail while
# the others succeed -- that's neither `resolver_ok` (something genuinely
# didn't resolve) nor `resolver_failed` (the successful rows still need
# merging, so `next`-ing past the merge below would silently discard them).
# A resolver signals this by returning its normal successful tibble with a
# `partial_failures` attribute (character vector of reasons) attached --
# read below, right after a successful merge, and folded into total_failed
# the same way resolver_failed/watcher_failed already are, so this can
# still trip exit code 11 (changes + a failure) instead of looking like a
# clean run.
resolver_partial_failed <- list()

for (src in names(registry)) {
  cat("Running resolver:", src, "... ")
  owned_rows <- old[!is.na(old$resolver) & old$resolver == src, ]

  result <- tryCatch(
    registry[[src]](owned_rows),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    cat("FAILED:", conditionMessage(result), "\n")
    resolver_failed[[src]] <- conditionMessage(result)
    next
  }
  if (!is.data.frame(result) || nrow(result) == 0) {
    cat("FAILED: resolver returned no rows\n")
    resolver_failed[[src]] <- "resolver returned an empty/invalid result"
    next
  }

  for (i in seq_len(nrow(result))) {
    r <- result[i, ]
    # geo_level/year use %in%, not ==, so that an NA on both sides (a
    # dataset's own base row) matches -- `==` against NA yields NA, which
    # which() drops, silently treating every base-row update as a
    # brand-new row and duplicating it. dataset is never NA under the
    # self-sufficient-rows schema (every resolver fans its output out to
    # real dataset rows -- see actions/scrapers/resolve_*.R), so `==` would
    # do here too, but %in% costs nothing and stays consistent.
    match_idx <- which(
      candidate$survey == r$survey & candidate$dataset %in% r$dataset &
        candidate$geo_level %in% r$geo_level & candidate$year %in% r$year
    )
    cols_present <- intersect(names(r), MANIFEST_ALL_COLS)

    if (length(match_idx) == 0) {
      # A brand-new row (e.g. a new ANEEL year, or a geo_level a resolver
      # just started tracking). Seed it from the AGREED value across its
      # dataset's existing sibling rows (see seed_new_row() above), THEN
      # overlay the resolver's values -- under the self-sufficient-rows
      # schema there is no fallthrough at read time, so a row born all-NA
      # would resolve every field the resolver didn't just set to NA, even
      # though sensible values (available_time, available_geo, resolver,
      # ...) are sitting right there on its siblings. This used to seed
      # from the dataset's base row specifically -- removed along with
      # base rows themselves (see R/manifest.R): a base row could silently
      # drift stale relative to its own overrides (confirmed live:
      # mapbiomas_mining's base row stayed on Collection 8 for weeks after
      # both real override rows had already moved to 9), which would have
      # seeded every brand-new row with that same staleness. The collapse
      # can't do that -- a column its siblings disagree on becomes NA
      # instead of a possibly-wrong value, forcing the resolver's own
      # value to win instead.
      new_row <- seed_new_row(candidate, r$survey, r$dataset)
      new_row[cols_present] <- r[cols_present]
      new_row$resolver <- src
      candidate <- dplyr::bind_rows(candidate, tibble::as_tibble(new_row))
    } else {
      # Only overwrite fields the resolver actually set a non-NA value for.
      # A resolver's contract is "return only the values you want to
      # change" -- but when a resolver dplyr::bind_rows()'s together several
      # differently-shaped sub-tibbles (one per dataset/field it touches,
      # e.g. resolve_epe.R), any column one sub-tibble lacks gets padded
      # with NA on every OTHER row in the bind. Blindly writing all of
      # cols_present would treat that NA-padding as "clear this field",
      # silently wiping a perfectly good existing value the resolver never
      # intended to touch -- confirmed live: resolve_epe.R's 3 sub-tibbles
      # don't share columns, and without this guard its run blanked
      # national_energy_balance's url and consumer/industrial_energy_
      # consumption's available_time on every existing-row update.
      non_na_cols <- cols_present[!is.na(r[1, cols_present])]
      candidate[match_idx[1], non_na_cols] <- r[1, non_na_cols]
    }
  }

  partial <- attr(result, "partial_failures")
  if (!is.null(partial) && length(partial) > 0) {
    cat("OK (", nrow(result), "row(s) touched) -- but with", length(partial), "partial failure(s):\n")
    cat(paste("   -", partial, collapse = "\n"), "\n")
    resolver_partial_failed[[src]] <- partial
  } else {
    cat("OK (", nrow(result), "row(s) touched)\n")
  }
  resolver_ok <- c(resolver_ok, src)
}

# ---- Resolver verification cache diff ---------------------------------------
# See mapbiomas_resolver_cache.R's header. resolve_mapbiomas() writes a
# candidate cache as a SIDE EFFECT (not through its normal return value,
# which is reserved for manifest rows) and signals via these globals
# whether anything changed -- absent/FALSE if resolve_mapbiomas() was
# skipped entirely (--only excluded it) or genuinely found nothing new to
# record. Folded into total_changed below so a run that only refreshed
# cache entries (no real manifest/inventory change) still opens a small PR
# rather than silently updating a committed file with no review -- see this
# feature's plan for why that's an accepted tradeoff, not a bug.
mapbiomas_cache_out <- NA_character_
cache_diff_n <- 0
new_fail_rows <- NULL
if (isTRUE(get0("mapbiomas_cache_changed", ifnotfound = FALSE))) {
  mapbiomas_cache_out <- get0("mapbiomas_cache_candidate_path", ifnotfound = NA_character_)
  if (!is.na(mapbiomas_cache_out) && file.exists(mapbiomas_cache_out)) {
    committed_cache_path <- file.path(repo_root, "actions", "cache", "mapbiomas_resolver_cache.csv")
    old_cache <- read_resolver_cache(committed_cache_path)
    new_cache <- read_resolver_cache(mapbiomas_cache_out)
    cache_key <- function(df) paste(df$dataset, dplyr::coalesce(df$geo_level, ""), df$file_id, sep = "\r")
    old_key <- cache_key(old_cache)
    new_key <- cache_key(new_cache)
    added <- sum(!(new_key %in% old_key))
    removed <- sum(!(old_key %in% new_key))
    changed <- 0
    for (k in intersect(old_key, new_key)) {
      o <- old_cache[old_key == k, RESOLVER_CACHE_COLS][1, ]
      n <- new_cache[new_key == k, RESOLVER_CACHE_COLS][1, ]
      # checked_at is informational only (human debugging); reason is
      # informational metadata about a verdict, not the verdict itself --
      # neither ever counts as a real change, same posture as the
      # inventory's first_seen column. Excluding `reason` here is what
      # makes it STRUCTURALLY impossible for a reason-string edit alone to
      # move cache_diff_n (and therefore total_changed), not just true by
      # argument.
      cmp_cols <- setdiff(RESOLVER_CACHE_COLS, c("checked_at", "reason"))
      if (!identical(as.list(o[cmp_cols]), as.list(n[cmp_cols]))) changed <- changed + 1
    }
    cache_diff_n <- added + removed + changed

    # Informational side-channel ONLY -- see new_failed_files() in
    # mapbiomas_resolver_cache.R. Deliberately computed AFTER cache_diff_n
    # and never folded into it: a new-but-rejected candidate must not move
    # total_changed, an exit code, or whether a PR opens. Wrapped so a bug
    # in a notification feature can never take down a manifest run.
    new_fail_rows <- tryCatch(
      new_failed_files(old_cache, new_cache),
      error = function(e) {
        message("new_failed_files() failed (notification skipped): ", conditionMessage(e))
        NULL
      }
    )
  }
}

# ---- Run watchers and merge their output ------------------------------------
#
# Unlike a resolver (which patches individual manifest rows field-by-field),
# a watcher reports the FULL current set of links it saw on its page(s) --
# so its output wholesale-REPLACES that source's rows in the candidate
# inventory, rather than being merged row-by-row. first_seen is carried
# forward from old_inventory for any identity that already existed (see
# carry_first_seen() in site_inventory.R) before the replace.

watcher_ok <- character(0)
watcher_failed <- list()

for (src in names(watch_registry)) {
  cat("Running watcher:", src, "... ")
  owned_rows <- old_inventory[!is.na(old_inventory$source) & old_inventory$source == src, ]

  result <- tryCatch(
    watch_registry[[src]](owned_rows),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    cat("FAILED:", conditionMessage(result), "\n")
    watcher_failed[[src]] <- conditionMessage(result)
    next
  }
  if (!is.data.frame(result) || nrow(result) == 0) {
    cat("FAILED: watcher returned no rows\n")
    watcher_failed[[src]] <- "watcher returned an empty/invalid result"
    next
  }

  result <- carry_first_seen(old_inventory, result)
  candidate_inventory <- candidate_inventory[
    is.na(candidate_inventory$source) | candidate_inventory$source != src,
  ]
  candidate_inventory <- dplyr::bind_rows(candidate_inventory, result)

  cat("OK (", nrow(result), "row(s) recorded)\n")
  watcher_ok <- c(watcher_ok, src)
}

# Brand-new rows (see the bind_rows() branch above) land at the end of
# `candidate`, not next to their siblings. Re-sort to the manifest's existing
# convention -- survey, then dataset/geo_level/year, base row before its own
# overrides -- so a diff against the previous manifest stays readable
# instead of showing new rows appended at EOF. coalesce(..., "") (not a
# plain NA-last sort) keeps an unkeyed dataset's one row sorting the same
# way it always did; for a keyed dataset every row has a real geo_level or
# year now (no more blank-key base row to sort first -- see R/manifest.R).
candidate <- dplyr::arrange(
  candidate, survey,
  dplyr::coalesce(dataset, ""), dplyr::coalesce(geo_level, ""), dplyr::coalesce(year, "")
)

# ---- Validate ----------------------------------------------------------------

errors <- c(
  validate_schema(candidate),
  validate_row_count(old, candidate),
  validate_no_deletions(old, candidate),
  validate_row_grouping(candidate),
  validate_dataset_level_agreement(candidate),
  validate_placeholders(old, candidate),
  validate_key_completeness(candidate),
  validate_inventory(candidate_inventory)
)

changes <- detect_changes(old, candidate)
inv_diff <- diff_inventory(old_inventory, candidate_inventory)

# Watcher Slack signal. `length(watcher_ok) > 0` is the right guard, not
# `length(watcher_failed) == 0`: a watcher that FAILS `next`s (see the
# watcher loop above) BEFORE its rows replace anything in
# candidate_inventory, so a failed watcher contributes exactly zero to
# inv_diff by construction -- candidate_inventory starts as a copy of
# old_inventory. Any diff we see therefore provably came from a watcher
# that succeeded, which is the confirmed requirement: failures never go to
# Slack, they keep going to the GitHub issue path, untouched.
#
# `length(errors) == 0` additionally guards against notifying on a diff
# that validate_inventory() (already run into `errors` above) rejected --
# e.g. a scrape that came back structurally malformed. Without this, a
# rejected diff would still produce a Slack message (with no PR to back it
# up, since exit 20 opens an issue instead), which reads as a normal
# heads-up when it's actually invalid data a human needs to go debug via
# the issue instead.
watch_notify <- length(watcher_ok) > 0 && inv_diff$n > 0 && length(errors) == 0

if (length(errors) == 0 && changes$n_changed > 0) {
  errors <- c(errors, validate_http(candidate, changes$changed_keys))
}

# ---- Optional full manifest health scan (--check-all) ------------------------
#
# validate_http() above only probes CHANGED rows -- a row that has been
# broken for a while (e.g. epe/energy_state_panel's manifested URL, 404
# since before this script existed) never shows up there, because nothing
# about it changed. --check-all probes every non-exempt url in the
# manifest and REPORTS what it finds; it never adds to `errors`, so a
# long-known, documented break never fails the scheduled run -- it just
# stays visible instead of being forgotten.

manifest_health_report <- function(candidate) {
  broken <- list()
  # Self-sufficient rows mean several can legitimately share one url (see
  # validate_http() above) -- probe each DISTINCT url once and report it
  # against every row that carries it, instead of re-probing the same
  # endpoint several times per run.
  non_exempt <- candidate[!vapply(seq_len(nrow(candidate)), function(i) is_http_exempt(candidate[i, ]), logical(1)), ]

  for (u in unique(non_exempt$url)) {
    probe <- probe_url(u)
    if (isTRUE(probe$ok)) next
    offenders <- non_exempt[non_exempt$url == u, ]
    for (i in seq_len(nrow(offenders))) {
      row <- offenders[i, ]
      broken[[length(broken) + 1]] <- list(
        survey = row$survey, dataset = row$dataset,
        geo_level = row$geo_level, year = row$year,
        url = row$url,
        reason = if (isFALSE(probe$ok)) probe$reason else "probe skipped (curl unavailable)"
      )
    }
  }
  broken
}

health <- if (check_all) manifest_health_report(candidate) else NULL

# ---- Report ------------------------------------------------------------------

# Compact, human-readable identity strings for the JSON report -- easier to
# scan in an issue/PR body than the raw columns.
fmt_inventory_rows <- function(df) {
  if (nrow(df) == 0) return(character(0))
  sprintf(
    "%s | %s | col=%s | %s",
    df$label, df$host, ifelse(is.na(df$collection), "NA", df$collection), df$url
  )
}

report <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  dry_run = dry_run,
  resolvers_run = names(registry),
  resolvers_skipped = skip_sources,
  resolvers_ok = resolver_ok,
  resolvers_failed = resolver_failed,
  resolvers_partial_failed = resolver_partial_failed,
  watchers_run = names(watch_registry),
  watchers_ok = watcher_ok,
  watchers_failed = watcher_failed,
  rows_old = nrow(old),
  rows_candidate = nrow(candidate),
  n_changed = changes$n_changed,
  changed_keys = changes$changed_keys,
  n_inventory_changed = inv_diff$n,
  inventory_added = fmt_inventory_rows(inv_diff$added),
  inventory_removed = fmt_inventory_rows(inv_diff$removed),
  inventory_changed = fmt_inventory_rows(inv_diff$changed),
  n_mapbiomas_cache_changed = cache_diff_n,
  n_mapbiomas_new_fail = if (is.null(new_fail_rows)) 0L else nrow(new_fail_rows),
  # Purely additive -- the "Open issue" workflow step only reads
  # resolvers_failed/watchers_failed/validation_errors by name, so adding
  # sibling keys here cannot affect it.
  mapbiomas_new_fail = if (is.null(new_fail_rows) || nrow(new_fail_rows) == 0) {
    character(0)
  } else {
    sprintf(
      "%s | %s | %s | %s",
      new_fail_rows$file_id, new_fail_rows$dataset,
      ifelse(is.na(new_fail_rows$geo_level), "NA", new_fail_rows$geo_level),
      dplyr::coalesce(new_fail_rows$reason, "NA")
    )
  },
  n_inventory_changed_notified = isTRUE(watch_notify),
  validation_errors = errors,
  manifest_health = health
)

report_path <- file.path(OUT_DIR, "manifest_report.json")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(report, report_path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  cat("\nReport written to:", report_path, "\n")
} else {
  cat("\n(jsonlite not installed -- skipping JSON report; printing summary only)\n")
}

cat("\n==================== build_manifest.R summary ====================\n")
cat("Rows: ", nrow(old), "->", nrow(candidate), "\n")
cat("Changed keys:", changes$n_changed, "\n")
cat("Resolvers OK:", if (length(resolver_ok)) paste(resolver_ok, collapse = ", ") else "(none)", "\n")
cat("Resolvers FAILED:", if (length(resolver_failed)) paste(names(resolver_failed), collapse = ", ") else "(none)", "\n")
cat("Resolvers PARTIALLY FAILED:", if (length(resolver_partial_failed)) paste(names(resolver_partial_failed), collapse = ", ") else "(none)", "\n")
cat("Resolvers SKIPPED (--skip):", if (length(skip_sources)) paste(skip_sources, collapse = ", ") else "(none)", "\n")
cat("Watchers OK:", if (length(watcher_ok)) paste(watcher_ok, collapse = ", ") else "(none)", "\n")
cat("Watchers FAILED:", if (length(watcher_failed)) paste(names(watcher_failed), collapse = ", ") else "(none)", "\n")
cat(sprintf(
  "Inventory changes: %d (added %d / removed %d / changed %d)\n",
  inv_diff$n, nrow(inv_diff$added), nrow(inv_diff$removed), nrow(inv_diff$changed)
))
cat("MapBiomas resolver cache changes:", cache_diff_n, "\n")
if (length(errors) > 0) {
  cat("Validation errors:\n")
  cat(paste(" -", errors, collapse = "\n"), "\n")
}
cat("====================================================================\n")

if (check_all) {
  cat("\n================ manifest health (--check-all, report-only) ================\n")
  if (length(health) == 0) {
    cat("All non-exempt URLs responded OK.\n")
  } else {
    for (h in health) {
      key <- sprintf(
        "%s/%s%s%s", h$survey, h$dataset,
        if (is.na(h$geo_level)) "" else paste0("/", h$geo_level),
        if (is.na(h$year)) "" else paste0("/", h$year)
      )
      cat(sprintf(" - %s: %s (%s)\n", key, h$reason, h$url))
    }
  }
  cat("==============================================================================\n")
}

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_path)) {
  con <- file(summary_path, open = "a")
  writeLines(c(
    "## Manifest refresh report",
    sprintf("- Rows: %d -> %d", nrow(old), nrow(candidate)),
    sprintf("- Changed keys: %d", changes$n_changed),
    sprintf("- Resolvers OK: %s", if (length(resolver_ok)) paste(resolver_ok, collapse = ", ") else "(none)"),
    sprintf("- Resolvers FAILED: %s", if (length(resolver_failed)) paste(names(resolver_failed), collapse = ", ") else "(none)"),
    sprintf("- Resolvers PARTIALLY FAILED: %s", if (length(resolver_partial_failed)) paste(names(resolver_partial_failed), collapse = ", ") else "(none)"),
    sprintf("- Resolvers SKIPPED (--skip): %s", if (length(skip_sources)) paste(skip_sources, collapse = ", ") else "(none)"),
    sprintf("- Watchers OK: %s", if (length(watcher_ok)) paste(watcher_ok, collapse = ", ") else "(none)"),
    sprintf("- Watchers FAILED: %s", if (length(watcher_failed)) paste(names(watcher_failed), collapse = ", ") else "(none)"),
    sprintf(
      "- Inventory changes: %d (added %d / removed %d / changed %d)",
      inv_diff$n, nrow(inv_diff$added), nrow(inv_diff$removed), nrow(inv_diff$changed)
    ),
    sprintf("- MapBiomas resolver cache changes: %d", cache_diff_n),
    if (check_all) sprintf("- Manifest health (--check-all): %d broken URL(s)", length(health)) else NULL
  ), con)
  close(con)
}

# ---- Write candidates + decide exit code --------------------------------------

candidate_out <- file.path(OUT_DIR, "datasets_link_candidate.csv")
readr::write_csv(candidate, candidate_out, na = "")
cat("\nCandidate manifest written to:", candidate_out, "\n")

inventory_out <- file.path(OUT_DIR, "site_links_candidate.csv")
write_inventory(candidate_inventory, inventory_out)
cat("Candidate inventory written to:", inventory_out, "\n")

# ---- PR body ------------------------------------------------------------------
# Composed here (not left as a static string in update-manifest.yaml) so the
# MapBiomas fragility section below can be included only when it's actually
# relevant -- a resolver change to, say, ANEEL shouldn't carry a reminder
# about R/mapbiomas.R lines nobody touched this run.

pr_body_lines <- c(
  "One or more resolvers/watchers found a change to the manifest",
  "and/or the site-link inventory (actions/watch/site_links.csv --",
  "see site_inventory.R's header for what that file tracks).",
  "Nothing is ever auto-committed -- every change, however small,",
  "is opened as a PR for human review. See manifest_report.json in",
  "the run artifacts for the full diff, including which inventory",
  "links were added/removed/changed.",
  "",
  "Checklist before merging:",
  "- [ ] Confirm the new URL(s) actually serve the expected data",
  "- [ ] Re-run the affected `check-load-<source>.yaml` workflow",
  "- [ ] If a collection/version bump, check whether R/*.R hardcodes",
  "      anything that also needs to change (sheet names, raster",
  "      legend codes -- see prodes.R, mapbiomas.R, epe.R)",
  "- [ ] If a link was added or removed in the inventory diff,",
  "      check whether the corresponding manifest row needs a",
  "      matching change (a resolver failure elsewhere would",
  "      otherwise go unnoticed even though the site already moved)"
)

# detect_changes()'s keys are "survey\rdataset\rgeo_level\ryear" (see
# manifest_validate.R) -- a leading "mapbiomas\r" identifies a MapBiomas row.
mapbiomas_changed <- any(startsWith(changes$changed_keys, "mapbiomas\r"))
if (isTRUE(mapbiomas_changed)) {
  fragility_notes <- collect_mapbiomas_fragility_notes(file.path(repo_root, "R", "mapbiomas.R"))
  if (length(fragility_notes) > 0) {
    pr_body_lines <- c(
      pr_body_lines,
      "",
      "**MapBiomas structural fragility check** -- a mapbiomas manifest row",
      "changed this run. R/mapbiomas.R's treatment of the downloaded file",
      "makes several assumptions about its SHAPE, not just its column names,",
      "that a collection bump can silently violate without erroring. Re-check",
      "these specifically (see mapbiomas_treat()'s header in R/mapbiomas.R for",
      "the tagging convention this list is generated from):",
      "",
      paste0("- ", fragility_notes)
    )
  }
}

pr_body_path <- file.path(OUT_DIR, "pr_body.md")
writeLines(pr_body_lines, pr_body_path)
cat("PR body written to:", pr_body_path, "\n")

# ---- Slack notification payloads (informational side-channel) --------------
#
# The message BODY is composed here, in R, where the structured data already
# lives, and handed to the workflow as a single already-JSON-escaped line on
# GITHUB_OUTPUT. That's deliberate: the sibling datazoom_viz repo builds a
# multi-line value with a bash heredoc and interpolates it straight into a
# JSON string literal, which produces invalid JSON the moment there are two
# or more items and makes the Slack step fail silently. Escaping in R with
# jsonlite (the same library that already writes the report) means the value
# is JSON-valid by construction, and the workflow never re-parses anything.
#
# Nothing here is gated on dry_run: GITHUB_OUTPUT being unset is the guard,
# exactly like the GITHUB_STEP_SUMMARY block above. A local --dry-run run is
# therefore Slack-silent with no extra branch.

SLACK_MAX_BULLETS <- 8L

# Slack mrkdwn treats &, < and > as markup. Escape them in TEXT we
# interpolate (scraped labels can contain any of them); never inside a URL,
# where <url|label> needs the raw characters.
slack_text_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Turns a character vector into ONE line whose newlines are the literal two
# characters \ and n, correctly escaped for embedding in a JSON string.
# jsonlite handles quotes, backslashes, control characters and newlines in
# one pass -- no hand-rolled regex chain to get subtly wrong.
slack_line <- function(lines) {
  if (length(lines) == 0) return("")
  s <- paste(lines, collapse = "\n")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    # Degraded fallback: at minimum, never emit a real newline.
    return(gsub("[\r\n\t\"\\\\]", " ", s))
  }
  j <- as.character(jsonlite::toJSON(s, auto_unbox = TRUE))  # includes the outer quotes
  substr(j, 2L, nchar(j) - 1L)
}

# "• label (col=N) -- <url|host>" -- shorter and clickable, vs the raw-URL
# one-liners fmt_inventory_rows() writes into the JSON report.
fmt_inventory_slack <- function(df, heading) {
  if (nrow(df) == 0) return(character(0))
  bullets <- sprintf(
    "• %s (col=%s) — <%s|%s>",
    slack_text_escape(df$label),
    ifelse(is.na(df$collection), "NA", df$collection),
    df$url, slack_text_escape(df$host)
  )
  extra <- length(bullets) - SLACK_MAX_BULLETS
  bullets <- utils::head(bullets, SLACK_MAX_BULLETS)
  if (extra > 0) bullets <- c(bullets, sprintf("• ...and %d more (see the run artifact)", extra))
  c(sprintf("*%s (%d):*", heading, nrow(df)), bullets)
}

watch_slack_text <- if (isTRUE(watch_notify)) {
  slack_line(c(
    fmt_inventory_slack(inv_diff$added,   "Added"),
    fmt_inventory_slack(inv_diff$removed, "Removed"),
    fmt_inventory_slack(inv_diff$changed, "Changed")
  ))
} else ""

# --- new-but-rejected Dataverse candidates ---
new_fail_notify <- !is.null(new_fail_rows) && nrow(new_fail_rows) > 0
new_fail_lines <- character(0)
if (isTRUE(new_fail_notify)) {
  known_ids <- if (exists("old_cache")) unique(old_cache$file_id) else character(0)
  kind <- ifelse(new_fail_rows$file_id %in% known_ids, "content replaced", "new file")
  new_fail_lines <- sprintf(
    "• file %s (%s) — %s/%s — %s\n   <%s|open on Dataverse>",
    new_fail_rows$file_id, kind,
    new_fail_rows$dataset,
    ifelse(is.na(new_fail_rows$geo_level), "—", new_fail_rows$geo_level),
    slack_text_escape(dplyr::coalesce(new_fail_rows$reason, "reason not recorded")),
    # Mirrors dv_access_url() in resolve_mapbiomas.R (a closure, not
    # reachable from here) -- keep the two in sync if the Dataverse base
    # URL ever moves.
    paste0("https://data.mapbiomas.org/api/access/datafile/", new_fail_rows$file_id, "?format=original")
  )
  extra <- length(new_fail_lines) - SLACK_MAX_BULLETS
  new_fail_lines <- utils::head(new_fail_lines, SLACK_MAX_BULLETS)
  if (extra > 0) new_fail_lines <- c(new_fail_lines, sprintf("• ...and %d more", extra))
}
new_fail_slack_text <- if (isTRUE(new_fail_notify)) slack_line(new_fail_lines) else ""

# ---- Expose real paths to the calling workflow ------------------------------
# tempdir() is randomized per R session, so bash steps in the workflow have no
# way to know these paths unless we tell them. Mirrors the GITHUB_STEP_SUMMARY
# pattern above; safe to run outside CI (falls through silently).
gh_output_path <- Sys.getenv("GITHUB_OUTPUT", "")
if (nzchar(gh_output_path)) {
  con <- file(gh_output_path, open = "a")
  # enc2utf8()+useBytes: inventory labels are Portuguese with accents
  # (confirmed live in actions/watch/site_links.csv) and must survive into
  # $GITHUB_OUTPUT regardless of the runner's locale.
  writeLines(enc2utf8(c(
    sprintf("candidate_path=%s", candidate_out),
    sprintf("inventory_path=%s", inventory_out),
    sprintf("report_path=%s", report_path),
    sprintf("pr_body_path=%s", pr_body_path),
    sprintf("mapbiomas_cache_path=%s", if (is.na(mapbiomas_cache_out)) "" else mapbiomas_cache_out),
    # --- Slack side-channel (informational; never affects the exit code) ---
    sprintf("watch_notify=%s", if (isTRUE(watch_notify)) "true" else "false"),
    sprintf("watch_n_added=%d",   nrow(inv_diff$added)),
    sprintf("watch_n_removed=%d", nrow(inv_diff$removed)),
    sprintf("watch_n_changed=%d", nrow(inv_diff$changed)),
    sprintf("watch_slack_text=%s", watch_slack_text),
    sprintf("new_fail_notify=%s", if (isTRUE(new_fail_notify)) "true" else "false"),
    sprintf("new_fail_n=%d", if (isTRUE(new_fail_notify)) nrow(new_fail_rows) else 0L),
    sprintf("new_fail_slack_text=%s", new_fail_slack_text)
  )), con, useBytes = TRUE)
  close(con)
}

# Neither committed path (MANIFEST_PATH or INVENTORY_PATH) is ever written by
# this script -- every change, however small, goes through the PR path below
# instead (see the "Open PR" step in update-manifest.yaml). --dry-run only
# affects whether that behavior is even reachable in principle; there is
# nothing left for it to suppress here, but the flag is kept (and still gates
# the smoke-test/PR steps in the workflow) for symmetry with local dry runs.

if (length(errors) > 0) {
  cat("\nRESULT: validation FAILED. Exit code 20.\n")
  quit(status = 20, save = "no")
}
# "changes" and "failures" each now mean "from a resolver OR a watcher" --
# checking the combined change count BEFORE the combined failure count means
# a failing resolver/watcher never suppresses a PR for what everything ELSE
# actually found this run. Exit 11 carries both signals at once so
# update-manifest.yaml can open the PR AND raise the issue, instead of one
# silently winning over the other.
total_changed <- changes$n_changed + inv_diff$n + cache_diff_n
total_failed <- length(resolver_failed) + length(watcher_failed) + length(resolver_partial_failed)

if (total_changed > 0 && total_failed > 0) {
  cat("\nRESULT: changes present, but one or more resolvers/watchers also failed. Exit code 11.\n")
  quit(status = 11, save = "no")
}
if (total_changed > 0) {
  cat("\nRESULT: changes present -- needs human review / PR. Exit code 10.\n")
  quit(status = 10, save = "no")
}
if (total_failed > 0) {
  cat("\nRESULT: one or more resolvers/watchers failed, no changes to report. Exit code 30.\n")
  quit(status = 30, save = "no")
}

cat("\nRESULT: OK, no changes. Exit code 0.\n")
quit(status = 0, save = "no")
