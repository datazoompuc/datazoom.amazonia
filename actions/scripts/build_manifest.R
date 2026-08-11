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
#   1. Reads the committed manifest (inst/extdata/manifest/v1/datasets_link.csv).
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
#   4. Runs the full validation gate from manifest_validate.R against the
#      merged candidate, including validate_key_completeness() -- every
#      geo_level/year the base row declares (available_geo/available_time)
#      must have an actual row backing it, since there is no more
#      inheritance to fall back on for one that's missing.
#   5. Writes a JSON report and a plain-text summary to OUT_DIR, and always
#      writes the candidate manifest to OUT_DIR too. OUT_DIR is RUNNER_TEMP
#      when set (GitHub Actions' job-scoped temp dir, which survives across
#      steps/processes within the same job) or R's own tempdir() otherwise
#      (local/interactive runs) -- plain tempdir() would NOT work for the CI
#      case, because R deletes its own session tempdir() when the Rscript
#      process exits, and each workflow step is a separate process. The
#      committed manifest (inst/extdata/manifest/v1/datasets_link.csv) is
#      NEVER written by this script -- every change, however small, is left
#      for the calling workflow to stage onto a PR branch for human review
#      (see update-manifest.yaml's "Open PR" step). Both resolved OUT_DIR
#      paths are exposed to the workflow as candidate_path/report_path step
#      outputs via GITHUB_OUTPUT (same pattern as GITHUB_STEP_SUMMARY
#      below), since a later step's shell has no other way to know them.
#
# Usage:
#   Rscript actions/scripts/build_manifest.R --dry-run
#   Rscript actions/scripts/build_manifest.R --dry-run --only aneel
#   Rscript actions/scripts/build_manifest.R --dry-run --only aneel --only baci
#   Rscript actions/scripts/build_manifest.R --dry-run --skip seeg
#   Rscript actions/scripts/build_manifest.R --dry-run --check-all
#
# --check-all additionally probes every non-exempt `url` in the candidate
# manifest (not just the rows a resolver just changed) and REPORTS which
# ones are broken -- it never adds to `errors` or changes the exit code, so
# a long-standing, documented break (e.g. epe/energy_state_panel, 404 since
# before this flag existed) stays visible on every run instead of only
# being noticed the next time that row happens to change.
#
# --skip <source> excludes one resolver from this run entirely (repeatable,
# same shape as --only). This is NOT the same as blanking a row's `resolver`
# column in the manifest -- that column only decides which rows an already-
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
# Exit codes:
#   0  candidate validated; no changes -- nothing to do
#   10 candidate validated; changes present -- open a PR for human review
#   20 structural or HTTP validation failed -- candidate is NOT trustworthy
#   30 one or more resolvers errored (their rows were left untouched, but
#      the run as a whole should raise an issue for a maintainer)

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

MANIFEST_PATH <- file.path(repo_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
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

# ---- Load resolvers ----------------------------------------------------------
# Fase 2 ships this driver with ZERO resolvers registered -- it only
# validates and HTTP-probes the manifest as it stands today. Fase 5 adds
# actions/scrapers/resolve_<source>.R files one at a time; each simply
# defines a function resolve_<source>(rows) and is picked up automatically
# by the convention below (no registry file to keep in sync).

resolver_files <- list.files(SCRAPERS_DIR, pattern = "^resolve_.*\\.R$", full.names = TRUE)
for (f in resolver_files) source(f)

resolver_names <- grep("^resolve_", ls(), value = TRUE)
registry <- setNames(
  lapply(resolver_names, get),
  sub("^resolve_", "", resolver_names)
)

if (length(only_sources) > 0) {
  unknown <- setdiff(only_sources, names(registry))
  if (length(unknown) > 0) {
    stop(
      "--only referenced resolver(s) not registered yet: ", paste(unknown, collapse = ", "),
      if (length(registry) == 0) " (no resolvers exist yet -- Fase 5 adds them one at a time)" else ""
    )
  }
  registry <- registry[only_sources]
}

if (length(skip_sources) > 0) {
  unknown <- setdiff(skip_sources, names(registry))
  if (length(unknown) > 0) {
    stop(
      "--skip referenced resolver(s) not registered (or already excluded by --only): ",
      paste(unknown, collapse = ", ")
    )
  }
  registry <- registry[setdiff(names(registry), skip_sources)]
}

cat(sprintf("build_manifest.R: %d resolver(s) registered, %d selected to run.\n",
            length(resolver_names), length(registry)))
if (length(skip_sources) > 0) {
  cat("Skipped by --skip:", paste(skip_sources, collapse = ", "), "\n")
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

# ---- Run resolvers and merge their output -----------------------------------

resolver_ok <- character(0)
resolver_failed <- list()

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
      # just started tracking). Seed it from its dataset's own base row
      # when one exists, THEN overlay the resolver's values -- under the
      # self-sufficient-rows schema there is no fallthrough at read time,
      # so a row born all-NA would resolve every field the resolver didn't
      # just set to NA, even though sensible values (available_time,
      # available_geo, docs_url, resolver, ...) are sitting right there on
      # the sibling base row.
      base_idx <- which(
        candidate$survey == r$survey & candidate$dataset == r$dataset &
          is.na(candidate$geo_level) & is.na(candidate$year)
      )
      new_row <- if (length(base_idx) == 1) {
        as.list(candidate[base_idx[1], MANIFEST_ALL_COLS])
      } else {
        setNames(as.list(rep(NA_character_, length(MANIFEST_ALL_COLS))), MANIFEST_ALL_COLS)
      }
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

  cat("OK (", nrow(result), "row(s) touched)\n")
  resolver_ok <- c(resolver_ok, src)
}

# Brand-new rows (see the bind_rows() branch above) land at the end of
# `candidate`, not next to their siblings. Re-sort to the manifest's existing
# convention -- survey, then dataset/geo_level/year, base row before its own
# overrides -- so a diff against the previous manifest stays readable
# instead of showing new rows appended at EOF. coalesce(..., "") (not a
# plain NA-last sort) puts each dataset's base row (geo_level/year both NA)
# immediately before its overrides.
candidate <- dplyr::arrange(
  candidate, survey,
  dplyr::coalesce(dataset, ""), dplyr::coalesce(geo_level, ""), dplyr::coalesce(year, "")
)

# ---- Validate ----------------------------------------------------------------

errors <- c(
  validate_schema(candidate),
  validate_row_count(old, candidate),
  validate_no_deletions(old, candidate),
  validate_unique_base_row(candidate),
  validate_placeholders(old, candidate),
  validate_key_completeness(candidate)
)

changes <- detect_changes(old, candidate)

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

report <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  dry_run = dry_run,
  resolvers_run = names(registry),
  resolvers_skipped = skip_sources,
  resolvers_ok = resolver_ok,
  resolvers_failed = resolver_failed,
  rows_old = nrow(old),
  rows_candidate = nrow(candidate),
  n_changed = changes$n_changed,
  changed_keys = changes$changed_keys,
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
cat("Resolvers SKIPPED (--skip):", if (length(skip_sources)) paste(skip_sources, collapse = ", ") else "(none)", "\n")
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
        "%s/%s%s%s", h$survey, if (is.na(h$dataset)) "<default>" else h$dataset,
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
    sprintf("- Resolvers SKIPPED (--skip): %s", if (length(skip_sources)) paste(skip_sources, collapse = ", ") else "(none)"),
    if (check_all) sprintf("- Manifest health (--check-all): %d broken URL(s)", length(health)) else NULL
  ), con)
  close(con)
}

# ---- Write candidate + decide exit code --------------------------------------

candidate_out <- file.path(OUT_DIR, "datasets_link_candidate.csv")
readr::write_csv(candidate, candidate_out, na = "")
cat("\nCandidate manifest written to:", candidate_out, "\n")

# ---- Expose real paths to the calling workflow ------------------------------
# tempdir() is randomized per R session, so bash steps in the workflow have no
# way to know these paths unless we tell them. Mirrors the GITHUB_STEP_SUMMARY
# pattern above; safe to run outside CI (falls through silently).
gh_output_path <- Sys.getenv("GITHUB_OUTPUT", "")
if (nzchar(gh_output_path)) {
  con <- file(gh_output_path, open = "a")
  writeLines(c(
    sprintf("candidate_path=%s", candidate_out),
    sprintf("report_path=%s", report_path)
  ), con)
  close(con)
}

# The committed manifest (MANIFEST_PATH) is never written by this script --
# every change, however small, goes through the PR path below instead (see
# the "Open PR" step in update-manifest.yaml). --dry-run only affects
# whether that behavior is even reachable in principle; there is nothing
# left for it to suppress here, but the flag is kept (and still gates the
# smoke-test/PR steps in the workflow) for symmetry with local dry runs.

if (length(errors) > 0) {
  cat("\nRESULT: validation FAILED. Exit code 20.\n")
  quit(status = 20, save = "no")
}
if (length(resolver_failed) > 0) {
  cat("\nRESULT: one or more resolvers failed. Exit code 30.\n")
  quit(status = 30, save = "no")
}
if (changes$n_changed > 0) {
  cat("\nRESULT: changes present -- needs human review / PR. Exit code 10.\n")
  quit(status = 10, save = "no")
}

cat("\nRESULT: OK, no changes. Exit code 0.\n")
quit(status = 0, save = "no")
