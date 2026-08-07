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
#      (identified by the manifest's `resolver` column).
#   4. Runs the full validation gate from manifest_validate.R against the
#      merged candidate.
#   5. Classifies every changed row as Tier A (url-only, version tokens
#      unchanged -- mechanically safe) or Tier B (anything that could change
#      the SHAPE of the downloaded data -- needs human review).
#   6. Writes a JSON report and a plain-text summary to OUT_DIR, and always
#      writes the candidate manifest to OUT_DIR too. OUT_DIR is RUNNER_TEMP
#      when set (GitHub Actions' job-scoped temp dir, which survives across
#      steps/processes within the same job) or R's own tempdir() otherwise
#      (local/interactive runs) -- plain tempdir() would NOT work for the CI
#      case, because R deletes its own session tempdir() when the Rscript
#      process exits, and each workflow step is a separate process. Only when
#      NOT running with --dry-run AND the result is Tier A / no-change does
#      it also overwrite the committed manifest in place (inst/extdata/
#      manifest/v1/datasets_link.csv) -- Tier B candidates are deliberately
#      left in OUT_DIR only; it is the CALLING WORKFLOW's job to stage that
#      candidate onto the committed path itself before opening a review PR
#      (see the "Stage Tier B candidate" step in update-manifest.yaml). Both
#      resolved OUT_DIR paths are exposed to the workflow as candidate_path /
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
#   0  candidate validated; no changes, or only Tier A changes (safe to commit)
#   10 candidate validated; contains Tier B changes (needs human review/PR)
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
    # dataset uses %in%, not ==, for the same reason geo_level/year already
    # do: r$dataset can be NA (a resolver updating a survey-DEFAULT row --
    # e.g. resolve_prodes()/resolve_seeg()/resolve_ips() writing shared
    # version/url/docs_url once instead of once per dataset). `==` against
    # NA yields NA for every row, which which() drops -- silently treating
    # every update to a default row as a brand-new row and duplicating it.
    match_idx <- which(
      candidate$survey == r$survey & candidate$dataset %in% r$dataset &
        candidate$geo_level %in% r$geo_level & candidate$year %in% r$year
    )
    cols_present <- intersect(names(r), MANIFEST_ALL_COLS)

    if (length(match_idx) == 0) {
      # A brand-new row (e.g. a new ANEEL year). Must carry every core column.
      new_row <- as.list(rep(NA_character_, length(MANIFEST_ALL_COLS)))
      names(new_row) <- MANIFEST_ALL_COLS
      new_row[cols_present] <- r[cols_present]
      new_row$resolver <- src
      candidate <- dplyr::bind_rows(candidate, tibble::as_tibble(new_row))
    } else {
      candidate[match_idx[1], cols_present] <- r[1, cols_present]
    }
  }

  cat("OK (", nrow(result), "row(s) touched)\n")
  resolver_ok <- c(resolver_ok, src)
}

# ---- Validate ----------------------------------------------------------------

errors <- c(
  validate_schema(candidate),
  validate_row_count(old, candidate),
  validate_no_deletions(old, candidate),
  validate_unique_base_row(candidate),
  validate_placeholders(old, candidate)
)

tiering <- classify_candidate(old, candidate)

if (length(errors) == 0 && tiering$n_changed > 0) {
  errors <- c(errors, validate_http(candidate, tiering$changed_keys))
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
  for (i in seq_len(nrow(candidate))) {
    row <- candidate[i, ]
    if (is_http_exempt(row)) next
    probe <- probe_url(row$url)
    if (!isTRUE(probe$ok)) {
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
  n_changed = tiering$n_changed,
  changed_keys = tiering$changed_keys,
  tier = tiering$overall,
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
cat("Changed keys:", tiering$n_changed, "\n")
cat("Tier:", tiering$overall, "\n")
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
    sprintf("- Changed keys: %d", tiering$n_changed),
    sprintf("- Tier: %s", tiering$overall),
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

if (!dry_run && length(errors) == 0 && tiering$overall %in% c("none", "A")) {
  readr::write_csv(candidate, MANIFEST_PATH, na = "")
  cat("Tier A / no-change candidate written to the committed manifest path:\n ", MANIFEST_PATH, "\n")
} else if (dry_run) {
  cat("--dry-run: the committed manifest was NOT modified.\n")
}

if (length(errors) > 0) {
  cat("\nRESULT: validation FAILED. Exit code 20.\n")
  quit(status = 20, save = "no")
}
if (length(resolver_failed) > 0) {
  cat("\nRESULT: one or more resolvers failed. Exit code 30.\n")
  quit(status = 30, save = "no")
}
if (identical(tiering$overall, "B")) {
  cat("\nRESULT: Tier B changes present -- needs human review / PR. Exit code 10.\n")
  quit(status = 10, save = "no")
}

cat("\nRESULT: OK. Exit code 0.\n")
quit(status = 0, save = "no")
