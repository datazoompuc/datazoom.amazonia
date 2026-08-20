# Runtime resolution of the dataset URL manifest.
#
# The manifest is a CSV that used to be a hardcoded tibble::tribble() inside
# datasets_link() (see NEWS/git history). It now lives at
# inst/extdata/manifest/v1/datasets_link.csv, which is BOTH the copy shipped
# inside the installed package AND the file a scheduled GitHub Action keeps
# fresh at the same relative path on GitHub. At runtime we try the remote
# copy first (so users get URL fixes without waiting for a CRAN release) and
# fall back silently to the packaged snapshot when the network is
# unavailable, slow, or the remote file is missing/malformed.
#
# SCHEMA (keyed vs. unkeyed groups, exact-key lookup, no base rows)
# ------------------------------------------------------------------
# A (survey, dataset) group is either:
#   - UNKEYED: exactly one row, geo_level and year both NA -- the dataset
#     has no overrides, that one row IS the dataset.
#   - KEYED: N>=1 rows, every one of them carrying a real geo_level (XOR a
#     real year, never both -- see validate_key_completeness() in
#     actions/scripts/manifest_validate.R). There is NO blank-key row for
#     a keyed dataset -- that "base row" existed in an earlier version of
#     this schema and was removed (see NEWS.md): it was provably capable of
#     silently drifting out of sync with its own overrides (a live example:
#     mapbiomas_mining's base row stayed pinned to Collection 8 for weeks
#     after both its real override rows had already moved to Collection 9),
#     which is exactly the "two rows disagree because only one was ever
#     asked" bug class this whole self-sufficient-rows design exists to
#     rule out in the first place. A row still carries every value it needs
#     explicitly, even when that repeats a value also present on a sibling
#     row (e.g. DEGRAD's ten yearly rows each carry their own archive_file
#     AND their own copy of url/available_time). A blank cell means the
#     field genuinely has no value for that row -- never "look elsewhere".
#
# There are now TWO ways to read a keyed dataset's data, deliberately kept
# separate so nothing has to guess which row "wins":
#   - dataset_field()/dataset_url(): exact-key lookup, unchanged in
#     behavior for every call that passes a real geo_level/year. NEW: if
#     the dataset is keyed and the caller passes neither, this now
#     stop()s -- a caller bug to fix at the call site, not a data
#     condition to paper over with a row nothing keeps in sync (see
#     dataset_meta() below for the one legitimate way to read a keyed
#     dataset without a key).
#   - dataset_meta(): the ONE legal cross-row read, for fields that are
#     genuinely dataset-wide (not spread across per-row values like url or
#     sheet). Returns the single value every row of the dataset agrees on;
#     ERRORS if they disagree -- it cannot silently pick a winner, which is
#     exactly why it can't reproduce the base-row bug either. Only call
#     this for a field you have good reason to believe truly doesn't vary
#     within the dataset; if it might, key your read instead.
#
# This replaced an earlier 5-tier field-coalescing model (a blank cell
# meant "inherit from the next tier down") -- see NEWS.md and
# data-raw/denormalize_manifest.R for why and how it was migrated away
# from: inheritance was the direct cause of several live bugs (a resolver
# and its own anti-duplication test fighting over whether an override row
# was allowed to state its own version; a base row left silently stale
# because only its overrides were ever updated; bind_rows() NA-padding
# being misread as "clear this field"). The base-row-deletion pass above
# is the second half of closing that same class of bug: the base row
# itself, kept for backward-reading convenience, turned out to be exactly
# as capable of silently drifting stale as the tiers it replaced.
#
# Everything here is internal. The only thing every other file in the
# package should call is datasets_link() (R/download.R), which keeps its
# pre-existing signature and return shape untouched.

# Namespace-local cache: populated at most once per session (see link_table()).
.dz_manifest_cache <- new.env(parent = emptyenv())

MANIFEST_REL <- file.path("extdata", "manifest", "v1", "datasets_link.csv")

# The 4 key columns plus the 10 value columns each row carries explicitly
# (see the schema note above).
MANIFEST_KEY_COLS <- c("survey", "dataset", "geo_level", "year")

MANIFEST_VALUE_COLS <- c(
  "sidra_code", "url", "docs_url",
  "available_time", "available_geo",
  "archive_file", "sheet", "layer_name", "version",
  "resolver"
)

MANIFEST_CORE_COLS <- c(MANIFEST_KEY_COLS, MANIFEST_VALUE_COLS)

# The subset of MANIFEST_VALUE_COLS that dataset_meta() is allowed to read:
# fields expected to be genuinely dataset-wide rather than spread across
# per-row values. Deliberately EXCLUDES url, archive_file, and sheet --
# those vary by row by design (that's what makes a dataset keyed in the
# first place). Also excludes docs_url and version, even though an earlier
# design took those to be dataset-level too: several MapBiomas datasets
# deliberately pin one geo_level to an older Dataverse collection than its
# siblings (see actions/scrapers/resolve_mapbiomas.R -- mapbiomas_mining's
# indigenous_land row is intentionally one collection behind
# municipality's), so docs_url/version genuinely DO vary per row for a
# keyed dataset now, structurally and permanently, not as a bug to fix.
# Read them with dataset_field() and a real key instead (see R/mapbiomas.R).
MANIFEST_META_COLS <- c("sidra_code", "available_time", "available_geo", "layer_name", "resolver")
# One further, narrower exception lives in actions/scripts/manifest_validate.R's
# KNOWN_META_DISAGREEMENTS: mapbiomas_transition's available_time also
# varies per row (its municipality file is older than its siblings'), but
# available_time stays in this list because it's correctly meta for every
# other dataset that reads it (aneel.R, prodes.R) -- nothing calls
# dataset_meta() for that specific (dataset, field) pair today.

# The 6 columns datasets_link() has always returned, in their original
# order, with "link" now named "url" (see NEWS: no external contract ever
# depended on that literal name -- nothing outside this file greps `$link`
# off its result).
DATASETS_LINK_COLS <- c(
  "survey", "dataset", "sidra_code",
  "available_time", "available_geo", "url"
)

#' URL of the remote manifest.
#'
#' Exposed as an option so it can be pointed at a local file:// path in tests
#' and in the CI smoke test (build_manifest.R --dry-run validates a candidate
#' manifest before it is ever committed).
#' @noRd
manifest_url <- function() {
  getOption(
    "datazoom.amazonia.manifest_url",
    paste0(
      "https://raw.githubusercontent.com/datazoompuc/datazoom.amazonia/",
      "master/inst/extdata/manifest/v1/datasets_link.csv"
    )
  )
}

#' Parse a manifest CSV (local path or already-downloaded tempfile).
#' @noRd
read_manifest_file <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

#' Minimal structural validation. Not the full CI validation gate (that lives
#' in actions/scripts/build_manifest.R) -- just enough to refuse a corrupt or
#' truncated file before it gets cached for the rest of the session.
#' @noRd
validate_manifest <- function(x) {
  stopifnot(
    is.data.frame(x),
    all(MANIFEST_CORE_COLS %in% names(x)),
    nrow(x) >= 150,
    !any(is.na(x$survey)),
    # every row is a real dataset row -- no survey-default (dataset = NA)
    # row exists under the self-sufficient-rows schema
    !any(is.na(x$dataset))
  )

  invisible(TRUE)
}

#' Try to fetch the remote manifest. Returns NULL (never errors) on any
#' failure -- link_table() interprets NULL as "fall back to the snapshot".
#' @noRd
fetch_manifest <- function(timeout_s = 8) {
  old_options <- list(
    timeout = getOption("timeout"),
    download.file.method = getOption("download.file.method"),
    download.file.extra = getOption("download.file.extra")
  )
  on.exit(options(old_options), add = TRUE)

  options(timeout = timeout_s, download.file.extra = NULL)
  if (isTRUE(capabilities("libcurl"))) {
    options(download.file.method = "libcurl")
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  status <- tryCatch(
    suppressWarnings(
      utils::download.file(manifest_url(), destfile = tmp, quiet = TRUE, mode = "wb")
    ),
    error = function(e) 1L
  )

  if (!identical(as.integer(status), 0L)) {
    return(NULL)
  }
  if (!file.exists(tmp) || file.size(tmp) < 1000) {
    # Too small to be a real manifest -- most likely a 404 HTML page.
    return(NULL)
  }

  out <- tryCatch(read_manifest_file(tmp), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(out)) {
    return(NULL)
  }

  ok <- tryCatch({
    validate_manifest(out)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    return(NULL)
  }

  out
}

#' The manifest table for this session, fetched (and cached) at most once.
#'
#' Tries the remote manifest first, unless disabled via
#' `options(datazoom.amazonia.use_remote_manifest = FALSE)` (always the case
#' in tests -- see tests/testthat/setup.R). On any failure it falls back
#' silently to the snapshot shipped inside the package.
#' @noRd
link_table <- function() {
  if (!is.null(.dz_manifest_cache$tbl)) {
    return(.dz_manifest_cache$tbl)
  }

  out <- NULL
  if (!isFALSE(getOption("datazoom.amazonia.use_remote_manifest", TRUE))) {
    out <- fetch_manifest()
  }

  src <- "remote"
  if (is.null(out)) {
    src <- "snapshot"
    out <- read_manifest_file(
      system.file(MANIFEST_REL, package = "datazoom.amazonia")
    )
  }

  if (isTRUE(getOption("datazoom.amazonia.verbose_manifest", FALSE))) {
    message("datazoom.amazonia: using ", src, " manifest (", nrow(out), " rows).")
  }

  .dz_manifest_cache$tbl <- out
  .dz_manifest_cache$source <- src
  out
}

#' Drop the cached manifest so the next call to link_table() re-fetches it.
#' Internal only -- used by tests and by manifest_info().
#' @noRd
clear_manifest_cache <- function() {
  if (exists("tbl", envir = .dz_manifest_cache, inherits = FALSE)) {
    rm(list = ls(envir = .dz_manifest_cache), envir = .dz_manifest_cache)
  }
  invisible(NULL)
}

#' Diagnostic helper: which manifest is currently cached, and from where.
#' Not exported; enable `options(datazoom.amazonia.verbose_manifest = TRUE)`
#' for the same information printed as a message at fetch time.
#' @noRd
manifest_info <- function() {
  list(
    source = .dz_manifest_cache$source %||% NA_character_,
    url = manifest_url(),
    n = if (is.null(.dz_manifest_cache$tbl)) NA_integer_ else nrow(.dz_manifest_cache$tbl)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Parse a manifest "available_time" string into an integer vector of years.
#'
#' Grammar: comma-separated tokens, each either a single year ("2023") or a
#' range ("2007-2016"). This is the ONE parser for that grammar -- it
#' replaces three separate ad-hoc implementations that used to live in
#' check_params.R (comma+hyphen aware), epe.R and aneel.R (hyphen-only, via
#' eval(parse(text = ...)), which silently mishandled comma lists).
#' @noRd
parse_years <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(integer(0))
  }

  tokens <- trimws(strsplit(x, ",")[[1]])

  years <- lapply(tokens, function(tok) {
    if (grepl("-", tok, fixed = TRUE)) {
      bounds <- as.integer(trimws(strsplit(tok, "-", fixed = TRUE)[[1]]))
      seq.int(bounds[1], bounds[2])
    } else {
      as.integer(tok)
    }
  })

  unlist(years, use.names = FALSE)
}

#' Resolve a single field for (source, dataset, geo_level, year) with a
#' single exact-key lookup -- no fallthrough between rows. Every row is
#' self-sufficient (see the file header), so once the right row is found,
#' its value for `field` (possibly NA, meaning "genuinely no value") is the
#' answer.
#'
#' Callers (download.R, epe.R, mapbiomas.R, ...) pass geo_level/year on
#' every call regardless of whether the dataset actually varies by them.
#' geo_level is matched exactly only for datasets that HAVE geo_level rows;
#' same for year. A dataset never has both kinds of override (checked by
#' validate_key_completeness() in actions/scripts/manifest_validate.R), so
#' matching them independently is unambiguous.
#'
#' A KEYED dataset called with neither geo_level nor year now stop()s --
#' there is no base row left to fall back to (see the file header). Use
#' dataset_meta() instead if what you actually want is a value that's the
#' same across the whole dataset.
#' @noRd
dataset_field <- function(source, dataset, field, geo_level = NULL, year = NULL) {
  tbl <- link_table()
  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]

  has_geo <- any(!is.na(ds$geo_level))
  has_year <- any(!is.na(ds$year))

  if (has_geo && is.null(geo_level)) {
    stop(
      "dataset_field(): '", source, "/", dataset, "' is keyed by geo_level -- ",
      "call with a real geo_level, or use dataset_meta() if '", field,
      "' is genuinely the same across every geo_level."
    )
  }
  if (has_year && is.null(year)) {
    stop(
      "dataset_field(): '", source, "/", dataset, "' is keyed by year -- ",
      "call with a real year, or use dataset_meta() if '", field,
      "' is genuinely the same across every year."
    )
  }

  g <- if (is.null(geo_level)) NA_character_ else as.character(geo_level)
  y <- if (is.null(year)) NA_character_ else as.character(year)

  # A caller may pass geo_level/year unconditionally even for a dataset
  # that doesn't actually vary by it (see the roxygen note above) -- ignore
  # it in that case so the match still lands on the row's own blank key,
  # same as before this function grew the stop() guard above.
  if (!has_geo) g <- NA_character_
  if (!has_year) y <- NA_character_

  hit <- ds[ds$geo_level %in% g & ds$year %in% y, ]
  if (nrow(hit) == 0) return(NA_character_)

  hit[[field]][1]
}

#' @noRd
dataset_url <- function(source, dataset, geo_level = NULL, year = NULL) {
  dataset_field(source, dataset, "url", geo_level, year)
}

#' Collapse a vector of values (as seen across one dataset's rows, for one
#' column) to a single agreed value. Ignores NA (a row with no opinion
#' doesn't count as disagreement). Returns list(value, agree):
#'   - zero non-NA values -> value = NA, agree = TRUE (genuinely no value
#'     anywhere, same as today's "blank cell" meaning)
#'   - exactly one distinct non-NA value -> that value, agree = TRUE
#'   - more than one distinct non-NA value -> value = NA, agree = FALSE
#' Shared by dataset_meta() (propagates disagree as a stop()) and
#' effective_table() (silently collapses disagree to NA -- building the
#' whole display table can't be allowed to crash over one dataset with
#' expected per-row divergence; see MANIFEST_META_COLS's comment for why
#' that divergence is real and permanent for some MapBiomas datasets).
#' @noRd
agreed_value <- function(x) {
  vals <- unique(x[!is.na(x)])
  if (length(vals) == 0) return(list(value = NA_character_, agree = TRUE))
  if (length(vals) == 1) return(list(value = vals[[1]], agree = TRUE))
  list(value = NA_character_, agree = FALSE)
}

#' The one legal cross-row read for a keyed dataset: a field from
#' MANIFEST_META_COLS that's expected to be the same across every row of
#' the dataset. Returns that single agreed value (or NA if genuinely blank
#' everywhere); stop()s if the dataset's rows actually disagree -- it
#' cannot silently pick a winner, which is exactly the bug class the old
#' base-row design was capable of (a stale base row nothing kept in sync
#' with its own overrides). If you hit this error, the field you're asking
#' about is not safe to read without a key for this specific dataset --
#' key your read with dataset_field() instead.
#' @noRd
dataset_meta <- function(source, dataset, field) {
  if (!field %in% MANIFEST_META_COLS) {
    stop(
      "dataset_meta(): '", field, "' is not in MANIFEST_META_COLS -- it is ",
      "expected to vary per row for some dataset. Use dataset_field() with ",
      "a real key instead."
    )
  }

  tbl <- link_table()
  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]
  if (nrow(ds) == 0) return(NA_character_)

  res <- agreed_value(ds[[field]])
  if (!res$agree) {
    disagreeing <- paste(unique(ds[[field]][!is.na(ds[[field]])]), collapse = "' vs '")
    stop(
      "dataset_meta(): '", source, "/", dataset, "' rows disagree on '",
      field, "' ('", disagreeing, "') -- this field is not safe to read ",
      "without a key for this dataset. Use dataset_field() with a real ",
      "geo_level/year instead."
    )
  }

  res$value
}

#' One synthesized display row per (survey, dataset) -- the group's key
#' columns as-is, every value column collapsed to "the single value every
#' row of the group agrees on, else NA" (see agreed_value()). This is what
#' datasets_link() and check_params()'s "list every supported dataset"
#' queries consume; a keyed dataset's individual override rows stay
#' reachable only through dataset_field()/dataset_meta().
#'
#' A collapsed cell reading NA here does not mean "no data" -- for a keyed
#' dataset it can mean "this genuinely differs by geo_level/year, ask for
#' a specific one" (e.g. mapbiomas_cover's url, which points at a
#' different Dataverse file for indigenous_land than for everything else).
#' @noRd
effective_table <- function() {
  tbl <- link_table()

  groups <- unique(tbl[!is.na(tbl$dataset), c("survey", "dataset")])

  out <- lapply(seq_len(nrow(groups)), function(i) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- tbl[tbl$survey == s & !is.na(tbl$dataset) & tbl$dataset == d, ]

    vals <- lapply(MANIFEST_VALUE_COLS, function(col) agreed_value(rows[[col]])$value)
    names(vals) <- MANIFEST_VALUE_COLS

    tibble::as_tibble(c(
      list(survey = s, dataset = d, geo_level = NA_character_, year = NA_character_),
      vals
    ))
  })

  dplyr::bind_rows(out) %>%
    dplyr::select(dplyr::all_of(MANIFEST_CORE_COLS))
}
