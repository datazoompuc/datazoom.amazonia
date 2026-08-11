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
# SCHEMA (self-sufficient rows, exact-key lookup)
# ------------------------------------------------
# Every row is either a DATASET's base row (geo_level and year both NA) or
# one of its geo_level/year overrides. There is no survey-default row and
# no field-by-field inheritance: every row carries every value it needs,
# even when that repeats a value also present on the dataset's base row
# (e.g. DEGRAD's ten yearly rows each carry their own archive_file AND
# their own copy of url/available_time). A blank cell means the field
# genuinely has no value for that row -- never "look elsewhere".
#
# Resolving (source, dataset, field, geo_level, year) is a single lookup:
#   - if the dataset has any geo_level-keyed rows, match geo_level exactly
#     (falling back to NA/no-geo_level only when the caller passed none);
#   - else if the dataset has any year-keyed rows, match year exactly;
#   - else match the dataset's one base row.
# No dataset mixes geo_level and year overrides today; see dataset_field().
#
# This replaced an earlier 5-tier field-coalescing model (a blank cell
# meant "inherit from the next tier down") -- see NEWS.md and
# data-raw/denormalize_manifest.R for why and how it was migrated away
# from: inheritance was the direct cause of several live bugs (a resolver
# and its own anti-duplication test fighting over whether an override row
# was allowed to state its own version; a base row left silently stale
# because only its overrides were ever updated; bind_rows() NA-padding
# being misread as "clear this field").
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
#' @noRd
dataset_field <- function(source, dataset, field, geo_level = NULL, year = NULL) {
  tbl <- link_table()
  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]

  g <- if (is.null(geo_level)) NA_character_ else as.character(geo_level)
  y <- if (is.null(year)) NA_character_ else as.character(year)

  if (!any(!is.na(ds$geo_level))) g <- NA_character_
  if (!any(!is.na(ds$year))) y <- NA_character_

  hit <- ds[ds$geo_level %in% g & ds$year %in% y, ]
  if (nrow(hit) == 0) return(NA_character_)

  hit[[field]][1]
}

#' @noRd
dataset_url <- function(source, dataset, geo_level = NULL, year = NULL) {
  dataset_field(source, dataset, "url", geo_level, year)
}

#' One row per (survey, dataset) -- each dataset's base row, geo_level/year
#' overrides excluded. This is what datasets_link() and check_params()'s
#' "list every supported dataset" queries consume; overrides stay reachable
#' only through dataset_field().
#' @noRd
effective_table <- function() {
  geo_level <- year <- NULL

  tbl <- link_table()

  tbl %>%
    dplyr::filter(is.na(geo_level), is.na(year)) %>%
    dplyr::select(dplyr::all_of(MANIFEST_CORE_COLS))
}
