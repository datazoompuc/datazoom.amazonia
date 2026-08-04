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
# SCHEMA (5-tier field coalescing)
# ---------------------------------
# A row is either a survey DEFAULT (dataset is NA -- values shared by every
# dataset in that survey) or a DATASET row (dataset is set), which may
# itself be overridden for a specific geo_level and/or year. Resolving a
# single field walks from most to least specific and stops at the first
# non-NA value:
#
#   1. (survey, dataset, geo_level, year)  -- fully specific override
#   2. (survey, dataset, geo_level, NA)    -- override by geographic level
#   3. (survey, dataset, NA, year)         -- override by year
#   4. (survey, dataset, NA, NA)           -- the dataset's own row
#   5. (survey, NA, NA, NA)                -- the survey's default row
#
# A cell left empty means "inherit from the next tier down" -- it does NOT
# mean "no value". This is what lets e.g. DEGRAD's ten yearly rows carry
# only the one thing that actually changes per year (archive_file) instead
# of repeating the shared URL and available_time ten times over.
#
# Everything here is internal. The only thing every other file in the
# package should call is datasets_link() (R/download.R), which keeps its
# pre-existing signature and return shape untouched.

# Namespace-local cache: populated at most once per session (see link_table()).
.dz_manifest_cache <- new.env(parent = emptyenv())

MANIFEST_REL <- file.path("extdata", "manifest", "v1", "datasets_link.csv")

# The 4 key columns (never inherited) plus the 10 value columns (each
# resolved independently via the 5-tier walk above).
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
  dataset <- survey <- n <- NULL

  stopifnot(
    is.data.frame(x),
    all(MANIFEST_CORE_COLS %in% names(x)),
    nrow(x) >= 150,
    !any(is.na(x$survey))
  )

  # at most one survey-default row (dataset == NA) per survey
  n_defaults <- x %>%
    dplyr::filter(is.na(dataset)) %>%
    dplyr::count(survey) %>%
    dplyr::pull(n)
  stopifnot(all(n_defaults <= 1))

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

#' The 5 candidate row-sets for (source, dataset, geo_level, year), ordered
#' from most to least specific (see the tier table in the file header).
#' Each element is a 0-or-1-row data frame; dataset_field() takes the first
#' non-NA value of the requested field walking down the list.
#' @noRd
dataset_rows <- function(source, dataset, geo_level = NULL, year = NULL) {
  survey <- NULL

  tbl <- link_table()
  geo_level <- if (is.null(geo_level)) NA_character_ else as.character(geo_level)
  year <- if (is.null(year)) NA_character_ else as.character(year)

  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]
  default_row <- tbl[tbl$survey == source & is.na(tbl$dataset), ]

  list(
    ds[ds$geo_level %in% geo_level & ds$year %in% year, ],
    ds[ds$geo_level %in% geo_level & is.na(ds$year), ],
    ds[is.na(ds$geo_level) & ds$year %in% year, ],
    ds[is.na(ds$geo_level) & is.na(ds$year), ],
    default_row
  )
}

#' Resolve a single field by walking the 5 tiers from dataset_rows() and
#' returning the first non-NA value. This is the one place the "empty cell
#' = inherit from the tier below" rule is implemented.
#' @noRd
dataset_field <- function(source, dataset, field, geo_level = NULL, year = NULL) {
  tiers <- dataset_rows(source, dataset, geo_level, year)

  for (tier in tiers) {
    if (nrow(tier) == 0) next
    val <- tier[[field]][1]
    if (!is.na(val)) return(val)
  }

  NA_character_
}

#' @noRd
dataset_url <- function(source, dataset, geo_level = NULL, year = NULL) {
  dataset_field(source, dataset, "url", geo_level, year)
}

#' One row per (survey, dataset) -- the survey-default-only base rows,
#' overrides excluded -- with every value column coalesced against the
#' survey's default row. This is what datasets_link() and check_params()'s
#' "list every supported dataset" queries consume; it never sees geo_level/
#' year overrides, which stay reachable only through dataset_field().
#' @noRd
effective_table <- function() {
  survey <- dataset <- geo_level <- year <- NULL

  tbl <- link_table()

  ds_base <- tbl %>%
    dplyr::filter(!is.na(dataset), is.na(geo_level), is.na(year))

  defaults <- tbl %>%
    dplyr::filter(is.na(dataset)) %>%
    dplyr::select(survey, dplyr::all_of(MANIFEST_VALUE_COLS)) %>%
    dplyr::rename_with(~ paste0(., "__default"), dplyr::all_of(MANIFEST_VALUE_COLS))

  out <- ds_base %>%
    dplyr::left_join(defaults, by = "survey")

  for (col in MANIFEST_VALUE_COLS) {
    out[[col]] <- dplyr::coalesce(out[[col]], out[[paste0(col, "__default")]])
  }

  out %>% dplyr::select(dplyr::all_of(MANIFEST_CORE_COLS))
}
