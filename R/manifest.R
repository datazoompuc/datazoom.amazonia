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
# Everything here is internal. The only thing every other file in the
# package should call is datasets_link() (R/download.R), which keeps its
# pre-existing signature and return shape untouched.

# Namespace-local cache: populated at most once per session (see link_table()).
.dz_manifest_cache <- new.env(parent = emptyenv())

MANIFEST_REL <- file.path("extdata", "manifest", "v1", "datasets_link.csv")

MANIFEST_CORE_COLS <- c(
  "survey", "dataset", "geo_level", "year", "sidra_code",
  "available_time", "available_geo", "link",
  "archive_file", "sheet", "collection", "layer_name", "resolver"
)

# The 6 columns datasets_link() has always returned, in their original order.
# This is the contract that keeps every existing call site untouched.
DATASETS_LINK_COLS <- c(
  "survey", "dataset", "sidra_code",
  "available_time", "available_geo", "link"
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

#' Most-specific-match lookup used by external_download().
#'
#' Resolution order: (survey, dataset, geo_level, year) ->
#' (survey, dataset, geo_level, NA) -> (survey, dataset, NA, year) ->
#' (survey, dataset, NA, NA). Falls back to the base row (both override keys
#' NA) whenever geo_level/year are not supplied or have no specific override.
#' @noRd
dataset_row <- function(source, dataset, geo_level = NULL, year = NULL) {
  survey <- NULL

  tbl <- link_table() %>%
    dplyr::filter(survey == source, dataset == !!dataset)

  if (nrow(tbl) == 0) {
    return(tbl)
  }

  geo_level <- if (is.null(geo_level)) NA_character_ else as.character(geo_level)
  year <- if (is.null(year)) NA_character_ else as.character(year)

  candidates <- list(
    tbl[tbl$geo_level %in% geo_level & tbl$year %in% year, ],
    tbl[tbl$geo_level %in% geo_level & is.na(tbl$year), ],
    tbl[is.na(tbl$geo_level) & tbl$year %in% year, ],
    tbl[is.na(tbl$geo_level) & is.na(tbl$year), ]
  )

  for (cand in candidates) {
    if (nrow(cand) > 0) {
      return(cand[1, ])
    }
  }

  tbl[0, ]
}

#' @noRd
dataset_field <- function(source, dataset, field, geo_level = NULL, year = NULL) {
  row <- dataset_row(source, dataset, geo_level, year)
  if (nrow(row) == 0) {
    return(NA_character_)
  }
  row[[field]]
}

#' @noRd
dataset_url <- function(source, dataset, geo_level = NULL, year = NULL) {
  dataset_field(source, dataset, "link", geo_level, year)
}
