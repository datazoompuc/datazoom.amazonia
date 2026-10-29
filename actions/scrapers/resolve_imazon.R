# actions/scrapers/resolve_imazon.R
#
# imazon/imazon_shp has no upstream to follow, so this resolver does not look for
# a newer file: it checks that the one file load_imazon() reads is still there,
# is still the file it was, and still has the shape R/imazon.R relies on.
#
# WHAT THE FILE IS (checked 2026-10-29). `url` is a Google Drive link to
# "imazon.rds": a gzip-compressed R object, 20,342,788 bytes, created and last
# modified 2022-06-20, shared to anyone with the link, owned by the Google
# account "luizguilherme.lopesm" -- the address of Luiz Guilherme Lopes Moussatche,
# who wrote load_imazon() the same day (git history: commits 45729fb and 2997055,
# June 2022, "creating function to scrape the imazon dataset from DataZoom's google
# drive"; the owner field is Drive's, the identification is an inference from the
# address and the date). The first version of that function read
# C:\...\DataZoom\IMAZON\Fronteiras_Amazonia\Fronteiras_Amazonia_2020.shp from
# his own machine; the .rds is that shapefile saved as an R object and uploaded.
# So it is a Datazoom conversion of a file Imazon gave Datazoom, not a file
# Imazon publishes. It is a 772-row `sf` (the 772 municipalities of the 2020
# Legal Amazon), five attribute columns -- CD_GEOCMU, NM_MUNICIP, Area, UF and
# Front_2020 -- plus the geometry, in SIRGAS 2000.
#
# NO OFFICIAL COPY WAS FOUND, so unlike resolve_ips.R (whose personal Drive
# upload had the publisher's own copy to be replaced by) there is nothing to
# switch to. imazon.org.br and imazongeo.org.br list SAD, Simex, FloreSer,
# PrevisIA and "Ameaca e Pressao em Areas Protegidas" (a ranking of protected
# areas, not of municipalities, no 0-3 scale, no shapefile download); searches
# for "Fronteiras_Amazonia_2020" and "Front_2020" return nothing. Note what the
# names say: the file is "Fronteiras da Amazonia" (frontiers), its column is
# Front_2020 -- "deforestation pressure" is the package's reading of it
# (R/imazon.R, the vignette), and the vignette's "periodic updates as Imazon
# releases new assessments" has no source I could find. If Imazon publishes a
# newer vintage, nothing here will find it; what this resolver does catch is the
# Drive file being replaced (below).
#
# WHAT IS CHECKED, EVERY RUN, through the same call load_imazon() makes
# (googledrive::drive_download() on the manifest's url, no sign-in):
#   1. Drive still knows the file: not trashed, not unshared, md5 on record.
#   2. The download is intact: its md5 is the one Drive reports.
#   3. It reads: readRDS() gives an `sf` object (imazon_check_object()).
#   4. It has what R/imazon.R reads: CD_GEOCMU (numeric once coerced), NM_MUNICIP,
#      Area, UF, and the column R/imazon.R renames, Front_2020 -- with values 0 to
#      3 and no NA. A file that carries a different Front_<year>, or one in
#      addition to Front_2020, is a new vintage: R/imazon.R's literal rename
#      would break (or, for an additional one, silently leave it unrenamed), so
#      it is a failure, not a value to adopt -- the loader has to become
#      version-aware first.
# It emits `available_time` (the year in the Front_<year> column, which the manifest
# used to state by hand) and `version` = the file's md5. `url` is echoed (it is the
# one just verified); `docs_url` stays empty, there is no official page to point at.
# `version` is what makes a replaced file visible: if the owner uploads another
# file over this one, the md5 changes and the weekly run opens a PR for it.
#
# Needs googledrive (a Suggests of the package, installed by the workflow) and, for
# the structure check, nothing but base R: an sf object is recognised by its
# classes, so sf is not loaded.
#
# The risk this does NOT remove: the file lives in one person's Google account.
# If that account is closed, or the sharing changed, this fails (loudly) and so
# does load_imazon() for users; moving the file somewhere Datazoom owns is
# the real fix and is not something a resolver can do.

# The raw columns R/imazon.R reads (before its rename), and the vintage column it
# renames. A copy: tests/testthat/test-imazon-resolver.R fails if R/imazon.R and
# these ever differ.
IMAZON_COLUMNS <- c("CD_GEOCMU", "NM_MUNICIP", "Area", "UF")
IMAZON_LOADER_FRONT <- "Front_2020"
IMAZON_FRONT_PATTERN <- "^Front_[0-9]{4}$"
IMAZON_PRESSURE_LEVELS <- 0:3

# A value from Drive's metadata, or "?" when Drive did not send it (a note in the log, nothing more).
imazon_or <- function(x) if (is.null(x) || length(x) == 0L) "?" else as.character(x)

# The file id in a Google Drive link ("...uc?export=download&id=<id>"), NA if none.
imazon_drive_id <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !grepl("^https://(docs|drive)\\.google\\.com/", url)) {
    return(NA_character_)
  }
  hit <- regmatches(url, regexec("[?&]id=([A-Za-z0-9_-]+)", url))[[1]]
  if (length(hit) == 2L) hit[2] else NA_character_
}

# What is wrong with the object the .rds holds, as readable sentences (character(0)
# when nothing is), and the vintage year found. Pure: no network, no sf needed.
imazon_check_object <- function(x, loader_front = IMAZON_LOADER_FRONT) {
  problems <- character(0)
  if (!inherits(x, "sf")) {
    return(list(
      problem = sprintf("the .rds does not hold an sf object (it holds class %s)", paste(class(x), collapse = "/")),
      year = NA_integer_
    ))
  }
  geometry <- attr(x, "sf_column")
  if (is.null(geometry) || !geometry %in% names(x) || !inherits(x[[geometry]], "sfc")) {
    problems <- c(problems, "it has no geometry column")
  }
  if (nrow(x) == 0L) problems <- c(problems, "it has no rows")

  missing <- setdiff(c(IMAZON_COLUMNS, loader_front), names(x))
  if (length(missing) > 0) {
    problems <- c(problems, sprintf("columns that R/imazon.R reads are missing: %s", paste(missing, collapse = ", ")))
  }

  fronts <- grep(IMAZON_FRONT_PATTERN, names(x), value = TRUE)
  year <- NA_integer_
  if (length(fronts) == 0L) {
    problems <- c(problems, "it has no Front_<year> column")
  } else if (!identical(fronts, loader_front)) {
    problems <- c(problems, sprintf(
      paste(
        "it carries %s where R/imazon.R renames only %s -- a newer vintage; load_imazon() must become",
        "version-aware (it would break, or leave the new column unrenamed) before this file is adopted"
      ),
      paste(fronts, collapse = ", "), loader_front
    ))
  }
  if (loader_front %in% fronts) {
    year <- as.integer(sub("^Front_", "", loader_front))
    level <- suppressWarnings(as.numeric(as.character(x[[loader_front]])))
    if (anyNA(level) || !all(level %in% IMAZON_PRESSURE_LEVELS)) {
      problems <- c(problems, sprintf(
        "%s has values other than %s (or NA): %s", loader_front, paste(range(IMAZON_PRESSURE_LEVELS), collapse = " to "),
        paste(utils::head(unique(as.character(x[[loader_front]][is.na(level) | !(level %in% IMAZON_PRESSURE_LEVELS)])), 5), collapse = ", ")
      ))
    }
  }
  if ("CD_GEOCMU" %in% names(x) && anyNA(suppressWarnings(as.numeric(as.character(x[["CD_GEOCMU"]]))))) {
    problems <- c(problems, "CD_GEOCMU has values that are not numbers (R/imazon.R converts them with as.numeric())")
  }

  list(problem = problems, year = year)
}

resolve_imazon <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_imazon(): no manifest rows tagged resolver == 'imazon' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("resolve_imazon() needs the 'googledrive' package (a Suggests of the package; load_imazon() needs it too).")
  }

  verified <- list()
  for (url in unique(rows$url)) {
    id <- imazon_drive_id(url)
    if (is.na(id)) {
      stop("resolve_imazon(): the manifest url is not a Google Drive link with an id: ", url)
    }

    # No sign-in: the file is shared with anyone who has the link, which is how
    # load_imazon() reads it (and check_all_loads.R, which does the same).
    googledrive::drive_deauth()

    meta <- tryCatch(
      googledrive::drive_get(googledrive::as_id(id)),
      error = function(e) stop("resolve_imazon(): Drive does not give the metadata of file ", id, " -- deleted, no longer shared, or Drive is down: ", conditionMessage(e), call. = FALSE)
    )
    resource <- meta$drive_resource[[1]]
    if (isTRUE(resource$trashed)) {
      stop("resolve_imazon(): file ", id, " is in Drive's trash.")
    }
    drive_md5 <- resource$md5Checksum
    if (is.null(drive_md5) || !nzchar(drive_md5)) {
      stop("resolve_imazon(): Drive reports no md5 checksum for file ", id, " -- not a stored file any more?")
    }
    message(sprintf(
      "resolve_imazon(): Drive file '%s', %s bytes, modified %s, owner %s, md5 %s",
      imazon_or(resource$name), imazon_or(resource$size), imazon_or(resource$modifiedTime),
      imazon_or(resource$owners[[1]]$displayName), drive_md5
    ))

    # the call load_imazon() makes (R/download.R, download_method "googledrive")
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    tryCatch(
      googledrive::drive_download(url, path = tmp, overwrite = TRUE),
      error = function(e) stop("resolve_imazon(): downloading ", url, " failed: ", conditionMessage(e), call. = FALSE)
    )
    if (!file.exists(tmp) || file.size(tmp) == 0) {
      stop("resolve_imazon(): downloading ", url, " produced no file.")
    }
    downloaded_md5 <- unname(tools::md5sum(tmp))
    if (!identical(downloaded_md5, drive_md5)) {
      stop("resolve_imazon(): the downloaded file's md5 (", downloaded_md5, ") is not the one Drive reports (", drive_md5, ") -- a partial or altered download.")
    }

    x <- tryCatch(readRDS(tmp), error = function(e) stop("resolve_imazon(): the downloaded file is not readable by readRDS(): ", conditionMessage(e), call. = FALSE))
    check <- imazon_check_object(x)
    if (length(check$problem) > 0) {
      stop("resolve_imazon(): ", url, " no longer has the shape load_imazon() relies on -- ", paste(check$problem, collapse = "; "), ".")
    }
    verified[[url]] <- list(version = drive_md5, available_time = as.character(check$year))
  }

  # One output row per input row, echoing each row's own keys (the manifest rows are
  # self-sufficient, see resolve_ips.R).
  tibble::tibble(
    survey = rows$survey,
    dataset = rows$dataset,
    geo_level = rows$geo_level,
    year = rows$year,
    url = rows$url,
    available_time = vapply(rows$url, function(u) verified[[u]]$available_time, character(1), USE.NAMES = FALSE),
    version = vapply(rows$url, function(u) verified[[u]]$version, character(1), USE.NAMES = FALSE)
  )
}
