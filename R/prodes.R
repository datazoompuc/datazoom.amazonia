#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads data on deforestation in the Legal Amazon region.
#'
#' @param dataset A dataset name. Can be one of "deforestation", "residual_deforestation", "native_vegetation", "hydrography", "non_forest", or "clouds".
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#'    * Between 2007 - 2025 for dataset "deforestation". Deforestation for 2007 includes all cumulative deforestation up to 2007. For other years, deforestation is incremental
#'    * Between 2010 - 2025 for dataset "residual_deforestation"
#'    * Only 2025 for all other datasets
#' @inheritParams load_baci
#'
#' @return A \code{tibble} with the selected data if raw_data is \code{FALSE}, and a \code{SpatRaster} is \code{TRUE}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated deforestation data for 2025
#' deforestation <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   time_period = 2025,
#'   language = "eng"
#' )
#'
#' # download treated deforestation data for 2008 to 2025
#' deforestation_series <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   time_period = 2008:2025,
#'   language = "eng"
#' )
#'
#' # download treated residual deforestation data for 2020
#' residual <- load_prodes(
#'   dataset = "residual_deforestation",
#'   raw_data = FALSE,
#'   time_period = 2020,
#'   language = "eng"
#' )
#'
#' @importFrom rlang :=
#'
#' @export

load_prodes <- function(dataset = "deforestation", raw_data = FALSE,
                        time_period = 2025, language = "eng") {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package \"terra\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("units", quietly = TRUE)) {
    stop(
      "Package \"units\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ###########################
  ## Bind Global Variables ##
  ###########################

  . <- area_km2 <- km <- ID <- name_muni <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list(
    source = "prodes",
    dataset = dataset,
    raw_data = raw_data,
    time_period = time_period,
    language = language
  )

  # forcing years
  # native_vegetation/non_forest/hydrography/clouds only exist for a single
  # year -- read from the manifest's available_time instead of hardcoding it,
  # so a PRODES year rollover does not require a code change here too.

  if (!param$dataset %in% c("deforestation", "residual_deforestation")) {
    # single-year datasets -- available_time is dataset-wide here (all of
    # PRODES's datasets are unkeyed today), so this is the one legitimate
    # cross-row read: dataset_meta(), not dataset_field() with no key (see
    # R/manifest.R). Using dataset_meta() rather than relying on today's
    # unkeyed-ness keeps this call correct if PRODES ever adds a real
    # geo_level/year override to one of these datasets.
    param$time_period <- as.numeric(dataset_meta(param$source, param$dataset, "available_time"))
  }

  # check if dataset and time_period are supported

  check_params(param)

  ###################
  ## Download Data ##
  ###################

  ## Column Names come with numbers at the side - we need to clean those

  dat <- external_download(
    dataset = param$dataset,
    source = param$source
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # The full-resolution steps below (dat == code, aggregate()) are too big
  # for memory, so terra writes them to temp files -- possibly over 1 GB
  # each. Delete the ones THIS call created when it returns, and only
  # those: terra::tmpFiles(remove = TRUE), used before, also deleted the
  # files behind the user's own unrelated SpatRasters. gc() first, so the
  # SpatRasters made here are finalized and release their files (Windows
  # won't delete a file that is still open). The result is a data frame,
  # nothing in it points at these files.
  terra_tmp_before <- terra::tmpFiles()
  on.exit(
    {
      gc()
      unlink(setdiff(terra::tmpFiles(), terra_tmp_before))
    },
    add = TRUE
  )

  # raster has values for each dataset/year -- see prodes_raster_code() at
  # the bottom of this file for the legend and how it is kept honest

  raster_codes <- prodes_raster_code(param$dataset, param$time_period)

  # cheap check of the hardcoded codes against the legend (.qml) shipped in
  # the zip this user actually downloaded -- warns, never stops
  prodes_check_qml(dat, param$dataset, param$time_period)

  # name of the raster band/column produced by the downloaded file -- lives
  # in the manifest (layer_name) because it changes whenever the PRODES
  # filename itself changes (e.g. a new year in "prodes_amazonia_legal_2023").
  # dataset-wide, not per-row -- dataset_meta(), same reasoning as
  # available_time above.

  layer <- dataset_meta(param$source, param$dataset, "layer_name")

  message("Downloading map of Brazilian municipalities")

  munic <- external_download(
    source = "internal",
    dataset = "geo_municipalities"
  )

  # calculate area to join later

  munic_areas <- munic %>%
    dplyr::mutate(
      area_km2 = sf::st_area(.),
      area_km2 = units::set_units(area_km2, km^2)
    ) %>%
    sf::st_drop_geometry()

  # reading layers of the raster

  dat <- raster_codes %>%
    purrr::map2(
      param$time_period,
      function(code, year) {
        message(paste("Reading data for", year, "\n", "Processing this large raster may take a while\n"))

        # filtering raster to only the deforestation in that year

        message("Converting into binary raster")

        df <- dat == code

        message("Aggregating pixel values into <1km^2 rectangles")

        # FRAGILE: (40, 20) assumes PRODES' raster keeps its current pixel
        # resolution and grid orientation. A resolution change upstream
        # would silently change what "aggregate to <1km^2" actually
        # produces -- no error, just a wrong area calculation downstream.
        df <- df %>%
          terra::aggregate(fact = c(40, 20), fun = "mean")

        message("Calculating area of each pixel")

        pixel_areas <- terra::cellSize(df, unit = "km")

        message("Extracting pixel values\n")

        counts <- terra::extract(
          df * pixel_areas, # adds the number of marked pixels * the area
          terra::vect(munic),
          fun = sum,
          weights = TRUE,
          na.rm = TRUE
        )

        # add year column
        counts <- counts %>%
          dplyr::mutate(year = year)

        # combining with municipality areas

        counts <- munic_areas %>%
          dplyr::bind_cols(counts) %>%
          dplyr::select(-ID)

        # add km2 units

        counts <- counts %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(layer), ~ units::set_units(., "km^2")))

        # drop cities with no pixels

        counts <- counts %>%
          tidyr::drop_na(dplyr::all_of(layer))

        # rename variable to match the dataset

        counts <- counts %>%
          dplyr::rename(!!paste(param$dataset, "km2", sep = "_") := !!rlang::sym(layer))

        # return data frame
        counts
      }
    )

  # combining data frames

  dat <- dat %>%
    dplyr::bind_rows()

  # remove exceding municipality
  ## Passagem Franca (MA) is at 43.95o west, thus mistakenly included through the 44o west rule
  # FRAGILE: a single hardcoded municipality name, matched by exact string.
  # If IBGE's municipality boundaries shift, or another municipality edges
  # over the same 44 degrees-west rule in a future geo_municipalities
  # release, this filter neither catches the new case nor notices if
  # "Passagem Franca" itself stops needing the exclusion -- no error either
  # way, just a silently wrong municipality count.
  dat <- dat %>%
    dplyr::filter(name_muni != "Passagem Franca")

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "pt") {
    # FRAGILE: this map must track the manifest's dataset names AND this
    # function's own hardcoded "<dataset>_km2" rename (line ~217) by hand --
    # a new PRODES dataset, or a renamed existing one, silently leaves that
    # column un-translated in pt output instead of erroring.
    col_names <- c(
      cod_ibge = "code_muni",
      municipio = "name_muni",
      cod_uf = "code_state",
      uf = "abbrev_state",
      nome_uf = "name_state",
      cod_regiao = "code_region",
      regiao = "name_region",
      desmatamento_km2 = "deforestation_km2",
      desmatamento_residual_km2 = "residual_deforestation_km2",
      vegetacao_nativa_km2 = "native_vegetation_km2",
      nao_floresta_km2 = "non_forest_km2",
      hidrografia_km2 = "hydrography_km2",
      nuvens_km2 = "clouds_km2",
      ano = "year"
    )

    dat_mod <- dat %>%
      dplyr::rename(dplyr::any_of(col_names))
  }

  if (param$language == "eng") dat_mod <- dat

  #################
  ## Return data ##
  #################

  return(dat_mod)
}

# ---- PRODES raster legend --------------------------------------------------
#
# The raster's pixel values are PRODES' own encoding (year offsets and fixed
# codes). It is not in the manifest, but INPE ships it in the same zip as the
# .tif, as a QGIS style file (prodes_amazonia_legal_<year>_v<date>.qml) whose
# <paletteEntry value=".." label=".."> pairs read e.g. value="7" label="7 d2007".
#
# FRAGILE: the codes are hardcoded here on purpose, not read from the .qml at
# load time -- reading them would just swap these numbers for a table of
# Portuguese labels to search for (more likely to change than the codes), the
# .qml has no clouds entry at all, and a change would then break on a user's
# machine instead of in CI. Instead, three things check them:
#   1. actions/scrapers/resolve_prodes.R checks every new release's .qml
#      before the manifest moves to it, and stops the refresh if it disagrees;
#   2. that resolver keeps its own copy of these codes/labels (it runs
#      without the package), and tests/testthat/test-prodes-legend.R fails if
#      the two copies ever disagree -- change both together;
#   3. prodes_check_qml() warns when the .qml in the zip a user downloaded
#      disagrees (e.g. a manifest edit that skipped the resolver).
# Code 99 (clouds) is absent from the 2025 .qml (likely zero cloud pixels
# that year), so it cannot be verified against that release.

prodes_raster_code <- function(dataset, year) {
  switch(dataset,
    deforestation = year - 2000,
    residual_deforestation = year - 1960,
    native_vegetation = 100,
    non_forest = 101,
    hydrography = 91,
    clouds = 99,
    stop("prodes_raster_code(): unknown PRODES dataset '", dataset, "'", call. = FALSE)
  )
}

# The label the .qml gives prodes_raster_code(dataset, year). NA for clouds:
# no release checked so far has an entry for it, so there is no known label
# to expect -- prodes_legend_problems() only checks it looks like clouds.
prodes_legend_label <- function(dataset, year) {
  code <- prodes_raster_code(dataset, year)
  switch(dataset,
    deforestation = sprintf("%d d%d", code, year),
    residual_deforestation = sprintf("%d r%d", code, year),
    native_vegetation = "100 Vegeta\u00e7\u00e3o nativa florestal",
    non_forest = "101 Vegeta\u00e7\u00e3o nativa n\u00e3o florestal",
    hydrography = "91 Hidrografia",
    clouds = NA_character_
  )
}

# .qml text -> named character vector, label by code (names are the codes).
# A regex rather than an XML parser, to keep xml2 out of Imports; `[^>]*`
# (not `[^/]*`) so a "/" inside a label doesn't hide the entry, and XML
# escapes (&amp;, &#xe3;, ...) are decoded so an escaped accent still matches.
prodes_parse_qml <- function(txt) {
  entries <- regmatches(txt, gregexpr("<paletteEntry\\b[^>]*>", txt, perl = TRUE))[[1]]
  attr_of <- function(name) {
    m <- regmatches(entries, regexec(sprintf("\\b%s=\"([^\"]*)\"", name), entries, perl = TRUE))
    vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  }
  values <- suppressWarnings(as.integer(attr_of("value")))
  labels <- prodes_xml_unescape(attr_of("label"))
  keep <- !is.na(values) & !is.na(labels)
  stats::setNames(labels[keep], values[keep])
}

prodes_xml_unescape <- function(x) {
  num <- gregexpr("&#(x[0-9A-Fa-f]+|[0-9]+);", x, perl = TRUE)
  regmatches(x, num) <- lapply(regmatches(x, num), function(ents) {
    vapply(ents, function(e) {
      code <- sub("^&#(.*);$", "\\1", e)
      n <- if (startsWith(code, "x")) strtoi(substring(code, 2), 16L) else as.integer(code)
      intToUtf8(n)
    }, character(1), USE.NAMES = FALSE)
  })
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# What disagrees between `legend` (from prodes_parse_qml()) and the codes
# load_prodes() is about to use; character(0) when nothing does. A code
# missing from the legend is a problem, except for clouds, whose absence is
# the known state -- clouds only counts if its code is there meaning
# something else.
prodes_legend_problems <- function(legend, dataset, years) {
  problems <- character(0)
  for (year in years) {
    code <- prodes_raster_code(dataset, year)
    got <- unname(legend[as.character(code)])
    if (dataset == "clouds") {
      if (!is.na(got) && !grepl("nuvem", got, ignore.case = TRUE)) {
        problems <- c(problems, sprintf("code %d is labelled '%s', which is not clouds", code, got))
      }
      next
    }
    want <- prodes_legend_label(dataset, year)
    if (is.na(got)) {
      problems <- c(problems, sprintf("code %d ('%s') is not in the legend", code, want))
    } else if (!identical(trimws(got), want)) {
      problems <- c(problems, sprintf("code %d is labelled '%s', expected '%s'", code, got, want))
    }
  }
  unique(problems)
}

# Warns if the .qml next to `raster`'s .tif disagrees with the codes about
# to be used. Silent when there is nothing to compare (no .qml, or one that
# cannot be read): this is a guard on top of the resolver's check, so it
# must never be the reason load_prodes() fails.
prodes_check_qml <- function(raster, dataset, years) {
  src <- tryCatch(terra::sources(raster), error = function(e) character(0))
  if (is.data.frame(src)) src <- src$source
  src <- src[nzchar(src)]
  if (length(src) == 0) {
    return(invisible(character(0)))
  }
  qml <- list.files(dirname(src[1]), pattern = "\\.qml$", full.names = TRUE)
  if (length(qml) != 1) {
    return(invisible(character(0)))
  }
  legend <- tryCatch(
    prodes_parse_qml(paste(readLines(qml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
    error = function(e) NULL
  )
  if (length(legend) == 0) {
    return(invisible(character(0)))
  }
  problems <- prodes_legend_problems(legend, dataset, years)
  if (length(problems) > 0) {
    warning(
      "The legend (", basename(qml), ") that came with this PRODES file does not match ",
      "the raster codes load_prodes() uses for \"", dataset, "\":\n- ",
      paste(problems, collapse = "\n- "),
      "\nThe result may count the wrong pixels. Please report it at ",
      "https://github.com/datazoompuc/datazoom.amazonia/issues",
      call. = FALSE
    )
  }
  invisible(problems)
}
