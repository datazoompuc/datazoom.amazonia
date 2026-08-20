#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads data on deforestation in the Legal Amazon region.
#'
#' @param dataset A dataset name. Can be one of "deforestation", "residual_deforestation", "native_vegetation", "hydrography", "non_forest", or "clouds".
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#'    * Between 2007 - 2023 for dataset "deforestation". Deforestation for 2007 includes all cumulative deforestation up to 2007. For other years, deforestation is incremental
#'    * Between 2010 - 2023 for dataset "residual_deforestation"
#'    * Only 2023 for all other datasets
#' @inheritParams load_baci
#'
#' @return A \code{tibble} with the selected data if raw_data is \code{FALSE}, and a \code{SpatRaster} is \code{TRUE}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated deforestation data for 2023
#' deforestation <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   time_period = 2023,
#'   language = "eng"
#' )
#'
#' # download treated deforestation data for 2008 to 2023
#' deforestation_series <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   time_period = 2008:2023,
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

#' @importFrom rlang :=
#'
#' @export

load_prodes <- function(dataset = "deforestation", raw_data = FALSE,
                        time_period = 2023, language = "eng") {
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

  # raster has values for each dataset/year
  #
  # This legend (year offsets and fixed codes) is PRODES' own raster encoding
  # scheme. It is not in the manifest, but INPE does ship it in the same zip
  # as the .tif, as a QGIS style file (prodes_amazonia_legal_<year>_v<date>.qml)
  # with a <colorPalette> of value/label pairs (e.g. value="7" label="7 d2007").
  # download.R only keeps the *.tif from the zip and discards the .qml/.txt
  # sidecars, so this mapping is hardcoded here instead of read at runtime.
  # If INPE changes the encoding in a future collection, re-check the .qml
  # from that release's zip and update by hand alongside `layer_name` below.
  # Note: code 99 (clouds) was absent from the 2025 .qml/.txt (likely zero
  # cloud pixels that year) and could not be re-verified against that release.

  if (param$dataset == "deforestation") raster_codes <- param$time_period - 2000
  if (param$dataset == "residual_deforestation") raster_codes <- param$time_period - 1960
  if (param$dataset == "native_vegetation") raster_codes <- 100
  if (param$dataset == "non_forest") raster_codes <- 101
  if (param$dataset == "hydrography") raster_codes <- 91
  if (param$dataset == "clouds") raster_codes <- 99

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
  dat <- dat %>%
    dplyr::filter(name_muni != "Passagem Franca")

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "pt") {
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
