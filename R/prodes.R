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
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE)
#' # in portuguese (language = 'pt').
#' data <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   time_period = 2020:2023,
#'   language = "pt"
#' )
#' }
#'
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

  . <- area_km2 <- km <- ID <- prodes_amazonia_legal_2023 <- NULL

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

  if (param$dataset == "deforestation") raster_codes <- param$time_period - 2000
  if (param$dataset == "residual_deforestation") raster_codes <- param$time_period - 1960
  if (param$dataset == "native_vegetation") raster_codes <- 100
  if (param$dataset == "non_forest") raster_codes <- 101
  if (param$dataset == "hydrography") raster_codes <- 91
  if (param$dataset == "clouds") raster_codes <- 99

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
        message(paste("Reading data for", year, "\n"))

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
          dplyr::mutate(dplyr::across(prodes_amazonia_legal_2023, ~ units::set_units(., "km^2")))

        # drop cities with no pixels

        counts <- counts %>%
          tidyr::drop_na(prodes_amazonia_legal_2023)

        # rename variable to match the dataset

        counts <- counts %>%
          dplyr::rename(!!paste(param$dataset, "km2", sep = "_") := prodes_amazonia_legal_2023)

        # return data frame
        counts
      }
    )

  # combining data frames

  dat <- dat %>%
    dplyr::bind_rows()

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
