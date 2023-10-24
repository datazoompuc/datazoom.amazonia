#' @title DETER - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on changes in forest cover in the Amazon.
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado") with information about both Amazon and Cerrado
#' @inheritParams load_baci
#'
#' @return A \code{tibble} (if \code{raw_data} = \code{TRUE}) or a \code{sf} object (if \code{raw_data} = \code{FALSE}).
#'
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE) from Amazonia (dataset = "deter_amz")
#' deter_amz <- load_deter(
#'   dataset = "deter_amz",
#'   raw_data = FALSE
#' )
#' }
#'
#' @export

load_deter <- function(dataset, raw_data = FALSE,
                       language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  .data <- view_date <- name_muni <- code_muni <- sensor <- satellite <- NULL
  uc <- classname <- path_row <- area <- quadrant <- geometry <- id_alerta <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    dataset = param$dataset,
    source = "deter"
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ stringi::stri_trans_general(., id = "Latin-ASCII")
      )
    )

  # Loading municipal map data
  geo_br <- external_download(
    dataset = "geo_municipalities",
    source = "internal"
  )

  # Adding alert_id variable to preserve the information of which rows belong to the same alert

  dat <- dat %>%
    dplyr::mutate(id_alerta = dplyr::row_number())

  ###################
  ## Harmonize CRS ##
  ###################

  # The crs that will be used to overlap maps below
  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")

  # Changing crs of both data to the common crs chosen above
  dat$geometry <- sf::st_make_valid(sf::st_transform(dat$geometry, operation_crs))
  geo_br$geom <- sf::st_transform(geo_br$geom, operation_crs)

  # Overlaps shapefiles
  sf::st_geometry(dat) <- dat$geometry
  sf::st_geometry(geo_br) <- geo_br$geom

  dat <- suppressWarnings(sf::st_intersection(dat, geo_br)) %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  dat <- dat %>%
    dplyr::select(
      view_date, name_muni, code_muni, sensor, satellite,
      classname, path_row, area, quadrant, geometry, id_alerta
    )

  ###################
  ## Renaming Data ##
  ###################

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "data" = view_date,
        "municipio" = name_muni,
        "cod_municipio" = code_muni,
        "satelite" = satellite,
        "classe" = classname,
        "quadrante" = quadrant
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "date" = view_date,
        "municipality" = name_muni,
        "municipality_code" = code_muni,
        "class_name" = classname,
        "alert_id" = id_alerta
      )
  }

  #################
  ## Return Data ##
  #################

  return(dat_mod)
}
