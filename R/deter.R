#' @title DETER - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads data on changes in forest cover in the Legal Amazon and the Cerrado biome.
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado") with information about the Legal Amazon and Cerrado, respectively
#' @inheritParams load_baci
#'
#' @return A \code{sf} object.
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

  .data <- view_date <- name_muni <- code_muni <- sensor <- satellite <- abbrev_state <- NULL
  uc <- classname <- path_row <- area <- quadrant <- geometry <- id_alerta <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "deter"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset is valid


  #################
  ## Downloading ##
  #################

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

  dat <- dat %>%
    janitor::clean_names() %>%
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
      view_date, name_muni, code_muni, abbrev_state, classname,
      area, geometry, id_alerta
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
        "uf" = abbrev_state,
        "tipo_de_alerta" = classname
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "date" = view_date,
        "municipality" = name_muni,
        "municipality_code" = code_muni,
        "state" = abbrev_state,
        "alert_id" = id_alerta,
        "alert_type" = classname
      )
  }

  #################
  ## Return Data ##
  #################

  return(dat_mod)
}
