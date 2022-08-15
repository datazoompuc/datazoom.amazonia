utils::globalVariables("where") # the selection helper 'where' is not exported from tidyselect, so this must be used to avoid notes
# they've recently made the move to export it, but it's still not is CRAN

#' @title DETER - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on change in forest cover in the Amazon. See \url{http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter}
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado") with information about both Amazon and Cerrado
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} (if \code{raw_data} = \code{TRUE}) or a \code{sf} object (if \code{raw_data} = \code{FALSE}).
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download raw data (raw_data = TRUE) from Amazonia (dataset = "deter_amz")
#' deter_amz <- load_deter(
#'   dataset = "deter_amz",
#'   raw_data = TRUE
#' )
#'
#' # Download treated data (raw_data = FALSE) from Cerrado (dataset = "deter_cerrado")
#' # in portuguese (language = 'pt')
#' deter_cer <- load_deter(
#'   dataset = "deter_cerrado",
#'   raw_data = FALSE,
#'   language = "pt"
#' )
#' }
#'
load_deter <- function(dataset, raw_data = FALSE,
                       language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  ## Bind Global Variables

  .data <- view_date <- name_muni <- code_muni <- sensor <- satellite <- NULL
  uc <- classname <- path_row <- area <- quadrant <- geometry <- NULL

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

  if (raw_data == TRUE) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ stringi::stri_trans_general(., id = "Latin-ASCII")
      )
    )

  # Loading municipal map data
  geo_br <- external_download(
    dataset = "geo_municipalities",
    source = "internal"
  )

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
      view_date, name_muni, code_muni, sensor, satellite, uc,
      classname, path_row, area, quadrant, geometry
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
        "pathrow" = path_row
      )
  }

  #################
  ## Return Data ##
  #################

  return(dat_mod)
}
