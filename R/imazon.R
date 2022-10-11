#' @title IMAZON - Deforestation pressure by municipality
#'
#' @description Loads data categorizing each municipality by the level of deforestation pressure it faces
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examples \dontrun{
#' # Download treated data
#' data <- load_imazon(raw_data = FALSE)
#' }
#'
#' @export

load_imazon <- function(dataset = "imazon_shp", raw_data = FALSE, language = "eng") {

  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  CD_GEOCMU <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  ######################
  ## Downloading Data ##
  ######################

  dat <- external_download(
    dataset = param$dataset,
    source = "imazon_shp"
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # Dropping geometry column

  dat <- dat %>%
    sf::st_drop_geometry()

  # Converting municipality codes into numeric

  dat <- dat %>%
    dplyr::mutate(dplyr::across(CD_GEOCMU, as.numeric))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality_code" = "CD_GEOCMU",
        "municipality" = "NM_MUNICIP",
        "area" = "Area",
        "state" = "UF",
        "deforestation_pressure" = "Front_2020"
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "cod_municipio" = "CD_GEOCMU",
        "municipio" = "NM_MUNICIP",
        "area" = "Area",
        "uf" = "UF",
        "pressao_desmatamento" = "Front_2020"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
