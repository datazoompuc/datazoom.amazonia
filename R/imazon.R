#' @title IMAZON
#'
#' @description Loads information on ...  See \url{http://www.ipsamazonia.org.br/}
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download raw data from 2014
#' imazon <- load_imazon()
#' }
load_imazon <- function(dataset = "imazon_shp", raw_data = FALSE, language = "eng") {

  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
