#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads information on clearcut deforestation in the Legal Amazon and annual deforestation rates in the region. Survey is done at state or municipality level and data is available from 2000 to 2020.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("prodes").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download raw data from 2000 to 2020
#' raw_prodes_all <- load_prodes(
#'   dataset = "prodes",
#'   raw_data = TRUE,
#'   time_period = 2000:2020
#' )
#' }
#'
#' @export

load_prodes <- function(dataset = "prodes", raw_data,
                        time_period,
                        language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  survey <- link <- cod_ibge <- desmatado <- estado <- floresta <- hidrografia <- NULL
  incremento <- lat <- latgms <- long <- longms <- municipio <- nao_floresta <- NULL
  nao_observado <- nr <- nuvem <- soma <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$time_period <- time_period
  param$language <- language

  ###################
  ## Download Data ##
  ###################

  ## Column Names come with numbers at the side - we need to clean those

  dat <- as.list(param$time_period) %>%
    purrr::map(
      function(t) {
        external_download(dataset = param$dataset, source = "prodes", year = t) %>%
          dplyr::mutate(ano = t) # Adding year variable to each dataframe
      }
    )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # Removing years from each data frame's column names

  dat <- dat %>%
    purrr::map(
      dplyr::rename_with,
      .fn = ~ gsub("(.*)\\d{4}?", "\\1", .)
    )

  dat <- dat %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  # Removing coordinate variables

  dat <- dat %>%
    dplyr::select(-c(nr, lat, long, latgms, longms))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality" = municipio,
        "municipality_code" = cod_ibge,
        "state" = estado,
        "deforestation" = desmatado,
        "increment" = incremento,
        "forest" = floresta,
        "cloud" = nuvem,
        "not_observed" = nao_observado,
        "not_forest" = nao_floresta,
        "hydrography" = hidrografia,
        "sum" = soma
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "cod_municipio" = cod_ibge,
        "uf" = estado
      )
  }

  return(dat_mod)
}
