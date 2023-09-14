#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads information on clearcut deforestation in the Legal Amazon and annual deforestation rates in the region.
#'
#' @param dataset A dataset name ("prodes").
#' @inheritParams load_baci
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE) from 2010 (time_period = 2010)
#' # in portuguese (language = 'pt').
#' data <- load_prodes(
#'   raw_data = FALSE,
#'   time_period = 2010,
#'   language = "pt"
#' )
#' }
#'
#' @export

load_prodes <- function(dataset = "prodes", raw_data = FALSE,
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

  # check if dataset and time_period are supported

  check_params(param, "PRODES-INPE")

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
