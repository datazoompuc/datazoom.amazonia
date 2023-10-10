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

  year <- Municipio <- CodIbge <- Estado <- AreaKm2 <- increment <- NULL
  municipality <- municipality_code <- state <- deforestation <- NULL

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

  dat <- external_download(
    dataset = param$dataset,
    source = "prodes",
    year = 2022)

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    dplyr::bind_rows() %>%
    dplyr::rename("incremento2000" = "Desmatamento2000") %>%
    tidyr::pivot_longer(
      dplyr::starts_with("incremento"),
      names_prefix = "incremento",
      names_to = "year",
      values_to = "increment"
    ) %>%
    dplyr::select(
      year,
      "municipality" = Municipio,
      "municipality_code" = CodIbge,
      "state" = Estado,
      "area" = AreaKm2,
      increment
    )

  # calcutating cumulative deforestation

  dat <- dat %>%
    dplyr::arrange(municipality, year) %>%
    dplyr::mutate(
      deforestation = cumsum(increment),
      .by = municipality_code
    )

  dat <- dat %>%
    dplyr::mutate(
      increment = dplyr::case_when(
        year == 2000 ~ NA, .default = increment
      )
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "ano" = year,
        "municipio" = municipality,
        "cod_municipio" = municipality_code,
        "uf" = state,
        "incremento" = increment,
        "desmatamento" = deforestation
      )
  }

  return(dat_mod)
}
