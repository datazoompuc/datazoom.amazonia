#' @title IMAZON - Deforestation pressure by municipality
#'
#' @description Loads data categorizing each municipality by the level of deforestation pressure it faces
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' ### Example 1: High-Pressure Municipalities
#'
#' Identify municipalities facing highest deforestation pressure:
#'
#' library(dplyr)
#'
#' # Load Imazon pressure data
#' imazon_data <- load_imazon(
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # High and very high pressure municipalities
#' high_pressure <- imazon_data %>%
#'   as.data.frame() %>%
#'   filter(pressure_category >= 2) %>%
#'   select(municipality, state, pressure_category) %>%
#'   arrange(desc(pressure_category), municipality)
#'
#' print(high_pressure)
#'
#' # Count by pressure level
#' pressure_summary <- imazon_data %>%
#'   as.data.frame() %>%
#'   group_by(pressure_category) %>%
#'   summarize(
#'     num_municipalities = n(),
#'     percentage = (n() / nrow(imazon_data)) * 100,
#'     .groups = 'drop'
#'   ) %>%
#'   arrange(desc(pressure_category))
#'
#' print(pressure_summary)
#'
#' ### Example 2: State-Level Pressure Distribution
#'
#' Analyze deforestation pressure across states:
#'
#' library(dplyr)
#'
#' imazon_data <- load_imazon(
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # State-level pressure distribution
#' state_pressure <- imazon_data %>%
#'   as.data.frame() %>%
#'   group_by(state, pressure_category) %>%
#'   summarize(num_municipalities = n(), .groups = 'drop') %>%
#'   pivot_wider(
#'     names_from = pressure_category,
#'     values_from = num_municipalities,
#'     values_fill = 0,
#'     names_prefix = "category_"
#'   )
#'
#' # Overall state risk score
#' state_risk <- imazon_data %>%
#'   as.data.frame() %>%
#'   group_by(state) %>%
#'   summarize(
#'     total_municipalities = n(),
#'     avg_pressure = mean(pressure_category, na.rm = TRUE),
#'     high_risk_count = sum(pressure_category >= 2, na.rm = TRUE),
#'     pct_high_risk = (high_risk_count / total_municipalities) * 100,
#'     .groups = 'drop'
#'   ) %>%
#'   arrange(desc(pct_high_risk))
#'
#' print(state_risk)
#'
#' @export
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
  param$source <- "imazon"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset is valid

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

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
