#' @title Population
#'
#' @description Loads information on (estimated) population
#'
#' @param dataset A dataset name ("population").
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # Download raw data (raw_data = TRUE) at the country level
#' # from 2008 to 2010 (time_period = 2008:2010).
#' data <- load_population(
#'   raw_data = TRUE,
#'   geo_level = "country",
#'   time_period = 2008:2010
#' )
#'
#' # Download treted data (raw_data = FALSE) by state (geo_level = "state")
#' # from 2008 to 2010 (time_period = 2008:2010) in portuguese (language = "pt").
#' data <- load_population(
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2008:2010,
#'   language = "pt"
#' )
#' }
#'
#' @export

load_population <- function(dataset = "population", raw_data = FALSE,
                            geo_level, time_period, language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- geo_id <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida <- unidade_de_medida_codigo <- valor <- variavel <- variavel_codigo <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language

  if (!is.numeric(param$dataset)) {
    param$code <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {
    param$code <- param$dataset
  }

  ## Check if year is acceptable

  year_check <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>%
    as.character() %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    as.numeric()

  if (min(time_period) < year_check[1]) {
    stop("Provided time period less than supported. Check documentation for time availability.")
  }
  if (max(time_period) > year_check[2]) {
    stop("Provided time period greater than supported. Check documentation for time availability.")
  }

  ##############
  ## Download ##
  ##############

  dat <- as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num) {
      suppressWarnings(
      sidra_download(
        sidra_code = param$code,
        year = year_num,
        geo_level = param$geo_level
      )
      )
    }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Enginnering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor)) %>%
    dplyr::select(unidade_de_medida_codigo:variavel)

  ## Only Keep Valid Observations

  dat <- dat %>%
    dplyr::filter(!is.na(valor))

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  if (geo_level == "country") {
    dat$geo_id <- dat$brasil
    dat <- dplyr::select(dat, -"brasil_codigo", -"brasil")
  }

  if (geo_level == "state") {
    dat$geo_id <- dat$unidade_da_federacao_codigo
    dat <- dplyr::select(dat, -"unidade_da_federacao_codigo", -"unidade_da_federacao")
  }
  if (geo_level == "municipality") {
    dat$geo_id <- dat$municipio_codigo
    dat <- dplyr::select(dat, -"municipio", -"municipio_codigo")
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat <- dat %>%
    dplyr::select(-unidade_de_medida, -unidade_de_medida_codigo)


  dat <- dat %>%
    dplyr::arrange(variavel_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = c(ano, geo_id),
      names_from = variavel:variavel_codigo,
      values_from = valor,
      names_sep = "_V",
      values_fn = sum,
      values_fill = NA
    ) %>%
    janitor::clean_names()

  if (language == "eng") {
    dat <- dat %>%
      dplyr::rename_with(dplyr::recode,
                         "ano" = "year",
                         "populacao_residente_v93" = "population_v93",
                         "populacao_residente_estimada_v9324" = "estimated_population_v9324"
        )
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}
