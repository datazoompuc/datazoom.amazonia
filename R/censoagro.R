#' @title Censo Agropecuario
#'
#' @description Loads information on agricultural establishments and activities
#'
#' @param dataset A dataset name ("agricultural_land_area", "agricultural_area_use", "agricultural_employees_tractors", "agricultural_producer_condition", "animal_species", "animal_products", "vegetable_production_area", "vegetable_production_permanent", "vegetable_production_temporary", "livestock_production").
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be of "country" or "state".
#'    * For dataset "livestock_production", can be one of "country", "state", or "municipality"
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # Download total land area data at the country level in year 2006
#' data <- load_censoagro(
#'   dataset = "land_area_total",
#'   raw_data = TRUE,
#'   geo_level = "country",
#'   time_period = 2006
#' )
#'
#' # Download temporary production crops data by state (geo_level = "state") in year 2006
#'   in portuguese (language = "pt").
#' data <- load_censoagro(
#'   dataset = "production_temporary_crops",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 1996,
#'   language = "pt"
#' )
#' }
#'
#'
#'## We should include support for microregion/mesoregion
#'
#'
#' @export

load_censoagro <- function(dataset,raw_data = FALSE,
                            geo_level, time_period, language = "eng") {


  ###########################
  ## Bind Global Variables ##
  ###########################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- geo_id <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida <- unidade_de_medida_codigo <- valor <- variavel <- variavel_codigo <- NULL
  group_id <- unit_id <- NULL


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
    dplyr::mutate(valor = as.numeric(valor))

  # grouping variable, with classifics, when applicable

  if (param$dataset %in% c("land_area_total", "area_use", "land_area_producer_condition",
                           "animal_production", "animal_products", "vegetable_production_area",
                           "vegetable_production_temporary", "vegetable_production_permanent")) {
    dat <- dat %>%
      dplyr::rename("group_id" = dplyr::last_col())
  }

  # standardizing measuring units

  if (param$dataset == "animal_production") {
    dat <- dat %>%
      dplyr::mutate(
        valor = dplyr::case_when(
          unidade_de_medida_codigo == 1620 ~ 1000 * valor,
          .default = valor
        )
      )
  }
  if (param$dataset %in% c("animal_production", "vegetable_production_area",
                           "vegetable_production_temporary", "vegetable_production_permanent")) {
    dat <- dat %>%
      dplyr::mutate(
        "unit_id" = dplyr::case_when(variavel_codigo != 216 ~ unidade_de_medida)
      ) %>%
      dplyr::group_by(ano, group_id) %>%
      tidyr::fill(unit_id) %>%
      dplyr::ungroup()
  }

  ## Only Keep Valid Observations

  dat <- dat %>%
    tidyr::drop_na(valor)

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

  dat <- dat %>%
    dplyr::select(-unidade_de_medida, -unidade_de_medida_codigo)

  dat <- dat %>%
    dplyr::arrange(variavel_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::any_of(c("ano", "geo_id", "group_id", "unit_id")),
      names_from = variavel:variavel_codigo,
      values_from = valor,
      names_sep = "_V",
      values_fill = NA
    ) %>%
    janitor::clean_names()

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat_mod <- dat

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat_mod)
}
