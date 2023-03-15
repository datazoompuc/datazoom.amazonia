#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("energy_development_budget", "energy_generation" or "energy_enterprises_distributed")
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data about energy generation
#' clean_aneel <- load_aneel(
#'   dataset = "energy generation",
#'   raw_data = FALSE
#' )
#' }
#'
#' @export

load_aneel <- function(dataset, raw_data = FALSE, language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  ano <- variable <- label <- var_code <- operation_start <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language

  skip <- NULL

  if (param$dataset == "energy_generation") {
    skip <- 1
    # skips first row of excel sheet for this dataset
  }

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    source = "ANEEL",
    dataset = param$dataset,
    skip_rows = skip
  )

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  # Loading dictionary to recode variable values

  dic <- load_dictionary(param$dataset)

  if (param$language == "pt") {
    dic <- dic %>%
      dplyr::rename("label" = "label_pt")
  }
  if (param$language == "eng") {
    dic <- dic %>%
      dplyr::rename("label" = "label_eng")
  }

  available_vars <- dic %>%
    dplyr::select(variable) %>%
    unlist()

  dat <- names(dat) %>%
    purrr::map_dfc(
      function(var) {
        df <- dat %>%
          dplyr::select(var)

        if (var %in% available_vars) {
          dic <- dic %>%
            dplyr::filter(variable == var)

          var_labels <- dic %>%
            dplyr::select(label) %>%
            unlist()

          var_codes <- dic %>%
            dplyr::select(var_code) %>%
            unlist()

          names(var_labels) <- var_codes

          df <- df %>%
            dplyr::mutate(dplyr::across(var, dplyr::recode, !!!var_labels))
        }

        return(df)
      }
    )

  # changing operation_start to date format

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        operation_start,
        as.Date,
        format = "%d/%m/%Y", origin = "1970-01-01"
      )
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "soma_de_valor" = "value",
        "participacao" = "participacao_no_total",
        "municipio_s" = "municipios",
        "potencia_outorgada_k_w" = "potencia_outorgada_kw",
        "potencia_fiscalizada_k_w" = "potencia_fiscalizada_kw",
        "garantia_fisica_k_w" = "garantia_fisica_kw",
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "ano" = "year",
        "tipo_de_despesa" = "type_of_expense",
        "soma_de_valor" = "value",
        "participacao" = "share_of_total",
        "empreendimento" = "venture",
        "uf" = "state",
        "fonte" = "source",
        "fase" = "stage",
        "origem" = "origin",
        "tipo" = "type",
        "tipo_de_atuacao" = "type_of_permission",
        "combustivel_final" = "final_fuel",
        "entrada_em_operacao" = "operation_start",
        "potencia_outorgada_k_w" = "granted_power_kw",
        "potencia_fiscalizada_k_w" = "fiscalized_power_kw",
        "garantia_fisica_k_w" = "physical_guarantee_kw",
        "geracao_qualificada" = "qualified_generation",
        "latitude_decimal" = "latitude_dd",
        "longitude_decimal" = "longitude_dd",
        "inicio_vigencia" = "validity_start",
        "fim_vigencia" = "validity_end",
        "proprietario_regime_de_exploracao" = "owner_or_exploration_regime",
        "sub_bacia" = "sub_basin",
        "municipio_s" = "municipalities"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
