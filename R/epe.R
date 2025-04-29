#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name, ("energy_consumption_per_class") or ("national_energy_balance")
#' @param geo_level A geographical level, ("state") or ("subsystem"), only available for "energy_consumption_per_class"
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data about energy consumption at the state level
#' clean_epe <- load_epe(
#'   dataset = "energy_consumption_per_class",
#'   geo_level = "state",
#'   raw_data = FALSE
#' )
#' }
#'
#' @export
# Nova versão da função load_epe(), considerando estrutura real do Excel

load_epe <- function(dataset, table = NULL, geo_level = "state", raw_data = FALSE, language = "eng") {
  # Variáveis globais para o R CMD Check
  ano <- mes <- uf <- sistema <- tipo_consumidor <- classe_consumo <- atividade <- cnae <- quantidade <- NULL

  # Define parâmetros
  param <- list()
  param$source <- "epe"
  param$dataset <- dataset
  param$table <- table
  param$geo_level <- geo_level
  param$raw_data <- raw_data
  param$language <- language

  # Validação de inputs
  check_params(param)

  if (!param$dataset %in% c("energy_consumption_per_class", "national_energy_balance")) {
    stop("Invalid dataset. Choose 'energy_consumption_per_class' or 'national_energy_balance'.")
  }

  # Para "national_energy_balance", download é direto
  if (param$dataset == "national_energy_balance") {
    dat <- external_download(source = param$source, dataset = param$dataset)
    dat <- dat %>% janitor::clean_names()
    if (raw_data) return(dat)
    return(dat)
  }

  # Definindo sheets para "energy_consumption_per_class"
  sheets_available <- list(
    consumer_type = list(
      state = "CONSUMO E NUMCONS SAM UF",
      subsystem = "CONSUMO E NUMCONS SAM SISTEMA/REGIAO"
    ),
    industrial_sector = list(
      state = "SETOR INDUSTRIAL POR UF",
      subsystem = "SETOR INDUSTRIAL POR SISTEMA/REGIAO"
    )
  )

  if (is.null(param$table) || !param$table %in% names(sheets_available)) {
    stop("For 'energy_consumption_per_class', specify 'table' as 'consumer_type' or 'industrial_sector'.")
  }

  if (!param$geo_level %in% c("state", "subsystem")) {
    stop("Invalid geo_level. Choose 'state' or 'subsystem'.")
  }

  sheet_selected <- sheets_available[[param$table]][[param$geo_level]]

  # Download e leitura
  dat <- external_download(
    source = param$source,
    dataset = param$dataset,
    sheet = sheet_selected
  )

  if (param$raw_data) {
    return(dat)
  }

  # Limpeza básica
  dat <- dat %>%
    dplyr::mutate_if(is.character, ~ stringi::stri_trans_general(., "Latin-ASCII")) %>%
    janitor::clean_names()

  # Renomeia colunas conforme tipo de tabela
  if (param$table == "consumer_type") {
    dat <- dat %>%
      dplyr::rename(
        year = ano,
        month = mes,
        state = uf,
        system = sistema,
        consumer_class = classe_consumo,
        consumer_type = tipo_consumidor,
        consumption_mwh = quantidade
      )
  }

  if (param$table == "industrial_sector") {
    dat <- dat %>%
      dplyr::rename(
        year = ano,
        month = mes,
        state = uf,
        system = sistema,
        cnae_code = cnae,
        activity = atividade,
        consumption_mwh = quantidade
      )
  }

  # Correção de tipos
  dat <- dat %>%
    dplyr::mutate(
      year = as.integer(year),
      month = as.integer(month),
      consumption_mwh = as.numeric(consumption_mwh)
    )

  # Tradução se language == "pt"
  if (param$language == "en") {
    dat <- dat %>%
      dplyr::rename_with(~ dplyr::recode(.,
                                         "state" = "estado",
                                         "system" = "sistema",
                                         "consumer_class" = "classe_consumo",
                                         "consumer_type" = "tipo_consumidor",
                                         "cnae_code" = "codigo_cnae",
                                         "activity" = "atividade",
                                         "year" = "ano",
                                         "month" = "mes",
                                         "consumption_mwh" = "consumo_mwh"
      ))
  }

  return(dat)
}
