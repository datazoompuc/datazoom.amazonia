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

  if (param$dataset == "national_energy_balance") {
    dat <- external_download(source = param$source, dataset = param$dataset)
    dat <- janitor::clean_names(dat)
    if (param$raw_data) return(dat)
    return(dat) # ainda não tratado
  }

  # RAW_DATA = TRUE → retorna todas as sheets em uma lista
  if (param$raw_data) {
    raw_sheets <- c(
      "SETOR INDUSTRIAL POR RG",
      "SETOR INDUSTRIAL POR UF",
      "CONSUMO E NUMCONS SAM UF",
      "CONSUMO E NUMCONS SAM"
    )

    dat_list <- lapply(raw_sheets, function(sheet_name) {
      external_download(
        source = param$source,
        dataset = param$dataset,
        sheet = sheet_name
      )
    })

    names(dat_list) <- raw_sheets
    return(dat_list)
  }

  # Caso não seja raw_data, continua com tratamento normal

  # Definindo sheets mapeadas
  sheets_available <- list(
    consumer_type = list(
      state = "CONSUMO E NUMCONS SAM UF",
      subsystem = "CONSUMO E NUMCONS SAM"
    ),
    industrial_sector = list(
      state = "SETOR INDUSTRIAL POR UF",
      subsystem = "SETOR INDUSTRIAL POR RG"
    )
  )

  if (is.null(param$table) || !param$table %in% names(sheets_available)) {
    stop("For 'energy_consumption_per_class', specify 'table' as 'consumer_type' or 'industrial_sector'.")
  }

  if (!param$geo_level %in% c("state", "subsystem")) {
    stop("Invalid geo_level. Choose 'state' or 'subsystem'.")
  }

  sheet_selected <- sheets_available[[param$table]][[param$geo_level]]

  # Download e leitura do sheet específico
  dat <- external_download(
    source = param$source,
    dataset = param$dataset,
    sheet = sheet_selected
  )

  # Limpeza
  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, ~ stringi::stri_trans_general(., "Latin-ASCII"))

  # Tratamento da coluna de data
  if ("data_excel" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(data_excel = as.Date(data_excel)) %>%
      dplyr::rename(!!ifelse(param$language == "en", "Date", "Data") := data_excel)
  }

  dat <- dat %>%
    dplyr::select(-dplyr::any_of(c("data", "data_versao")))

  # Renomear colunas específicas
  if (param$table == "consumer_type") {
    dat <- dat %>%
      dplyr::rename(
        State = uf,
        Region = regiao,
        System = sistema,
        Class = classe,
        ConsumerType = tipo_consumidor,
        Consumption = consumo,
        Consumers = consumidores
      )
  }

  if (param$table == "industrial_sector") {
    dat <- dat %>%
      dplyr::rename(
        IndustrialSector = setor_industrial,
        State = uf,
        Region = regiao,
        Consumption = consumo
      )
  }

  # Tradução (apenas se language == "en")
  if (param$language == "en") {
    names(dat) <- dplyr::recode(names(dat),
                                "uf" = "State",
                                "regiao" = "Region",
                                "sistema" = "System",
                                "classe" = "Class",
                                "tipo_consumidor" = "ConsumerType",
                                "consumo" = "Consumption",
                                "consumidores" = "Consumers",
                                "setor_industrial" = "IndustrialSector"
    )
  }

  return(dat)
}
