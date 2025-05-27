#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class or Industrial Sector
#'
#' @param dataset Dataset name: "consumer_energy_consumption", "industrial_energy_consumption", or "national_energy_balance"
#' @param geo_level Geographical level: "state" or "subsystem". Only applies to consumer or industrial datasets.
#' @param raw_data If TRUE, returns raw sheets.
#' @param language "pt" or "en". Applies to processed outputs.
#'
#' @export
load_epe <- function(dataset, geo_level = "state", raw_data = FALSE, language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################
  uf <- regiao <- sistema <- classe <- tipo_consumidor <- consumo <- consumidores <- setor_industrial <- data_excel <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################
  param <- list(
    source = "epe",
    dataset = dataset,
    geo_level = geo_level,
    raw_data = raw_data,
    language = language
  )

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################
  if (param$dataset == "national_energy_balance") {
    if (param$raw_data) {
      years <- as.character(2003:2023)
      dat_list <- lapply(years, function(sheet_name) {
        external_download(
          source = param$source,
          dataset = param$dataset,
          sheet = sheet_name
        )
      })
      names(dat_list) <- years
      return(dat_list)
    }

    message("Processed data for 'national_energy_balance' is not yet available.")
    return(NULL)
  }

  if (param$dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption")) {
    if (param$raw_data) {
      raw_sheets <- if (param$dataset == "consumer_energy_consumption") {
        c("CONSUMO E NUMCONS SAM UF", "CONSUMO E NUMCONS SAM")
      } else {
        c("SETOR INDUSTRIAL POR UF", "SETOR INDUSTRIAL POR RG")
      }

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

    ##############################
    ## Harmonizing Sheet Names ##
    ##############################
    sheets_available <- list(
      consumer_energy_consumption = list(
        state = "CONSUMO E NUMCONS SAM UF",
        subsystem = "CONSUMO E NUMCONS SAM"
      ),
      industrial_energy_consumption = list(
        state = "SETOR INDUSTRIAL POR UF",
        subsystem = "SETOR INDUSTRIAL POR RG"
      )
    )

    if (!param$geo_level %in% c("state", "subsystem")) {
      stop("Invalid geo_level. Choose 'state' or 'subsystem'.")
    }

    sheet_selected <- sheets_available[[param$dataset]][[param$geo_level]]

    ######################
    ## Downloading Data ##
    ######################
    dat <- external_download(
      source = param$source,
      dataset = param$dataset,
      sheet = sheet_selected
    )

    ######################
    ## Data Engineering ##
    ######################
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::mutate_if(is.character, ~ stringi::stri_trans_general(., "Latin-ASCII"))

    if ("data_excel" %in% names(dat)) {
      dat <- dat %>%
        dplyr::mutate(data_excel = as.Date(data_excel)) %>%
        dplyr::rename(!!ifelse(param$language == "en", "Date", "Data") := data_excel)
    }

    dat <- dat %>%
      dplyr::select(-dplyr::any_of(c("data", "data_versao")))

    ################################
    ## Harmonizing Variable Names ##
    ################################
    if (param$dataset == "consumer_energy_consumption") {
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

    if (param$dataset == "industrial_energy_consumption") {
      dat <- dat %>%
        dplyr::rename(
          State = uf,
          Region = regiao,
          IndustrialSector = setor_industrial,
          Consumption = consumo
        )
    }

    if (param$language == "en") {
      dat <- dat %>%
        dplyr::rename_with(~ dplyr::recode(.x,
                                           "uf" = "State",
                                           "regiao" = "Region",
                                           "sistema" = "System",
                                           "classe" = "Class",
                                           "tipo_consumidor" = "ConsumerType",
                                           "consumo" = "Consumption",
                                           "consumidores" = "Consumers",
                                           "setor_industrial" = "IndustrialSector"))
    }

    ####################
    ## Returning Data ##
    ####################
    return(dat)
  }
}

