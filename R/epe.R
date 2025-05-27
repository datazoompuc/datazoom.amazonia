#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name, ("consumer_energy_consumption"), ("industrial_energy_consumption") or ("national_energy_balance")
#' @param geo_level A geographical level, ("state") or ("subsystem"), only available for consumer or industrial datasets
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' clean_epe <- load_epe(
#'   dataset = "consumer_energy_consumption",
#'   geo_level = "state",
#'   raw_data = FALSE
#' )
#' }
#'
#' @export
load_epe <- function(dataset, geo_level = "state", raw_data = FALSE, language = "eng") {
  # Set parameters
  param <- list()
  param$source <- "epe"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$raw_data <- raw_data
  param$language <- language

  # Check inputs
  check_params(param)

  if (!param$dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption", "national_energy_balance")) {
    stop("Invalid dataset. Choose 'consumer_energy_consumption', 'industrial_energy_consumption' or 'national_energy_balance'.")
  }

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

    dat <- external_download(
      source = param$source,
      dataset = param$dataset,
      sheet = sheet_selected
    )

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
          IndustrialSector = setor_industrial,
          State = uf,
          Region = regiao,
          Consumption = consumo
        )
    }

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
}

