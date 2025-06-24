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

  conta <- bloco <- fonte <- valor <- ano <- account <- year <- value <- data <- NULL

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

  # defining sheet names for each dataset

  if (param$dataset == "national_energy_balance") {
    sheets <- as.character(2003:2023)
  }
  if (param$dataset == "consumer_energy_consumption") {
    if (param$geo_level == "state") {
      sheets <- "CONSUMO E NUMCONS SAM UF"
    }
    if (param$geo_level == "subsystem") {
      sheets <- "CONSUMO E NUMCONS SAM"
    }
  }
  if (param$dataset == "industrial_energy_consumption") {
    if (param$geo_level == "state") {
      sheets <- "SETOR INDUSTRIAL POR UF"
    }
    if (param$geo_level == "subsystem") {
      sheets <- "SETOR INDUSTRIAL POR RG"
    }
  }

  dat <- external_download(
    source = param$source,
    dataset = param$dataset,
    sheet = sheets
  )

  names(dat) <- sheets

  # returning raw data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "national_energy_balance") {
    # clean each sheet separately

    dat <- dat %>%
      purrr::imap(
        function(df, year) {
          names(df) <- as.character(unlist(df[3, ]))

          names(df)[1] <- "conta"

          # remove initial 3 rows

          df <- df[-(1:3), ]

          # reshaping into long format

          df <- df %>%
            tidyr::pivot_longer(
              cols = -conta,
              names_to = "fonte",
              values_to = "valor"
            ) %>%
            dplyr::mutate(
              bloco = dplyr::case_when(
                conta == "TOTAL TRANSFORMACAO" ~ "TRANSFORMACAO",
                conta == "CONSUMO FINAL" ~ "CONSUMO",
                conta == "AJUSTES" ~ "CONSUMO",
                .default = NA_character_
              )
            ) %>%
            tidyr::fill(bloco, .direction = "down") %>%
            dplyr::mutate(
              conta = dplyr::case_when(
                bloco == "TRANSFORMACAO" & !stringr::str_detect(conta, "^TRANSFORMACAO") ~ paste0("TRANSFORMACAO - ", conta),
                bloco == "CONSUMO" & !stringr::str_detect(conta, "^CONSUMO") ~ paste0("CONSUMO - ", conta),
                is.na(bloco) & !stringr::str_detect(conta, "^CONTA") ~ paste0("CONTA - ", conta),
                .default = conta
              )
            ) %>%
            dplyr::select(-bloco)

          df <- df %>%
            dplyr::mutate(
              valor = suppressWarnings(as.numeric(valor)),
              ano = year
            )
        }
      )

    # combine them

    dat <- dat %>%
      dplyr::bind_rows()
  }

  if (param$dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption")) {
    dat <- dat[[1]] %>%
      janitor::clean_names() %>%
      dplyr::mutate_if(is.character, ~ stringi::stri_trans_general(., "Latin-ASCII")) %>%
      dplyr::mutate(data_excel = as.Date(data_excel))

    # remove other consusing date columns

    dat <- dat %>%
      dplyr::select(-dplyr::any_of(c("data", "data_versao")))
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$dataset == "national_energy_balance") {
    if (param$language == "en") {
      dat_mod <- dat %>%
        dplyr::rename(
          "account" = conta,
          "source" = fonte,
          "value" = valor,
          "year" = ano
        )
    }
  }

  if (param$dataset == "consumer_energy_consumption") {
    dat_mod <- dat %>%
      dplyr::rename(
        "data" = data_excel
      )

    if (param$language == "en") {
      dat_mod <- dat_mod %>%
        dplyr::rename(
          state = uf,
          region = regiao,
          system = sistema,
          class = classe,
          type_of_consumer = tipo_consumidor,
          consumption = consumo,
          consumers = consumidores,
          date = data
        )
    }
  }

  if (param$dataset == "industrial_energy_consumption") {
    dat_mod <- dat %>%
      dplyr::rename(
        "data" = data_excel
      )

    if (param$language == "en") {
      dat_mod <- dat_mod %>%
        dplyr::rename(
          state = uf,
          region = regiao,
          industrial_sector = setor_industrial,
          consumption = consumo,
          date = data
        )
    }
  }


  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
