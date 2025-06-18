#'@title EPE
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

  Conta <- bloco <-Fonte <- Valor <- Ano <- account <- year <- value <- NULL

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
      data_list <- lapply(years, function(sheet_name) {
        external_download(
          source = param$source,
          dataset = param$dataset,
          sheet = sheet_name
        )
      })
      names(data_list) <- years
      return(data_list)
    }

    ######################
    ## Data Engineering ##
    ######################

    years <- as.character(2003:2023)
    data_list <- lapply(years, function(sheet_name) {
      raw_data <- external_download(
        source = param$source,
        dataset = param$dataset,
        sheet = sheet_name
      )


      names(raw_data) <- as.character(unlist(raw_data[3, ]))
      colnames(raw_data)[1] <- "Conta"

      sources <- as.character(unlist(raw_data[2, -1]))
      names(sources) <- names(raw_data)[-1]

      raw_data <- raw_data[-c(1, 2, 3), ]


      tidy_data <- raw_data %>%
        tidyr::pivot_longer(
          cols = -Conta,
          names_to = "Fonte",
          values_to = "Valor"
        ) %>%


        dplyr::mutate(
          bloco = dplyr::case_when(
            Conta == "TOTAL TRANSFORMACAO" ~ "TRANSFORMACAO",
            Conta == "CONSUMO FINAL" ~ "CONSUMO",
            Conta == "AJUSTES" ~ "CONSUMO",
            TRUE ~ NA_character_
          )
        ) %>%
        tidyr::fill(bloco, .direction = "down") %>%
        dplyr::mutate(
          Conta = dplyr::case_when(
            bloco == "TRANSFORMACAO" & !stringr::str_detect(Conta, "^TRANSFORMACAO") ~ paste0("TRANSFORMACAO - ", Conta),
            bloco == "CONSUMO" & !stringr::str_detect(Conta, "^CONSUMO") ~ paste0("CONSUMO - ", Conta),
            is.na(bloco) & !stringr::str_detect(Conta, "^CONTA") ~ paste0("CONTA - ", Conta),
            TRUE ~ Conta
          )
        ) %>%
        dplyr::select(-bloco)


      tidy_data <- tidy_data %>%
        dplyr::mutate(
          Fonte = stringr::str_to_upper(Fonte),
          Valor = suppressWarnings(as.numeric(Valor)),
          Ano = as.integer(sheet_name)
        ) %>%
        dplyr::filter(!is.na(Valor), Fonte != "TOTAL")


      tidy_data <- tidy_data %>%
        dplyr::rename(account = Conta, source = Fonte, year = Ano, value = Valor)


      if (param$language == "en") {
        tidy_data <- tidy_data %>%
          dplyr::rename(
            Account = account,
            Source = source,
            Year = year,
            Value = value
          )
      } else {
        tidy_data <- tidy_data %>%
          dplyr::rename(
            Conta = account,
            Fonte = source,
            Ano = year,
            Valor = value
          )
      }

      return(tidy_data)
    })

    names(data_list) <- years

    ####################
    ## Returning Data ##
    ####################
    data <- dplyr::bind_rows(data_list)
    return(data)
  }


  ######################
  ## Downloading Data ##
  ######################

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
    ## Harmonizing Sheet Names   #
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
          state = uf,
          region = regiao,
          system = sistema,
          class = classe,
          type_of_consumer = tipo_consumidor,
          consumption = consumo,
          consumers = consumidores
        )
    }

    if (param$dataset == "industrial_energy_consumption") {
      dat <- dat %>%
        dplyr::rename(
          state = uf,
          region = regiao,
          industrial_sector = setor_industrial,
          consumption = consumo
        )
    }

    if (param$language == "en") {
      dat <- dat %>%
        dplyr::rename_with(~ dplyr::recode(.x,
                                           "uf" = "state",
                                           "regiao" = "region",
                                           "sistema" = "system",
                                           "classe" = "class",
                                           "tipo_consumidor" = "type_of_consumer",
                                           "consumo" = "consumption",
                                           "consumidores" = "consumers",
                                           "setor_industrial" = "industrial_sector"))
    }


    ####################
    ## Returning Data ##
    ####################


    return(dat)
  }
}

