#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class or Industrial Sector
#'
#' @param dataset Dataset name: "consumer_energy_consumption", "industrial_energy_consumption", "national_energy_balance", or "energy_state_panel"
#' @param geo_level Geographical level: "state" or "subsystem". Only applies to consumer or industrial datasets.
#' @inheritParams load_baci
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated (raw_data = FALSE) data about
#' # consumer energy consumption (dataset = "consumer_energy_consumption")
#' # at the state level (geo_level = "state")
#' data <- load_epe(
#'   dataset = "consumer_energy_consumption",
#'   geo_level = "state",
#'   raw_data = FALSE
#' )
#' # download treated (raw_data = FALSE) data
#' # from the National Energy Balance (dataset = "national_energy_balance")
#' balance <- load_epe(
#'   dataset = "national_energy_balance",
#'   raw_data = FALSE
#' )
#'
#' @export
load_epe <- function(dataset, geo_level = "state", raw_data = FALSE, language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################


  uf <- regiao <- sistema <- classe <- tipo_consumidor <- consumo <- consumidores <- setor_industrial <- data_excel <- NULL

  . <- ano_tag <- conta <- bloco <- fonte <- valor <- ano <- account <- year <- value <- data <- total_produzido <- hidro <- eolica <- solar <- nuclear <- termo <- cana <- lenha <- lixivia <- outras_fontes_renovaveis <- carvao_vapor <- gas_natural <- gas_de_coqueira <- combustivel <- diesel <- outras_fontes_nao_renovaveis <- amz_legal <- state <- legal_amazon <- total_produced <- hydro <- wind <- other_renewable_sources <- steam_coal <- natural_gas <- coke_oven_gas <- fuel_oil <- diesel_oil <- other_non_renewable_sources <- NULL

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

  if (param$dataset == "energy_state_panel") {
    sheets <- "8.1 part 3"
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


  if (param$dataset == "energy_state_panel") {
    raw <- dat[[1]]

    amz_legal_estados <- c("Amapa", "Acre", "Amazonas", "Mato Grosso",
                           "Tocantins", "Maranhao", "Rondonia", "Roraima", "Para")

    # Cada bloco de ano começa com "ANO BASE XXXX" na col 1
    ano_rows <- which(grepl("ANO BASE", as.character(raw[[1]]), ignore.case = TRUE))
    anos     <- as.integer(stringr::str_extract(as.character(raw[[1]][ano_rows]), "\\d{4}"))
    raw$ano_tag <- NA_integer_
    for (i in seq_along(ano_rows)) {
      end_row <- if (i < length(ano_rows)) ano_rows[i + 1] - 1 else nrow(raw)
      raw$ano_tag[ano_rows[i]:end_row] <- anos[i]
    }

    dat_mod <- raw %>%
      dplyr::filter(!is.na(ano_tag)) %>%
      dplyr::filter(!is.na(suppressWarnings(as.numeric(as.character(.[[2]]))))) %>%
      dplyr::filter(stringr::str_detect(as.character(.[[1]]), "[:lower:]")) %>%
      dplyr::select(
        uf                             = 1,
        total_produzido                = 2,
        hidro                          = 3,
        eolica                         = 4,
        solar                          = 5,
        nuclear                        = 6,
        termo                          = 7,
        cana                           = 8,
        lenha                          = 9,
        lixivia                        = 10,
        outras_fontes_renovaveis       = 11,
        carvao_vapor                   = 12,
        gas_natural                    = 13,
        gas_de_coqueira                = 14,
        combustivel                    = 15,
        diesel                         = 16,
        outras_fontes_nao_renovaveis   = 17,
        ano                            = ano_tag
      ) %>%
      dplyr::mutate(
        uf = dplyr::case_when(
          uf == "Mato G. do Sul"  ~ "Mato Grosso do Sul",
          uf == "Rio G. do Sul"   ~ "Rio Grande do Sul",
          uf == "Rio G. do Norte" ~ "Rio Grande do Norte",
          TRUE ~ uf
        ),
        uf = stringi::stri_trans_general(uf, "Latin-ASCII"),
        amz_legal = dplyr::if_else(uf %in% amz_legal_estados, 1L, 0L)
      ) %>%
      dplyr::mutate(dplyr::across(-c(uf, amz_legal), as.numeric))
  }
  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$dataset == "national_energy_balance") {
    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::rename(
          "account" = conta,
          "source" = fonte,
          "value" = valor,
          "year" = ano
        )
    } else {
      dat_mod <- dat
    }
  }

  if (param$dataset == "consumer_energy_consumption") {
    dat_mod <- dat %>%
      dplyr::rename(
        "data" = data_excel
      )

    if (param$language == "eng") {
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

    if (param$language == "eng") {
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

  if (param$dataset == "energy_state_panel") {
    if (param$language == "eng") {
      dat_mod <- dat_mod %>%
        dplyr::rename(
          state                        = uf,
          legal_amazon                 = amz_legal,
          year                         = ano,
          total_produced               = total_produzido,
          hydro                        = hidro,
          wind                         = eolica,
          nuclear                      = nuclear,
          thermal                      = termo,
          sugar_cane                   = cana,
          firewood                     = lenha,
          black_liquor                 = lixivia,
          other_renewable_sources      = outras_fontes_renovaveis,
          steam_coal                   = carvao_vapor,
          natural_gas                  = gas_natural,
          coke_oven_gas                = gas_de_coqueira,
          fuel_oil                     = combustivel,
          diesel_oil                   = diesel,
          other_non_renewable_sources  = outras_fontes_nao_renovaveis
        )
    }
  }


  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
