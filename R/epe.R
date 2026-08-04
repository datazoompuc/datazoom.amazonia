#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class or Industrial Sector
#'
#' @param dataset Dataset name: "consumer_energy_consumption", "industrial_energy_consumption", "national_energy_balance", or "energy_state_panel"
#' @param geo_level Geographical level: "state" or "subsystem". Only applies to consumer or industrial datasets.
#' @inheritParams load_baci
#'
#' @return A \code{list} of tibbles (if \code{raw_data} = \code{TRUE}) or a tibble (if \code{raw_data} = \code{FALSE}).
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

  . <- ano_tag <- conta <- grupo <- tipo <- bloco <- fonte <- valor <- ano <- account <- year <- value <- data <- total_produzido <- hidro <- eolica <- solar <- nuclear <- termo <- cana <- lenha <- lixivia <- outras_fontes_renovaveis <- carvao_vapor <- gas_natural <- gas_de_coqueira <- combustivel <- diesel <- outras_fontes_nao_renovaveis <- amz_legal <- state <- legal_amazon <- total_produced <- hydro <- wind <- other_renewable_sources <- steam_coal <- natural_gas <- coke_oven_gas <- fuel_oil <- diesel_oil <- other_non_renewable_sources <- NULL

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
    # single sheet: the manifest points at EPE's consolidated BEN table
    # (grupo/tipo/fonte/ano/valor, already long-format, 1970-current) instead
    # of the old one-sheet-per-year workbook -- see the Data Engineering
    # section below for why that removed most of this dataset's cleaning code
    sheets <- dataset_field(param$source, param$dataset, "sheet")
  }
  if (param$dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption")) {
    # sheet name per geo_level -- one manifest override row per (dataset,
    # geo_level), the same pattern already used for MapBiomas (see
    # inst/extdata/manifest/v1/datasets_link.csv)
    sheets <- dataset_field(param$source, param$dataset, "sheet", geo_level = param$geo_level)
  }

  if (param$dataset == "energy_state_panel") {
    sheets <- dataset_field(param$source, param$dataset, "sheet")
  }

  if (param$dataset %in% c("consumer_energy_consumption", "industrial_energy_consumption")) {
    # The EPE source file (Dados_abertos_Consumo_Mensal.xlsx) declares a corrupted
    # "count" attribute in xl/sharedStrings.xml (~2^32), which makes readxl abort
    # with "vector::reserve". openxlsx builds the shared strings from the actual
    # entries and is not affected, so it is used to read these datasets.

    temp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp), add = TRUE)

    utils::download.file(
      url = dataset_url(source = param$source, dataset = param$dataset),
      destfile = temp,
      mode = "wb"
    )

    dat <- list(
      openxlsx::read.xlsx(
        temp,
        sheet = sheets,
        detectDates = TRUE
      )
    )
  } else {
    dat <- external_download(
      source = param$source,
      dataset = param$dataset,
      sheet = sheets
    )
  }

  names(dat) <- sheets

  # returning raw data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "national_energy_balance") {
    # the consolidated table (see this file's header) already comes
    # long-format, one row per (grupo, tipo, fonte, ano) -- unlike the old
    # one-sheet-per-year workbook, there is no header-row surgery or
    # pivot_longer() left to do; only rename "grupo" to match this
    # dataset's existing account-column name and coerce types (readxl
    # leaves ano/valor as character/numeric depending on cell formatting).

    dat <- dat[[1]] %>%
      dplyr::rename(conta = grupo) %>%
      dplyr::mutate(
        ano = as.integer(ano),
        valor = suppressWarnings(as.numeric(valor))
      )
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
          "type" = tipo,
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
