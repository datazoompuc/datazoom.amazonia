#' @title IBAMA - Brazilian Institute for the Environment and Renewable Natural Resources
#'
#' @description Loads information on environmental fines in the Amazon region
#'
#' @param dataset A dataset name ("embargoed_areas", "distributed_fines", or "collected_fines")
#' @inheritParams load_baci
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM"). Does not apply to the "areas_embargadas" dataset.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # Download treated embargoes data (raw_data = FALSE) in english (language = "eng")
#' data <- load_ibama(
#'   dataset = "embargoed_areas", raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # Download treated collected fines data from "BA"
#' data <- load_ibama(
#'   dataset = "collected_fines", raw_data = FALSE,
#'   states = "BA", language = "pt"
#' )
#' }
#'
#' @export

load_ibama <- function(dataset,
                       raw_data = FALSE,
                       states = "all",
                       language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- legal_amazon <- codigo_ibge_municipio_embargo <- NULL
  municipio_embargo <- uf_embargo <- julgamento <- infracao <- data_de_insercao_na_lista <- NULL
  cpf_ou_cnpj <- cod_municipio <- ano <- mes <- NULL
  month <- year <- municipality_code <- n_infracoes <- n_ja_julgado <- NULL
  n_cpf_cnpj_unicos <- NULL
  municipio_infracao <- uf_infracao <- uf <- NULL
  municipio <- name_muni <- code_muni <- municipality <- NULL
  data_auto <- abbrev_state <- ultima_atualizacao_relatorio <- tipo_auto <- valor_do_auto <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data
  param$states <- states

  if (states == "all") {
    param$states <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
  }
  if (dataset == "embargoed_areas") {
    param$states <- "all"
  }

  ##############
  ## Download ##
  ##############

  # Downloads each state separately

  dat <- param$states %>%
    purrr::imap(
      function(uf, iteration) {
        base::message(paste0("Downloading ", uf, " (", iteration, " out of ", length(param$states), ")"))

        external_download(
          source = "ibama",
          dataset = param$dataset,
          state = uf
        )
      }
    )

  names(dat) <- param$states

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()

  if (dataset %in% c("collected_fines", "distributed_fines")) {

    # data come without municipality codes, only their names. +so we turn everything lowercase and remove accents to try and match.

    dat <- dat %>%
      dplyr::mutate( # removing accents from municipality names
        dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
      ) %>%
      dplyr::mutate(dplyr::across(municipio, tolower)) # making all municipality names lowercase

    geo <- municipalities %>%
      dplyr::select(
        code_muni,
        name_muni,
        "uf" = abbrev_state,
        legal_amazon
      )

    # Removing accents from the dataset with IBGE codes to make the city names compatible and merge

    geo <- geo %>%
      dplyr::mutate(
        dplyr::across(
          name_muni,
          ~ stringi::stri_trans_general(., id = "Latin-ASCII"),
          .names = "municipio"
        )
      ) %>%
      dplyr::mutate(dplyr::across(municipio, tolower))

    # 41 municipalities don't match due to spelling inconsistencies

    dat <- dat %>%
      dplyr::mutate(
        municipio = dplyr::case_when(
          municipio == "sao luiz do anuaa" & uf == "RR" ~ "sao luiz",
          municipio == "eldorado dos carajas" & uf == "PA" ~ "eldorado do carajas",
          municipio == "santa isabel do para" & uf == "PA" ~ "santa izabel do para",
          municipio == "couto de magalhaes" & uf == "TO" ~ "couto magalhaes",
          municipio == "fortaleza do tabocao" & uf == "TO" ~ "tabocao",
          municipio == "pau d arco" & uf == "TO" ~ "pau d'arco",
          municipio == "sao valerio da natividade" & uf == "TO" ~ "sao valerio",
          municipio == "governador edson lobao" & uf == "MA" ~ "governador edison lobao",
          municipio == "assu" & uf == "RN" ~ "acu",
          municipio == "presidente juscelino" & uf == "RN" ~ "serra caiada",
          municipio == "olho-d'agua do borges" & uf == "RN" ~ "olho d'agua do borges",
          municipio == "augusto severo" & uf == "RN" ~ "campo grande",
          municipio == "campo de santana" & uf == "PB" ~ "tacima",
          municipio == "santarem" & uf == "PB" ~ "joca claudino",
          municipio == "sao domingos de pombal" & uf == "PB" ~ "sao domingos",
          municipio == "lagoa do itaenga" & uf == "PE" ~ "lagoa de itaenga",
          municipio == "iguaraci" & uf == "PE" ~ "iguaracy",
          municipio == "belem de sao francisco" & uf == "PE" ~ "belem do sao francisco",
          municipio == "amparo de sao francisco" & uf == "SE" ~ "amparo do sao francisco",
          municipio == "santa teresinha" & uf == "BA" ~ "santa terezinha",
          municipio == "muquem de sao francisco" & uf == "BA" ~ "muquem do sao francisco",
          municipio == "brasopolis" & uf == "MG" ~ "brazopolis",
          municipio == "passa-vinte" & uf == "MG" ~ "passa vinte",
          municipio == "sao thome das letras" & uf == "MG" ~ "sao tome das letras",
          municipio == "dona eusebia" & uf == "MG" ~ "dona euzebia",
          municipio == "justinopolis" & uf == "MG" ~ "ribeirao das neves",
          municipio == "trajano de morais" & uf == "RJ" ~ "trajano de moraes",
          municipio == "embu" & uf == "SP" ~ "embu das artes",
          municipio == "moji mirim" & uf == "SP" ~ "mogi mirim",
          municipio == "florinia" & uf == "SP" ~ "florinea",
          municipio == "biritiba-mirim" & uf == "SP" ~ "biritiba mirim",
          municipio == "sao luis do paraitinga" & uf == "SP" ~ "sao luiz do paraitinga",
          municipio == "grao para" & uf == "SC" ~ "grao-para",
          municipio == "presidente castelo branco" & uf == "SC" ~ "presidente castello branco",
          municipio == "passos de torres" & uf == "SC" ~ "passo de torres",
          municipio == "santana do livramento" & uf == "RS" ~ "sant'ana do livramento",
          municipio == "poxoreo" & uf == "MT" ~ "poxoreu",
          municipio == "colinas de goiais" & uf == "GO" ~ "colinas do sul",
          TRUE ~ municipio
        )
      )

    dat <- dat %>%
      dplyr::mutate(uf = dplyr::case_when(
        municipio == "ponte alta do norte" & uf == "GO" ~ "SC",
        TRUE ~ uf
      ))

    # Changing dates to date format

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("data_auto", "data_pagamento")), as.Date, format = "%d/%m/%Y")
      ) %>%
      dplyr::mutate(
        dplyr::across(ultima_atualizacao_relatorio, as.POSIXct, format = "%d/%m/%Y %H:%M")
      )
  }

  if (dataset == "embargoed_areas") {

    ## Aggregate to municipality-level
    dat <- dat %>%
      dplyr::select(
        municipio_infracao, uf_infracao, julgamento,
        infracao, data_de_insercao_na_lista,
        cpf_ou_cnpj
      ) %>%
      dplyr::mutate(
        julgamento = ifelse(julgamento == "pendente de julgamento", FALSE, TRUE),
        data_de_insercao_na_lista = lubridate::dmy(data_de_insercao_na_lista),
        ano = lubridate::year(data_de_insercao_na_lista),
        mes = lubridate::month(data_de_insercao_na_lista)
      ) %>%
      dplyr::rename(
        municipio = municipio_infracao,
        uf = uf_infracao
      ) %>%
      dplyr::group_by(uf, municipio, ano, mes) %>%
      dplyr::summarise(
        n_ja_julgado = sum(!is.na(julgamento), na.rm = TRUE),
        n_infracoes = dplyr::n(),
        n_cpf_cnpj_unicos = length(unique(cpf_ou_cnpj)),
        .groups = "drop"
      )

    geo <- municipalities %>%
      dplyr::select(
        code_muni,
        "municipio" = name_muni,
        "uf" = abbrev_state,
        legal_amazon
      )
  }

  ## Adding IBGE municipality codes

  municipalities <- municipalities %>%
    dplyr::select(
      municipio = name_muni,
      cod_municipio = code_muni
    )

  # Merging with IBGE municipalities

  dat <- dat %>%
    dplyr::left_join(geo, by = c("municipio", "uf"))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (dataset == "embargoed_areas") {
    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::select(
          ano, mes, uf, municipio, cod_municipio,
          n_ja_julgado, n_infracoes, n_cpf_cnpj_unicos
        ) %>%
        dplyr::arrange(ano, mes, municipio)
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::select(
          year = ano, month = mes, state = uf,
          municipality = municipio, municipality_code = cod_municipio,
          n_already_judged = n_ja_julgado,
          n_infringement = n_infracoes,
          n_unique_cpf_cnpj = n_cpf_cnpj_unicos
        ) %>%
        dplyr::arrange(year, month, municipality)
    }
  }

  if (dataset %in% c("collected_fines", "distributed_fines")) {
    dat <- dat %>%
      dplyr::select(-municipio) %>%
      dplyr::relocate(code_muni, name_muni, uf, data_auto)

    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::rename(
          "cod_municipio" = name_muni,
          "municipio" = name_muni,
          "penalidade" = tipo_auto,
          "data_penalidade" = data_auto,
          "valor_penalidade" = valor_do_auto,
          "amazonia_legal" = legal_amazon
        )
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::rename_with(dplyr::recode,
          code_muni = "municipality_code",
          name_muni = "municipality",
          uf = "state",
          data_auto = "penalty_date",
          no_ai = "report_number",
          nome_ou_razao_social = "name_or_corporate_name",
          status_debito = "debt_status",
          tipo_auto = "penalty",
          tipo_infracao = "infraction",
          enquadramento_legal = "legal_framework",
          valor_do_auto = "penalty_value",
          moeda = "currency",
          parcela = "installment",
          quantidade_de_parcelas = "number_of_installments",
          valor_base_da_parcela = "base_installment_value",
          valor_pago = "value_paid",
          data_pagamento = "payment_date",
          ultima_atualizacao_relatorio = "report_last_update"
        )
    }
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
