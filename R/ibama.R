#' @title IBAMA - Brazilian Institute for the Environment and Renewable Natural Resources
#'
#' @description Loads information on environmental fines in the Amazon region
#'
#' @param dataset A dataset name ("embargoed_areas", "distributed_fines", or "collected_fines")
#' @inheritParams load_baci
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM"). Does not apply to the "embargoed_areas" dataset.
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
  acao_fiscalizatoria <- amazonia_legal <- cod_tipo_bioma <- cod_uf_tad <- cpf_cnpj_embargado <- NULL
  dat_embargo <- dat_impressao <- dat_ult_alter_geom <- data_embargo <- data_impressao <- NULL
  data_ult_alter_geom <- des_infracao <- des_localizacao <- des_tad <- hora_embargo <- NULL
  hora_impressao <- hora_ult_alter_geom <- nome_pessoa_embargada <- num_auto_infracao <- num_processo <- NULL
  num_tad <- ordem_fiscalizacao <- qtd_area_desmatada <- qtd_area_embargada <- ser_auto_infracao <- NULL
  ser_tad <- sit_desmatamento <- state <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data
  param$states <- states

  if (states == "all") {
    param$states <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN",
                      "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC",
                      "RS", "MS", "MT", "GO", "DF")
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

  # data come without municipality codes, only their names. +so we turn everything lowercase and remove accents to try and match.
  dat <- dat %>%
      dplyr::mutate(
        name_muni = municipio,
        # removing accents from municipality names
        dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
      ) %>%
      dplyr::mutate(dplyr::across(municipio, tolower)) # making all municipality names lowercase

    geo <- datazoom.amazonia::municipalities %>%
      dplyr::select(
        code_muni,
        "municipio" = name_muni,
        "uf" = abbrev_state,
        legal_amazon
      )

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

    # Merging with IBGE municipalities
    dat <- dat %>%
      dplyr::left_join(geo, by = c("municipio", "uf"))


    if (dataset %in% c("collected_fines", "distributed_fines")) {

      # Changing dates to date format
      dat <- dat %>%
        dplyr::mutate(
          dplyr::across(dplyr::any_of(c("data_auto", "data_pagamento")), as.Date, format = "%d/%m/%Y")
        ) %>%
        dplyr::mutate(
          dplyr::across(dplyr::where(is.character), ~dplyr::na_if(.x, "")),
          dplyr::across(ultima_atualizacao_relatorio, as.POSIXct, format = "%d/%m/%Y %H:%M")
        ) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("valor"), .fns = ~ gsub("[.]", "", .x))) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("valor"), ~ gsub("[,]", ".", .x) %>% as.numeric()))

    }

    if (dataset == "embargoed_areas") {

      ## Minor corrections
      dat <- dat %>%
        tidyr::separate(col = dat_embargo, into = c("data_embargo", "hora_embargo"), sep = " ") %>%
        tidyr::separate(col = dat_impressao, into = c("data_impressao", "hora_impressao"), sep = " ") %>%
        tidyr::separate(col = dat_ult_alter_geom, into = c("data_ult_alter_geom", "hora_ult_alter_geom"), sep = " ") %>%
        suppressWarnings() %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(.x, "")),
                      data_embargo = as.Date(data_embargo, "%d/%m/%Y"),
                      data_impressao = as.Date(data_impressao, "%d/%m/%Y"),
                      data_ult_alter_geom = as.Date(data_ult_alter_geom, "%d/%m/%Y"),
                      dplyr::across(dplyr::starts_with("qtd"), ~ gsub("[,]", ".", .x) %>% as.numeric())
                      )

    }

    dat <- dat %>%
      tibble::as_tibble()

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (dataset == "embargoed_areas") {
    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::select(-municipio) %>%
        dplyr::select(-cod_uf_tad) %>%
        dplyr::rename(municipio = name_muni,
                      cod_municipio = code_muni,
                      amazonia_legal = legal_amazon) %>%
        dplyr::relocate(c(municipio, cod_municipio, amazonia_legal), .before = uf)
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::select(-municipio) %>%
        dplyr::select(-cod_uf_tad) %>%
        dplyr::rename(municipality = name_muni,
                      municipality_code = code_muni,
                      num_tad = num_tad,
                      ser_tad = ser_tad,
                      embargo_date = data_embargo,
                      embargo_time = hora_embargo,
                      embargoed_person_name = nome_pessoa_embargada,
                      embargoed_cpf_cnpj = cpf_cnpj_embargado,
                      process_number = num_processo,
                      tad_description = des_tad,
                      state = uf,
                      location_description = des_localizacao,
                      embargoed_area_amount = qtd_area_embargada,
                      deforestation_status = sit_desmatamento,
                      last_geom_update_date = data_ult_alter_geom,
                      last_geom_update_time = hora_ult_alter_geom,
                      infringement_number = num_auto_infracao,
                      infringement_series = ser_auto_infracao,
                      deforested_area_amount = qtd_area_desmatada,
                      biome_type_code = cod_tipo_bioma,
                      fiscal_action = acao_fiscalizatoria,
                      fiscal_order = ordem_fiscalizacao,
                      infringement_description = des_infracao,
                      print_date = data_impressao,
                      print_time = hora_impressao) %>%
        dplyr::relocate(c(municipality, municipality_code, legal_amazon), .before = state)
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
