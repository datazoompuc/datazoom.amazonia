#' @title IBAMA - Brazilian Institute for the Environment and Renewable Natural Resources
#'
#' @description Loads information on environmental distributed or collected fines at the municipality or state levels considering the Amazon region
#'
#' @param dataset A dataset name ("distributed_fines", "collected_fines")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality".
#' @param uf A\code{string} that defines a specific state within Brazil. Can be only written with each state two-letter specification.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults for Portuguese.
#
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download distributed raw data from state Bahia
#' raw_ibama_multas <- load_ibama_multas(
#'   dataset = "distributed_fines",
#'   raw_data = TRUE,
#'   uf = "BA"
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @export


load_ibama_multas <- function(dataset = NULL, raw_data = FALSE,
                     geo_level = "municipality",
                     uf = NULL, language = "pt") {


  abbrev_state <- code_muni <- code_state <- data_auto <- data_pagamento <- NULL
  data_penalidade <- enquadramento_legal <- link <- moeda <- municipio <- name_muni <- NULL
  nome_ou_razao_social <- parcela <- quantidade_de_parcelas <- survey <- tipo_auto <- NULL
  tipo_infracao <- ultima_atualizacao_relatorio <- valor_base_da_parcela <- valor_do_auto <- valor_pago <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data
  param$uf = uf

  param$survey_name <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()


  ## Dataset

  if (is.null(param$dataset)) {
    stop("Missing Dataset!")
  }
  if (is.null(param$raw_data)) {
    stop("Missing TRUE/FALSE for Raw Data")
  }

  ##############
  ## Download ##
  ##############

  geo <- municipalities %>%
    dplyr::select(
      code_muni,
      "municipio" = name_muni,
      code_state,
      "uf" = abbrev_state
    )

  uf <- geo %>%
    dplyr::filter(
      uf == param$uf
    ) %>%
    dplyr::select(uf) %>%
    dplyr::distinct() %>%
    unlist()


  dat <- external_download(
    dataset = param$dataset,
    source = "ibama_multas",
    geo_level = param$geo_level,
    uf = param$uf
  )


  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  ## Return Raw Data
  if (param$raw_data) {
    return(dat)
  }


  ##############
  ## Cleaning ##
  ##############

  dat = dat %>%
    dplyr::rename(penalidade = tipo_auto,
                  data_penalidade = data_auto,
                  valor_da_penalidade = valor_do_auto) %>%
    dplyr::relocate(uf, municipio, data_penalidade)



  if(param$dataset %in% c("collected_fines", "distributed_fines") & param$language == "eng"){

    dat = dat %>%
      dplyr::rename(penalty = tipo_auto,
                    penalty_date = data_auto,
                    penaly_value = valor_do_auto,
                    state = uf,
                    city = municipio,
                    name_or_corportate_name = nome_ou_razao_social,
                    violation_type = tipo_infracao,
                    legal_framework = enquadramento_legal,
                    currency = moeda,
                    installment = parcela,
                    installments_amount = quantidade_de_parcelas,
                    base_installment_value = valor_base_da_parcela,
                    paid_value = valor_pago,
                    payment_date = data_pagamento,
                    report_last_update = ultima_atualizacao_relatorio)


  }
return(dat)
}
