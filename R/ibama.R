#' @title IBAMA - Brazilian Institute for the Environment and Renewable Natural Resources
#'
#' @description Loads information on on environmental fines at the municipality or state levels considering the Amazon region
#'
#' @param dataset A dataset name ("areas_embargadas")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data or not
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download raw data from all country
#' raw_ibama_all <- load_ibama(dataset = "areas_embargadas",
#'                             raw_data = TRUE,
#'                             legal_amazon_only = FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @export


load_ibama <- function(dataset = "areas_embargadas",
                       raw_data,
                       geo_level = "municipality",
                       language = "eng",
                       legal_amazon_only = FALSE){


  survey <- link <- AMZ_LEGAL <- codigo_ibge_municipio_embargo <- NULL
  municipio_embargo <- uf_embargo <- julgamento <- infracao <- data_de_insercao_na_lista <- NULL
  cpf_ou_cnpj <- cod_municipio <- ano <- mes <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$language = language
  param$raw_data = raw_data

  param$survey_name = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}


  ##############
  ## Download ##
  ##############

  dat <- external_download(dataset = param$dataset,
                           source = 'ibama',
                           geo_level = param$geo_level)


  ## Filter for Legal Amazon
  if (legal_amazon_only) {
    legal_amazon_filtered <- legal_amazon %>% dplyr::filter(AMZ_LEGAL == 1)

    dat <- dat %>%
      dplyr::filter(codigo_ibge_municipio_embargo %in% unique(legal_amazon_filtered$CD_MUN))
  }


  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}


  ## Aggregate to municipality-level
  dat <- dat %>%
    dplyr::select(
      municipio_embargo, uf_embargo,
      codigo_ibge_municipio_embargo, julgamento,
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
      municipio = municipio_embargo,
      uf = uf_embargo,
      cod_municipio = codigo_ibge_municipio_embargo
    ) %>%
    dplyr::mutate(cod_municipio = as.numeric(cod_municipio)) %>%
    dplyr::group_by(cod_municipio, ano, mes) %>%
    dplyr::summarise(
      n_ja_julgado = sum(!is.na(julgamento), na.rm = TRUE),
      n_infracoes = dplyr::n(),
      n_cpf_cnpj_unicos = length(unique(cpf_ou_cnpj)),
      .groups = "drop"
    )

}
