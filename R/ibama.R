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
  month <- year <- municipality_code <- n_infracoes <- n_ja_julgado <- NULL
  n_cpf_cnpj_unicos <- NULL

  ##############################
  ## Define Basic Parameters ##
  ##############################

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

  list_dat = dat

  ## Filter for Legal Amazon
  if (legal_amazon_only) {
    legal_amazon_filtered <- legal_amazon %>% dplyr::filter(AMZ_LEGAL == 1)

    dat <- dat %>%
      dplyr::filter(codigo_ibge_municipio_embargo %in% unique(legal_amazon_filtered$CD_MUN))
  }


  colnames(dat) = c("numero_TAD", "serie_TAD", "area_ha", "numero_A_I", "nome_ou_razao_social",
                    "CPF_ou_CNPJ", "localizacao_do_imovel", "uf_infracao", "municipio_infracao",
                    "municipio_infrator", "bairro", "endereco", "julgamento",
                    "infracao", "data_insercao_lista")


  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}


  treat_ibama_data = function(df, language) {

    numero_TAD <- n_infringement <- n_unique_cpf_cnpj<- NULL
    serie_TAD <- AMZ_LEGAL <- NULL
    julgamento <- infracao <-localizacao_do_imovel <- data_insercao_lista <- NULL
    CPF_ou_CNPJ <- ano <- mes <- area_ha <- numero_A_I <- bairro <- endereco <- NULL
    month <- year <-n_already_judged<- n_infracoes <-nome_ou_razao_social <- n_ja_julgado <- NULL
    n_CPF_CNPJ_unicos <- uf_infracao <- municipio_infracao <- municipio_infrator <- NULL




    ## Aggregate to year/month-level
    df <- df %>%
      dplyr::select(
        municipio_infracao, uf_infracao,
        julgamento,
        infracao, data_insercao_lista,
        CPF_ou_CNPJ
      ) %>%
      dplyr::mutate(
        julgamento = ifelse(julgamento == "pendente de julgamento", FALSE, TRUE),
        data_insercao_lista = lubridate::dmy(data_insercao_lista),
        ano = lubridate::year(data_insercao_lista),
        mes = lubridate::month(data_insercao_lista)
      ) %>%
      dplyr::group_by(ano, mes) %>%
      dplyr::summarise(
        n_ja_julgado = sum(!is.na(julgamento), na.rm = TRUE),
        n_infracoes = dplyr::n(),
        n_CPF_CNPJ_unicos = length(unique(CPF_ou_CNPJ)),
        municipio_infracao,
        .groups = "drop"
      )

    language = tolower(language)

    if (language == "pt"){

      df = df %>%
        dplyr::select(ano, mes, municipio_infracao,
                      n_ja_julgado, n_infracoes, n_CPF_CNPJ_unicos
        ) %>%
        dplyr::arrange(ano, mes)

    }

    if (language == "eng"){

      df = df %>%
        dplyr::select(year = ano, month = mes,
                      n_already_judged = n_ja_julgado,
                      n_infringement = n_infracoes,
                      n_unique_cpf_cnpj = n_CPF_CNPJ_unicos,
                      city = municipio_infracao
        ) %>%
        dplyr::arrange(year, month)

    }

    return(df)
  }

  dat_mod <- treat_ibama_data(dat, language)

  return(dat_mod)

}
