#' @title PIB MUNICIPAL - Municipal GDP
#'
#' @description Loads information on gross domestic product at current prices, taxes, net of subsidies, on products at current prices and gross value added at current prices, total and by economic activity, and respective shares. Survey is done at Country, state and municipality level and data is available from 2002 to 2018.
#'
#' @param dataset A dataset name ("pibmunic") with Municipal GDP information. You can also use SIDRA codes (See \url{https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas})
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE}).
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' # download state raw data from 2012 for all country
#' pibmunic <- load_pibmunic(dataset = 'pibmunic',
#'                           raw_data = TRUE,
#'                           geo_level = 'state',
#'                           time_period = 2012,
#'                           legal_amazon_only = FALSE)
#' }


load_pibmunic <- function(dataset = "pibmunic", raw_data,
                          geo_level, time_period,
                          language = "eng",
                          legal_amazon_only = FALSE) {

  sidra_code <- available_time <- AMZ_LEGAL <- municipio_codigo <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language

  if (!is.numeric(param$dataset)){
    param$code = datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {param$code = param$dataset}

  ## Check if year is acceptable

  year_check = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>% as.character() %>%
    stringr::str_split(pattern = '-') %>%
    unlist() %>% as.numeric()

  if (min(time_period) < year_check[1]){stop('Provided time period less than supported. Check documentation for time availability.')}
  if (max(time_period) > year_check[2]){stop('Provided time period greater than supported. Check documentation for time availability.')}
  if (legal_amazon_only & geo_level != "municipality"){stop('legal_amazon_only = TRUE is only available for geo_level = "municipality".')}


  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  dat = as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num){
      #suppressMessages(
      sidra_download(sidra_code = param$code,
                     year = year_num,
                     geo_level = param$geo_level)
      #)
    }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()


  ## Filter for Legal Amazon
  if (legal_amazon_only) {
    legal_amazon_filtered <- legal_amazon %>% dplyr::filter(AMZ_LEGAL == 1)

    dat <- dat %>%
      dplyr::filter(municipio_codigo %in% unique(legal_amazon_filtered$CD_MUN))
  }


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}


}
