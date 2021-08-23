#' @title CEMPRE - Central Register of Companies
#'
#'
#' @description Loads information on companies and other organizations and their respective formally constituted local units, registered with the CNPJ - National Register of Legal Entities. Data is available from 2006 to 2019. See \url {https://sidra.ibge.gov.br/pesquisa/cempre/tabelas}
#'
#'  @encoding UTF-8
#'
#'   @param dataset A dataset name ("cempre").
#'   @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#'   @param geo_level A \code{string} that defines the geographic level of the data. Defaults to national level, but can be one of "country", "state" or "municipality". See documentation of \code{sidrar}.
#'   @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#'   @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("en") are supported. Defaults to "en".
#'   @param sectors A \code{boolean} that defines if the data will be return separated by sectors (\code{TRUE}) or not (\code{FALSE})
#'   @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE})
#'
#'   @return A \code{tibble} with a panel of N x T observations by municipality-year
#'
#'
#' @examples
#' \dontrun{
#' # download treated data from 2006 to 2019
#' cempre_all <- load_cempre(dataset = "cempre", raw_data = FALSE, time_period = 2006:2019)
#'
#' # download raw data from 2007 to 2016
#' raw_cempre_all <- load_cempre(dataset = "cempre", raw_data = TRUE, time_period = 2006:2019)
#' }
#'
#' @importFrom magrittr %>%
#'  @export load_cempre

load_cempre <- function(dataset = "cempre", raw_data = FALSE,
                        geo_level = "municipality",
                        time_period = 2017:2018,
                        language = "eng", sectors = FALSE,
                        legal_amazon_only = FALSE) {


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$raw_data = raw_data
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

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}
  if (legal_amazon_only & geo_level != "municipality"){stop('legal_amazon_only = TRUE is only available for geo_level = "municipality".')}


  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  if (sectors & geo_level == "municipality"){warning("This may take too long")}

  ## Download separate by sectors
  if (sectors) {
    cnaes <- list("117897", "116830", "116880", "116910", "117296",
                  "117307", "117329", "117363", "117484", "117543",
                  "117555", "117608", "117666", "117673", "117714",
                  "117774", "117788", "117810", "117838", "117861",
                  "117888", "117892")

    year_cnaes <- purrr::cross2(as.character(param$time_period), cnaes)

    dat = year_cnaes %>%
      purrr::map(function(year_cnae) {
        #suppressMessages(
        sidra_download(sidra_code = param$code,
                       year = year_cnae[[1]],
                       geo_level = param$geo_level,
                       classific = c("C12762"),
                       category = list(year_cnae[[2]]))
        #)
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

  ## Download only the total
  } else {
    cnaes <- list("117897")

    dat = as.list(as.character(param$time_period)) %>%
      purrr::map(function(year_num){
        #suppressMessages(
        sidra_download(sidra_code = param$code,
                       year = year_num,
                       geo_level = param$geo_level,
                       classific = c("C12762"),
                       category = cnaes)
        #)
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  }


  ## Filter for Legal Amazon
  if (legal_amazon_only) {
    legal_amazon_filtered <- legal_amazon %>% dplyr::filter(AMZ_LEGAL == 1)

    dat <- dat %>%
      dplyr::filter(municipio_codigo %in% unique(legal_amazon_filtered$CD_MUN))
  }


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}


}