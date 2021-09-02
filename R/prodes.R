#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads information on clearcut deforestation in the Legal Amazon and annual deforestation rates in the region. Survey is done at state or municipality level and data is available from 2000 to 2020.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("prodes").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download raw data from 2000 to 2020
#' raw_prodes_all <- load_prodes(dataset = "prodes",
#'                               raw_data = TRUE,
#'                               time_period = 2000:2020)
#' }
#'
#'
#' @importFrom magrittr %>%
#' @export

load_prodes <- function(dataset = "prodes", raw_data,
                        #geo_level,
                        time_period,
                        language = 'eng') {

  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$raw_data = raw_data
  #param$geo_level = geo_level
  param$time_period = time_period
  param$language = language
  #param$time_id = time_id

  param$survey_name = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ###################
  ## Download Data ##
  ###################

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}

  ## Column Names come with numbers at the side - we need to clean those

  dat = as.list(param$time_period) %>%
    purrr::map(
      function(t){external_download(dataset = param$dataset,source='prodes',year = t) %>%
          dplyr::mutate(ano = t)}
    )

  ## Include function that treats the data here

  dat = dat %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ######################
  ## Data Engineering ##
  ######################

  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

  ## Change Data Type

  dat = dat %>%
    dplyr::mutate_if(is.numeric,as.double)


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}



  # ---------------------------------------------------------------------

  # treat_prodes_data = function(df, geo_level, language) {
  #
  #   ## Bind Global Variables
  #
  #   cod_ibge <- NULL
  #   estado <- NULL
  #   cod_uf <- NULL
  #   cod_munic_ibge <- NULL
  #   ano <- NULL
  #   desmatado <- NULL
  #   incremento <- NULL
  #   floresta <- NULL
  #   nuvem <- NULL
  #   nao_observado <- NULL
  #   nao_floresta <- NULL
  #   hidrografia <- NULL
  #
  #   ####################
  #   ## Data Carpentry ##
  #   ####################
  #
  #   geo_level <- tolower(geo_level)
  #
  #   df = df %>%
  #     #dat_mod = df %>%
  #     janitor::clean_names() %>%
  #     # Adds column with UF codes, eg. MA = 21
  #     dplyr::mutate(cod_uf = as.factor(base::substr(cod_ibge,start=1,stop=2))) %>%
  #     dplyr::rename(cod_munic_ibge = cod_ibge) %>%
  #     # Removes unnecessary columns - under review
  #     dplyr::select(-c('lat','long','latgms','longms','municipio','nr','soma'))
  #
  #   if (geo_level == "state") {
  #
  #     df = df %>%
  #       dplyr::group_by(estado,cod_uf) %>%
  #       # Collapses data by state
  #       dplyr::summarize_if(is.numeric, sum, na.rm = TRUE)
  #   }
  #
  #   if (geo_level == "municipality") {
  #     df = df %>%
  #       janitor::clean_names() %>%
  #       dplyr::mutate(cod_munic_ibge = as.factor(cod_munic_ibge)) %>%
  #       dplyr::relocate(cod_uf,.after=estado)
  #   }
  #
  #   if (!(geo_level %in% c('municipality','state'))){
  #
  #     stop("Aggregation level not supported. Please see documentation.")
  #
  #   }
  #
  #   ###############################
  #   ## Extract Year From Columns ##
  #   ###############################
  #
  #   df$ano =
  #     colnames(df) %>%
  #     purrr::detect(function(x) startsWith(x, "des")) %>%
  #     gsub(pattern = ".*(\\d{4}).*", replacement = "\\1") %>%
  #     as.numeric()
  #
  #
  #   # Removes year from column name
  #   colnames(df) <- gsub("(.*)\\d{4}?", "\\1", colnames(df))
  #
  #   df = df %>%
  #     dplyr::relocate(ano,.after=cod_munic_ibge)
  #
  #   #################
  #   ## Translation ##
  #   #################
  #
  #   language <- tolower(language)
  #
  #   if (language == 'eng'){
  #     df = df %>%
  #       dplyr::rename(
  #         #munic_code_ibge = cod_munic_ibge,
  #         state = estado,
  #         deforestation = desmatado,
  #         increment = incremento,
  #         forest = floresta,
  #         cloud = nuvem,
  #         not_observed = nao_observado,
  #         not_forest = nao_floresta,
  #         hydrography = hidrografia,
  #         year = ano)
  #   } else if (language != "pt") {
  #     warning("Selected language is not supported. Proceeding with Portuguese.")
  #   }
  #
  #
  #   return(df)
  # }

}

