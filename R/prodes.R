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
  list_dat <- dat

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

   treat_prodes_data = function(df, language) {

     ## Bind Global Variables

     cod_ibge <- nr <- lat <- long <- latgms <- longms <- NULL
     estado <- soma <- area_km2 <- NULL
     cod_uf <- NULL
     cod_munic_ibge <- NULL
     ano <- NULL
     desmatado <- NULL
     incremento <- NULL
     floresta <- NULL
     nuvem <- NULL
     nao_observado <- NULL
     nao_floresta <- NULL
     hidrografia <- NULL

     ####################
     ## Data Carpentry ##
     ####################

     df = df %>%
       janitor::clean_names() %>%
       # Adds column with UF codes, eg. MA = 21
       dplyr::mutate(cod_uf = as.factor(base::substr(cod_ibge,start=1,stop=2))) %>%
       dplyr::rename(cod_munic_ibge = cod_ibge)

     ###############################
     ## Extract Year From Columns ##
     ###############################

     # Removes year from column name
     colnames(df) <- gsub("(.*)\\d{4}?", "\\1", colnames(df))

     #################
     ## Translation ##
     #################

     language <- tolower(language)

     if (language == 'eng'){
       df = df %>%
         dplyr::select(
           year = ano,
           nr, lat, lon = long, latgms, longms,
           state_code = cod_uf,
           municipality_code = cod_munic_ibge,
           area_km2,
           deforestation = desmatado,
           increment = incremento,
           forest = floresta,
           cloud = nuvem,
           not_observed = nao_observado,
           not_forest = nao_floresta,
           hydrography = hidrografia,
           sum = soma
           )
     }

     if (language == "pt") {
       df = df %>%
         dplyr::select(
           ano,
           nr, lat, lon = long, latgms, longms,
           cod_uf,
           cod_municipio = cod_munic_ibge,
           area_km2,
           desmatado,
           incremento,
           floresta,
           nuvem,
           nao_observado,
           nao_floresta,
           hidrografia,
           soma
         )
     }

     return(df)
   }

  dat_mod <- purrr::map(list_dat, ~treat_prodes_data(.x, param$language)) %>%
    dplyr::bind_rows()

  return(dat_mod)

}

