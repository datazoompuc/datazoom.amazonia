#' @title Greenhouse gases emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases of Brazilian cities
#'
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_seeg
#'
#' @examples \dontrun{datazoom.amazonia::load_seeg()}
#'

load_seeg <- function(dataset = "seeg", raw_data = FALSE,
                      geo_level = "municipality",
                      language = "eng"){

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

  if (param$geo_level == "municipality"){message("Please follow the steps from `googledrive` package to download the data. This may take a while.")}

  dat <- external_download(dataset = param$dataset,
                           source = 'seeg',
                           geo_level = param$geo_level)


  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}

}
