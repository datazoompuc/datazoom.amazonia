#' @title Greenhouse gases emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases
#'
#' @param dataset A dataset name ("seeg").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download state raw data
#' seeg <- load_seeg(dataset = 'seeg',
#'                   raw_data = TRUE,
#'                   geo_level = "state")
#' }

load_seeg <- function(dataset = "seeg", raw_data,
                      geo_level, language = "eng"){


  survey <- link <- NULL

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
