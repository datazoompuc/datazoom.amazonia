#' @title SIGA
#'
#' @description ANEEL's Generation Information System
#'
#' @param dataset A dataset name ("siga").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated data
#' clean_siga <- load_siga(
#'   dataset = "SIGA",
#'   raw_data = FALSE,
#' )
#' }
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export

load_siga <- function(dataset, raw_data = FALSE, time_period = NA,
                       language = "pt", geo_level = "Municipality"){

  ###########################
  ## Bind Global Variables ##
  ###########################



  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$language <- language
  param$time_period <- time_period

  dat <- external_download(
    source = "SIGA",
    dataset = param$dataset,
    year = param$time_period
  )

  if(raw_data == TRUE){
    dat <- "No raw data available."
  }

  return(dat)
}
