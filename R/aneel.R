#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("CCC").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016 (takes a long time to download)
#' clean_aneel <- load_aneel(
#'   raw_data = FALSE,
#'   time_period = 2016
#' )
#' }
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export

load_aneel <- function(dataset, raw_data = FALSE, time_period = 2013:2022,
                     language = "pt", geo_level = "Country"){

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
    source = "ANEEL",
    dataset = param$dataset,
    year = param$time_period
  )
  if(raw_data == TRUE){
    dat <- "No raw data available."
  }

  dat <- filter(dat, Ano == param$time_period)

  return(dat)
}
