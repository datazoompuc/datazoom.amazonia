#' @title BEN
#'
#' @description National Energy Balance
#'
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016
#' clean_ben <- load_ben(
#'   dataset = "EPE",
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

load_ben <- function(dataset, raw_data = FALSE, time_period = 2004:2021,
                     language = "pt") {
  ###########################
  ## Bind Global Variables ##
  ###########################



  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$time_period <- time_period

  dat <- external_download(
    source = "BEN",
    dataset = param$dataset,
    year = param$time_period
  )
  if (raw_data == TRUE) {
    dat <- "No raw data available"
  }
  return(dat)
}
