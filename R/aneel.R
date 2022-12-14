#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("ccc")
#' @param language Only available in Portuguese ("pt") as of now
#' @inheritParams load_baci
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
#' @export

load_aneel <- function(dataset = "ccc", raw_data = FALSE, time_period = 2013:2022,
                       language = "pt") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  Ano <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$time_period <- time_period

  dat <- external_download(
    source = "ANEEL",
    dataset = param$dataset,
    year = param$time_period
  )
  if (raw_data == TRUE) {
    dat <- "No raw data available."
  }

  dat <- dplyr::filter(dat, ano %in% param$time_period)

  return(dat)
}
