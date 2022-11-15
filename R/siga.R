#' @title SIGA
#'
#' @description ANEEL's Generation Information System
#'
#' @inheritParams load_baci
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
    source = "SIGA",
    dataset = param$dataset,
    year = param$time_period
  )

  if (raw_data == TRUE) {
    dat <- "No raw data available."
  }

  return(dat)
}
