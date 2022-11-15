#' @title SIGA
#'
#' @description ANEEL's Generation Information System
#'
#' @param dataset A dataset name ("siga")
#' @param language Only available in Portuguese ("pt") as of now
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data
#' clean_siga <- load_siga()
#' }
#'
#' @export

load_siga <- function(dataset = "siga", raw_data = FALSE, language = "pt") {
  
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

  dat <- external_download(
    source = "SIGA",
    dataset = param$dataset
  )

  if (raw_data == TRUE) {
    dat <- "No raw data available."
  }

  return(dat)
}
