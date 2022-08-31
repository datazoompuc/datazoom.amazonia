#' @title IMAZON
#'
#' @description Loads information on ...  See \url{http://www.ipsamazonia.org.br/}
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese is supported.
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples \dontrun{
#' # download raw data from 2014
#' imazon <- load_imazon(dataset = "imazon_shp")
#' }
load_imazon <- function(dataset = "imazon_shp", raw_data = TRUE, language = "pt") {

  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  dat <- external_download(
    dataset = param$dataset,
    source = "imazon_shp"
  )

  return(dat)
}
