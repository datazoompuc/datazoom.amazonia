#' @title IMAZON
#'
#' @description Loads information on ...  See \url{http://www.ipsamazonia.org.br/}
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Only "municipality" available.
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
load_imazon <- function(dataset = "imazon_shp", raw_data = TRUE,
                        geo_level = "municipality", language = "pt") {

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
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data



  if (param$geo_level == "state" | param$raw_data == FALSE) {
    stop("This dataset is only available for geo_level = 'municipality' and raw_data = TRUE")
  }

  if (param$geo_level == "municipality") {
    message("Please follow the steps from `googledrive` package to download the data. This may take a while.")
    message("IMPORTANT: In order to download the files, you must accept the requirements from the package.
          Don´t worry! It won´t change or affect data that belongs to your personal account.")
  }


  dat <- external_download(
    dataset = param$dataset,
    source = "imazon_shp",
    geo_level = param$geo_level
  )

  return(dat)
}
