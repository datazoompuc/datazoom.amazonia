#' @title DATASUS - No longer available
#'
#' @description The \code{load_datasus} function is no longer part of the package.
#' Updates on the matter coming soon.
#' It is kept only for compatibility and will return a warning when called.
#'
#' @param dataset Ignored. Kept only for compatibility.
#' @param raw_data Ignored. Kept only for compatibility.
#' @param time_period Ignored. Kept only for compatibility.
#' @param language Ignored. Kept only for compatibility.
#'
#' @return \code{NULL}. Always returns empty.
#'
#' @export
load_datasus <- function(dataset = NULL, raw_data = NULL, time_period = NULL,
                         language = "eng") {
  .Defunct(
    msg = "The `load_datasus` function is no longer part of the package and is no longer available. The functionalities are already available in the beta version of the new package Datazoom Saude, currently under development. You can access it and download it via GitHub: https://github.com/datazoompuc/datazoom.saude ."
  )
}
