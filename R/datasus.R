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
  if (language == "pt") {
    warning("A fun\u00e7\u00e3o `load_datasus` n\u00e3o faz mais parte do pacote e n\u00e3o est\u00e1 mais dispon\u00edvel. Novidades em breve.",
            call. = FALSE)
  } else {
    warning("The `load_datasus` function is no longer part of the package and is no longer available. Updates on the matter coming soon.",
            call. = FALSE)
  }
  return(invisible(NULL))
}
