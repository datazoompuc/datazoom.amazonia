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
    warning("A fun\u00e7\u00e3o `load_datasus` n\u00e3o faz mais parte do pacote e n\u00e3o est\u00e1 mais dispon\u00vel. As funcionalidades ser\u00E3o integradas no novo pacote chamado Datazoom Sa\u00FAde, atualmente em desenvolvimento.",
            call. = FALSE)
  } else {
    warning("The `load_datasus` function is no longer part of the package and is no longer available. The functionalities will be explored in a new package called Datazoom Saude, currently under development.",
            call. = FALSE)
  }
  return(invisible(NULL))
}
