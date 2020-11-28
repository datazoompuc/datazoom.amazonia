#' @importFrom rlang .data
NULL

#' Gets GDP at current prices for the Brazilian Legal Amazon
#'
#' @param years A numeric vector with years of interest.
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "City" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble}.
#'
#' @export
#'
#' @examples
#' load_pib(2017)
load_pib <- function(years, aggregation_level = "City", language = "eng") {
  states <- dplyr::filter(legal_amazon, .data$label == "State")

  df <- tibble::as_tibble(
    sidrar::get_sidra(
      5938,
      period = as.character(years),
      variable = 37,
      geo = rep(aggregation_level, times = nrow(states)),
      geo.filter = stats::setNames(states$code, states$label),
      classific = "all",
      category = NULL
    )
  )

  if (aggregation_level == "City") {
    df <- df[df[["Munic\u00edpio (C\u00f3digo)"]] %in% legal_amazon$code, ] %>%
      dplyr::select("Munic\u00edpio (C\u00f3digo)", "Munic\u00edpio", "Ano", "Valor") %>%
      dplyr::rename(PIB = Valor)

    if (language == "eng") {
      colnames(df) <- c("City (code)", "City", "Year", "GDP")
    }
  }
  else {
    df <- df %>%
      dplyr::select("Unidade da Federa\u00e7\u00e3o (C\u00f3digo)", "Unidade da Federa\u00e7\u00e3o", "Ano", "Valor") %>%
      dplyr::rename(PIB = Valor)


    if (language == "eng") {
      colnames(df) <- c("State (code)", "State", "Year", "Variable", "GDP")
    }
  }

  df
}
