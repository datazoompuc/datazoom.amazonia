#' @importFrom rlang .data
NULL

#' Gets GDP at current prices for the Brazilian Legal Amazon
#'
#' @param years A numeric vector with years of interest. Supported years are 2002-2017.
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble}.
#'
#' @export
#'
#' @examples
#' load_amazon_gdp(2017)
load_amazon_gdp <- function(years, aggregation_level = "municipality", language = "eng") {
  states <- unique(legal_amazon$CD_UF)

  df <- tibble::as_tibble(
    sidrar::get_sidra(
      5938, # Table code at Sidra
      period = as.character(years),
      variable = 37,
      geo = rep("City", times = length(states)),
      geo.filter = stats::setNames(states, rep("State", times = length(states))),
      classific = "all",
      category = NULL
    )
  )

  df <- df[df[["Munic\u00edpio (C\u00f3digo)"]] %in% legal_amazon$CD_MUN, ] %>%
    dplyr::rename(CD_MUN = .data[["Munic\u00edpio (C\u00f3digo)"]]) %>%
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>%
    dplyr::full_join(legal_amazon, by = "CD_MUN") %>%
    dplyr::mutate(CodIBGE = as.factor(CD_MUN))

  if (tolower(aggregation_level) == "state") {
    df <- df %>%
      dplyr::group_by(.data$CD_UF, .data$Ano, .data$NM_UF) %>%
      dplyr::summarise(
        PIB = mean(.data$Valor)
      ) %>%
      dplyr::mutate(CodIBGE = as.factor(CD_UF)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("CD_UF")) %>%
      dplyr::rename(Estado = NM_UF)

    if (language == "eng") {
      df <- dplyr::rename(
        df,
        State = Estado,
        Year = Ano,
        GDP = PIB
      )
    }
    else if (language != "pt") {
      warning("Selected language is not supported. Proceeding with Portuguese.")
    }

    df
  }
  else {
    if (tolower(aggregation_level) != "municipality") warning("Aggregation level is not supported. Proceeding with municipality")

    df <- df %>%
      dplyr::select("CodIBGE", "Munic\u00edpio", "Ano", "Valor") %>%
      dplyr::rename(PIB = .data$Valor)

    if (language == "eng") {
      df <- dplyr::rename(
        df,
        County = "Munic\u00edpio",
        Year = Ano,
        GDP = PIB
      )
    }
    else if (language != "pt") {
      warning("Selected language is not supported. Proceeding with Portuguese.")
    }

    df
  }
}
