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

  # GDP data
  gdp <- tibble::as_tibble(
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
  # Population data
  pop <- tibble::as_tibble(
    sidrar::get_sidra(
      6579, # Table code at Sidra
      period = as.character(years),
      geo = rep("City", times = length(states)),
      geo.filter = stats::setNames(states, rep("State", times = length(states))),
      classific = "all",
      category = NULL
    )
  )
  df <- dplyr::full_join(
    gdp,
    pop,
    by = c(
      "Munic\u00edpio (C\u00f3digo)",
      "Munic\u00edpio",
      "Ano (C\u00f3digo)",
      "Ano"
    )
  )

  df <- df[df[["Munic\u00edpio (C\u00f3digo)"]] %in% legal_amazon$CD_MUN, ]

  if (tolower(aggregation_level) == "state") {
    df <- df %>%
      dplyr::rename(CD_MUN = .data[["Munic\u00edpio (C\u00f3digo)"]]) %>%
      dplyr::mutate(CD_MUN = as.numeric(.data$CD_MUN)) %>%
      dplyr::full_join(legal_amazon, by = "CD_MUN") %>%
      dplyr::group_by(.data$CD_UF, .data$Ano, .data$NM_UF) %>%
      dplyr::summarise(
        PIB = sum(.data$Valor)
      ) %>%
      dplyr::mutate(CodIBGE = as.factor(.data$CD_UF)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("CD_UF")) %>%
      dplyr::rename(Estado = .data$NM_UF)
  }
  else {
    if (tolower(aggregation_level) != "municipality") warning("Aggregation level is not supported. Proceeding with municipality")

    df <- df %>%
      dplyr::select("Munic\u00edpio (C\u00f3digo)", "Munic\u00edpio", "Ano", "Valor.x", "Valor.y") %>%
      dplyr::rename(CodIBGE = .data[["Munic\u00edpio (C\u00f3digo)"]], PIB = .data$Valor.x, Pop = .data$Valor.y) %>%
      dplyr::mutate(CodIBGE = as.factor(CodIBGE))
  }

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets GDP at current prices
#'
#' @inheritParams load_amazon_gdp
#' @return A \code{tibble}.
#'
#' @export
#'
#' @examples
#' load_gdp(2017)
load_gdp <- function(years, aggregation_level = "municipality", language = "eng") {
  if (tolower(aggregation_level) == "state") {
    df <- tibble::as_tibble(
      sidrar::get_sidra(
        5938, # Table code at Sidra
        period = as.character(years),
        variable = 37,
        geo = "State",
        classific = "all",
        category = NULL
      )
    ) %>%
      dplyr::select("Unidade da Federa\u00e7\u00e3o (C\u00f3digo)", "Unidade da Federa\u00e7\u00e3o", "Ano", "Valor") %>%
      dplyr::rename(
        PIB = .data$Valor,
        Estado = .data[["Unidade da Federa\u00e7\u00e3o"]],
        CodIBGE = .data[["Unidade da Federa\u00e7\u00e3o (C\u00f3digo)"]]
      )
  }
  else {
    if (tolower(aggregation_level) != "municipality") warning("Aggregation level is not supported. Proceeding with municipality")

    df <- tibble::as_tibble(
      sidrar::get_sidra(
        5938, # Table code at Sidra
        period = as.character(years),
        variable = 37,
        geo = "City",
        classific = "all",
        category = NULL
      )
    ) %>%
      dplyr::select("Munic\u00edpio (C\u00f3digo)", "Munic\u00edpio", "Ano", "Valor") %>%
      dplyr::rename(
        PIB = .data$Valor,
        CodIBGE = .data[["Munic\u00edpio (C\u00f3digo)"]]
      )
  }

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

translate_munics_to_english <- function(df) {
  dplyr::rename_with(
    df,
    dplyr::recode,
    "Munic\u00edpio" = "Municipality",
    "Ano" = "Year",
    "PIB" = "GDP",
    "Estado" = "State"
  )
}
