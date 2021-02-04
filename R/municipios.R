#' @importFrom rlang .data
NULL

#' Gets GDP at current prices and Population data for the Brazilian Legal Amazon
#'
#' @param years A numeric vector with years of interest. Supported years 2002-2017 for GDP data and 2001-2009, 2011-present for population data.
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with GDP (in thousands), Population and GDP Per Capita (in units).
#'
#' @export
#'
#' @examples
#' \dontrun{load_amazon_gdp(2017)}
load_amazon_gdp <- function(years, aggregation_level = "municipality", language = "eng") {
  states <- legal_amazon %>%
    dplyr::filter(.data$AMZ_LEGAL == 1)

  states <- unique(states$CD_UF)

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
  ) %>% dplyr::distinct() # Necessary for some misterious reason
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
  ) %>% dplyr::distinct()
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

  if (tolower(aggregation_level) == "state") {
    df <- df %>%
      dplyr::rename(CD_MUN = .data[["Munic\u00edpio (C\u00f3digo)"]]) %>%
      dplyr::mutate(CD_MUN = as.numeric(.data$CD_MUN)) %>%
      dplyr::left_join(legal_amazon, by = "CD_MUN") %>%
      dplyr::filter(.data$AMZ_LEGAL == 1) %>%
      dplyr::select(-.data$AMZ_LEGAL) %>%
      dplyr::group_by(.data$CD_UF, .data$Ano, .data$NM_UF) %>%
      dplyr::summarise(
        PIB = sum(.data$Valor.x),
        Pop = sum(.data$Valor.y),
      ) %>%
      dplyr::mutate(CodIBGE = as.factor(.data$CD_UF)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("CD_UF")) %>%
      dplyr::rename(Estado = .data$NM_UF) %>%
      dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)
  }
  else {
    if (tolower(aggregation_level) != "municipality") warning("Aggregation level is not supported. Proceeding with municipality")

    amz <- legal_amazon %>%
      dplyr::select(.data$CD_MUN, .data$AMZ_LEGAL) %>%
      dplyr::mutate(CD_MUN = as.character(.data$CD_MUN))

    df <- df %>%
      dplyr::rename(CodIBGE = .data[["Munic\u00edpio (C\u00f3digo)"]]) %>%
      dplyr::left_join(amz, by = c("CodIBGE" = "CD_MUN")) %>%
      dplyr::filter(.data$AMZ_LEGAL == 1) %>%
      dplyr::select(-.data$AMZ_LEGAL)

    df <- df %>%
      dplyr::select(.data$CodIBGE, "Munic\u00edpio", "Ano", "Valor.x", "Valor.y") %>%
      dplyr::rename(PIB = .data$Valor.x, Pop = .data$Valor.y) %>%
      dplyr::mutate(CodIBGE = as.factor(.data$CodIBGE)) %>%
      dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)
  }

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets GDP at current prices and Population data for Brazil
#'
#' @inheritParams load_amazon_gdp
#' @return A \code{tibble} with GDP (in thousands), Population and GDP Per Capita (in units).
#'
#' @export
#'
#' @examples
#' load_gdp(2017)
load_gdp <- function(years, aggregation_level = "municipality", language = "eng") {
  if (tolower(aggregation_level) == "state") {
    # GDP data
    gdp <- tibble::as_tibble(
      sidrar::get_sidra(
        5938, # Table code at Sidra
        period = as.character(years),
        variable = 37,
        geo = "State",
        classific = "all",
        category = NULL
      )
    ) %>% dplyr::distinct()
    # Population data
    pop <- tibble::as_tibble(
      sidrar::get_sidra(
        6579, # Table code at Sidra
        period = as.character(years),
        geo = "State",
        classific = "all",
        category = NULL
      )
    ) %>% dplyr::distinct()
    df <- dplyr::full_join(
      gdp,
      pop,
      by = c(
        "Unidade da Federa\u00e7\u00e3o (C\u00f3digo)",
        "Unidade da Federa\u00e7\u00e3o",
        "Ano (C\u00f3digo)",
        "Ano"
      )
    )

    df <- df %>%
      dplyr::select("Unidade da Federa\u00e7\u00e3o (C\u00f3digo)", "Unidade da Federa\u00e7\u00e3o", "Ano", "Valor.x", "Valor.y") %>%
      dplyr::rename(
        PIB = .data$Valor.x,
        Pop = .data$Valor.y,
        Estado = .data[["Unidade da Federa\u00e7\u00e3o"]],
        CodIBGE = .data[["Unidade da Federa\u00e7\u00e3o (C\u00f3digo)"]]
      ) %>%
      dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)
  }
  else {
    if (tolower(aggregation_level) != "municipality") warning("Aggregation level is not supported. Proceeding with municipality")

    # GDP data
    gdp <- tibble::as_tibble(
      sidrar::get_sidra(
        5938, # Table code at Sidra
        period = as.character(years),
        variable = 37,
        geo = "City",
        classific = "all",
        category = NULL
      )
    ) %>% dplyr::distinct()
    # Population data
    pop <- tibble::as_tibble(
      sidrar::get_sidra(
        6579, # Table code at Sidra
        period = as.character(years),
        geo = "City",
        classific = "all",
        category = NULL
      )
    ) %>% dplyr::distinct()
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

    df <- df %>%
      dplyr::select("Munic\u00edpio (C\u00f3digo)", "Munic\u00edpio", "Ano", "Valor.x", "Valor.y") %>%
      dplyr::rename(
        PIB = .data$Valor.x,
        Pop = .data$Valor.y,
        CodIBGE = .data[["Munic\u00edpio (C\u00f3digo)"]]
      ) %>%
      dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)
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
  df <- df %>%
    dplyr::rename_with(function(cols)"Municipality", dplyr::starts_with("Munic"))

  dplyr::rename_with(
    df,
    dplyr::recode,
    "Ano" = "Year",
    "PIB" = "GDP",
    "PIBpc" = "GDPpc",
    "Estado" = "State"
  )
}
