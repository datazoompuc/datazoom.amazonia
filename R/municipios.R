#' @importFrom rlang .data
NULL

#' Gets GDP at current prices and (estimated) population data for the Brazilian Legal Amazon
#'
#' @inheritParams load_gdp
#' @return A \code{tibble} with GDP (in thousands), Population and GDP Per Capita (in units).
#'
#' @export
#'
#' @examples
#' load_amazon_gdp(2017)
load_amazon_gdp <- function(years, space_aggregation = "municipality", language = "eng") {
  filter_amazon(
    load_gdp(years, space_aggregation = space_aggregation, language = language),
    space_aggregation = space_aggregation
  )
}

#' Gets GDP at current prices and (estimated) population data for Brazil
#'
#' @param years A numeric vector with years of interest. Supported years 2002-2017 for GDP data and 2001-2009, 2011-present for population data.
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with GDP (in thousands), Population and GDP Per Capita (in units).
#'
#' @export
#'
#' @examples
#' load_gdp(2017)
load_gdp <- function(years, space_aggregation = "municipality", language = "eng") {
  # GDP data
  gdp <- tibble::as_tibble(
    sidrar::get_sidra(
      5938, # Table code at Sidra
      period = as.character(years),
      variable = 37,
      geo = translate_for_api(space_aggregation),
      classific = "all",
      category = NULL
    )
  ) %>% dplyr::distinct()
  # Population data
  pop <- tibble::as_tibble(
    sidrar::get_sidra(
      6579, # Table code at Sidra
      period = as.character(years),
      geo = translate_for_api(space_aggregation),
      classific = "all",
      category = NULL
    )
  ) %>% dplyr::distinct()

  df <- dplyr::full_join(
    gdp,
    pop,
    by = c(
      paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
      translate_for_response(space_aggregation),
      "Ano (C\u00f3digo)",
      "Ano"
    )
  )

  df <- df %>%
    dplyr::select(
      paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
      translate_for_response(space_aggregation),
      "Ano",
      "Valor.x",
      "Valor.y"
    ) %>%
    dplyr::rename_with(function(cols) "CodIBGE", dplyr::ends_with("digo)")) %>% # ends with (Codigo)
    dplyr::rename_with(
      dplyr::recode,
      "Valor.x" = "PIB",
      "Valor.y" = "Pop",
      "Unidade da Federa\u00e7\u00e3o" = "Estado"
    ) %>%
    dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets formal employment data for the Brazilian Legal Amazon
#'
#' @inheritParams load_employment
#' @return A \code{tibble} with (total) Salary (in R$), (number of) Employed people and (number of) Firms.
#'
#' @export
#'
#' @examples
#' load_amazon_employment(2017)
load_amazon_employment <- function(years, space_aggregation = "municipality", language = "eng") {
  filter_amazon(
    load_employment(years, space_aggregation = space_aggregation, language = language),
    space_aggregation = space_aggregation
  )
}

#' Gets formal employment data for Brazil.
#'
#' @param years A numeric vector with years of interest. Supported years 2002-2017 for GDP data and 2001-2009, 2011-present for population data.
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with (total) Salary (in R$), (number of) Employed people and (number of) Firms.
#'
#' @export
#'
#' @examples
#' load_employment(2017)
load_employment <- function(years, space_aggregation = "municipality", language = "eng") {
  # Employment data
  salaries <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 662,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()
  employed <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 707,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()
  firms <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 2585,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()

  by <- c(
    paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
    translate_for_response(space_aggregation),
    "Ano (C\u00f3digo)",
    "Ano"
  )
  df <- salaries %>%
    dplyr::full_join(
      employed,
      by
    ) %>%
    dplyr::full_join(
      firms,
      by
    )

  df <- df %>%
    dplyr::select(
      paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
      translate_for_response(space_aggregation),
      "Ano",
      "Valor.x",
      "Valor.y",
      "Valor"
    ) %>%
    dplyr::rename_with(function(cols) "CodIBGE", dplyr::ends_with("digo)")) %>% # ends with (Codigo)
    dplyr::rename_with(
      dplyr::recode,
      "Valor.x" = "Salario",
      "Valor.y" = "Empregados",
      "Valor" = "Firmas",
      "Unidade da Federa\u00e7\u00e3o" = "Estado"
    )

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets census data for Brazil.
#'
#' @param years A numeric vector with years of interest. Supported years 2002-2017 for GDP data and 2001-2009, 2011-present for population data.
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with (total) Salary (in R$), (number of) Employed people and (number of) Firms.
#'
#' @export
#'
#' @examples
#' load_census(2017)
load_census <- function(years, space_aggregation = "municipality", language = "eng") {
  # Employment data
  salaries <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 662,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()
  employed <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 707,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()
  firms <- tibble::as_tibble(
    sidrar::get_sidra(
      6449, # Table code at Sidra
      period = as.character(years),
      variable = 2585,
      geo = translate_for_api(space_aggregation),
      classific = "C12762",
      category = list("117897")
    )
  ) %>% dplyr::distinct()

  by <- c(
    paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
    translate_for_response(space_aggregation),
    "Ano (C\u00f3digo)",
    "Ano"
  )
  df <- salaries %>%
    dplyr::full_join(
      employed,
      by
    ) %>%
    dplyr::full_join(
      firms,
      by
    )

  df <- df %>%
    dplyr::select(
      paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
      translate_for_response(space_aggregation),
      "Ano",
      "Valor.x",
      "Valor.y",
      "Valor"
    ) %>%
    dplyr::rename_with(function(cols) "CodIBGE", dplyr::ends_with("digo)")) %>% # ends with (Codigo)
    dplyr::rename_with(
      dplyr::recode,
      "Valor.x" = "Salario",
      "Valor.y" = "Empregados",
      "Valor" = "Firmas",
      "Unidade da Federa\u00e7\u00e3o" = "Estado"
    )

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

filter_amazon <- function(df, space_aggregation) {
  columns <- colnames(df)

  code <- if (tolower(space_aggregation) == "state") "CD_UF" else "CD_MUN"
  legal_amazon <- legal_amazon %>%
    dplyr::mutate(CodIBGE = as.character(.data[[code]]))

  df %>%
    dplyr::left_join(legal_amazon, by = "CodIBGE") %>%
    dplyr::filter(.data$AMZ_LEGAL == 1) %>%
    dplyr::select(columns) %>%
    unique()
}

translate_for_api <- function(s) {
  if (tolower(s) == "municipality") {
    "City"
  } else {
    "State"
  }
}

translate_for_response <- function(s) {
  if (tolower(s) == "municipality") {
    "Munic\u00edpio"
  } else {
    "Unidade da Federa\u00e7\u00e3o"
  }
}

translate_munics_to_english <- function(df) {
  df %>%
    dplyr::rename_with(function(cols) "Municipality", dplyr::starts_with("Munic")) %>%
    dplyr::rename_with(
      dplyr::recode,
      "Ano" = "Year",
      "Estado" = "State",
      "PIB" = "GDP",
      "PIBpc" = "GDPpc",
      "Salario" = "Salary",
      "Empregados" = "Employed",
      "Firmas" = "Firms"
    )
}
