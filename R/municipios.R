#' @importFrom rlang .data
#' @importFrom rlang :=
NULL

#' Gets GDP at current prices and (estimated) population data for the Brazilian Legal Amazon
#'
#' @inheritParams load_gdp
#' @return A \code{tibble} with GDP (in thousands), Population and GDP Per Capita (in units).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_amazon_gdp(2017)
#' }
load_amazon_gdp <- function(years, space_aggregation = "municipality", language = "eng") {
  df <- filter_amazon(
    load_gdp(years, space_aggregation = "municipality", language = "pt")
  )

  space_aggregation <- tolower(space_aggregation)
  if (space_aggregation == "state") {
    df <- df %>%
      dplyr::mutate(CodIBGE = substr(.data$CodIBGE, 1, 2)) %>%
      dplyr::select(-c("PIBpc")) %>%
      dplyr::group_by(.data$Estado, .data$CodIBGE, .data$Ano) %>%
      dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PIBpc = .data$PIB / .data$Pop * 1000)
  }
  else if (space_aggregation != "municipality") {
    warning("Selected spatial aggregation is not supported. Proceeding with Municipality.")
  }

  language <- tolower(language)
  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
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
#' \dontrun{
#' load_gdp(2017)
#' }
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
    dplyr::rename_with(function(cols) "Estado", dplyr::starts_with("Unidade da ")) %>%
    dplyr::rename_with(
      dplyr::recode,
      "Valor.x" = "PIB",
      "Valor.y" = "Pop"
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
#' @return A \code{tibble} with (total) Salary (in R$1.000,00), (number of) Employed people and (number of) Firms.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_amazon_employment(2017)
#' }
load_amazon_employment <- function(years, sectors = FALSE, space_aggregation = "municipality", language = "eng") {
  df <- filter_amazon(
    load_employment(years, sectors = sectors, space_aggregation = "municipality", language = "pt")
  )

  space_aggregation <- tolower(space_aggregation)
  if (space_aggregation == "state") {
    df <- df %>%
      dplyr::mutate(CodIBGE = substr(.data$CodIBGE, 1, 2)) %>%
      dplyr::group_by(.data$Estado, .data$CodIBGE, .data$Ano) %>%
      dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
      dplyr::ungroup()
  }
  else if (space_aggregation != "municipality") {
    warning("Selected spatial aggregation is not supported. Proceeding with Municipality.")
  }

  language <- tolower(language)
  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets formal employment data for Brazil.
#'
#' @param years A numeric vector with years of interest. Supported years 2006-2018.
#' @param sectors If sectors is set to TRUE, loads data separated by economics sectors.
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with (total) Salary (in R$1.000,00), (number of) Employed people and (number of) Firms.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_employment(2017)
#' }
load_employment <- function(years, sectors = FALSE, space_aggregation = "municipality", language = "eng") {
  if (sectors) {
    cnaes <- list("117897", "116830", "116880", "116910", "117296", "117307", "117329", "117363", "117484", "117543", "117555", "117608", "117666", "117673", "117714", "117774", "117788", "117810", "117838", "117861", "117888", "117892")
  }
  else {
    cnaes <- list("117897")
  }

  # for data frame joins
  by <- c(
    paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
    translate_for_response(space_aggregation),
    "Ano"
  )

  # Employment data
  get_salaries <- function(cnae) {
    df <- tibble::as_tibble(
      sidrar::get_sidra(
        6449, # Table code at Sidra
        period = as.character(years),
        variable = 662,
        geo = translate_for_api(space_aggregation),
        classific = "C12762",
        category = list(cnae)
      )
    ) %>% dplyr::distinct()
    sector <- dplyr::pull(df, "Classifica\u00E7\u00E3o Nacional de Atividades Econ\u00F4micas (CNAE 2.0)")[1]
    df %>%
      dplyr::select(c(by, "Valor")) %>%
      dplyr::rename(!!sector := "Valor")
  }
  salaries <- purrr::map(cnaes, get_salaries) %>% purrr::reduce(dplyr::left_join, by = by)

  get_employed <- function(cnae) {
    df <- tibble::as_tibble(
      sidrar::get_sidra(
        6449, # Table code at Sidra
        period = as.character(years),
        variable = 707,
        geo = translate_for_api(space_aggregation),
        classific = "C12762",
        category = list(cnae)
      )
    ) %>% dplyr::distinct()
    sector <- dplyr::pull(df, "Classifica\u00E7\u00E3o Nacional de Atividades Econ\u00F4micas (CNAE 2.0)")[1]
    df %>%
      dplyr::select(c(by, "Valor")) %>%
      dplyr::rename(!!sector := "Valor")
  }
  employed <- purrr::map(cnaes, get_employed) %>% purrr::reduce(dplyr::left_join, by = by)

  get_firms <- function(cnae) {
    df <- tibble::as_tibble(
      sidrar::get_sidra(
        6449, # Table code at Sidra
        period = as.character(years),
        variable = 2585,
        geo = translate_for_api(space_aggregation),
        classific = "C12762",
        category = list(cnae)
      )
    ) %>% dplyr::distinct()
    sector <- dplyr::pull(df, "Classifica\u00E7\u00E3o Nacional de Atividades Econ\u00F4micas (CNAE 2.0)")[1]
    df %>%
      dplyr::select(c(by, "Valor")) %>%
      dplyr::rename(!!sector := "Valor")
  }
  firms <- purrr::map(cnaes, get_firms) %>% purrr::reduce(dplyr::left_join, by = by)

  salaries <- dplyr::rename_with(salaries, function(name) paste0("Salarios", " - ", name), !dplyr::any_of(by))
  employed <- dplyr::rename_with(employed, function(name) paste0("Empregados", " - ", name), !dplyr::any_of(by))
  firms <- dplyr::rename_with(firms, function(name) paste0("Firmas", " - ", name), !dplyr::any_of(by))

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
    dplyr::rename_with(function(cols) "CodIBGE", dplyr::ends_with("digo)")) %>% # ends with (Codigo)
    dplyr::rename_with(function(cols) "Estado", dplyr::starts_with("Unidade da "))

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets income data for the Brazilian Legal Amazon
#'
#' @inheritParams load_income
#' @return A \code{tibble} with Employed population, Mean_Income and Median_Income (in R$).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_amazon_income(2010)
#' }
load_amazon_income <- function(years, space_aggregation = "municipality", language = "eng") {
  df <- filter_amazon(
    load_income(years, space_aggregation = "municipality", language = "pt")
  )

  space_aggregation <- tolower(space_aggregation)
  if (space_aggregation == "state") {
    df <- df %>%
      dplyr::mutate(CodIBGE = substr(.data$CodIBGE, 1, 2)) %>%
      dplyr::group_by(.data$Estado, .data$CodIBGE, .data$Ano) %>%
      dplyr::summarize(
        Renda = sum(.data$Ocupados * .data$Renda_media),
        Ocupados = sum(.data$Ocupados)
      ) %>%
      dplyr::mutate(Renda_media = .data$Renda / .data$Ocupados) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("Renda"))
  }
  else if (space_aggregation != "municipality") {
    warning("Selected spatial aggregation is not supported. Proceeding with Municipality.")
  }

  language <- tolower(language)
  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

#' Gets income data for Brazil
#'
#' @param years A numeric vector with years of interest. Supported years are 2000, 2010 (census years).
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State"
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble} with Employed population, Mean_Income and Median_Income (in R$).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_income(2010)
#' }
load_income <- function(years, space_aggregation = "municipality", language = "eng") {
  # Income data
  people <- tibble::as_tibble(
    sidrar::get_sidra(
      2033, # Table code at Sidra
      period = as.character(years),
      variable = 841,
      geo = translate_for_api(space_aggregation),
      classific = "C2",
      category = list("6794")
    )
  ) %>% dplyr::distinct()
  mean_income <- tibble::as_tibble(
    sidrar::get_sidra(
      2033, # Table code at Sidra
      period = as.character(years),
      variable = 842,
      geo = translate_for_api(space_aggregation),
      classific = "C2",
      category = list("6794")
    )
  ) %>% dplyr::distinct()
  median_income <- tibble::as_tibble(
    sidrar::get_sidra(
      2033, # Table code at Sidra
      period = as.character(years),
      variable = 843,
      geo = translate_for_api(space_aggregation),
      classific = "C2",
      category = list("6794")
    )
  ) %>% dplyr::distinct()

  by <- c(
    paste0(translate_for_response(space_aggregation), " (C\u00f3digo)"),
    translate_for_response(space_aggregation),
    "Ano (C\u00f3digo)",
    "Ano"
  )
  df <- people %>%
    dplyr::full_join(
      mean_income,
      by
    ) %>%
    dplyr::full_join(
      median_income,
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
    dplyr::rename_with(function(cols) "Estado", dplyr::starts_with("Unidade da ")) %>%
    dplyr::rename_with(
      dplyr::recode,
      "Valor.x" = "Ocupados",
      "Valor.y" = "Renda_media",
      "Valor" = "Renda_mediana"
    )

  if (language == "eng") {
    df <- translate_munics_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

filter_amazon <- function(df) {
  columns <- colnames(df)

  legal_amazon <- legal_amazon %>%
    dplyr::mutate(CodIBGE = as.character(.data$CD_MUN))

  df %>%
    dplyr::left_join(legal_amazon, by = "CodIBGE") %>%
    dplyr::filter(.data$AMZ_LEGAL == 1) %>%
    dplyr::mutate(Estado = .data$SIGLA) %>%
    dplyr::select(columns, "Estado")
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
    dplyr::rename_with(translate_cnae, dplyr::contains(" - ")) %>%
    dplyr::rename_with(translate_munics_terms)
}

translate_cnae <- function(terms) {
  splitted <- strsplit(terms, " - ")

  metadata <- purrr::map_chr(splitted, 1)
  translated <- translate_munics_terms(metadata)
  translated_with_suffix <- paste0(translated, " - ")

  english_cnae <- c("Total", "A Agriculture, farming, forest production, fishing and aquiculture", "B Extractive industry", "C Manufacturing", "D Electricity and gas", "E Water, sewage, waste management and decontamination", "F Construction", "G Commerce; auto and motorcycle repair", "H Transportation, storage and mailing", "I Food and lodging", "J Information and communication", "K Finance, insurance and related services", "L Real estate", "M Professional, scientific and technical activities", "N Administration and related activities", "O Public administration, defense and social security", "P Education", "Q Human health and social services", "R Arts, culture, sports and recreation", "S Other service activities", "T Domestic services", "U International organizations and other extraterritorial institutions")

  mapply(paste0, translated_with_suffix, english_cnae)
}

translate_munics_terms <- function(terms) {
  dplyr::recode(terms,
    "Ano" = "Year",
    "Munic\u00EDpio" = "Municipality",
    "Estado" = "State",
    "PIB" = "GDP",
    "PIBpc" = "GDPpc",
    "Salarios" = "Salary",
    "Empregados" = "Employed",
    "Firmas" = "Firms",
    "Ocupados" = "Employed",
    "Renda_media" = "Mean_Income",
    "Renda_mediana" = "Median_Income"
  )
}
