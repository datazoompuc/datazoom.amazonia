#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Loads and cleans deforestation data from INPE.
#'
#' @inheritParams load_prodes_raw
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "municipality" or
#'   "state".
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble}.
#'
#' @seealso [load_prodes_raw] for loading raw data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_prodes(2018)
#'
#' load_prodes(
#'   c(2017, 2018),
#'   aggregation_level = "state",
#'   language = "pt"
#' )
#'
#' load_prodes(
#'   system.file("extdata", package = "dz.amazonia"),
#'   aggregation_level = "state",
#'   language = "en"
#' )
#'
#' load_prodes(
#'   system.file("extdata", "DesmatamentoMunicipios2015.txt", package = "dz.amazonia"),
#'   aggregation_level = "municipality",
#'   language = "pt"
#' )
load_prodes <- function(source, aggregation_level = "municipality", language = "eng") {
  raw_list <- load_prodes_raw(source)

  list_df <- lapply(raw_list, treat_data, aggregation_level = aggregation_level, language = language)

  dplyr::bind_rows(list_df)
}

#' Loads and cleans the deforestation data from INPE in your computer from a specified directory.
#'
#' @param source A number of different sources are supported:
#'
#' Passing a numeric \code{vector} of years will download the corresponding data from the INPE website.
#'
#' Passing a \code{string} with a directory's path will read data from all files named
#'   "\{path\}/DesmatamentoMunicipiosXXXX.txt".
#'
#' Alternatively, \code{source} may be a list of full file paths, or anything else readable by \code{utils::read.csv()}.
#'
#' @return A list of \code{tibble}.
#'
#' @seealso [load_prodes] for loading and treating the data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_prodes_raw(2017)
#'
#' load_prodes_raw(c(2016, 2017, 2018))
#'
#' load_prodes_raw(system.file("extdata", package = "dz.amazonia"))
#'
#' load_prodes_raw(
#'   system.file("extdata", "DesmatamentoMunicipios2015.txt", package = "dz.amazonia")
#' )
load_prodes_raw <- function(source) {
  # If source is a list of numbers, we construct the URLs to INPE
  if (is.numeric(source)) {
    source <- purrr::map(source, function(year) {
      paste0(
        "http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=",
        year,
        "&estado=&ordem=MUNICIPIO&type=tabela&output=txt&"
      )
    })
  }
  # If source is a directory, we expand and filter the list of files
  else if (is.character(source) && length(source) == 1 && dir.exists(source)) {
    source <-
      list.files(path = source, full.names = TRUE) %>%
      grep(pattern = "DesmatamentoMunicipios\\d{4}.txt", value = TRUE)
  }
  # Otherwise, we assume that source is something that can already be interpreted by read.csv()

  # Useful as integrity check on database
  csv_types <- readr::cols("d", "d", "d", "c", "c", "c", "d", "c", "d", "d", "d", "d", "d", "d", "d", "d", "d")
  lapply(source, readr::read_csv, col_types = csv_types, locale = readr::locale(encoding = "latin1"))
}

treat_data <- function(df, aggregation_level, language) {
  aggregation_level <- tolower(aggregation_level)
  if (aggregation_level == "state") {
    df <-
      df %>%
      dplyr::select(-c("Latgms", "Lat", "Long", "Longms", "Municipio", "CodIbge")) %>%
      dplyr::group_by(.data$Estado) %>%
      dplyr::summarize_if(is.numeric, sum, na.rm = TRUE)
  }
  else if (aggregation_level != "municipality") {
    warning("Aggregation level not supported. Proceeding with Municipality.")
  }

  # Get year from column names and put it into its own column
  df$Ano <-
    colnames(df) %>%
    purrr::detect(function(x) startsWith(x, "Des")) %>%
    gsub(pattern = ".*(\\d{4}).*", replacement = "\\1") %>%
    as.numeric()


  # Removes year from column name
  colnames(df) <- gsub("(.*)\\d{4}?", "\\1", colnames(df))

  language <- tolower(language)
  if (language == "eng") {
    df <- translate_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  # Removes useless columns
  dplyr::select(df, -c("Nr", "Soma"))
}

translate_to_english <- function(df) {
  dplyr::rename_with(
    df,
    dplyr::recode,
    Municipio = "Municipality",
    CodIbge = "IBGECode",
    Estado = "State",
    Incremento = "Increment",
    Desmatado = "Deforested",
    Floresta = "Forest",
    Nuvem = "Cloud",
    NaoObservado = "NotObserved",
    NaoFloresta = "NotForest",
    Hidrografia = "Hydrography",
    Ano = "Year"
  )
}