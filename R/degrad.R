#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Loads and cleans degradation data from INPE.
#'
#' @inheritParams load_degrad_raw
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "Municipality" or
#'   "State".
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @return A \code{tibble}.
#'
#' @seealso [load_degrad_raw] for loading raw data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_degrad(2018)
#'
#' load_degrad(
#'   c(2017, 2018),
#'   aggregation_level = "state",
#'   language = "pt"
#' )
#'
#' load_degrad(
#'   system.file("extdata", package = "datazoom.amazonia"),
#'   aggregation_level = "state",
#'   language = "en"
#' )
#'
#' load_degrad(
#'   system.file("extdata", "DesmatamentoMunicipios2015.txt", package = "datazoom.amazonia"),
#'   aggregation_level = "municipality",
#'   language = "pt"
#' )
# load_degrad <- function(source, aggregation_level = "municipality", language = "eng") {
#   raw_list <- load_degrad_raw(source)
#
#   list_df <- lapply(raw_list, treat_degrad_data, aggregation_level = aggregation_level, language = language)
#
#   dplyr::bind_rows(list_df)
# }

#' Loads or downloads degradation data from INPE.
#'
#' @param source A number of different sources are supported:
#'
#' Passing a numeric \code{vector} of years will download the corresponding data from the INPE website. Available years are 2007-2016.
#'
#' Passing a \code{string} with a directory's path will read data from all files named
#'   "\{path\}/DesmatamentoMunicipiosXXXX.txt".
#'
#' Alternatively, \code{source} may be a list of full file paths, or anything else readable by \code{utils::read.csv()}.
#'
#' @return A list of \code{tibble}.
#'
#' @seealso [load_degrad] for loading and treating the data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_degrad_raw(2016)
#'
#' load_degrad_raw(c(2013, 2014, 2016))
#'
#' \dontrun{
#' load_degrad_raw("~/Downloads")
#'
#' load_degrad_raw("~/Downloads/degrad2016_final_shp/DEGRAD_2016_pol.shp")
#' }
load_degrad_raw <- function(source) {
  # If source is a list of numbers, we retrieve data from INPE
  if (is.numeric(source)) {
    source <- purrr::map(source, function(year) {
      url <- paste0(
        "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad/arquivos/degrad",
        year,
        "_final_shp.zip"
      )

      zfile <- tempfile(fileext = ".zip")
      download.file(url, zfile)
      dir <- gsub(zfile, pattern = "\\.zip", replacement = "")
      unzip(zfile, exdir = dir)
      find_from_dir(dir)
    })
  }
  # If source is a directory, we expand and filter the list of files
  else if (is.character(source) && length(source) == 1 && dir.exists(source)) {
    source <- find_from_dir(source)
  }
  # Otherwise, we assume that source is something that can already be interpreted by sf::read_sf

  lapply(source, sf::read_sf)
}

find_from_dir <- function(dir) {
  list.files(path = dir, full.names = TRUE) %>%
    grep(pattern = "\\.shp", value = TRUE)
}
#
# treat_degrad_data <- function(df, aggregation_level, language) {
#   aggregation_level <- tolower(aggregation_level)
#   if (aggregation_level == "state") {
#     df <-
#       df %>%
#       dplyr::mutate(CodIBGE = as.factor(substr(.data$CodIbge, start = 1, stop = 2))) %>%
#       dplyr::select(-c("Latgms", "Lat", "Long", "Longms", "Municipio", "CodIbge")) %>%
#       dplyr::group_by(.data$Estado, .data$CodIBGE) %>%
#       dplyr::summarize_if(is.numeric, sum, na.rm = TRUE)
#   }
#   else if (aggregation_level == "municipality") {
#     df <- dplyr::mutate(df, CodIBGE = as.factor(.data$CodIbge)) %>% dplyr::select(-c("CodIbge"))
#   }
#   else {
#     warning("Aggregation level not supported. Proceeding with Municipality.")
#   }
#
#   # Get year from column names and put it into its own column
#   df$Ano <-
#     colnames(df) %>%
#     purrr::detect(function(x) startsWith(x, "Des")) %>%
#     gsub(pattern = ".*(\\d{4}).*", replacement = "\\1") %>%
#     as.numeric()
#
#
#   # Removes year from column name
#   colnames(df) <- gsub("(.*)\\d{4}?", "\\1", colnames(df))
#
#   language <- tolower(language)
#   if (language == "eng") {
#     df <- translate_degrad_to_english(df)
#   }
#   else if (language != "pt") {
#     warning("Selected language is not supported. Proceeding with Portuguese.")
#   }
#
#   # Removes useless columns
#   dplyr::select(df, -c("Nr", "Soma"))
# }
#
# translate_degrad_to_english <- function(df) {
#   dplyr::rename_with(
#     df,
#     dplyr::recode,
#     Municipio = "Municipality",
#     CodIBGE = "CodIBGE",
#     Estado = "State",
#     Incremento = "Increment",
#     Desmatado = "Deforested",
#     Floresta = "Forest",
#     Nuvem = "Cloud",
#     NaoObservado = "NotObserved",
#     NaoFloresta = "NotForest",
#     Hidrografia = "Hydrography",
#     Ano = "Year"
#   )
# }
