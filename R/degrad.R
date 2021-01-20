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
#' load_degrad(2016)
#'
#' load_degrad(
#'   c(2015, 2016),
#'   aggregation_level = "state",
#'   language = "pt"
#' )
#'
#' \dontrun{
#' load_degrad(
#'   "~/Downloads",
#'   aggregation_level = "state",
#'   language = "en"
#' )
#'
#' load_degrad(
#'   "~/Downloads/degrad2016_final_shp/DEGRAD_2016_pol.shp",
#'   aggregation_level = "municipality",
#'   language = "pt"
#' )
#' }
load_degrad <- function(source, aggregation_level = "municipality", language = "eng") {
  raw_list <- load_degrad_raw(source)

  # Optimize?
  message("Downloading map data.")
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE) # 2019 relates to the definition of legal_amazon
  geo_amazon <- dplyr::filter(geo_br, .data$code_muni %in% legal_amazon$CD_MUN)

  list_df <- lapply(raw_list, treat_degrad_data, aggregation_level = aggregation_level, language = language, geo_amazon = geo_amazon)

  dplyr::bind_rows(list_df)
}

#' Loads degradation data from INPE.
#'
#' @param source A number of different sources are supported:
#'
#' Passing a numeric \code{vector} of years will download the corresponding data from the INPE website. Available years are 2007-2016.
#'
#' Passing a \code{string} with a directory's path will read data from all shapefiles in the directory.
#'
#' Alternatively, \code{source} may be a list of full file paths, or anything else readable by \code{sf::read_sf()}.
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
      utils::download.file(url, zfile)
      dir <- gsub(zfile, pattern = "\\.zip", replacement = "")
      utils::unzip(zfile, exdir = dir)
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

treat_degrad_data <- function(df, aggregation_level, language, geo_amazon) {
  message("Processing data. This should take a few minutes.")
  # Clean
  names(df) <- tolower(names(df))
  df <- dplyr::filter(df, grepl(.data$class_name, pattern = "degrad", ignore.case = TRUE))

  # Insert CRS
  if(is.na(sf::st_crs(df))) {
    data_crs <- sf::st_crs("+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")
    sf::st_crs(df) <- data_crs
  }

  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
  df <- sf::st_make_valid(sf::st_transform(df, operation_crs))
  geo_amazon <- sf::st_transform(geo_amazon, operation_crs)

  # Extract date
  if(grepl(df$class_name[1], pattern = "\\d{4}$")) {
    year <- as.numeric(substr(df$class_name[1], 7, 10))
    month <- NA
  }
  else if(any(grepl(colnames(df), pattern = "ano"))) {
    year <- df[["ano"]][1]
    month <- NA
  }
  else {
    year <- as.numeric(substr(as.character(as.Date(df$view_date)), 1, 4))
    month <- as.numeric(substr(as.character(as.Date(df$view_date)), 6, 7))
  }
  df$Ano <- year
  df$Mes <- month

  # Municipalize
  sf::st_agr(df) = "constant"
  sf::st_agr(geo_amazon) = "constant"
  df <- sf::st_intersection(df, geo_amazon) %>%
    dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
    dplyr::group_by(.data$code_muni, .data$name_muni, .data$code_state, .data$abbrev_state, .data$Ano, .data$Mes) %>%
    sf::st_drop_geometry() %>%
    dplyr::summarise(Area = sum(.data$calculated_area)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Municipio = .data$name_muni, Estado = .data$abbrev_state)

  # Set aggregation level
  aggregation_level <- tolower(aggregation_level)
  if (aggregation_level == "state") {
    df <-
      df %>%
      dplyr::mutate(CodIBGE = as.factor(.data$code_state)) %>%
      dplyr::group_by(.data$Estado, .data$CodIBGE, .data$Ano, .data$Mes) %>%
      dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("code_muni", "code_state"))
  }
  else {
    if (aggregation_level != "municipality") {
      warning("Aggregation level not supported. Proceeding with Municipality.")
    }

    df <- dplyr::mutate(df, CodIBGE = as.factor(.data$code_muni)) %>%
      dplyr::select(-c("code_muni", "code_state"))
  }


  # Set language
  language <- tolower(language)
  if (language == "eng") {
    df <- translate_degrad_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }

  df
}

translate_degrad_to_english <- function(df) {
  dplyr::rename_with(
    df,
    dplyr::recode,
    Municipio = "Municipality",
    CodIBGE = "CodIBGE",
    Estado = "State",
    Ano = "Year",
    Mes = "Month"
  )
}
