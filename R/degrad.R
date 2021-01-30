#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Loads and cleans degradation data from INPE.
#'
#' @inheritParams load_degrad_raw
#' @param space_aggregation A string that indicates the level of spatial aggregation of the data. It can be by "Municipality" or
#'   "State".
#' @param time_aggregation A string that indicates the level of temporal aggregation of the data. It can be by "Month" or
#'   "Year".
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#' @param all_events Set to TRUE if you want non-degradation data from the DEGRAD project.
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
#'   space_aggregation = "state",
#'   language = "pt"
#' )
#'
#' \dontrun{
#' load_degrad(
#'   "~/Downloads",
#'   space_aggregation = "state",
#'   time_aggregation = "year",
#'   language = "en"
#' )
#'
#' load_degrad(
#'   "~/Downloads/degrad2016_final_shp/DEGRAD_2016_pol.shp",
#'   time_aggregation = "municipality",
#'   language = "pt"
#' )
#' }
load_degrad <- function(source, space_aggregation = "municipality", time_aggregation = "year", language = "eng", all_events = FALSE) {
  raw_list <- load_degrad_raw(source)

  geo_amazon <- download_map()

  list_df <- lapply(raw_list, treat_degrad_data, space_aggregation = space_aggregation, time_aggregation = time_aggregation, language = language, geo_amazon = geo_amazon, filter = !all_events)

  dplyr::bind_rows(list_df)
}

#' Loads DEGRAD data from INPE.
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

  suppressWarnings(lapply(source, sf::read_sf))
}

download_map <- function() {
  message("Downloading map data.")
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE) # 2019 relates to the definition of legal_amazon
  dplyr::filter(geo_br, .data$code_muni %in% legal_amazon$CD_MUN)
}

find_from_dir <- function(dir) {
  list.files(path = dir, full.names = TRUE) %>%
    grep(pattern = "\\.shp", value = TRUE)
}

treat_degrad_data <- function(df, space_aggregation, time_aggregation, language, geo_amazon, filter) {
  message("Processing data. This should take a few minutes.")
  # Clean
  names(df) <- tolower(names(df))

  if(filter) {
    df <- dplyr::filter(df, grepl("degrad", .data$class_name, ignore.case = TRUE))
  }

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

  df[grep(df$class_name, pattern = "degrad[^_]", ignore.case = TRUE), ]$class_name <- "DEGRAD"

  # Insert CRS
  if(is.na(sf::st_crs(df))) {
    data_crs <- sf::st_crs("+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")
    sf::st_crs(df) <- data_crs
  }

  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
  df <- sf::st_make_valid(sf::st_transform(df, operation_crs))
  geo_amazon <- sf::st_transform(geo_amazon, operation_crs)

  # Municipalize
  sf::st_agr(df) = "constant"
  sf::st_agr(geo_amazon) = "constant"
  df <- sf::st_intersection(df, geo_amazon) %>%
    dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
    dplyr::mutate(CodIBGE = "") %>%
    dplyr::group_by(.data$code_muni, .data$name_muni, .data$code_state, .data$abbrev_state, .data$Ano, .data$Mes, .data$class_name, .data$CodIBGE) %>%
    sf::st_drop_geometry()

  # Set aggregation level
  space_aggregation <- tolower(space_aggregation)
  if (space_aggregation == "state") {
    df <- df %>%
      dplyr::ungroup(.data$code_muni, .data$name_muni) %>%
      dplyr::mutate(CodIBGE = as.factor(.data$code_state)) %>%
      dplyr::rename(Estado = .data$abbrev_state, Evento = .data$class_name)
  }
  else {
    if (space_aggregation != "municipality") {
      warning("Spatial aggregation level not supported. Proceeding with Municipality.")
    }

    df <- df %>%
      dplyr::mutate(CodIBGE = as.factor(.data$code_muni)) %>%
      dplyr::rename(Municipio = .data$name_muni, Estado = .data$abbrev_state, Evento = .data$class_name)
  }

  time_aggregation <- tolower(time_aggregation)
  if (time_aggregation == "year") {
    df <- dplyr::ungroup(df, .data$Mes)
  }
  else if (time_aggregation != "month") {
    warning("Temporal aggregation level not supported. Proceeding with Month.")
  }

  if(filter) {
    df <- dplyr::ungroup(df, .data$Evento)
  }

  df <- df %>%
    dplyr::summarise(Area = sum(.data$calculated_area)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("code_muni", "code_state"))

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
    Mes = "Month",
    Evento = "Event"
  )
}
