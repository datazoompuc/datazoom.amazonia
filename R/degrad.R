#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Loads and cleans degradation data from INPE.
#'
#' @inheritParams load_degrad_raw
#' @param geo_aggregation A string that indicates the level of spatial aggregation of the data. It can be by "Municipality" or
#'   "State".
#' @param time_period A string that indicates the level of temporal aggregation of the data. It can be by "Month" or
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
#' \dontrun{
#' load_degrad(2016)
#'
#' load_degrad(
#'   c(2015, 2016),
#'   geo_aggregation = "state",
#'   language = "pt"
#' )
#'
#' load_degrad(
#'   "~/Downloads",
#'   geo_aggregation = "state",
#'   time_period = "year",
#'   language = "en"
#' )
#'
#' load_degrad(
#'   "~/Downloads/degrad2016_final_shp/DEGRAD_2016_pol.shp",
#'   time_period = "municipality",
#'   language = "pt"
#' )
#' }
load_degrad <- function(source, geo_aggregation = "municipality", time_period = "year", language = "eng", all_events = FALSE) {
  # Downloading raw data
  raw_data <- load_degrad_raw(source)

  # Downloading municipal map from geobr filtered to legl amazon municipalities
  geo_amazon <- download_map()

  # Treating data according to parameters selected
  list_df <- lapply(raw_data, treat_degrad_data, geo_aggregation = geo_aggregation, time_period = time_period, language = language, geo_amazon = geo_amazon, filter = !all_events)

  dplyr::bind_rows(list_df)
}

#' Loads DEGRAD data from INPE.
#'
#' @param source A number of different sources are supported:
#'
#' Passing a numeric \code{vector} of years will download the corresponding DEGRAD datasets from INPE's website. Available versions are 2007-2016.
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
#' \dontrun{
#' load_degrad_raw(2016)
#'
#' load_degrad_raw(c(2013, 2014, 2016))
#'
#' load_degrad_raw("~/Downloads")
#'
#' load_degrad_raw("~/Downloads/degrad2016_final_shp/DEGRAD_2016_pol.shp")
#' }
load_degrad_raw <- function(source) {
  # If source is a list of numbers (years), we retrieve data from INPE
  if (is.numeric(source)) {
    source <- purrr::map(source, webscrapping_degrad)
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

  # Downloading municipal map from geobr
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE) # 2019 relates to the definition of legal_amazon

  # legal_amazon belongs to package's sysdata.rda and filters for municipalities in legal amazon
  amazon_municipalities <- dplyr::filter(legal_amazon, .data$AMZ_LEGAL == 1)

  # Filters geobr shapefiles to legal amazon municipalities
  dplyr::filter(geo_br, .data$code_muni %in% amazon_municipalities$CD_MUN)
}

find_from_dir <- function(dir) {
  list.files(path = dir, full.names = TRUE) %>%
    grep(pattern = "\\.shp", value = TRUE)
}

treat_degrad_data <- function(df, geo_aggregation, time_period, language, geo_amazon, filter) {
  message("Processing data. This should take a few minutes.")
  # Lowers letters
  names(df) <- tolower(names(df))

  if (filter) {
    # Removes non-degradation data
    df <- dplyr::filter(df, grepl("degrad", .data$class_name, ignore.case = TRUE))
  }

  # Extracts month and year of observation

  # There are some archives (older ones) that don't have the exact date from observation
  # Thus we can only recover the year from the type of the data, eg. DEGRAD2007 for data from 2007
  # So this first "if" gets the year from older data (2007 and 2008)
  if (grepl(df$class_name[1], pattern = "\\d{4}$")) {
    year <- as.numeric(substr(df$class_name[1], 7, 10))
    month <- NA
  }
  # Data for year 2009 has a column containing the year, which is collected
  # ATT: THERE IS A WAY TO FIND OUT THE DATE SOMEONE MIGHT WANT TO LOOK AT IT
  else if (any(grepl(colnames(df), pattern = "ano"))) {
    year <- df[["ano"]][1]
    month <- NA
  }
  # Since 2010, there is a column named "view_date" with the exact date (day) of the observation
  # Therefore, we collect both month and year of each observation
  else {
    year <- as.numeric(substr(as.character(as.Date(df$view_date)), 1, 4))
    month <- as.numeric(substr(as.character(as.Date(df$view_date)), 6, 7))
  }
  df$Ano <- year
  df$Mes <- month

  # There are multiple names for degradation data eg. "DEGRAD2007", "DEGRADACAO", "DEGRAD"
  # The grep function gives the row of all degradation observation because of the common format "DEGRAD"
  # But returns a zero vector if the name is already "DEGRAD", and a vector of numbers (of the rows) elsewhere
  # Then, the name of all of those is padronized to "DEGRAD"
  rows_degradation <- grep(df$class_name, pattern = "degrad[^_]", ignore.case = TRUE)
  if(length(rows_degradation) != 0){
    df[rows_degradation, ]$class_name <- "DEGRAD"
  }

  # Insert crs
  if (is.na(sf::st_crs(df))) {
    # If df comes without crs, we set the correct crs, the used in the data, of degrad data to the df
    df$geometry <- sf::st_set_crs(df$geometry, "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")
  }

  # The crs that will be used to overlap maps below
  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")

  # Changing crs of both data to the common crs chosen above
  df <- sf::st_make_valid(sf::st_transform(df, operation_crs))
  geo_amazon <- sf::st_transform(geo_amazon, operation_crs)

  # Municipalize
  sf::st_agr(df) <- "constant"
  sf::st_agr(geo_amazon) <- "constant"

  # Overlaps shapefiles
  df <- sf::st_intersection(df, geo_amazon) %>%
    # Creates column with calculated areas
    dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
    dplyr::group_by(.data$abbrev_state, .data$Ano, .data$class_name) %>%
    sf::st_drop_geometry()

  # Set aggregation level
  geo_aggregation <- tolower(geo_aggregation)
  if (geo_aggregation == "state") {
    df <- df %>%
      # Replaces state names (with errors because of accents) with state code
      dplyr::mutate(CodIBGE = as.factor(.data$code_state)) %>%
      dplyr::group_by(.data$CodIBGE, .add = TRUE) %>%
      dplyr::rename(Estado = .data$abbrev_state, Evento = .data$class_name) %>%
      # Removes useless columns
      dplyr::select(-c("code_muni", "code_state"))
  }
  else {
    if (geo_aggregation != "municipality") {
      warning("Spatial aggregation level not supported. Proceeding with Municipality.")
    }

    df <- df %>%
      # Adds from which municipality each observation is
      dplyr::mutate(CodIBGE = as.factor(.data$code_muni)) %>%
      dplyr::group_by(.data$CodIBGE, .data$name_muni, .add = TRUE) %>%
      dplyr::rename(Municipio = .data$name_muni, Estado = .data$abbrev_state, Evento = .data$class_name) %>%
      dplyr::select(-c("code_muni", "code_state"))
  }

  time_period <- tolower(time_period)
  # Some data don't allow to separate by month, be careful
  # WE NEED TO LOOK AT THIS A LITTLE BIT FURTHER AND THOROUGHLY EXPLAIN TO THE USER
  if (time_period == "month") {
    df <- dplyr::group_by(df, .data$Mes, .add = TRUE)
  }
  else if (time_period != "year") {
    warning("Temporal aggregation level not supported. Proceeding with Year.")
  }

  df <- df %>%
    dplyr::group_by(.data$CodIBGE, .add = TRUE) %>%
    dplyr::summarise(Area = sum(.data$calculated_area)) %>%
    dplyr::ungroup()

  if (filter) {
    df <- df %>%
      dplyr::rename(Degradacao = .data$Area) %>%
      dplyr::select(-c("Evento"))
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
    Mes = "Month",
    Evento = "Event",
    Degradacao = "Degradation"
  )
}

webscrapping_degrad <- function(year) {
  # Gets urls from which to retrieve data
  url <- paste0(
    "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad/arquivos/degrad",
    year,
    "_final_shp.zip"
  )

  # Creates temporary file to put to be downloaded data
  zfile <- tempfile(fileext = ".zip")

  # Downloads file and puts in the temporary file created above
  utils::download.file(url, zfile)

  dir <- gsub(zfile, pattern = "\\.zip", replacement = "")
  utils::unzip(zfile, exdir = dir)
  find_from_dir(dir)
}
