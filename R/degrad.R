#' @title Degrad - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on forest degradation in the Brazilian Amazon, replaced by DETER-B in December 2016. Data is available from 2007 to 2016. See \url{http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad}.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("degrad").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{list} with the selected data.
#'
#'
#' @examples
#' \dontrun{
#' # download raw data from 2007 to 2016
#' raw_degrad_all <- load_degrad(dataset = "degrad",
#'                               raw_data = TRUE,
#'                               time_period = 2007:2016)
#' }
#'
#' @importFrom magrittr %>%
#' @export

load_degrad <- function(dataset = 'degrad', raw_data,
                        time_period,
                        language = 'eng') {

  # ,all_events = FALSE

  ## To-Do:
    # Include Safety Download and Message if any Error Occurs
    # Harmonize Columns Names, Create Panel and Deliver Raw Data

  survey <- link <- NULL
  geo_amazon = NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$time_period = time_period
  param$language = language
  param$raw_data = raw_data

  param$survey_name = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}


  ######################
  ## Downloading Data ##
  ######################

  # find_from_dir <- function(dir) {
  #   list.files(path = dir, full.names = TRUE) %>%
  #     grep(pattern = "\\.shp", value = TRUE)
  # }
  #
  # # If source is a list of numbers (years), we retrieve data from INPE
  # if (is.numeric(source)) {
  #   source <- purrr::map(source, webscrapping_degrad)
  # }
  #
  # # If source is a directory, we expand and filter the list of files
  # else if (is.character(source) && length(source) == 1 && dir.exists(source)) {
  #   source <- find_from_dir(source)
  # }
  #
  # # Otherwise, we assume that source is something that can already be interpreted by sf::read_sf
  # suppressWarnings(lapply(source, sf::read_sf))

  dat = as.list(param$time_period) %>%
      purrr::map(
        function(t){external_download(dataset = param$dataset,
                                      source='degrad', year = t) %>%
            janitor::clean_names()
        }
      )


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}

  ## The Columns are not always the same name, we have to edit that

  ###################################
  ## Filter Legal Amazon Geography ##
  ###################################

  ## Selecting Municipalities in the Legal Amazon

  # Downloading municipal map from geobr filtered to legl amazon municipalities

  # message("Downloading map data.")

  # Downloading municipal map from geobr
  # geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE) # 2019 relates to the definition of legal_amazon

  # legal_amazon belongs to package's sysdata.rda and filters for municipalities in legal amazon
  # amazon_municipalities <- dplyr::filter(legal_amazon, .data$AMZ_LEGAL == 1)

  # Filters geobr shapefiles to legal amazon municipalities
  # dplyr::filter(geo_br, .data$code_muni %in% amazon_municipalities$CD_MUN)

  ######################
  ## Data Engineering ##
  ######################

  ## Month information is only available after 2010

  # Since 2010, there is a column named "view_date" with the exact date (day) of the observation
  # Therefore, we collect both month and year of each observation

  # There are some archives (older ones) that don't have the exact date from observation
  # Thus we can only recover the year from the type of the data, eg. DEGRAD2007 for data from 2007
  # So this first "if" gets the year from older data (2007 and 2008)
  # if (grepl(df$class_name[1], pattern = "\\d{4}$")) {
  #   year <- as.numeric(substr(df$class_name[1], 7, 10))
  #   month <- NA
  # }
  # # Data for year 2009 has a column containing the year, which is collected
  # # ATT: THERE IS A WAY TO FIND OUT THE DATE SOMEONE MIGHT WANT TO LOOK AT IT
  # else if (any(grepl(colnames(df), pattern = "ano"))) {
  #   year <- df[["ano"]][1]
  #   month <- NA
  # }
  # # Since 2010, there is a column named "view_date" with the exact date (day) of the observation
  # # Therefore, we collect both month and year of each observation
  # else {
  #   year <- as.numeric(substr(as.character(as.Date(df$view_date)), 1, 4))
  #   month <- as.numeric(substr(as.character(as.Date(df$view_date)), 6, 7))
  # }
  # df$Ano <- year
  # df$Mes <- month

  # There are multiple names for degradation data eg. "DEGRAD2007", "DEGRADACAO", "DEGRAD"
  # The grep function gives the row of all degradation observation because of the common format "DEGRAD"
  # But returns a zero vector if the name is already "DEGRAD", and a vector of numbers (of the rows) elsewhere
  # Then, the name of all of those is padronized to "DEGRAD"
  # rows_degradation <- grep(df$class_name, pattern = "degrad[^_]", ignore.case = TRUE)
  # if(length(rows_degradation) != 0){
  #   df[rows_degradation, ]$class_name <- "DEGRAD"
  # }

  #######################################
  ## Harmonize Degradation Measurement ##
  #######################################

  # There are multiple names for degradation data eg. "DEGRAD2007", "DEGRADACAO", "DEGRAD"
  # The grep function gives the row of all degradation observation because of the common format "DEGRAD"
  # But returns a zero vector if the name is already "DEGRAD", and a vector of numbers (of the rows) elsewhere
  # Then, the name of all of those is padronized to "DEGRAD"
  # rows_degradation <- grep(df$class_name, pattern = "degrad[^_]", ignore.case = TRUE)
  # if(length(rows_degradation) != 0){
  #   df[rows_degradation, ]$class_name <- "DEGRAD"
  # }

  ###################
  ## Harmonize CRS ##
  ###################

  # Insert crs
  # if (is.na(sf::st_crs(df))) {
  #   # If df comes without crs, we set the correct crs, the used in the data, of degrad data to the df
  #   df$geometry <- sf::st_set_crs(df$geometry, "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")
  # }
  #
  # # The crs that will be used to overlap maps below
  # operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
  #
  # # Changing crs of both data to the common crs chosen above
  # df <- sf::st_make_valid(sf::st_transform(df, operation_crs))
  # geo_amazon <- sf::st_transform(geo_amazon, operation_crs)
  #
  # # Municipalize
  # sf::st_agr(df) <- "constant"
  # sf::st_agr(geo_amazon) <- "constant"

  ##########################################################
  ## Aggregation Municipality or State Level x Time Level ##
  ##########################################################

  # Overlaps shapefiles
  # df <- sf::st_intersection(df, geo_amazon) %>%
  #   # Creates column with calculated areas
  #   dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
  #   dplyr::group_by(.data$abbrev_state, .data$Ano, .data$class_name) %>%
  #   sf::st_drop_geometry()
  #
  # # Set aggregation level
  # geo_level <- tolower(geo_level)
  # if (geo_level == "state") {
  #   df <- df %>%
  #     # Replaces state names (with errors because of accents) with state code
  #     dplyr::mutate(CodIBGE = as.factor(.data$code_state)) %>%
  #     dplyr::group_by(.data$CodIBGE, .add = TRUE) %>%
  #     dplyr::rename(Estado = .data$abbrev_state, Evento = .data$class_name) %>%
  #     # Removes useless columns
  #     dplyr::select(-c("code_muni", "code_state"))
  # }
  # else {
  #   if (geo_level != "municipality") {
  #     warning("Spatial aggregation level not supported. Proceeding with Municipality.")
  #   }
  #
  #   df <- df %>%
  #     # Adds from which municipality each observation is
  #     dplyr::mutate(CodIBGE = as.factor(.data$code_muni)) %>%
  #     dplyr::group_by(.data$CodIBGE, .data$name_muni, .add = TRUE) %>%
  #     dplyr::rename(Municipio = .data$name_muni, Estado = .data$abbrev_state, Evento = .data$class_name) %>%
  #     dplyr::select(-c("code_muni", "code_state"))
  # }
  #
  # time_period <- tolower(time_period)
  # # Some data don't allow to separate by month, be careful
  # # WE NEED TO LOOK AT THIS A LITTLE BIT FURTHER AND THOROUGHLY EXPLAIN TO THE USER
  # if (time_period == "month") {
  #   df <- dplyr::group_by(df, .data$Mes, .add = TRUE)
  # }
  # else if (time_period != "year") {
  #   warning("Temporal aggregation level not supported. Proceeding with Year.")
  # }
  #
  # df <- df %>%
  #   dplyr::group_by(.data$CodIBGE, .add = TRUE) %>%
  #   dplyr::summarise(Area = sum(.data$calculated_area)) %>%
  #   dplyr::ungroup()
  #
  # if (filter) {
  #   df <- df %>%
  #     dplyr::rename(Degradacao = .data$Area) %>%
  #     dplyr::select(-c("Evento"))
  # }

  #################
  ## Translation ##
  #################

  # Set language
  # language <- tolower(language)
  # if (language == "eng") {
  #   df <- dplyr::rename_with(
  #     df,
  #     dplyr::recode,
  #     Municipio = "Municipality",
  #     CodIBGE = "CodIBGE",
  #     Estado = "State",
  #     Ano = "Year",
  #     Mes = "Month",
  #     Evento = "Event",
  #     Degradacao = "Degradation"
  #   )
  # }
  # else if (language != "pt") {
  #   warning("Selected language is not supported. Proceeding with Portuguese.")
  # }

  ####################
  ## Returning Data ##
  ####################

  # Treating data according to parameters selected
  # list_df <- lapply(raw_data, treat_degrad_data,
  #                   geo_level = geo_level, time_period = time_period,
  #                   language = language, geo_amazon = geo_amazon, filter = !all_events)
  #
  # dplyr::bind_rows(list_df)


}



