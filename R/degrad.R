#' @title Degrad - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on forest degradation in the Brazilian Amazon, replaced by DETER-B in December 2016.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("degrad").
#' @inheritParams load_baci
#' @return A \code{list} of tibbles (if \code{raw_data} = \code{TRUE}) or a tibble (if \code{raw_data} = \code{FALSE}).
#'
#'
#' @examples
#' \dontrun{
#' # download treated data (raw_data = TRUE) related to forest degradation
#' # from 2010 to 2012 (time_period = 2010:2012).
#' data <- load_degrad(
#'   dataset = "degrad",
#'   raw_data = FALSE,
#'   time_period = 2010:2012
#' )
#' }
#'
#' @export

load_degrad <- function(dataset = "degrad", raw_data = FALSE,
                        time_period,
                        language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- .data <- abbrev_state <- ano <- area <- NULL
  areameters <- areametros <- class_name <- classe <- cod_municipio <- NULL
  code_muni <- code_state <- codigouf <- geometry <- julday <- linkcolumn <- NULL
  municipality_code <- name_muni <- name_region <- name_state <- nome <- NULL
  pathrow <- code_region <- scene_id <- sigla <- uf <- view_date <- year <- NULL
  geo_amazon <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # .shp file names for each year

  file_list <- c(
    "2007" = "Degrad2007_Final_pol.shp",
    "2008" = "Degrad2008_Final_pol.shp",
    "2009" = "Degrad2009_Final_pol.shp",
    "2010" = "DEGRAD_2010_UF_pol.shp",
    "2011" = "DEGRAD_2011_INPE_pol.shp",
    "2012" = "DEGRAD_2012_INPE_pol.shp",
    "2013" = "DEGRAD_2013_INPE_pol.shp",
    "2014" = "DEGRAD_2014_pol.shp",
    "2015" = "DEGRAD_2015.shp",
    "2016" = "DEGRAD_2016_pol.shp"
  )

  file_names <- param$time_period %>%
    dplyr::recode(!!!file_list)


  ######################
  ## Downloading Data ##
  ######################

  dat <- base::suppressWarnings(purrr::map2(
    param$time_period, file_names,
    function(year, file) {
      external_download(
        dataset = param$dataset,
        source = "degrad",
        year = year,
        file_name = file
      ) %>%
        janitor::clean_names()
    }
  ))

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  # Join all tables
  temp <- tibble::tribble(
    ~linkcolumn, ~scene_id, ~class_name, ~pathrow, ~uf,
    ~area, ~geometry, ~year, ~con_ai_id, ~julday,
    ~ano, ~objet_id_3, ~cell_oid, ~view_date,
    ~codigouf, ~nome, ~inter_oid, ~areametros, ~areameters
  )

  dat <- c(dat, list(temp)) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  # Remove useless columns
  dat <- dat %>%
    dplyr::select(-ano, -nome)

  # Add sigla UF
  ufs <- tibble::tribble(
    ~sigla, ~codigouf,
    "AC", 12,
    "AL", 27,
    "AP", 16,
    "AM", 13,
    "BA", 29,
    "CE", 23,
    "ES", 32,
    "GO", 52,
    "MA", 21,
    "MT", 51,
    "MS", 50,
    "MG", 31,
    "PA", 15,
    "PB", 25,
    "PR", 41,
    "PE", 26,
    "PI", 22,
    "RJ", 33,
    "RN", 24,
    "RS", 43,
    "RO", 11,
    "RR", 14,
    "SC", 42,
    "SP", 35,
    "SE", 28,
    "TO", 17,
    "DF", 53
  )


  dat <- dat %>%
    dplyr::mutate(codigouf = as.numeric(codigouf)) %>%
    dplyr::left_join(ufs, by = "codigouf") %>%
    dplyr::mutate(uf = ifelse(is.na(uf), sigla, uf)) %>%
    dplyr::select(-codigouf, -sigla) %>%
    dplyr::select(-area, -areametros, -areameters)


  # Clean geometry
  dat$geometry <- sf::st_set_crs(
    dat$geometry,
    "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs"
  )

  dat$geometry <- dat$geometry %>%
    sf::st_make_valid()

  ######################
  ## Data Engineering ##
  ######################

  # There are multiple names for degradation data eg. "DEGRAD2007", "DEGRADACAO", "DEGRAD"
  # The name of all of those is padronized to "DEGRAD"
  dat <- dat %>%
    dplyr::mutate(class_name = ifelse(stringr::str_detect(class_name, "DEGRAD"),
      "DEGRAD", class_name
    ))

  ###################################
  ## Filter Legal Amazon Geography ##
  ###################################

  ## Selecting Municipalities in the Legal Amazon

  # Loading municipal map data
  geo_br <- external_download(
    dataset = "geo_municipalities",
    source = "internal"
  )

  # legal_amazon belongs to package's sysdata.rda and filters for municipalities in legal amazon
  amazon_municipalities <- dplyr::filter(datazoom.amazonia::municipalities, .data$legal_amazon == 1)

  # Filters geobr shapefiles to legal amazon municipalities
  geo_amazon <- dplyr::filter(geo_br, .data$code_muni %in% amazon_municipalities$code_muni)

  ###################
  ## Harmonize CRS ##
  ###################

  # The crs that will be used to overlap maps below
  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")

  # Changing crs of both data to the common crs chosen above
  dat$geometry <- sf::st_make_valid(sf::st_transform(dat$geometry, operation_crs))
  geo_amazon$geom <- sf::st_transform(geo_amazon$geom, operation_crs)

  ##########################################################
  ## Aggregation Municipality or State Level x Time Level ##
  ##########################################################

  # Overlaps shapefiles
  sf::st_geometry(dat) <- dat$geometry
  sf::st_geometry(geo_amazon) <- geo_amazon$geom

  dat <- suppressWarnings(sf::st_intersection(dat, geo_amazon)) %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  dat <- dat %>%
    dplyr::select(
      -name_muni, -abbrev_state, -name_state,
      -code_region, -name_region
    )

  dat <- dat %>%
    dplyr::select(
      year, linkcolumn, scene_id, code_state, code_muni,
      class_name, pathrow, area, view_date, julday, geometry
    )

  #################
  ## Translation ##
  #################

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::select(
        ano = year, linkcolumn, scene_id,
        cod_uf = code_state, cod_municipio = code_muni,
        classe = class_name, pathrow, area, data = view_date,
        julday, geometry
      ) %>%
      dplyr::arrange(ano, cod_municipio, classe)
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::select(year, linkcolumn, scene_id,
        state_code = code_state, municipality_code = code_muni,
        class_name, pathrow, area, date = view_date,
        julday, geometry
      ) %>%
      dplyr::arrange(year, municipality_code, class_name)
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
