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
#' @return A \code{list} (if \code{raw_data} = \code{TRUE}) or a tibble (if \code{raw_data} = \code{FALSE}) with the selected data.
#'
#'
#' @examples
#' \dontrun{
#' # download raw data (raw_data = TRUE) related to forest degradation
#' # from 2010 to 2012 (time_period = 2010:2012).
#' data <- load_degrad(dataset = 'degrad',
#'                     raw_data = TRUE,
#'                     time_period = 2010:2012)
#'
#' # download treated data (raw_data = FALSE) related to forest degradation
#' # from 2013 (time_period = 2013) in portuguese (language = "pt").
#' data <- load_degrad(dataset = 'degrad',
#'                     raw_data = FALSE,
#'                     time_period = 2013,
#'                     language = 'pt')
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

  survey <- link <- .data <- abbrev_state <- ano <- area <-  NULL
  areameters <- areametros <- class_name <- classe <- cod_municipio <- NULL
  code_muni <- code_state <- codigouf <- geometry <- julday <- linkcolumn <- NULL
  municipality_code <- name_muni <- name_region <- name_state <- nome <- NULL
  pathrow <- code_region <- scene_id <- sigla <- uf <- view_date <- year <- NULL
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

  dat = suppressWarnings(as.list(param$time_period) %>%
      purrr::map(
        function(t){external_download(dataset = param$dataset,
                                      source='degrad', year = t) %>%
            janitor::clean_names()
        }
      ))

  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}

  # Join all tables
  temp <- tibble::tribble(~linkcolumn, ~scene_id, ~class_name, ~pathrow, ~uf,
                          ~area, ~geometry, ~year, ~con_ai_id, ~julday,
                          ~ano, ~objet_id_3, ~cell_oid, ~view_date,
                          ~codigouf, ~nome, ~inter_oid, ~areametros, ~areameters)

  dat = c(dat, list(temp)) %>%
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
    "DF", 53)


  dat <- dat %>%
    dplyr::mutate(codigouf = as.numeric(codigouf)) %>%
    dplyr::left_join(ufs, by = "codigouf") %>%
    dplyr::mutate(uf = ifelse(is.na(uf), sigla, uf)) %>%
    dplyr::select(-codigouf, -sigla) %>%
    dplyr::select(-area, -areametros, -areameters)


  # Clean geometry
  dat$geometry <- sf::st_set_crs(dat$geometry,
                                 "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")

  dat$geometry <- dat$geometry %>%
    sf::st_make_valid()

  ######################
  ## Data Engineering ##
  ######################

  # There are multiple names for degradation data eg. "DEGRAD2007", "DEGRADACAO", "DEGRAD"
  # The name of all of those is padronized to "DEGRAD"
  dat <- dat %>%
    dplyr::mutate(class_name = ifelse(stringr::str_detect(class_name, "DEGRAD"),
                                      "DEGRAD", class_name)
                  )

  ###################################
  ## Filter Legal Amazon Geography ##
  ###################################

  ## Selecting Municipalities in the Legal Amazon

  # Downloading municipal map from geobr filtered to legl amazon municipalities

  message("Downloading map data.")

  # Downloading municipal map from geobr
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE) # 2019 relates to the definition of legal_amazon

  # legal_amazon belongs to package's sysdata.rda and filters for municipalities in legal amazon
  amazon_municipalities <- dplyr::filter(legal_amazon, .data$AMZ_LEGAL == 1)

  # Filters geobr shapefiles to legal amazon municipalities
  geo_amazon <- dplyr::filter(geo_br, .data$code_muni %in% amazon_municipalities$CD_MUN)

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
    dplyr::select(-name_muni, -abbrev_state, -name_state,
                  -code_region, -name_region)

  dat <- dat %>%
    dplyr::select(year, linkcolumn, scene_id, code_state, code_muni,
                  class_name, pathrow, area, view_date, julday, geometry)

  # # Set aggregation level
  # geo_level <- tolower(geo_level)
  # if (geo_level == "state") {
  #   df <- df %>%
  #     # Replaces state names (with errors because of accents) with state code
  #     dplyr::mutate(code_state = as.factor(.data$code_state)) %>%
  #     dplyr::group_by(.data$code_state) %>%
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

  #################
  ## Translation ##
  #################

  if (param$language == 'pt'){

    dat_mod = dat %>%
      dplyr::select(ano = year, linkcolumn, scene_id,
                    cod_uf = code_state, cod_municipio = code_muni,
                    classe = class_name, pathrow, area, data = view_date,
                    julday, geometry
      ) %>%
      dplyr::arrange(ano, cod_municipio, classe)

  }

  if (param$language == 'eng'){

    dat_mod = dat %>%
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



