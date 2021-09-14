#' @title DETER - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on change in forest cover in the Amazon. See \url{http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter}
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado") with information about both Amazon and Cerrado
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Download raw data (raw_data = TRUE) from Amazonia (dataset = "deter_amz")
#' deter_amz <- load_deter(dataset = 'deter_amz',
#'                         raw_data = TRUE)
#'
#' # Download treated data (raw_data = FALSE) from Cerrado (dataset = "deter_cerrado")
#' # in portuguese (language = 'pt')
#' deter_cer <- load_deter(dataset = 'deter_cerrado',
#'                         raw_data = FALSE,
#'                         language = "pt")
#'
#' }


load_deter <- function(dataset = NULL, raw_data,
                       language = 'eng') {

  ## Dataset can be either Amazonia or Cerrado
  # Default is all time-periods

  ###########################
  ## Bind Global Variables ##
  ###########################

  ## Bind Global Variables

  quadrant <- .data <- abbrev_state <- name_state <- code_region <- NULL
  path_row <- name_region <- code_muni <- area <- geometry <- NULL
  sensor <- data <- cod_municipio <- class <- municipality_code <- NULL
  satellite <- class_name <- classe <- NULL
  view_date <- NULL
  municipali <- NULL
  uc <- NULL
  uf <- NULL
  ano <- NULL
  mes <- NULL
  classname <- NULL
  areauckm <- NULL
  areamunkm <- NULL
  name_muni <- NULL
  code_state <- NULL
  area_uc_km <- NULL
  area_geo_km <- NULL
  survey <- link <- NULL
  time_period <- NULL

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

  #################
  ## Downloading ##
  #################

  dat = external_download(dataset = param$dataset,source = 'deter')

  dat = dat %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Engineering ##
  ######################
  # Downloading municipal map from geobr filtered to legl amazon municipalities

  message("Downloading map data.")

  # Downloading municipal map from geobr
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE)

  ###################
  ## Harmonize CRS ##
  ###################

  # The crs that will be used to overlap maps below
  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")

  # Changing crs of both data to the common crs chosen above
  dat$geometry <- sf::st_make_valid(sf::st_transform(dat$geometry, operation_crs))
  geo_br$geom <- sf::st_transform(geo_br$geom, operation_crs)

  # Overlaps shapefiles
  sf::st_geometry(dat) <- dat$geometry
  sf::st_geometry(geo_br) <- geo_br$geom

  dat <- suppressWarnings(sf::st_intersection(dat, geo_br)) %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  dat <- dat %>%
    dplyr::select(-name_muni, -abbrev_state, -name_state,
                  -code_region, -name_region)

  dat <- dat %>%
    dplyr::select(view_date, code_state, code_muni, sensor, satellite, uc,
                  classname, path_row, area, quadrant, geometry)

  ################### ### -------------------- Need to Work
  ## Renaming Data ##
  ###################

  if (param$language == 'pt'){

    dat_mod = dat %>%
      dplyr::select(data = view_date, cod_uf = code_state,
                    cod_municipio = code_muni, sensor = sensor,
                    satelite = satellite, uc, classe = classname,
                    path_row, area, quadrante = quadrant, geometry) %>%
      dplyr::arrange(data, cod_municipio, classe)

  }

  if (param$language == 'eng'){

    dat_mod = dat %>%
      dplyr::select(date = view_date, state_code = code_state,
                    municipality_code = code_muni, sensor = sensor,
                    satellite, uc, class_name = classname,
                    pathrow = path_row, area, quadrant, geometry) %>%
      dplyr::arrange(date, municipality_code, class_name)

  }
  # df <- df %>%
  #   dplyr::rename_with(dplyr::recode,
  #                      CLASSNAME = "Classe",
  #                      AREAUCKM = "Area_em_UC_km2",
  #                      AREAMUNKM = "Area_em_Municipio_km2",
  #                      MUNICIPALI = "Municipio",
  #   )
  #
  # df$Classe <- df$Classe %>% dplyr::recode(
  #   CICATRIZ_DE_QUEIMADA = "Cicatriz de Queimada",
  #   CS_DESORDENADO = "Corte Seletivo Desordenado",
  #   CS_GEOMETRICO = "Corte Seletivo Geometrico",
  #   DEGRADACAO = "Degradacao",
  #   DESMATAMENTO_CR = "Desmatamento Corte Raso",
  #   DESMATAMENTO_VEG = "Desmatamento com Vegetacao",
  #   MINERACAO = "Mineracao",
  #   CORTE_SELETIVO = "Corte Seletivo"
  # )

  ###########################
  ## Translating Variables ##
  ###########################

  # language <- tolower(language)
  #
  # if (language == "eng") {
  #
  #   ## Translate
  #
  #   df$Classe <- df$Classe %>% dplyr::recode(
  #     "Cicatriz de Queimada" = "Fire Scar",
  #     "Corte Seletivo Desordenado" = "Unorganized Selection Cutting",
  #     "Corte Seletivo Geometrico" = "Geometric Selection Cutting",
  #     "Degradacao" = "Degradation",
  #     "Desmatamento Corte Raso" = "Clear Cut Deforestation",
  #     "Desmatamento com Vegetacao" = "Vegetation Remains Deforestation",
  #     "Mineracao" = "Mining",
  #     "aviso" = "Warning",
  #     "Corte Seletivo" = "Selection Cutting"
  #   )
  #
  #   ## Data Engineering
  #
  #   df <- df %>%
  #     dplyr::rename_with(dplyr::recode,
  #                        Classe = "Class",
  #                        UC = "ConservationUnit",
  #                        Area_em_UC_km2 = "Area_in_CU_km2",
  #                        Area_em_Municipio_km2 = "Area_in_Municipality_km2",
  #                        Municipio = "Municipality",
  #                        Mes = "Month",
  #                        Ano = "Year"
  #     )
  #
  # }
  # else if (language != "pt") {
  #   warning("Selected language not supported. Proceeding with Portuguese.")
  # }

  #################
  ## Return Data ##
  #################

  return(dat_mod)

}


