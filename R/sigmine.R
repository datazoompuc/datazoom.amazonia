#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads information the mines being explored legally in Brazil, including their location, status, product being mined and area in square meters etc. Survey is done at municipal and state level
#'
#' @param dataset A dataset name ("sigmine_active")
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
#' @examples \dontrun{
#' # download state raw data
#' sigmine_active <- load_sigmine(dataset = 'sigmine_active',
#'                                raw_data = TRUE)
#' }


load_sigmine = function(dataset = 'sigmine_active',
                        raw_data,
                        language = 'eng'){


  survey <- link <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  #param$geo_level = geo_level
  #param$time_period = time_period
  param$language = language
  #param$time_id = time_id
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

  # if (is.null(source)) {
  #   p1f <- tempfile(fileext = ".zip")
  #   tryCatch(
  #     expr = {
  #       url1 <- "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip"
  #       download.file(url1, p1f, mode = "wb")
  #     },
  #     error = function(e) {
  #       url1 <- "http://sigmine.dnpm.gov.br/sirgas2000/brasil.zip"
  #       download.file(url1, p1f, mode = "wb")
  #     }
  #   )
  # } else {
  #   p1f <- paste0(source, "/brasil.zip")
  # }
  # dir <- gsub(p1f, pattern = "\\.zip", replacement = "")
  # unzip(p1f, exdir = dir)
  # a <- sf::read_sf(dir)

  dat = external_download(dataset = param$dataset,source='sigmine') %>%
          janitor::clean_names()

  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}


  ######################
  ## Data Engineering ##
  ######################

  ## State Level Data

  # if (space_aggregation == "state" | space_aggregation == "estado") {
  #   if (language == "pt") {
  #     a$AREA_HA <- a$AREA_HA * 10000
  #     names(a)[names(a) == "NUMERO"] <- "numero"
  #     names(a)[names(a) == "ULT_EVENTO"] <- "ultimo_evento"
  #     names(a)[names(a) == "UF"] <- "estado"
  #     names(a)[names(a) == "ANO"] <- "ano"
  #     names(a)[names(a) == "PROCESSO"] <- "processo"
  #     names(a)[names(a) == "ID"] <- "id"
  #     names(a)[names(a) == "FASE"] <- "fase"
  #     names(a)[names(a) == "NOME"] <- "empresa"
  #     names(a)[names(a) == "SUBS"] <- "mineral"
  #     names(a)[names(a) == "USO"] <- "uso"
  #     names(a)[names(a) == "AREA_HA"] <- "area_m2"
  #   } else if (language == "eng") {
  #     a$AREA_HA <- a$AREA_HA * 10000
  #     names(a)[names(a) == "NUMERO"] <- "number"
  #     names(a)[names(a) == "ULT_EVENTO"] <- "last_event"
  #     names(a)[names(a) == "UF"] <- "state"
  #     names(a)[names(a) == "ANO"] <- "year"
  #     names(a)[names(a) == "PROCESSO"] <- "process"
  #     names(a)[names(a) == "ID"] <- "id"
  #     names(a)[names(a) == "FASE"] <- "phase"
  #     names(a)[names(a) == "NOME"] <- "company"
  #     names(a)[names(a) == "SUBS"] <- "mineral"
  #     names(a)[names(a) == "USO"] <- "use"
  #     names(a)[names(a) == "AREA_HA"] <- "area_m2"
  #   }
  #   ret <- a
  #
  #   ## Municipality Level Data
  #
  # } else if (space_aggregation == "municipality" | space_aggregation == "municipio") {
  #   geo_amazon <- geobr::read_municipality(year = 2019, simplified = TRUE)
  #   operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
  #   a <- sf::st_transform(a, operation_crs)
  #   geo_amazon <- sf::st_transform(geo_amazon, operation_crs)
  #
  #   sf::st_agr(a) <- "constant"
  #   sf::st_agr(geo_amazon) <- "constant"
  #   al <- sf::st_zm(a)
  #   al <- sf::st_intersection(sf::st_make_valid(al), geo_amazon) %>%
  #     dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
  #     dplyr::group_by(.data$abbrev_state, .data$ANO) %>%
  #     sf::st_drop_geometry()
  #
  #   al$AREA_HA <- NULL
  #   al$UF <- NULL
  #   al$code_region <- NULL
  #   al$code_state <- NULL
  #   al$ULT_EVENTO <- NULL
  #   al$NUMERO <- NULL
  #   al$name_region <- NULL
  #   al$name_state <- NULL
  #
  #   ########################
  #   ## Renaming Variables ##
  #   ########################
  #
  #
  #   if (language == "pt") {
  #     names(al)[names(al) == "ANO"] <- "ano"
  #     names(al)[names(al) == "PROCESSO"] <- "processo"
  #     names(al)[names(al) == "ID"] <- "id"
  #     names(al)[names(al) == "FASE"] <- "fase"
  #     names(al)[names(al) == "NOME"] <- "empresa"
  #     names(al)[names(al) == "SUBS"] <- "mineral"
  #     names(al)[names(al) == "USO"] <- "uso"
  #     names(al)[names(al) == "name_muni"] <- "municipio"
  #     names(al)[names(al) == "abbrev_state"] <- "estado"
  #     names(al)[names(al) == "calculated_area"] <- "area_calculada_m2"
  #   } else if (language == "eng") {
  #     names(al)[names(al) == "ANO"] <- "year"
  #     names(al)[names(al) == "PROCESSO"] <- "process"
  #     names(al)[names(al) == "ID"] <- "id"
  #     names(al)[names(al) == "FASE"] <- "phase"
  #     names(al)[names(al) == "NOME"] <- "company"
  #     names(al)[names(al) == "SUBS"] <- "mineral"
  #     names(al)[names(al) == "USO"] <- "use"
  #     names(al)[names(al) == "name_muni"] <- "municipality"
  #     names(al)[names(al) == "abbrev_state"] <- "state"
  #     names(al)[names(al) == "calculated_area"] <- "calculated_area_m2"
  #   }
  #   ret <- al
  # }
  #
  # return(ret)

}
