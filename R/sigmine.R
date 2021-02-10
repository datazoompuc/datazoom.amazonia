#' @title load_sigmine
#' 
#' @description Download and filter data on type of soil covering by year
#' 
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by municipality or state
#' @param source A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' 
#' @return A data base with data containing the area of each selected type of soil covering in each selected year
#'
#' @importFrom readxl read_excel
#' @importFrom utils download.file            ATUALIZAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#' 
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_sigmine(space_aggregation = 'municipality', source = NULL)




load_sigmine<-function(space_aggregation = c('municipality', 'state','municipio','estado'), source = NULL){
  if(is.null(source)){
    url1<-'http://sigmine.dnpm.gov.br/sirgas2000/brasil.zip'
    p1f <- tempfile(fileext = ".zip")
    download.file(url1, p1f, mode="wb")
  }else{
    p1f<-paste0(source,'/brasil.zip')
  }
  dir <- gsub(p1f, pattern = "\\.zip", replacement = "")
  unzip(p1f, exdir = dir)
  a<-read_sf(dir)
  
  if(space_aggregation == 'state' | space_aggregation == 'estado'){
    names(a)[names(a) == 'NUMERO'] <- 'numero'
    names(a)[names(a) == 'ULT_EVENTO'] <- 'ultimo_evento'
    names(a)[names(a) == 'UF'] <- 'estado'
    names(a)[names(a) == 'ANO'] <- 'ano'
    names(a)[names(a) == 'PROCESSO'] <- 'processo'
    names(a)[names(a) == 'ID'] <- 'id'
    names(a)[names(a) == 'FASE'] <- 'fase'
    names(a)[names(a) == 'NOME'] <- 'empresa'
    names(a)[names(a) == 'SUBS'] <- 'mineral'
    names(a)[names(a) == 'USO'] <- 'uso'
    names(a)[names(a) == 'AREA_HA'] <- 'area_hectares'
    ret <- a
    
  }else if(space_aggregation == 'municipality' | space_aggregation == 'municipio'){
    geo_amazon <- download_map()
    operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
    a <- sf::st_transform(a, operation_crs)
    geo_amazon <- sf::st_transform(geo_amazon, operation_crs)
    
    sf::st_agr(a) = "constant"
    sf::st_agr(geo_amazon) = "constant"
    al<-sf::st_zm(a)
    al <- sf::st_intersection(sf::st_make_valid(al), geo_amazon) %>%
      dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
      dplyr::group_by(.data$abbrev_state, .data$ANO) %>%
      sf::st_drop_geometry()
    
    al$AREA_HA<-NULL
    al$UF<-NULL
    al$code_region<-NULL
    al$code_state<-NULL
    al$code_muni<-NULL
    al$ULT_EVENTO<-NULL
    al$NUMERO<-NULL
    al$name_region<-NULL
    al$name_state<-NULL
    names(al)[names(al) == 'ANO'] <- 'ano'
    names(al)[names(al) == 'PROCESSO'] <- 'processo'
    names(al)[names(al) == 'ID'] <- 'id'
    names(al)[names(al) == 'FASE'] <- 'fase'
    names(al)[names(al) == 'NOME'] <- 'empresa'
    names(al)[names(al) == 'SUBS'] <- 'mineral'
    names(al)[names(al) == 'USO'] <- 'uso'
    names(al)[names(al) == 'name_muni'] <- 'municipio'
    names(al)[names(al) == 'abbrev_state'] <- 'estado'
    names(al)[names(al) == 'calculated_area'] <- 'area_calculada_m2'
    ret <- al
  }
  return(ret)
}


download_map <- function() {
  geo_br <- geobr::read_municipality(year = 2019, simplified = FALSE)
}
