#' @title load_sigmine
#'
#' @description Download and filter data on the mines being explored legally in Brazil.
#' Source data is available at \link{https://dados.gov.br/dataset/sistema-de-informacoes-geograficas-da-mineracao-sigmine}
#'
#' @param space_aggregation A \code{string} for the geographical aggregation of the data. It can be by municipality, state or country. Defaul is municipality.
#' @param info A \code{string} for the data variables of interest. Can be area, commodity, usage, licensing. Default to all.
#' @param local A \code{string} with the data path, if locally available. Default is web, which downloads directly from the website.
#'
#'
#' @return A \code{tibble} containing the area of each selected type of soil covering in each selected year
#'
#' @importFrom utils unzip
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{
#' load_sigmine(space_aggregation = 'municipality', local='web')
#' }




load_sigmine<-function(space_aggregation = 'municipality',info='all',local='web'){

  ## Defining Url to Download Data and Downlad the Zip File

  if(local == 'web'){
    url = "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip"
    temp_file = tempfile(fileext = ".zip")
    download.file(url, temp_file, mode="wb")
  }else{
    temp_file = paste0(local,'/brasil.zip')
  }

  ## Unzip

  dir = gsub(temp_file, pattern = "\\.zip", replacement = "")
  unzip(temp_file, exdir = dir)

  #######################
  ## Load SigMine Data ##
  #######################

  ## There is some repeated observations, we need to see what to do with them!

  dat = sf::read_sf(dir) %>%
    janitor::clean_names() %>%
    sf::st_zm(drop=TRUE,what="ZM") %>%
    sf::st_transform(4674)

  if (return == 'raw'){return(dat)}

  ##################
  ## Data Editing ##
  ##################

  ## Drop Last Event (63.313 unique obs) and Name (49.376)
  ## length(unique(dat$ult_evento)) = 63313
  ## length(unique(dat$nome)) = 49376

  dat2 = dat %>%
    dplyr::select(-ult_evento,-nome,-processo) %>%
    dplyr::filter(area_ha > 0) %>%
    dplyr::filter(subs != 'DADO NÃO CADASTRADO')

  ## Create Shape with Identifier

  dat_shp = dat2 %>%
    dplyr::select(id,geometry) %>%
    dplyr::mutate(dup = duplicated(id)) %>%
    dplyr::filter(dup == 0) %>%
    dplyr::select(-dup)

  dat3 = dat2 %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dup = duplicated(id)) %>%
    dplyr::filter(dup == 0) %>% ## This choice is clearly not perfect, need to improve!
    dplyr::select(-dup)


  ## Change Variable Names

  #names(dat)[names(dat) == 'nome'] <- 'empresa'
  #names(dat)[names(dat) == 'subs'] <- 'mineral'

  ######################
  ## Brazil Shapefile ##
  ######################

  geo = geobr::read_municipality(simplified = TRUE) %>%
      sf::st_transform(4674) %>%
      dplyr::select(code_muni,abbrev_state)

  ###############
  ## Join Data ##
  ###############

  dat2 = sf::st_join(dat,geo)

    #dat2$duplicated = ifelse(dat2$processo %in% unique(dat2$processo[duplicated(dat2$processo)]),1,0)

    #operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
    #dat <- sf::st_transform(dat, operation_crs)
    #geo_amazon <- sf::st_transform(geo_amazon, operation_crs)

    sf::st_agr(a) = "constant"
    sf::st_agr(geo_amazon) = "constant"
    al<-sf::st_zm(a)
    al <- sf::st_intersection(sf::st_make_valid(al), geo_amazon) %>%
      dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
      dplyr::group_by(.data$abbrev_state, .data$ANO) %>%
      sf::st_drop_geometry()


#
#     al$AREA_HA <- NULL
#     al$UF <- NULL
#     al$code_region <- NULL
#     al$code_state <- NULL
#     al$code_muni <- NULL
#     al$ULT_EVENTO <- NULL
#     al$NUMERO <- NULL
#     al$name_region <- NULL
#     al$name_state <- NULL
#
#     ret <- al

  return(ret)
}


