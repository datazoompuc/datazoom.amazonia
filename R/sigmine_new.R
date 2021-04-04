#' @title load_sigmine
#'
#' @description Download and filter data on the mines being explored legally in Brazil.
#' Source data is available at \link{https://dados.gov.br/dataset/sistema-de-informacoes-geograficas-da-mineracao-sigmine}
#'
#' @param space_aggregation A \code{string} for the geographical aggregation of the data. It can be by municipality, state or country. Currently, only state is available.
#' @param info A \code{string} for the data variables of interest. Can be area, commodity, usage, licensing. Default to all.
#' @param local A \code{string} with the data path, if locally available. Default is web, which downloads directly from the website.
#' @param teturn A \code{string} with the retuned data type. Can be edited or raw.
#'
#'
#' @return A \code{tibble} format panel data with N geographic units and T time periods
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


load_sigmine<-function(space_aggregation = 'state',info='all',local='web',raw=FALSE){

  ## Defining Url to Download Data and Downlad the Zip File

  if(local == 'web'){
    url = "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/MG.zip"
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
    sf::st_zm(drop=TRUE,what="ZM")

  if (raw==TRUE){return(dat)}

  ##################
  ## Data Editing ##
  ##################

  ## Drop Last Event (63.313 unique obs) and Name (49.376)
  ## length(unique(dat$ult_evento)) = 63313
  ## length(unique(dat$nome)) = 49376

  #dat = dat %>% dplyr::mutate_all(function(x){gsub('[^ -~]', '', x)}) ## Remove Special Characters

  dat = dat %>%
    dplyr::select(-ult_evento,-nome,-processo) %>% ## Agrupamento por palavras chave
    dplyr::filter(area_ha > 0)  %>%
    dplyr::mutate(dup = duplicated(id)) %>%
    dplyr::filter(dup == 0) %>% ## This choice is clearly not perfect, need to improve!
    dplyr::select(-dup)

  #%>% dplyr::filter(subs != 'DADO N?O CADASTRADO')

  ## Create Shape with Identifier

  dat_shp = dat %>%
    dplyr::select(id,geometry) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

  dat2 = dat %>%
    sf::st_drop_geometry()

  ## Change Variable Names

  #names(dat)[names(dat) == 'nome'] <- 'empresa'
  #names(dat)[names(dat) == 'subs'] <- 'mineral'

  ######################
  ## Brazil Shapefile ##
  ######################
#
  geo = geobr::read_municipality(simplified = TRUE) %>%
      sf::st_transform(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>%
      dplyr::select(code_muni,abbrev_state)

  ###############
  ## Join Data ##
  ###############

  #######################

  ## Data are in different projections (even transforming, bounding box are not equivalent)
  ## We need to look carefully at this

  #int = sf::st_intersection(geo,dat_shp)

  # dat_shp2 = sf::st_join(geo,dat_shp,join=sf::st_intersects) %>% dplyr::filter(!is.na(id))
  #
  # unmatched_id = dat_shp$id[is.na(dat_shp$code_muni)]
  #
  # dat_shp = dat_shp %>% dplyr::filter(!(id %in% unmatched_id))
  #
  # dat_uf = dat %>% dplyr::select(id,uf)
  #
  # dat_shp = dplyr::left_join(dat_shp,dat_uf,by='id')
  #
  # dat_shp_ok = dplyr::filter(dat_shp,abbrev_state == uf)
  #
  # dat_shp_diff = dplyr::filter(dat_shp,abbrev_state != uf)

    #dat2$duplicated = ifelse(dat2$processo %in% unique(dat2$processo[duplicated(dat2$processo)]),1,0)

    #operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")
    #dat <- sf::st_transform(dat, operation_crs)
    #geo_amazon <- sf::st_transform(geo_amazon, operation_crs)

    # sf::st_agr(a) = "constant"
    # sf::st_agr(geo_amazon) = "constant"
    # al<-sf::st_zm(a)
    # al <- sf::st_intersection(sf::st_make_valid(al), geo_amazon) %>%
    #   dplyr::mutate(calculated_area = sf::st_area(.data$geometry)) %>%
    #   dplyr::group_by(.data$abbrev_state, .data$ANO) %>%
    #   sf::st_drop_geometry()

  ######################
  ## Aggregating Data ##
  ######################

  ## Add States with No data for a given year

  dat_area = dat2 %>%
    dplyr::select(ano,area_ha,uf) %>%
    dplyr::group_by(uf,ano) %>%
    dplyr::summarise(area_ha = sum(area_ha)) %>%
    dplyr::ungroup()

  dat_licensing = dat2 %>%
    dplyr::select(ano,uf,fase) %>%
    dplyr::mutate(fase_num = as.numeric(as.factor(fase))) %>%
    tidyr::pivot_wider(id_cols = c(uf,ano),
                                         names_from = fase,
                                         values_from= fase_num,
                                         values_fn = length,
                                         values_fill = NA) %>% janitor::clean_names()

  dat_commodity = dat2 %>%
    dplyr::select(ano,uf,subs) %>%
    dplyr::mutate(subs_num = as.numeric(as.factor(subs))) %>%
    tidyr::pivot_wider(id_cols = c(uf,ano),
                       names_from = subs,
                       values_from= subs_num,
                       values_fn = length,
                       values_fill = NA) %>% janitor::clean_names()

  dat_usage = dat2 %>%
    dplyr::select(ano,uf,uso) %>%
    dplyr::mutate(uso_num = as.numeric(as.factor(uso))) %>%
    tidyr::pivot_wider(id_cols = c(uf,ano),
                       names_from = uso,
                       values_from= uso_num,
                       values_fn = length,
                       values_fill = NA) %>% janitor::clean_names()



  if (info == 'area'){return(dat_area)}

  if (info == 'licensing'){return(dat_licensing)}

  if (info == 'commodity'){return(dat_subs)}

  if (info == 'usage'){return(dat_usage)}

  if (info == 'all'){

    dat_all = dplyr::left_join(dat_area,dat_licensing,by=c('uf','ano'))
    dat_all = dplyr::left_join(dat_all,dat_commodity,by=c('uf','ano'))
    dat_all = dplyr::left_join(dat_all,dat_usage,by=c('uf','ano'))

    return(dat_all)

  }



  return(ret)
}


