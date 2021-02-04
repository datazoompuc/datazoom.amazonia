
#' VIIRS Day/Night Band
#'
#' Monthly average radiance composite images using nighttime data from the
#' Visible Infrared Imaging Radiometer Suite (VIIRS) Day/Night Band (DNB).
#'
#' @param db Google Earth Engine Data to be imported (TO-DO: Include support for other versions)
#' @param var Variable belonging to the db dataset
#' @param year_begin Year Begin
#' @param year_end Year End
#' @param type Geometry Type. Can be either "municip" or "uf". (TO-DO: Next version will include support for rasters)
#' @param state Municipalities of Interest; Can be 'all', 'RJ', ... (TO-DO: Include support for Legal Amazon)
#' @param nclusters Numbers of cluster to be used (TO:DO: Still need to implement this properly)
#'
#' @return A \code{tibble}
#' @export load_earth_engine
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @examples
#'
#' \dontrun{
#' load_ee(db = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG',state=c('RJ'))
#' }
#'
load_earth_engine = function(db = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG',var='avg_rad',year_begin = '2014',year_end = '2018',
                   type='municipality',state = 'all',nclusters=0){

  ## Initialize Google Earth Engine (EE)

  initialize_ee()

  ## Load Shapefiles

  if (type == 'municipality'){
    shp = geobr::read_municipality()
  }
  if (type == 'state'){shp = geobr::read_state()}

  if (state != 'all'){shp = shp %>% dplyr::filter(abbrev_state %in% state)}

  ## Picking Large Geometries

  if (type == 'municipality'){
  mun_num = shp %>% sf::st_drop_geometry() %>% dplyr::group_by(abbrev_state) %>%
    dplyr::summarise(count=length(code_muni))
  large = mun_num[mun_num$count >= 300,]$abbrev_state
  large = c(large,'MT')
  }

  ## Set CRS

  crs = "EPSG:4674"

  ## Define Begin Periods

  begin_day = paste(year_begin,'-01-01',sep='')
  end_day = paste(year_end,'-12-31',sep='')

  ## Select Image from Earth Engine

  ee_data = rgee::ee$ImageCollection(db)$
    filterDate(begin_day,end_day)$
    map(function(x) x$reproject(crs)$select(var))

  ## Define Units - if dissagregated (lower than state level, we may need to make this finer)

  units = unique(shp$abbrev_state)

  if (nclusters == 0){ee_df = purrr::map_df(units,ext_fun,
                                            shp=shp,ee_data=ee_data,large=large,type=type,un=units,var=var,
                                            year_begin=year_begin)}
  #if (nclusters > 0){nlight = pbapply::pblapply(units,ext_fun)} # We also have to unlist

  return(ee_df)

}

## Create Function to Extract Data for each UF

ext_fun = function(uf,shp=shp,ee_data=ee_data,large=large,type=type,un=units,var=var,year_begin=year_begin){

  ## Indicator of Progress

  print(paste("Working in",uf,"--",which(uf == unique(shp$abbrev_state)),"of",length(unique(un)),sep=' '))

  geo = shp %>% dplyr::filter(abbrev_state %in% uf)
  geo$id = 1:nrow(geo) ## This is super important to recover geometries later

  if (uf %in% large & type == 'municipality'){

    n1 = rgee::ee_extract(x = ee_data, y = geo$geom[1:ceiling(nrow(geo)/3)], sf = TRUE,fun=rgee::ee$Reducer$mean())
    n1$id = 1:ceiling(nrow(geo)/3)

    n2 = rgee::ee_extract(x = ee_data, y = geo$geom[(ceiling(nrow(geo)/3)+1):(2*(ceiling(nrow(geo)/3)))],
                          sf = TRUE,fun=rgee::ee$Reducer$mean())
    n2$id = (ceiling(nrow(geo)/3)+1):(2*(ceiling(nrow(geo)/3)))

    n3 = rgee::ee_extract(x = ee_data, y = geo$geom[(2*(ceiling(nrow(geo)/3))+1):nrow(geo)], sf = TRUE,
                          fun=rgee::ee$Reducer$mean())
    n3$id = (2*(ceiling(nrow(geo)/3))+1):nrow(geo)

    n1 = n1 %>% reshape2::melt(id=c('id','geometry'),value.name = var)
    n2 = n2 %>% reshape2::melt(id=c('id','geometry'),value.name = var)
    n3 = n3 %>% reshape2::melt(id=c('id','geometry'),value.name = var)

    ee_df = rbind(n1,n2,n3)
  }else{
    ee_df = rgee::ee_extract(x = ee_data, y = geo$geom, sf = TRUE,fun=rgee::ee$Reducer$mean())
    ee_df = ee_df %>% reshape2::melt(id=c('id','geometry'),value.name = var)
  }

  ## Editing Month and Year Variables

  ee_df$month = as.numeric(stringr::str_remove(ee_df$variable,'avg_rad.')) ## Edit
  ee_df$month = ee_df$month + 1
  ee_df$month[is.na(ee_df$month)] = 1
  ee_df$year = ceiling(ee_df$month/12)+as.numeric(year_begin)-1

  ## This will depend on panel type
  ee_df = ee_df %>% dplyr::select(-geometry,-variable)

  ee_df = ee_df %>% dplyr::group_by(id,year) %>%
    dplyr::summarise(mean_rad = base::mean(avg_rad),sd_rad = stats::sd(avg_rad)) %>%
    dplyr::ungroup()

  ee_df = dplyr::inner_join(ee_df,geo,by='id') %>% dplyr::select(-id)

  return(ee_df)

}

## Create Function to Initialize Earth Engine

initialize_ee = function(){

  if (!requireNamespace("rgee", quietly = TRUE)) {
    stop("Package \"rgee\" needed for this function to work. Please install it
    running remotes::install_github(\"r-spatial/rgee\") and then run rgee::ee_install()
    to proceed with configuration of the Python environment",
         call. = FALSE)
  }else{rgee::ee_Initialize()}

}

