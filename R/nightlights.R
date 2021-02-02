
#' VIIRS Day/Night Band
#'
#' Monthly average radiance composite images using nighttime data from the
#' Visible Infrared Imaging Radiometer Suite (VIIRS) Day/Night Band (DNB).
#'
#' @param db Google Earth Engine Data to be imported
#' @param var Variable belonging to the db dataset
#' @param year_begin Year Begin
#' @param year_end Year End
#' @param type Geometry Type
#'
#' @return
#' @export
#'
#' @examples
#'
load_ee = function(db = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG',var='avg_rad',year_begin = '2013',year_end = '2018',
                   type='munic',nclusters=0){

  ## Initialize Google Earth Engine (EE)

  initialize_ee()

  ## Load Shapefiles

  if (type == 'munic'){shp = geobr::read_municipality()}
  if (type == 'uf'){shp = geobr::read_state()}

  crs = "EPSG:4674"

  begin_day = paste(year_begin,'-01-01',sep='')
  end_day = paste(year_end,'-12-31',sep='')

  ee_data = rgee::ee$ImageCollection(db)$
    filterDate(begin_day,end_day)$
    map(function(x) x$reproject(crs)$select(var))

  ext_fun = function(uf){

    geo = shp %>% dplyr::filter(abbrev_state %in% uf)

    nlight = rgee::ee_extract(x = ee_data, y = geo$geom, sf = TRUE,fun=ee$Reducer$mean())
    nlight = nlight %>% reshape2::melt(id=c('id','geometry'),value.name = var)

    nlight$month = as.numeric(stringr::str_remove(nlight$variable,'avg_rad.'))
    nlight$month = nlight$month + 1
    nlight$month[is.na(nlight$month)] = 1
    nlight$year = ceiling(nlight$month/12)+as.numeric(year_begin)-1

    ## This will depend on panel type

    nlight_avg = nlight %>% dplyr::group_by(id,year) %>%
      dplyr::summarise(mean_rad = mean(avg_rad),sd_avg_rad = sd(avg_rad),geom=sf::st_union(geometry)) %>%
      dplyr::ungroup()

    return(nlight_avg)

  }

  units = unique(shp$abbrev_state)
  # units = units[which(units != "SP")]

  nlight = purrr::map_df(units,ext_fun)

  # ggplot2::ggplot() + ggplot2::geom_sf(data = sf::st_as_sf(nlight_avg[nlight_avg$year==2013,]),
  #                                      ggplot2::aes(fill = mean_rad))

  # df = left_join(geo,nlight_avg)

  return(nlight)

}
