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
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated forest degradation data from 2010 to 2012
#' data <- load_degrad(
#'   dataset = "degrad",
#'   raw_data = FALSE,
#'   time_period = 2010:2012,
#'   language = "eng"
#' )
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
  param$source <- "degrad"
  param$dataset <- dataset
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and time_period are valid

  check_params(param)

  # .shp file names for each year (one manifest row per year --
  # see inst/extdata/manifest/v1/datasets_link.csv)

  file_names <- param$time_period %>%
    purrr::map_chr(~ dataset_field(param$source, param$dataset, "archive_file", year = .))


  ######################
  ## Downloading Data ##
  ######################

  dat <- base::suppressWarnings(purrr::map2(
    param$time_period, file_names,
    function(year, file) {
      external_download(
        dataset = param$dataset,
        source = param$source,
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

  # Join all tables while preserving sf objects
  dat <- dplyr::bind_rows(dat)

  # Remove useless column
  dat <- dat %>%
    dplyr::select(-dplyr::any_of(c("nome", "codigouf", "area")))


  # Clean geometry
  # FRAGILE: the raw DEGRAD files do NOT all lack a CRS. Read live 2026-10-12:
  # 2007-2009 have none (the st_set_crs() below only ever applies to a load
  # made of those years, and assumes SAD69 like their neighbours -- not
  # verified); 2010-2014 embed SAD69 (EPSG:4618, with a TOWGS84); 2015-2016
  # embed SIRGAS 2000. Three consequences. (1) The three groups have
  # different CRSs, so a time_period that crosses a group boundary (2009:2010,
  # 2014:2015, 2007:2016) already fails in the bind_rows() above with
  # "arguments have different crs"; 2010:2012 and 2010:2014 work. (2) The
  # datum is never applied: operation_crs below names no datum, so PROJ does
  # a zero-shift ballpark transform and ignores the +towgs84 of this block and
  # of the embedded CRSs alike (0.000 m, measured), whereas a datum-aware
  # transform moves SAD69 points by 65-75 m. SAD69-era polygons (2007-2014)
  # are therefore overlaid on SIRGAS 2000 municipalities unshifted. Areas do
  # not care; municipality attribution near boundaries does. Measured with
  # IBGE's own WKT (it names D_SIRGAS2000) as operation_crs: 2012 total area
  # -0.0001%, +2 rows, per-municipality area median 0.03%. The two largest
  # individual moves found are from DIFFERENT municipalities, both small in
  # area (a fixed metric shift reads as a large percentage there): Santa
  # Maria das Barreiras (PA), +13.2%, DEGRAD polygon area there only
  # 1.97 km2; Barra do Corda (MA), -0.15%, at a similar scale, 0.72 km2.
  # 2016, already SIRGAS, unchanged.
  # (3) None of this is re-checked per download.
  if (is.na(sf::st_crs(dat$geometry))) {
    dat$geometry <- sf::st_set_crs(
      dat$geometry,
      "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs"
    )
  }

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
  geo_amazon <- dplyr::filter(
    geo_br,
    as.numeric(.data$code_muni) %in% as.numeric(amazon_municipalities$code_muni)
  )

  ###################
  ## Harmonize CRS ##
  ###################

  # The crs that will be used to overlap maps below
  # FRAGILE: hand-transcribed proj4 string for the projection IBGE itself uses
  # to compute official areas, its "Projecao Conica Equivalente de Albers"
  # (IBGE, "Malha Municipal Digital e Areas Territoriais 2024," Notas
  # metodologicas 01/2025, Apendice B, p. 21): central meridian -54, standard
  # parallels -2/-22, GRS80. It names no datum (PROJ reads "Unknown based on
  # GRS 1980 ellipsoid"), which is why the datum shifts in the tag above are a
  # no-op. Until 2026-10-12 this was a near-copy of EPSG:5880 ("SIRGAS 2000 /
  # Brazil Polyconic," not equal-area) with its ellipsoid mistyped to the
  # legacy preset "aust_SA"; R/deter.R carried the same string until
  # 2026-10-08 (commit 5aa2a22, see NEWS.md). The old string inflated `area`
  # the further a polygon sits from longitude -54. Measured 2026-10-12, old vs
  # new CRS, all 10 years: +0.41% in total (+0.30% to +0.74% by year); by
  # longitude, +4.7% west of -70, +2.5% in (-70,-65], +1.0% in (-65,-60],
  # +0.2% in (-60,-55], +0.06% in (-55,-50], +0.7% in (-50,-45] and +1.4% east
  # of -45. The new `area` matches an ellipsoidal geodesic area (lwgeom,
  # GRS80) and a Lambert equal-area one to within 0.001% (1st-99th
  # percentile) in every year. The named EPSG codes for this projection
  # (10856/10857, "SIRGAS 2000 / Brazil Albers," registered 2025-05-16)
  # silently resolve to NA on PROJ 9.3.1 / sf 1.0.19 -- hence the proj4
  # string; re-test before ever switching.
  operation_crs <- sf::st_crs("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

  # Changing crs of both data to the common crs chosen above
  dat$geometry <- sf::st_make_valid(sf::st_transform(dat$geometry, operation_crs))
  geo_amazon$geom <- sf::st_make_valid(sf::st_transform(geo_amazon$geom, operation_crs))

  ##########################################################
  ## Aggregation Municipality or State Level x Time Level ##
  ##########################################################

  # Overlaps shapefiles
  sf::st_geometry(dat) <- dat$geometry
  sf::st_geometry(geo_amazon) <- geo_amazon$geom

  # FRAGILE: a DEGRAD polygon overlapping a municipal boundary can be split
  # into multiple output rows by st_intersection() below, the same way
  # R/deter.R's municipality overlay can split one alert into several rows
  # (see that file's own FRAGILE tag) -- there's no stable scar/alert id in
  # the raw DEGRAD shapefiles to reassemble these by, and polygons touching
  # no Legal Amazon municipality drop out altogether, so rows cannot be
  # turned back into a polygon count. Measured 2026-10-12: the 10 years hold
  # 227,218 raw polygons and 231,489 output rows (+1.9% net; 2007: 44,012 ->
  # 44,781). Which sliver rows exist depends on the projection: the old and
  # new operation_crs differ by -1 to +6 rows per year (+6 over the 10 years,
  # 0.003%), and no municipality appears or disappears.
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
