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

  # FRAGILE: the raw DEGRAD files do NOT all share a CRS. Verified live
  # 2026-10-12: 2007-2009 carry none at all; 2010-2014 embed SAD69
  # (EPSG:4618, with its own TOWGS84); 2015-2016 embed SIRGAS 2000. Binding
  # years from different groups directly used to fail with "arguments have
  # different crs" (2009:2010, 2014:2015, 2007:2016 all crashed; 2010:2012
  # and 2010:2014 happened to stay inside one group and worked, by luck of
  # the request, not because anything was actually correct).
  #
  # Fixed 2026-10-16 by harmonizing CRS PER YEAR here, right after download,
  # before bind_rows() -- every year is transformed to one common target,
  # SIRGAS 2000 geographic (EPSG:4674; the 2015-2016 files already carry an
  # equivalent SIRGAS 2000 CRS and move by exactly 0.0 m), via a real
  # sf::st_transform(), not a label swap. Years with no CRS at all
  # (2007-2009) get the pre-existing SAD69 longlat approximation assigned
  # first (aust_SA + this towgs84 string -- unchanged, not reverified here,
  # same as before), then transformed onward like every other year.
  #
  # This incidentally also closes the separate datum-shift bug documented at
  # operation_crs below. The shift comes from the TOWGS84 that the 2010-2014
  # .prj files carry and that the 2007-2009 fallback string above spells out
  # (plain EPSG:4618 would give almost the same, 0.3 m apart). Before this
  # fix, operation_crs's missing datum made that shift 0 m. Measured
  # 2026-10-16 on the real data through this step: SAD69-era polygons end up
  # 59.5 to 75.0 m (median 65-69 m) from where the raw file's coordinates
  # would sit if read as SIRGAS 2000; 2016, exactly 0.0 m. Effect on the
  # output of moving them (vs the committed code without this step, same
  # target CRS): total area under 0.003% a year; per-municipality area median
  # 0.004-0.1% and 95th percentile 1.2-2.2% in the SAD69 years, exactly
  # unchanged for 2015-2016, the largest moves (4-15%) being on municipalities
  # that carry only 1.5-4 km2 of DEGRAD that year; output rows -14 to +3 per
  # year (-36 over the 10 years); 7 municipality-years appear or disappear,
  # slivers of 0.03 km2 in total. So this changes real output
  # coordinates/municipality attribution for 2007-2014, not just the
  # previously-crashing cross-group combinations.
  harmonize_degrad_year_crs <- function(d) {
    if (is.na(sf::st_crs(d$geometry))) {
      d$geometry <- sf::st_set_crs(
        d$geometry,
        "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs"
      )
    }
    d$geometry <- sf::st_transform(d$geometry, sf::st_crs(4674))
    d
  }
  dat <- purrr::map(dat, harmonize_degrad_year_crs)

  # Join all tables while preserving sf objects -- every year is now in the
  # same CRS (EPSG:4674), so this can no longer fail on a cross-group
  # time_period.
  dat <- dplyr::bind_rows(dat)

  # Remove useless column
  dat <- dat %>%
    dplyr::select(-dplyr::any_of(c("nome", "codigouf", "area")))

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
  # GRS 1980 ellipsoid"). Until 2026-10-16 this mattered: `dat` could still
  # arrive here in SAD69 (2010-2014) and this string's missing datum made
  # PROJ do a zero-shift transform instead of a real one (see the tag above
  # the download loop). Since 2026-10-16, every year is harmonized to SIRGAS
  # 2000 (EPSG:4674) before it ever reaches this point, so this transform's
  # input is always already in the right datum -- the same situation
  # R/deter.R's own data is always in (its raw files are already SIRGAS 2000
  # throughout) -- and the missing datum tag here is no longer consequential
  # for DEGRAD either. Until 2026-10-12 this was a near-copy of EPSG:5880
  # ("SIRGAS 2000 / Brazil Polyconic," not equal-area) with its ellipsoid
  # mistyped to the legacy preset "aust_SA"; R/deter.R carried the same
  # string until 2026-10-08 (commit 5aa2a22, see NEWS.md). The old string
  # inflated `area` the further a polygon sits from longitude -54. Measured
  # 2026-10-16 on the current code, original polyconic string vs current, all
  # 10 years loaded one at a time: +0.41% in total (+0.30% to +0.74% by year);
  # by longitude, +4.8% west of -70, +2.5% in (-70,-65], +1.0% in (-65,-60],
  # +0.2% in (-60,-55], +0.06% in (-55,-50], +0.7% in (-50,-45] and +1.5% east
  # of -45 (the datum fix above moves the total by under 0.003% a year: a
  # shift does not change an area). The current `area` matches an ellipsoidal
  # geodesic area (lwgeom, GRS80) and a Lambert equal-area one to within
  # 0.001% (1st-99th percentile) in every year. The named EPSG codes for this
  # projection (10856/10857, "SIRGAS 2000 / Brazil Albers," registered
  # 2025-05-16) silently resolve to NA on PROJ 9.3.1 / sf 1.0.19 -- hence the
  # proj4 string; re-test before ever switching.
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
  # turned back into a polygon count. Measured 2026-10-16 on the current
  # code: the 10 years hold 227,218 raw polygons and 231,453 output rows
  # (+1.9% net; 2007: 44,012 -> 44,773). Which sliver rows exist depends on
  # the projection and on the datum: against the original polyconic code the
  # output differs by -14 to +6 rows per year (-30 over the 10 years, 0.013%),
  # and the real datum shift of the SAD69-era polygons (see the tag above the
  # download loop) changes it by -14 to +3 rows per year (-36 over the 10
  # years; exactly 0 for 2015-2016).
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
