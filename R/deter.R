#' @title DETER - Forest Degradation in the Brazilian Amazon, Cerrado and Pantanal
#'
#' @description Loads data on changes in forest cover and vegetation detected by DETER,
#' across four biome/coverage products: the Legal Amazon, the Cerrado biome, the Pantanal
#' biome, and non-forest areas within the Legal Amazon.
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado", "deter_pantanal",
#' "deter_non_forest") with information about the Legal Amazon, Cerrado, Pantanal, and
#' non-forest areas within the Legal Amazon, respectively. Note that the raw source data
#' uses different class-name vocabularies across these four products (upper-snake-case
#' codes for deter_amz/deter_cerrado, e.g. "DESMATAMENTO_CR"; lowercase Portuguese labels
#' for deter_pantanal/deter_non_forest, e.g. "cicatriz de queimada") -- this is not
#' normalized by \code{load_deter()}, the returned \code{alert_type} column keeps
#' whichever vocabulary the source used for that biome.
#' @inheritParams load_baci
#'
#' @return A \code{sf} object.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated DETER Amazon data
#' deter_amz <- load_deter(
#'   dataset = "deter_amz",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # download treated DETER Cerrado data
#' deter_cerrado <- load_deter(
#'   dataset = "deter_cerrado",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # download treated DETER Pantanal data
#' deter_pantanal <- load_deter(
#'   dataset = "deter_pantanal",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # download treated DETER non-forest (Legal Amazon) data
#' deter_non_forest <- load_deter(
#'   dataset = "deter_non_forest",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' @export

load_deter <- function(dataset, raw_data = FALSE,
                       language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  .data <- view_date <- name_muni <- code_muni <- sensor <- satellite <- abbrev_state <- NULL
  uc <- classname <- path_row <- area <- quadrant <- geometry <- id_alerta <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "deter"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset is valid
  check_params(param)

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    dataset = param$dataset,
    source = param$source
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ stringi::stri_trans_general(., id = "Latin-ASCII")
      )
    )

  # FRAGILE: deter_pantanal and deter_non_forest ship the class column as
  # CLASS_NAME (class_name after clean_names()), not CLASSNAME like
  # deter_amz and deter_cerrado do -- confirmed live during the 2026-10
  # onboarding (without this rename the select() below silently drops
  # alert_type for those two: classname is bound to NULL above, which
  # tidyselect reads as "select nothing" rather than as a missing column,
  # so the output looked fine except for one vanished column). If
  # TerraBrasilis ever renames the column again, or a fifth DETER product
  # ships yet another spelling, this check doesn't widen itself -- it only
  # recognizes exactly "class_name" vs "classname".
  if ("class_name" %in% names(dat) && !("classname" %in% names(dat))) {
    dat <- dplyr::rename(dat, classname = "class_name")
  }

  # Loading municipal map data
  geo_br <- external_download(
    dataset = "geo_municipalities",
    source = "internal"
  )

  # Adding alert_id variable to preserve the information of which rows belong to the same alert

  dat <- dat %>%
    dplyr::mutate(id_alerta = dplyr::row_number())

  ###################
  ## Harmonize CRS ##
  ###################

  # The crs that will be used to overlap maps below -- IBGE's own official
  # equal-area projection for computing municipal/territorial areas
  # ("Projecao Conica Equivalente de Albers, definida pelo IBGE"), confirmed
  # 2026-10-08 against IBGE, "Malha Municipal Digital e Areas Territoriais
  # 2024" (Notas metodologicas 01/2025, Rio de Janeiro, 2025), the Appendix B
  # proj4 definition: central meridian -54, latitude of origin -12, standard
  # parallels -2/-22, false easting/northing 5,000,000/10,000,000, SIRGAS
  # 2000 datum on the GRS80 ellipsoid.
  #
  # Replaces the previous hardcoded string, which turned out to be IBGE's
  # OTHER published Brazil-wide projection -- the non-equal-area "SIRGAS
  # 2000 / Brazil Polyconic" (EPSG:5880, meant for general small-scale
  # mapping, not area calculations) -- with its ellipsoid typo'd on top of
  # that, to a legacy/unrelated PROJ preset ("aust_SA", which PROJ reads as
  # "Unknown based on Australian Natl & S. Amer. 1969 ellipsoid") instead of
  # GRS80/SIRGAS 2000. That wrong combination measurably biased the `area`
  # column -- see NEWS.md and this file's git history for the confirmed
  # numbers (up to +5% west of -70 degrees) -- without ever erroring, since
  # both inputs (geo_municipalities and the raw DETER shapefiles, both
  # SIRGAS 2000/EPSG:4674) went through the same wrong transform and stayed
  # mutually aligned regardless. The identical wrong string is still used by
  # R/degrad.R's own operation_crs -- out of scope for this change, flagged
  # separately rather than fixed silently alongside this one.
  #
  # FRAGILE: hand-transcribed from IBGE's PDF rather than a named EPSG code
  # -- IBGE's own document notes this projection "has no standard EPSG
  # code" as of its writing; EPSG:10857 ("SIRGAS 2000 / Brazil Albers")
  # was registered later (revision date 2025-05-16) encoding the identical
  # parameters, but isn't used here to avoid depending on a PROJ/EPSG
  # database recent enough to recognize it -- worth switching to
  # `sf::st_crs(10857)` once that's confirmed safe on every environment this
  # package runs in. If IBGE ever revises these parameters in a future
  # "Malha Municipal" edition, this string needs updating by hand; nothing
  # here re-checks it against a live IBGE source.
  operation_crs <- sf::st_crs(
    "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
  )

  # Changing crs of both data to the common crs chosen above
  dat$geometry <- sf::st_make_valid(sf::st_transform(dat$geometry, operation_crs))
  geo_br$geom <- sf::st_transform(geo_br$geom, operation_crs)

  # Overlaps shapefiles
  sf::st_geometry(dat) <- dat$geometry
  sf::st_geometry(geo_br) <- geo_br$geom

  # FRAGILE: sf::st_intersection() against municipality polygons can split
  # a single DETER alert into more than one output row whenever that
  # alert's polygon straddles a municipality boundary -- id_alerta (set
  # above, before this join) is the only thing that lets a consumer
  # reconstruct "how many distinct alerts" from "how many rows", and
  # row-counting this output without grouping by alert_id will silently
  # overcount alerts. Some alerts also vanish here altogether: measured
  # 2026-10-08 with the Albers CRS above, 16 of deter_amz's 461,071 raw
  # alerts, 6 of deter_cerrado's 134,165 and 2 of deter_pantanal's 24,155
  # have no row at all in the output (none lost for deter_non_forest; cause
  # not investigated). Boundary slivers differ between projections, so the
  # counts can move by one or two (the old polyconic CRS gave 7 for
  # deter_cerrado) and the number of output rows by a few dozen (under 2
  # km2 of area in total) -- compare alert counts via alert_id, not rows.
  # suppressWarnings() also hides any CRS/topology warning
  # st_intersection() would otherwise raise here, not just the expected
  # "attributes are assumed to be constant" one.
  dat <- suppressWarnings(sf::st_intersection(dat, geo_br)) %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))

  dat <- dat %>%
    dplyr::select(
      view_date, name_muni, code_muni, abbrev_state, classname,
      area, geometry, id_alerta
    )

  ###################
  ## Renaming Data ##
  ###################

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "data" = view_date,
        "municipio" = name_muni,
        "cod_municipio" = code_muni,
        "uf" = abbrev_state,
        "tipo_de_alerta" = classname
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "date" = view_date,
        "municipality" = name_muni,
        "municipality_code" = code_muni,
        "state" = abbrev_state,
        "alert_id" = id_alerta,
        "alert_type" = classname
      )
  }

  #################
  ## Return Data ##
  #################

  return(dat_mod)
}
