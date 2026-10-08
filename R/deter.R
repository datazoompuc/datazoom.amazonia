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

  # The crs that will be used to overlap maps below

  # FRAGILE: hardcoded proj4 string (polyconic projection on the South
  # American 1969 ellipsoid, with no datum and no towgs84 shift -- PROJ reads
  # it as "Unknown based on Australian Natl & S. Amer. 1969 ellipsoid")
  # rather than a named/EPSG CRS or IBGE's own published one; nothing checks
  # it against its inputs (geo_municipalities and the raw DETER shapefiles
  # were both SIRGAS 2000, EPSG:4674, in 2026-10). Both inputs go through
  # this same st_transform(), so a wrong choice would not misalign alerts
  # and municipalities. What it changes, silently, is the `area` column: the
  # st_area() below is a planar area in a polyconic (not equal-area)
  # projection centred on lon -54, and it drifts upward from an Albers
  # equal-area reference the further west an alert sits. Measured 2026-10 on
  # the treated output: +0.14% in total for deter_pantanal, +0.51% for
  # deter_non_forest, by alert longitude +0.1% near -52, +0.7% near -62,
  # +1.9% near -67 and +5% west of -70.
  operation_crs <- sf::st_crs("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs")

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
  # 2026-10, 16 of deter_amz's 461,081 raw alerts, 7 of deter_cerrado's
  # 134,165 and 2 of deter_pantanal's 24,155 have no row at all in the
  # output (none lost for deter_non_forest; cause not investigated).
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
