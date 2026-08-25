#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads information the mines being explored legally in Brazil, including their location, status, product being mined and area in square meters.
#'
#' @param dataset A dataset name ("sigmine_active")
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated active mining data in portuguese, by state
#' mining_active <- load_sigmine(
#'   dataset = "sigmine_active",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "pt"
#' )
#'
#' # download treated active mining data in portuguese, by municipality
#' mining_active_munic <- load_sigmine(
#'   dataset = "sigmine_active",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "pt"
#' )
#'
#' @export

load_sigmine <- function(dataset = "sigmine_active",
                         raw_data = FALSE,
                         geo_level = "state",
                         language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- nome <- uf <- processo <- geometry <- NULL
  code_muni <- name_muni <- abbrev_state <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "sigmine"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and geo_level are supported

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  dat <- external_download(dataset = param$dataset, source = param$source) %>%
    janitor::clean_names()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  a <- dat %>%
    dplyr::mutate(dplyr::across(
      nome:uf,
      ~ ifelse(stringr::str_detect(.x, "DADO N.O CADASTRADO"),
        NA, .x
      )
    ))

  a$area_ha <- a$area_ha * 10000
  names(a)[names(a) == "area_ha"] <- "area_m2"

  #########################################
  ## Municipality-level spatial matching ##
  #########################################

  # The raw data only carries a "uf" (state) column natively. To support
  # geo_level = "municipality", we spatially join each mining process to
  # the municipality polygon it falls in, using the package's internal
  # municipality shapefile (same source used by load_prodes()).
  #
  # We use the CENTROID of each mining polygon, not the polygon itself, to
  # avoid double-counting when a mine's polygon straddles two municipality
  # borders (confirmed to happen with the full polygon: ~9600 duplicated
  # "processo" entries when joining on the polygon vs. near-zero with the
  # centroid).
  #
  # The s2 spherical geometry engine is turned off for this join: several
  # polygons in the raw SIGMINE data have invalid geometries (self
  # intersections) that s2 refuses to process even after st_make_valid().
  # Planar geometry is an acceptable approximation at the scale of a single
  # mining process / municipality.

  if (geo_level == "municipality") {
    munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")

    a_sf <- a %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_make_valid()

    sf::sf_use_s2(FALSE)
    a_centroid <- sf::st_centroid(a_sf)
    a_munic <- sf::st_join(a_centroid, munic_shp, join = sf::st_intersects)
    sf::sf_use_s2(TRUE)

    # The raw ANM data itself contains duplicated "processo" entries
    # (confirmed independent of the spatial join: same magnitude of
    # duplicates appears in the raw download). Keep only the first
    # occurrence of each process.
    a_munic <- a_munic %>%
      dplyr::distinct(processo, .keep_all = TRUE)

    a <- a_munic %>%
      sf::st_drop_geometry() %>%
      dplyr::rename(
        municipality_code = code_muni,
        municipality = name_muni,
        state_from_shp = abbrev_state
      )

    # keep the ANM-reported "uf" as-is; municipality shapefile's own
    # abbrev_state is dropped to avoid ambiguity between the two sources
    a$state_from_shp <- NULL
  }

  ##############################
  ## Translate Variable Names ##
  ##############################

  if (language == "pt") {
    names(a)[names(a) == "ult_evento"] <- "ultimo_evento"
    names(a)[names(a) == "nome"] <- "empresa"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "uso"
    if (geo_level == "municipality") {
      names(a)[names(a) == "municipality_code"] <- "cod_municipio"
      names(a)[names(a) == "municipality"] <- "municipio"
      names(a)[names(a) == "code_state"] <- "cod_uf"
      names(a)[names(a) == "name_state"] <- "nome_uf"
      names(a)[names(a) == "code_region"] <- "cod_regiao"
      names(a)[names(a) == "name_region"] <- "nome_regiao"
    }
  } else if (language == "eng") {
    names(a)[names(a) == "numero"] <- "number"
    names(a)[names(a) == "ult_evento"] <- "last_event"
    names(a)[names(a) == "uf"] <- "state"
    names(a)[names(a) == "ano"] <- "year"
    names(a)[names(a) == "processo"] <- "process"
    names(a)[names(a) == "id"] <- "id"
    names(a)[names(a) == "fase"] <- "phase"
    names(a)[names(a) == "nome"] <- "company"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "use"
  }

  return(a)
}
