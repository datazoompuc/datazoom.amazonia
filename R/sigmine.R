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
  # Several polygons in the raw SIGMINE data have invalid geometries (self
  # intersections) that trip up spatial operations even after
  # st_make_valid(); the fix used here is reprojecting to a planar CRS
  # before computing centroids (see below), rather than disabling s2.

  if (geo_level == "municipality") {
    munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")

    a_sf <- a %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_make_valid()

    # Reprojeta para um CRS planar brasileiro antes de calcular o centroide,
    # em vez de desligar o s2 (que fazia o GEOS tratar graus de lon/lat como
    # coordenadas cartesianas planas, distorcendo o resultado -- pior
    # exatamente nos polígonos alongados que cruzam fronteira municipal,
    # caso que esta técnica tenta resolver bem).
    #
    # Projeção Albers Equal Area (+proj=aea), não Polyconic: um centroide é
    # uma média ponderada por área, e Albers preserva área localmente (fator
    # de escala de área = 1 em toda a projeção); Polyconic é adequada para
    # cálculos de distância, não de área, e pode enviesar o centroide.
    # Parâmetros padrão do IBGE para o Brasil: meridiano central -54°,
    # latitude de origem -12°, paralelos padrão -2° e -22°.
    #
    # Elipsoide GRS80, não aust_SA (datum SAD69): nem o shapefile de
    # municípios (SIRGAS2000/EPSG:4674) nem os dados de origem da ANM
    # (também SIRGAS2000) usam SAD69 -- aust_SA forçava uma conversão de
    # datum desnecessária antes mesmo do cálculo do centroide.
    operation_crs <- sf::st_crs("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

    a_sf_proj     <- sf::st_transform(a_sf, operation_crs)
    munic_shp_proj <- sf::st_transform(munic_shp, operation_crs)

    a_centroid <- sf::st_centroid(a_sf_proj)
    a_munic <- sf::st_join(a_centroid, munic_shp_proj, join = sf::st_intersects)

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
        municipality = name_muni
      )

    # The ANM-reported "uf" column has ~4% missing values (confirmed via
    # sum(is.na(dat$uf))), which fragments municipalities into two separate
    # groups downstream when "uf" is used to build a group key (e.g.
    # "Juruti - PA" and "Juruti - NA" for the same municipality). The
    # municipality shapefile's abbrev_state, coming from the spatial join,
    # is always present and authoritative for which state the municipality
    # belongs to -- so we overwrite the ANM's "uf" with it instead of
    # keeping both columns.
    a$uf <- a_munic$abbrev_state
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
