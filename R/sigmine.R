#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads information the mines being explored legally in Brazil, including their location, status, product being mined and area in square meters.
#'
#' @param dataset A dataset name ("sigmine_active" or "sigmine_inactive"). Note that "sigmine_inactive" has fewer columns available than "sigmine_active" (no mineral, company, phase, last event, or state information at the source) -- this reflects what ANM publishes for closed/inactive processes, not a limitation of this function.
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
#' # download treated inactive (closed) mining data, by municipality
#' mining_inactive_munic <- load_sigmine(
#'   dataset = "sigmine_inactive",
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

  ######################################################
  ## Data Engineering -- dataset == "sigmine_active"   ##
  ######################################################

  if (dataset == "sigmine_active") {
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

    if (geo_level == "municipality") {
      munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")

      a_sf <- a %>%
        sf::st_zm(drop = TRUE, what = "ZM") %>%
        sf::st_make_valid()

      operation_crs <- sf::st_crs("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

      a_sf_proj     <- sf::st_transform(a_sf, operation_crs)
      munic_shp_proj <- sf::st_transform(munic_shp, operation_crs)

      a_centroid <- sf::st_centroid(a_sf_proj)
      a_munic <- sf::st_join(a_centroid, munic_shp_proj, join = sf::st_intersects)

      a_munic <- a_munic %>%
        dplyr::distinct(processo, .keep_all = TRUE)

      a <- a_munic %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-uf) # Remove a coluna uf nativa para evitar duplicidade com abbrev_state
    }

    if (language == "pt") {
      names(a)[names(a) == "ult_evento"] <- "ultimo_evento"
      names(a)[names(a) == "nome"]       <- "empresa"
      names(a)[names(a) == "subs"]       <- "mineral"
      names(a)[names(a) == "uso"]        <- "uso"

      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "cod_municipio"
        names(a)[names(a) == "name_muni"]     <- "municipio"
        names(a)[names(a) == "code_state"]    <- "cod_uf"
        names(a)[names(a) == "abbrev_state"]  <- "uf"
        names(a)[names(a) == "name_state"]    <- "nome_uf"
        names(a)[names(a) == "code_region"]   <- "cod_regiao"
        names(a)[names(a) == "name_region"]   <- "nome_regiao"
      }
    } else if (language == "eng") {
      names(a)[names(a) == "numero"]     <- "number"
      names(a)[names(a) == "ult_evento"] <- "last_event"
      names(a)[names(a) == "uf"]         <- "state"
      names(a)[names(a) == "ano"]        <- "year"
      names(a)[names(a) == "processo"]   <- "process"
      names(a)[names(a) == "id"]         <- "id"
      names(a)[names(a) == "fase"]       <- "phase"
      names(a)[names(a) == "nome"]       <- "company"
      names(a)[names(a) == "subs"]       <- "mineral"
      names(a)[names(a) == "uso"]        <- "use"

      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "municipality_code"
        names(a)[names(a) == "name_muni"]     <- "municipality"
        names(a)[names(a) == "code_state"]    <- "state_code"
        names(a)[names(a) == "abbrev_state"]  <- "state"
        names(a)[names(a) == "name_state"]    <- "state_name"
        names(a)[names(a) == "code_region"]   <- "region_code"
        names(a)[names(a) == "name_region"]   <- "region_name"
      }
    }
  }

  ########################################################
  ## Data Engineering -- dataset == "sigmine_inactive"   ##
  ########################################################

  if (dataset == "sigmine_inactive") {
    a <- dat
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "area_ha"] <- "area_m2"

    if (geo_level == "municipality") {
      munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")

      a_sf <- a %>%
        sf::st_zm(drop = TRUE, what = "ZM") %>%
        sf::st_make_valid()

      operation_crs <- sf::st_crs("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

      a_sf_proj     <- sf::st_transform(a_sf, operation_crs)
      munic_shp_proj <- sf::st_transform(munic_shp, operation_crs)

      a_centroid <- sf::st_centroid(a_sf_proj)
      a_munic <- sf::st_join(a_centroid, munic_shp_proj, join = sf::st_intersects)

      a_munic <- a_munic %>%
        dplyr::distinct(processo, .keep_all = TRUE)

      a <- a_munic %>%
        sf::st_drop_geometry()

      # Remove a coluna 'uf' nativa (se existir) para evitar duplicidade com a vinda do IBGE ('abbrev_state')
      if ("uf" %in% names(a)) {
        a <- dplyr::select(a, -uf)
      }
    }

    if (language == "pt") {
      names(a)[names(a) == "ds_processo"] <- "descricao_processo"
      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "cod_municipio"
        names(a)[names(a) == "name_muni"]     <- "municipio"
        names(a)[names(a) == "code_state"]    <- "cod_uf"
        names(a)[names(a) == "abbrev_state"]  <- "uf"
        names(a)[names(a) == "name_state"]    <- "nome_uf"
        names(a)[names(a) == "code_region"]   <- "cod_regiao"
        names(a)[names(a) == "name_region"]   <- "nome_regiao"
      }
    } else if (language == "eng") {
      names(a)[names(a) == "numero"]        <- "number"
      names(a)[names(a) == "ano"]           <- "year"
      names(a)[names(a) == "processo"]      <- "process"
      names(a)[names(a) == "ds_processo"]   <- "process_description"
      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]    <- "municipality_code"
        names(a)[names(a) == "name_muni"]    <- "municipality"
        names(a)[names(a) == "code_state"]   <- "state_code"
        names(a)[names(a) == "abbrev_state"] <- "state"
        names(a)[names(a) == "name_state"]   <- "state_name"
        names(a)[names(a) == "code_region"]  <- "region_code"
        names(a)[names(a) == "name_region"]  <- "region_name"
      }
    }
  }

  return(a)
}
