#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_mining", "mapbiomas_water" or "mapbiomas_fire")
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data
#'   * For dataset "mapbiomas_cover", can only be "municipality"
#'   * For dataset "mapbiomas_transition", can be "municipality" or "biome" (faster download)
#'   * For dataset "mapbiomas_deforestation_regeneration", can only be "municipality"
#'   * For dataset "mapbiomas_mining", can be "indigenous_land" or "municipality"
#'   * For dataset "mapbiomas_irrigation" (temporarily unavailable, a new collection will be soon delivered), can be "state" or "biome"
#'   * For dataset "mapbiomas_water"(temporarily unavailable, a new collection will be soon delivered), can be "municipality", "state" or "biome"
#'   * For dataset "mapbiomas_fire", can only be "state"
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated Mapbiomas Cover data in English
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_cover",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "eng"
#' )
#'
#' # download treated data on mining on indigenous lands
#' data <- load_mapbiomas("mapbiomas_mining",
#'   raw_data = FALSE,
#'   geo_level = "indigenous_land"
#' )
#' }
#'
#' @export

load_mapbiomas <- function(dataset, raw_data = FALSE, geo_level = "municipality",
                           language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  value <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "mapbiomas"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and geo_level are supported

  check_params(param)

  # plucking sheet corresponding to each dataset/geo_level

  sheets <- tibble::tribble(
    ~dataset, ~geo_level, ~sheet,
    "mapbiomas_cover", "any", "COVERAGE_9",
    "mapbiomas_transition", "biome", "TRANSITION_9",
    "mapbiomas_transition", "municipality", "TRANSITION_9",
    "mapbiomas_deforestation_regeneration", "municipality", "DEF_SECVEG",
    "mapbiomas_irrigation", "state", "UF",
    "mapbiomas_irrigation", "biome", "BIOME",
    "mapbiomas_mining", "municipality", "CITY_STATE_BIOME",
    "mapbiomas_mining", "indigenous_land", "IL",
    "mapbiomas_water", "state", "states_annual",
    "mapbiomas_water", "biome", "biomes_annual",
    "mapbiomas_water", "municipality", "mun_annual",
    "mapbiomas_fire", "state", "a_ANNUAL",
  )

  sheet <- sheets %>%
    dplyr::filter(
      dataset == param$dataset,
      geo_level %in% c(param$geo_level, "any")
    ) %>%
    dplyr::select(sheet) %>%
    unlist()

  ## MapBiomas collections

  if (dataset %in% c(
    "mapbiomas_cover",
    "mapbiomas_transition",
    "mapbiomas_deforestation_regeneration"
  )) {
    message("Data from MapBiomas - Collection 9\n")
  }

  if (dataset %in% c("mapbiomas_mining")) {
    message("Data from Mapbiomas - Collection 8\n")
  }

  if (dataset %in% c("mapbiomas_irrigation")) {
    message("Data from Mapbiomas - Collection 7\n")
  }

  if (dataset %in% c("mapbiomas_fire")) {
    message("Data from Mapbiomas - Collection 3\n")
  }

  if (dataset %in% c("mapbiomas_water")) {
    message("Data from Mapbiomas - Collection 2\n")
  }

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    dataset = param$dataset,
    source = param$source,
    geo_level = param$geo_level,
    sheet = sheet
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
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  if (param$dataset != "mapbiomas_water") {
    dat <- dat %>%
      dplyr::select(-dplyr::contains("id"))

    # reshaping

    dat <- dat %>%
      tidyr::pivot_longer(
        dplyr::starts_with("x"),
        names_to = "year",
        values_to = "value",
        names_prefix = "x"
      ) %>%
      tidyr::drop_na(value)
  } else {
    if (param$geo_level == "municipality") {
      dat <- dat %>%
        dplyr::rename("municipality_code" = "code")
    }
    if (param$geo_level == "biome") {
      dat <- dat %>%
        dplyr::rename(
          "biome_code" = "code", "biome" = "name"
        )
    }
    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::rename(
          "state_code" = "code", "state" = "name"
        )
    }
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  rm_vars <- c(
    "biome_municipality", "color", "category", "biome_state",
    "to_color", "from_color", "from_class", "to_class",
    "class_lulc", "group"
  )

  dat_mod <- dat %>%
    dplyr::select(
      -dplyr::any_of(c(rm_vars))
    )

  if (param$language == "pt") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "municipality" ~ "municipio",
        "city" ~ "municipio",
        "biome" ~ "bioma",
        "geocode" ~ "cod_municipio",
        "state_acronym" ~ "uf",
        "value" ~ "valor",
        "year" ~ "ano",
        "dr_class_name" ~ "classe_desmatamento",
        "class_irrig" ~ "classe_irrigacao",
        "il" ~ "terra_indigena",
        "municipality_code" ~ "cod_municipio",
        "biome_code" ~ "cod_bioma",
        "biome" ~ "bioma",
        "state_code" ~ "cod_uf",
        "state" ~ "uf",
        .default = .
      )) %>%
      dplyr::rename_with(~ stringr::str_replace(., "to_level", "para_level")) %>%
      dplyr::rename_with(~ stringr::str_replace(., "from_level", "de_level"))
  }

  if (param$language == "eng") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "city" ~ "municipality",
        "geocode" ~ "municipality_code",
        "state_acronym" ~ "state",
        "dr_class_name" ~ "deforestation_class",
        "classe_irrig" ~ "irrigation_class",
        "il" ~ "indigenous_land",
        .default = .
      ))
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
