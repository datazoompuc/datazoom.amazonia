#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_mining", "mapbiomas_water" or "mapbiomas_fire")
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data.
#'   * For datasets "mapbiomas_cover", "mapbiomas_transition" can be "municipality" or "state" (faster download).
#'   * For dataset "mapbiomas_deforestation_regeneration" can be only "municipality".
#'   * For dataset "mapbiomas_mining", can be "indigenous_land" or "municipality".
#'   * For dataset "mapbiomas_irrigation", can be "state" or "biome".
#'   * For dataset "mapbiomas_water", can be "municipality", "state" or "biome".
#'   * For dataset "mapbiomas_fire", can be only "state".
#'   * Does not apply to other datasets.
#' @param cover_level A \code{numeric} or \code{string} that indicates the cover aggregation level. Can be "0", "1", "2", "3", "4", ranging from most to least aggregated. Aggregation only supported for "mapbiomas_cover" dataset.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated Mapbiomas Cover data in english at the highest aggregation level
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_cover",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "eng",
#'   cover_level = 0
#' )
#'
#' # download treated Mapbiomas Transition data in portuguese
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_transition", raw_data = FALSE,
#'   geo_level = "state", language = "pt"
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
                           language = "eng", cover_level = 1) {

  ###########################
  ## Bind Global Variables ##
  ###########################

  survey <- link <- x1985 <- x2019 <- NULL
  ano <- bioma <- category <- cidade <- city <- class_id <- country <- estado <- feature_id <- group <- terra_indigena <- NULL
  id <- indigenous_land <- level_2 <- level_3 <- name_pt_br <- pais <- x2020 <- code <- name <- NULL
  territory_id <- municipality <- state <- year <- value <- state_lower <- level_4 <- from_class <- to_class <- NULL
  abbrev_state <- code_muni <- name_state <- geo_code <- municipality_mapbiomas <- index <- NULL
  x1985_to_1986 <- x2018_to_2019 <- x1988 <- x2017 <- x2000 <- x2010 <- x2018 <- biome <- level_1 <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "mapbiomas"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data
  param$cover_level <- cover_level

  # check if dataset and geo_level are supported

  check_params(param)

  sheets <- tibble::tribble(
    ~dataset, ~geo_level, ~sheet,
    "mapbiomas_cover", "any", "COBERTURA_COL8.0",
    "mapbiomas_transition", "state", "TRANSICOES_COL8.0",
    "mapbiomas_transition", "municipality", "TRANSICOES_COL8.0",
    "mapbiomas_deforestation_regeneration", "municipality", "CITY_STATE_BIOME",
    "mapbiomas_irrigation", "state", "UF",
    "mapbiomas_irrigation", "biome", "BIOME",
    "mapbiomas_mining", "municipality", "CITY_STATE_BIOME",
    "mapbiomas_mining", "indigenous_land", "IL",
    "mapbiomas_water", "state", "states_annual",
    "mapbiomas_water", "biome", "biomes_annual",
    "mapbiomas_water", "municipality", "mun_annual",
    "mapbiomas_fire", "state", "ANNUAL",
  )

  sheet <- sheets %>%
    dplyr::filter(
      dataset == param$dataset,
      geo_level %in% c(param$geo_level, "any")
    ) %>%
    dplyr::select(sheet) %>%
    unlist()

  if (length(sheet) != 1) {
    stop("Please check if `dataset` and `geo_level` are supported.")
  }

  if (param$dataset == "mapbiomas_mining" & !param$raw_data){
    message("Treated data currently unavailable for mapbiomas_mining. Returning raw data instead.")
    param$raw_data <- TRUE
  }

  #################
  ##   Message   ##
  #################

  if(dataset %in% c("mapbiomas_cover",
                    "mapbiomas_transition",
                    "mapbiomas_deforestation_regeneration",
                    "mapbiomas_mining")) {
    message("Data from Mapbiomas - Collection 8")
    message("")
  }

  if(dataset %in% c("mapbiomas_irrigation")) {
    message("Data from Mapbiomas - Collection 7")
    message("")
  }

  if(dataset %in% c("mapbiomas_water", "mapbiomas_fire")) {
    message("Data from Mapbiomas - Collection 2")
    message("")
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

  if (param$dataset %in% c("mapbiomas_cover", "mapbiomas_transition")) {

    dat <- dat %>%
      dplyr::select(-dplyr::contains("id"))

    # Aggregate by cover_level

    if (param$cover_level != "4") {
      # dropping higher cover levels
      unwanted_levels <- paste0("level_", (as.numeric(param$cover_level)+1):4)
      dat <- dat %>%
        dplyr::select(-dplyr::contains(unwanted_levels))

    # sum all by-year variables up to the aggregation

    dat <- dat %>%
      dplyr::summarise(
        dplyr::across(dplyr::starts_with("x"), ~ sum(., na.rm = TRUE)),
        .by = c(
          dplyr::any_of(c("municipality", "state", "biome", "geocode", "state_acronym"))
          , dplyr::contains("level"))
      )
    }

    # reshaping

    dat <- dat %>%
      tidyr::pivot_longer(
        dplyr::starts_with("x"),
        names_to = "year",
        values_to = "value",
        names_prefix = "x"
      )

  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "pt") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "municipality" ~ "municipio",
        "biome" ~ "bioma",
        "geocode" ~ "cod_municipio",
        "state_acronym" ~ "uf",
        .default = .
      )
    ) %>%
      dplyr::rename_with(~ stringr::str_replace(., "to_level", "para_level")) %>%
      dplyr::rename_with(~ stringr::str_replace(., "from_level", "de_level"))
  }

  if (param$language == "eng") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "geocode" ~ "municipality_code",
        "state_acronym" ~ "state",
        .default = .
      )
    )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
