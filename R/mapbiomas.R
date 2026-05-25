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
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' @examples
#' # Example 1: Amazon Deforestation Trends
#' \dontrun{
#' library(datazoom.amazonia)
#' deforestation <- load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration",
#'   raw_data = FALSE, geo_level = "municipality", language = "eng")
#' top_deforested <- deforestation %>%
#'   group_by(municipality, state) %>%
#'   summarise(cumulative_deforestation = sum(deforestation_area, na.rm = TRUE),
#'             .groups = 'drop') %>%
#'   arrange(desc(cumulative_deforestation)) %>%
#'   head(10)
#' print(top_deforested)
#' }
#'
#' # Example 2: Land Cover Composition Analysis
#' \dontrun{
#' land_cover <- load_mapbiomas(dataset = "mapbiomas_cover", raw_data = FALSE,
#'   geo_level = "municipality", language = "eng")
#' forest_cover <- land_cover %>%
#'   filter(year == max(year)) %>%
#'   filter(land_cover_type %in% c("Forest", "Forest Formation")) %>%
#'   group_by(municipality, state) %>%
#'   summarise(forest_area_ha = sum(area_hectares, na.rm = TRUE), .groups = 'drop') %>%
#'   arrange(desc(forest_area_ha))
#' head(forest_cover, 15)
#' }
#'
#' # Example 3: Agricultural Expansion Tracking
#' \dontrun{
#' transitions <- load_mapbiomas(dataset = "mapbiomas_transition", raw_data = FALSE,
#'   geo_level = "municipality", language = "eng")
#' forest_to_ag <- transitions %>%
#'   filter(land_use_from %in% c("Forest", "Forest Formation"),
#'          land_use_to %in% c("Temporary Crops", "Sugarcane", "Pasture")) %>%
#'   group_by(year, land_use_to) %>%
#'   summarise(conversion_area = sum(transition_area, na.rm = TRUE), .groups = 'drop')
#' plot(forest_to_ag$year, forest_to_ag$conversion_area,
#'      main = "Conversion from Forest to Agriculture",
#'      xlab = "Year", ylab = "Area (hectares)")
#' }
#'
#' # Example 4: Biome-Level Analysis
#' \dontrun{
#' biome_transitions <- load_mapbiomas(dataset = "mapbiomas_transition", raw_data = FALSE,
#'   geo_level = "biome", language = "pt")
#' deforestation_by_biome <- biome_transitions %>%
#'   filter(land_use_from == "Forest") %>%
#'   group_by(biome, year) %>%
#'   summarise(annual_deforestation = sum(transition_area, na.rm = TRUE), .groups = 'drop') %>%
#'   arrange(biome, year)
#' print(deforestation_by_biome)
#' }
#'
#' # Example 5: Mining Impact Assessment
#' \dontrun{
#' mining_indigenous <- load_mapbiomas(dataset = "mapbiomas_mining", raw_data = FALSE,
#'   geo_level = "indigenous_land", language = "eng")
#' impacted_territories <- mining_indigenous %>%
#'   filter(mining_area > 0) %>%
#'   arrange(desc(mining_area)) %>%
#'   head(15)
#' print(impacted_territories)
#' }
#'
#' # Example 6: Fire Monitoring by State
#' \dontrun{
#' fire_data <- load_mapbiomas(dataset = "mapbiomas_fire", raw_data = FALSE,
#'   geo_level = "state", language = "eng")
#' fire_trends <- fire_data %>%
#'   group_by(state) %>%
#'   summarise(total_burned_area = sum(burned_area, na.rm = TRUE),
#'             recent_fires = sum(burned_area[year >= 2020], na.rm = TRUE),
#'             .groups = 'drop') %>%
#'   arrange(desc(recent_fires))
#' head(fire_trends, 10)
#' }
#'
#' # Example 7: Forest Regeneration Monitoring
#' \dontrun{
#' forest_dynamics <- load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration",
#'   raw_data = FALSE, geo_level = "municipality", language = "eng")
#' forest_summary <- forest_dynamics %>%
#'   group_by(municipality, state) %>%
#'   summarise(total_deforestation = sum(deforestation_area, na.rm = TRUE),
#'             total_regeneration = sum(regeneration_area, na.rm = TRUE),
#'             net_change = total_regeneration - total_deforestation,
#'             .groups = 'drop') %>%
#'   filter(net_change > 0) %>%
#'   arrange(desc(net_change))
#' print(head(forest_summary, 10))
#' }
#'
#' # Example 8: Temporal Trend Analysis
#' \dontrun{
#' library(ggplot2)
#' long_series <- load_mapbiomas(dataset = "mapbiomas_cover", raw_data = FALSE,
#'   geo_level = "municipality", language = "eng")
#' specific_municipality <- long_series %>%
#'   filter(municipality == "Altamira" & state == "PA") %>%
#'   group_by(year, land_cover_type) %>%
#'   summarise(area = sum(area_hectares, na.rm = TRUE), .groups = 'drop')
#' ggplot(specific_municipality, aes(x = year, y = area, fill = land_cover_type)) +
#'   geom_area() +
#'   theme_minimal() +
#'   labs(title = "Land Cover Changes in Altamira (PA)",
#'        x = "Year", y = "Area (hectares)")
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
        dplyr::rename("biome_code" = "code", "biome" = "name")
    }
    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::rename("state_code" = "code", "state" = "name")
    }
  }


  if (param$dataset == "mapbiomas_cover" & param$geo_level == "indigenous_land") {
    dat <- dat %>%
      tidyr::extract(
        col = 2,
        into = c("territory_name", "territory_sub_name", "territory_code"),
        regex = "^(.*?)\\s*(?:\\(([^()]+)\\))?\\s*\\((\\d+)\\)$"
      )
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
        "state_code" ~ "cod_uf",
        "state" ~ "uf",
        "territory_name" ~ "nome_territorio",
        "territory_subname" ~ "sub_nome_territorio",
        "territory_code" ~ "cod_territorio",
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

  dat_mod <- dat_mod %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("^class_level_\\d+$"),
        ~ sub("^\\s*\\d+(?:\\.\\d+)*\\.?\\s*", "", as.character(.))
      )
    )

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
