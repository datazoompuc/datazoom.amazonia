#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_mining", "mapbiomas_grazing_quality", "mapbiomas_water" or "mapbiomas_fire")
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data.
#'   * For datasets "mapbiomas_cover", "mapbiomas_transition", "mapbiomas_deforestation_regeneration" and "mapbiomas_fire", can be "municipality" or "state" (faster download).
#'   * For dataset "mapbiomas_mining", can be "indigenous_land", "municipality", "state", "biome" or "country".
#'   * For dataset "mapbiomas_irrigation", can be "state" or "biome".
#'   * For dataset "mapbiomas_water", can be "municipality", "state" or "biome".
#'   * Does not apply to other datasets.
#' @param cover_level A \code{numeric} or \code{string} that indicates the cover aggregation level. Can be "0", "1", "2", "3", "4", or "none", which means no aggregation. Aggregation only supported for "mapbiomas_cover" and "mapbiomas_grazing_quality" datasets.
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
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data
  param$cover_level <- cover_level

  sheets <- tibble::tribble(
    ~dataset, ~geo_level, ~sheet,
    "mapbiomas_cover", "any", "COBERTURA_COL7",
    "mapbiomas_transition", "state", "TRANSICOES_COL7",
    "mapbiomas_transition", "municipality", "TRANSICAO_COL7",
    "mapbiomas_deforestation_regeneration", "state", "DESMAT_VEGSEC_UF_COL7",
    "mapbiomas_deforestation_regeneration", "municipality", "DESMAT_VEGSEC_CITY_COL7",
    "mapbiomas_irrigation", "state", "UF",
    "mapbiomas_irrigation", "biome", "BIOME",
    "mapbiomas_grazing_quality", "any", "BD_Qualidade",
    "mapbiomas_mining", "country", "CITY_STATE_BIOME",
    "mapbiomas_mining", "state", "CITY_STATE_BIOME",
    "mapbiomas_mining", "biome", "CITY_STATE_BIOME",
    "mapbiomas_mining", "municipality", "CITY_STATE_BIOME",
    "mapbiomas_mining", "indigenous_land", "IL",
    "mapbiomas_water", "state", "states_annual",
    "mapbiomas_water", "biome", "biomes_annual",
    "mapbiomas_water", "municipality", "mun_annual",
    "mapbiomas_fire", "state", "MUNICIPIOS-UF",
    "mapbiomas_fire", "biome", "BIOMAS",
    "mapbiomas_fire", "municipality", "MUNICIPIOS-UF"
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
  if(dataset %in% c("mapbiomas_mining")) {
    message("Data from Mapbiomas - Collection 8")
    message("")
  }

  if(dataset %in% c("mapbiomas_cover",
                    "mapbiomas_transition",
                    "mapbiomas_deforestation_regeneration",
                    "mapbiomas_irrigation")) {
    message("Data from Mapbiomas - Collection 7")
    message("")
  }

  if(dataset %in% c("mapbiomas_grazing_quality")) {
    message("Data from Mapbiomas - Collection 5")
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
    source = "mapbiomas",
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

  ## Treat mapbiomas_water
  if (param$dataset == "mapbiomas_water") {

    dat_mod <- dat %>%
      dplyr::relocate(dplyr::any_of(c("year", "name")))

    if (param$geo_level != "municipality") dat_mod <- dat_mod %>% dplyr::select(-code)
    if (param$geo_level == "biome") dat_mod <- dat_mod %>% dplyr::rename(biome = name)

    if (param$language == "pt") {
      dat_mod <- dat_mod %>%
        dplyr::rename_with(dplyr::recode,
                           "code" = "cod_municipio",
                           "state" = "estado",
                           "name" = "estado",
                           "year" = "ano",
                           "area_ha" = "valor",
                           "city" = "municipio",
                           "biome" = "bioma"
        )
    }

    if (param$language == "eng") {
      dat_mod <- dat_mod %>%
        dplyr::rename_with(dplyr::recode,
                           "code" = "municipality_code",
                           "name" = "state",
                           "area_ha" = "value",
                           "city" = "municipality"
        )
    }

    return(dat_mod)

  }


  ## Else
  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  dat <- dat %>%
    dplyr::rename_with(dplyr::recode,
        "uf" = "state",
        "municipality" = "city",
    )

  if (param$geo_level == "municipality" &
      !(param$dataset %in% c("mapbiomas_transition",
                             "mapbiomas_deforestation_regeneration",
                             "mapbiomas_fire",
                             "mapbiomas_grazing_quality"))) {

    munic_codes <- datazoom.amazonia::municipalities %>%
      dplyr::select(state = abbrev_state, city = municipality_mapbiomas, geo_code = code_muni)

    dat <- dat %>%
      dplyr::left_join(munic_codes, by = dplyr::join_by(city, state))
  }

  if (param$geo_level == "municipality" & param$dataset == "mapbiomas_fire") {

    munic_codes <- datazoom.amazonia::municipalities %>%
      dplyr::select(state = name_state, city = municipality_mapbiomas, geo_code = code_muni) %>%
      dplyr::mutate(state = toupper(state),
                    city = toupper(city))


    dat <- dat %>%
      dplyr::left_join(munic_codes, by = dplyr::join_by(city, state))
  }

  if (param$geo_level == "municipality" &
      (param$dataset == "mapbiomas_transition" | param$dataset == "mapbiomas_deforestation_regeneration") ) {
    munic_biomes <- datazoom.amazonia::municipalities_biomes %>%
      dplyr::select(feature_id, city = municipality_mapbiomas, geo_code = code_muni)

    dat <- dat %>%
      dplyr::select(-city) %>%
      dplyr::left_join(munic_biomes, by = dplyr::join_by(feature_id))
  }


  ## Add transition columns
  if (param$dataset == "mapbiomas_transition" & param$geo_level == "municipality") {
    classes_mapbiomas <- dat %>%
      dplyr::select(class_id:level_4) %>%
      unique()

    from_classes <- classes_mapbiomas %>%
      dplyr::rename(from_class = class_id) %>%
      dplyr::rename_with(~paste0("from_", .x), dplyr::starts_with("level_"))

    to_classes <- classes_mapbiomas %>%
      dplyr::rename(to_class = class_id) %>%
      dplyr::rename_with(~paste0("to_", .x), dplyr::starts_with("level_"))

    dat <- dat %>%
      dplyr::left_join(from_classes, by = dplyr::join_by(from_class)) %>%
      dplyr::left_join(to_classes, by = dplyr::join_by(to_class)) %>%
      dplyr::select(-dplyr::starts_with("level_"))

  }

  ## Create Longer Data - Years as a Variable

  dat <- dat %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("x"),
      names_to = "year",
      names_prefix = "x",
      values_to = "value"
    )

  # Testing cover_level support
  if (param$dataset == "mapbiomas_grazing_quality" & !(param$cover_level %in% c(1, "1", "none"))) {
    base::message("The \"mapbiomas_grazing_quality\" dataset only supports cover_level 1 or \"none\"")
    param$cover_level <- 1
  }

  if (param$dataset %in% c("mapbiomas_cover", "mapbiomas_grazing_quality") & param$cover_level != "none") {

    ## Aggregating by Cover Level

    dat <- dat %>%
      tidyr::pivot_wider(
        id_cols = dplyr::any_of(c(
          "geo_code", "city",
          "state", "year",
          "biome", "feature_id"
        )),
        names_from = paste0("level_", param$cover_level),
        values_from = value,
        values_fn = ~sum(.x, na.rm = TRUE),
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }

  dat <- dat %>%
    dplyr::select(-dplyr::any_of("category"))

  ## Aggregate by geo_level
  if (param$dataset == "mapbiomas_transition" | param$dataset == "mapbiomas_deforestation_regeneration") {
    dat <- dat %>%
      dplyr::group_by(dplyr::across(-c(feature_id, biome, value))) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       state = unique(state))
  }

  if (param$dataset == "mapbiomas_cover") {
    dat <- dat %>%
      dplyr::group_by(dplyr::across(-c(feature_id, biome, dplyr::starts_with("x")))) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("x"), ~sum(.x, na.rm = TRUE)),
                       state = unique(state))
  }

  if (param$dataset == "mapbiomas_fire" & param$geo_level == "state") {
    dat <- dat %>%
      dplyr::group_by(dplyr::across(-c(feature_id, city, geo_code, index, value))) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       state = unique(state))
  }



  #################
  ## Translation ##
  #################

  dat_mod <- dat %>%
    dplyr::rename_with(dplyr::recode,
      "name_pt_br" = param$geo_level
    )

  dat_mod <- dat_mod %>%
    dplyr::relocate(dplyr::any_of(c("year", "state", "city", "geo_code", "biome", "indigenous_land")))

  if (param$language == "pt") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(dplyr::recode,
        "feature_id" = "id",
        "class_id" = "id_classe",
        "group" = "grupo",
        "year" = "ano",
        "value" = "valor",
        "state" = "estado",
        "municipality" = "municipio",
        "city" = "municipio",
        "geo_code" = "cod_municipio",
        "biome" = "bioma",
        "indigenous_land" = "terra_indigena",
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(dplyr::recode,
        "city" = "municipality",
        "geo_code" = "municipality_code",
        "codigobioma" = "biome_code",
        "bioma" = "biome",
        "uf" = "state",
        "cod_tipo_mudanca_n1" = "code_type_change_l1",
        "cod_tipo_mudanca_n2" = "code_type_change_l2",
        "cod_classe_des_ou_reg" = "code_class_def_or_reg",
        "tipo_mudanca_nivel1" = "type_change_level_1",
        "tipo_mudanca_nivel2" = "type_change_level_2",
        "cobertura_nivel_1" = "cover_level_1",
        "cobertura_nivel_2" = "cover_level_2",
        "cobertura_nivel_3" = "cover_level_3",
        "cobertura_nivel_4" = "cover_level_4"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
