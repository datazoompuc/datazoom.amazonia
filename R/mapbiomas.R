#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_grazing_quality" or "mapbiomas_mining")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality", except "mapbiomas_mining" which accepts more options ("biome", "indigenous_land")
#' @param time_period A \code{numeric} indicating what years will the data be loaded. Can be only "all".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#' @param time_id A \code{string} that indicates the time criteria for the data loaded. Can be "year" or "month". Defaults to year.
#' @param cover_level A \code{numeric} that indicates the cover aggregation level. Can be "0", "1", "2", "3" or "4".
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download treated data from mapbiomas_grazing_quality
#' treated_mapbiomas_grazing <- load_mapbiomas(
#'   dataset = "mapbiomas_grazing_quality",
#'   raw_data = FALSE, geo_level = "municipality",
#'   time_period = "all", language = "pt"
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @export


load_mapbiomas <- function(dataset = NULL, raw_data = NULL, geo_level = "municipality",
                           time_period = "all", language = "eng", time_id = "year", cover_level = 1) {

  ## To-Do:
  ## Add Support to Transition Info Download at the State and Biome Level
  ## Add Support to Covering Info Download at the State and Biome Level
  ## Include Support for raw raster download


  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- x1985 <- x2019 <- NULL
  ano <- bioma <- category <- cidade <- city <- class_id <- country <- estado <- feature_id <- group <- terra_indigena <- NULL
  id <- indigenous_land <- level_2 <- level_3 <- name_pt_br <- pais <- x2020 <- NULL
  territory_id <- municipality <- state <- year <- value <- NULL
  x1985_to_1986 <- x2018_to_2019 <- x1988 <- x2017 <- x2000 <- x2010 <- x2018 <- biome <- level_1 <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$time_id <- time_id
  param$raw_data <- raw_data

  param$survey_name <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)) {
    stop("Missing Dataset!")
  }
  if (is.null(param$raw_data)) {
    stop("Missing TRUE/FALSE for Raw Data")
  }

  sheets <- tibble::tribble(
    ~dataset, ~geo_level, ~sheet,
    "mapbiomas_cover", "any", "LAND COVER - BIOMAS e UF",
    "mapbiomas_transition", "any", "BD_TRANSICAO_BIOMA-UF",
    "mapbiomas_deforestation_regeneration", "any", "BD Colecao 5.0(h) - Hectares",
    "mapbiomas_irrigation", "any", "BD_IRRIGACAO",
    "mapbiomas_grazing_quality", "any", "BD_Qualidade",
    "mapbiomas_mining", "country", "BR",
    "mapbiomas_mining", "state", "UF",
    "mapbiomas_mining", "biome", "BIOME",
    "mapbiomas_mining", "municipality", "MUN",
    "mapbiomas_mining", "indigenous_land", "TI"
  )

  sheet <- sheets %>%
    dplyr::filter(
      dataset == param$dataset,
      geo_level %in% c(param$geo_level, "any")
    ) %>%
    dplyr::select(sheet) %>%
    unlist()

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    dataset = param$dataset,
    source = "mapbiomas",
    geo_level = param$geo_level,
    sheet = sheet
  )

  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  if (raw_data == TRUE) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  ## Create Longer Data - Years as a Variable

  dat <- dat %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("x"),
      names_to = "year",
      names_prefix = "x",
      values_to = "value"
    )

  if (param$dataset %in% c("mapbiomas_cover", "mapbiomas_transition")) {

    ## Aggregating by Cover Level

    if (cover_level == 0) {
      dat$cover_level <- dat$level_0
    }
    if (cover_level == 1) {
      dat$cover_level <- dat$level_1
    }
    if (cover_level == 2) {
      dat$cover_level <- dat$level_2
    }
    if (cover_level == 3) {
      dat$cover_level <- dat$level_3
    }
    if (cover_level == 4) {
      dat$cover_level <- dat$level_4
    }

    dat <- dat %>%
      tidyr::pivot_wider(
        id_cols = dplyr::any_of(c(
          territory_id, municipality, state, year,
          biome, state, year
        )),
        names_from = cover_level,
        values_from = value,
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()

    return(dat)
  }

  dat <- dat %>%
    dplyr::select(-category)

  #################
  ## Translation ##
  #################

  dat_mod <- dat %>%
    dplyr::rename_with(dplyr::recode,
      "name_pt_br" = param$geo_level
    )

  dat_mod <- dat_mod %>%
    dplyr::relocate(dplyr::any_of(c("year", "state", "city", "biome", "indigenous_land")))

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
        "biome" = "bioma",
        "indigenous_land" = "terra_indigena"
      )
  }



  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
