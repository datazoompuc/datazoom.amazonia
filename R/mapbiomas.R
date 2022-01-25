#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_grazing_quality")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality".
#' @param time_period A \code{numeric} indicating what years will the data be loaded. Can be only "all".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#' @param time_id A \code {string} that indicates the time criteria for the data loaded. Can be "year" or "month". Defaults to year.
#' @param cover_level A \code {numeric} that indicates the cover aggregation level. Can be "0", "1", "2", "3" or "4".
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download treated data from mapbiomas_grazing_quality
#' treated_mapbiomas_grazing <- load_mapbiomas(dataset = "mapbiomas_grazing_quality",
#'                             raw_data = FALSE, geo_level = "municipality",
#'                             time_period = "all", language = "eng")
#' }
#'
#' @importFrom magrittr %>%
#' @export



load_mapbiomas = function(dataset = NULL,raw_data=NULL,geo_level = 'municipality',
               time_period='all',language = 'eng',time_id = 'year',cover_level = 1){

  ## To-Do:
    ## Add Support to Transition Info Download at the State and Biome Level
    ## Add Support to Covering Info Download at the State and Biome Level
    ## Include Support for raw raster download


  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- x1985 <- x2019 <- tipo_atividade <- objetivo_atividade <- resultado_final_atividade<- bioma <- x1_floresta <- NULL
  territory_id <- municipality <- state <- year <- value <- activity <- activity_type <- activity_segment <-activity_final_result <- NULL
  x1985_to_1986 <- segmento_atividade<- id_municipio <- municipio <- ano <- estado <- atividade<- x2018_to_2019 <- x1988 <- x2017 <- x2000 <- x2010 <- x2018 <- biome <- level_1 <- NULL
  agropecuaria <- area_nao_vegetada <- floresta <- formacao_nao_natural_floresta <- agua <- nao_observado <- NULL
  x1_forest <- x2_non_forest_natural_formation <- x3_farming <- x4_non_vegetated_area <- x5_water <- non_observed <- NULL
  uf <- farming <- x3_agropecuaria <-non_vegetated_area <-x4_area_nao_vegetada <-forest <- x1_floresta <- NULL
  non_forest_natural_formation <- x2_formacao_nao_natural_florestal <- NULL
  water <- x5_corpo_dagua <- NULL
  ausente <-absent <- baixo <- low <- medio <- medium <-forte <- strong <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language
  param$time_id = time_id
  param$raw_data = raw_data

  param$survey_name = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}

  #################
  ## Downloading ##
  #################

  dat = external_download(dataset = param$dataset,source = 'mapbiomas')

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == 'mapbiomas_cover'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1985:x2019,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    ## Aggregating by Cover Level

    if (cover_level == 0){dat$cover_level = dat$level_0}
    if (cover_level == 1){dat$cover_level = dat$level_1}
    if (cover_level == 2){dat$cover_level = dat$level_2}
    if (cover_level == 3){dat$cover_level = dat$level_3}
    if (cover_level == 4){dat$cover_level = dat$level_4}


if(param$language == "eng") {
    dat = dat %>%
      tidyr::pivot_wider(id_cols = c(territory_id,municipality,state,year),
                         names_from = cover_level,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()}

    if(param$language == "pt") {

      dat = dat %>%
        dplyr::rename(id_municipio = territory_id,
               municipio = municipality,
               estado = state) %>%
        tidyr::pivot_wider(id_cols = c(id_municipio,municipio,estado,year),
                           names_from = cover_level,
                           values_from = value,
                           values_fn = sum,
                           values_fill = NA) %>%
        dplyr::rename(ano = year,
                      agropecuaria = x3_farming,
                     area_nao_vegetada = x4_non_vegetated_area,
                      floresta = x1_forest,
                      formacao_nao_natural_floresta = x2_non_forest_natural_formation,
                      agua = x5_water,
                      nao_observado = non_Observed) %>%
        janitor::clean_names()}

    ## Adjusting Geo Level Names

    ## Translating Names

    ## Add Labels

    ## Return Data

    return(dat)

  }

  if (param$dataset == 'mapbiomas_transition'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1985_to_1986:x2018_to_2019,
        names_to = 'transition_year',
        names_prefix = 'x',
        values_to = 'value'
      )

    # Aggregating by Cover Level

     if (cover_level == 0){dat$cover_level = dat$from_level_0}
     if (cover_level == 1){dat$cover_level = dat$from_level_1}
     if (cover_level == 2){dat$cover_level = dat$from_level_2}
     if (cover_level == 3){dat$cover_level = dat$from_level_3}
     if (cover_level == 4){dat$cover_level = dat$from_level_4}

     if(param$language == "eng") {
    dat = dat %>%
       tidyr::pivot_wider(id_cols = c(territory_id,municipality,state,year),
                          names_from = cover_level,
                          values_from = value,
                          values_fn = sum,
                          values_fill = NA) %>%
       janitor::clean_names()}


    if(param$language == "pt"){

      dat = dat %>%
        dplyr::rename(id_municipio = territory_id,
                      municipio = municipality,
                      uf = state) %>%
        tidyr::pivot_wider(id_cols = c(id_municipio,municipio,uf,year),
                           names_from = cover_level,
                           values_from = value,
                           values_fn = sum,
                           values_fill = NA) %>%
        janitor::clean_names()
    }

    ## Adjusting Geo Level Names

    ## Translating Names

    ## Add Labels

    ## Return Data

    return(dat)




  }

  if (param$dataset == 'mapbiomas_deforestation_regeneration'){

    ## Create Longer Data - Years as a Variable

    if (cover_level == 1){dat$cover_level = dat$cobertura_nivel_1}
    if (cover_level == 2){dat$cover_level = dat$cobertura_nivel_2}
    if (cover_level == 3){dat$cover_level = dat$cobertura_nivel_3}
    if (cover_level == 4){dat$cover_level = dat$cobertura_nivel_4}

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1988:x2017,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    if(param$language == "pt") {

      dat = dat %>%
        tidyr::pivot_wider(id_cols = c(id,bioma,uf,year),
                           names_from = cover_level,
                           values_from = value,
                           values_fn = sum,
                           values_fill = NA) %>%
        janitor::clean_names()}

    if(param$language == "eng"){

      dat = dat %>%
        tidyr::pivot_wider(id_cols = c(id,bioma,uf,year),
                           names_from = cover_level,
                           values_from = value,
                           values_fn = sum,
                           values_fill = NA) %>%
        janitor::clean_names() %>%
        dplyr::rename(biome = bioma, state = uf,
                      farming = x3_agropecuaria,
                      non_vegetated_area = x4_area_nao_vegetada,
                      forest = x1_floresta,
                      non_forest_natural_formation = x2_formacao_nao_natural_florestal,
                      water = x5_corpo_dagua)
    }


    ## Return Data

    return(dat)

  }

  if (param$dataset == 'mapbiomas_irrigation'){

    if (cover_level == 1){dat$cover_level = dat$level_1}

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2019,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    if(param$language == "pt") {

      dat = dat %>%
        dplyr::rename(bioma = biome,
                      uf = state) %>%
        tidyr::pivot_wider(id_cols = c(bioma,uf,year),
                           names_from = cover_level,
                           values_from = value,
                           values_fn = sum,
                           values_fill = NA) %>%
        dplyr::select(-c(4))}

    if(param$language == "eng"){

      dat = dat %>%
        tidyr::pivot_wider(id_cols = c(biome,state,year),
                         names_from = cover_level,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
        dplyr::select(-c(4))
    }

    ## Return Data

    return(dat)



  }

  if (param$dataset == 'mapbiomas_grazing_quality'){

    if (cover_level == 1){dat$cover_level = dat$level_1}


    dat = dat %>%
      tidyr::pivot_longer(
        cols = x2010:x2018,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    if(param$language == "eng") {

    dat = dat %>%
      tidyr::pivot_wider(id_cols = c(biome,state,year),
                         names_from = cover_level,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()}

    if(param$language == "pt") {

      dat = dat %>%
      dplyr::rename(bioma = bioma,
                    uf = state) %>%
      tidyr::pivot_wider(id_cols = c(bioma,uf,year),
                         names_from = cover_level,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
        dplyr::rename(ano = year,
                      ausente = absent,
                      baixo = low,
                      medio = medium,
                      forte = strong)
      janitor::clean_names()}

    ## Return Data

    return(dat)




  }


  return(dat)


}
