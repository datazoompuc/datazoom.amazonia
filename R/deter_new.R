load_deter <- function(source = "amazonia",
                       geo_level = "municipality",
                       time_aggregation = "year", language = "eng") {

  ## Apply the functions below sequentially

  df = load_deter_raw(source)

  df = treat_deter_data(df, geo_level, time_aggregation, language)

  return(df)
}

load_deter_raw <- function(source = "amazonia") {

  ###################
  ## Download Data ##
  ###################

  ## Source can be both amazonia or cerrado

  #if (tolower(source) %in% c("amazonia", "cerrado")) {
    if (tolower(source) == "amazonia") {
      source <- "amz"
    }

  ## Defining URL from TerraBrasilis

  url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", source, "/shape")

  ## Define Empty Directory and Files For Download

  dir <- tempdir()

  temp <- tempfile(fileext = ".zip", tmpdir = dir)

  ## Extraction through Curl Requests
    ## Investigate a bit more on how Curl Resquests work

  f <- RCurl::CFILE(temp, mode = "wb")

  RCurl::curlPerform(url = url, writedata = f@ref, noprogress = FALSE)

  RCurl::close(f)

  ## This Data Opening Part Should Include the .Shp, not DBF

  utils::unzip(temp, exdir = dir)

  df = sf::read_sf(paste(dir, "deter_public.shp", sep = "/"))

  df = df %>%
    #sf::st_simplify() %>%
    janitor::clean_names()

  # df <- foreign::read.dbf(paste(dir, "deter_public.dbf", sep = "/"), as.is = TRUE) %>%
  #     tibble::as_tibble()

  unlink(temp)

  Encoding(df$municipali) <- "UTF-8"

  return(df)

  #}
  # As the data is contained in a .zip file also containing other files, downloading to a tempfile provides a way to extract only the .dbf file we're interested in.


  # else if (file.exists(source)) {
  #   df <- foreign::read.dbf(utils::unzip(source, "deter_public.dbf")) %>% # If source is a valid path, the data is just pulled from there.
  #     tibble::as_tibble()
  #
  #   Encoding(df$MUNICIPALI) <- "UTF-8"
  #
  #   return(df)
  # }
  #
  #
  # else {
  #   warning("Invalid source.")
  # }


}

treat_deter_data <- function(df, geo_level, time_aggregation, language) {

  ######################
  ## Data Engineering ##
  ######################

  geo_level <- tolower(geo_level)

  # Can we extract information from the variables
  # we are dropping below

  # quadrant,path_row,sensor,satellite
  # uc is unidade de conservacao - we potentially want to create a dummy from it

  df = df %>%
    dplyr::select(-c(quadrant,path_row,sensor,satellite)) %>%
    dplyr::mutate(ano = lubridate::year(view_date),
                  mes = lubridate::month(view_date)) %>%
    tidyr::drop_na(municipali)


  # if (!(geo_level %in% c("state", "municipality"))) {
  #   warning("Aggregation level not supported. Proceeding with municipality.")
  # }

  ## State Level

  if (geo_level == "state") {
    df <- df %>%
      dplyr
      dplyr::select(-municipali,-uc) %>%
      dplyr::group_by(uf,ano,mes,classname) %>%
      dplyr::summarise(
        #dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)),
        area_uc_km = sum(areauckm),
        area_geo_km = sum(areamunkm))
  }

  ## Municipality Level

  if (geo_level == "municipality") {
    df <- df %>%
      dplyr::select(-uc) %>%
      dplyr::group_by(uf,municipali,ano,mes,classname) %>%
      dplyr::summarise(
        #dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)),
        area_uc_km = sum(areauckm),
        area_geo_km = sum(areamunkm)) %>%
      #dplyr::distinct() %>%
      dplyr::ungroup()

  }

  ###########################
  ## Add Municipality Code ##
  ###########################

  if (geo_level == 'municipality'){

    munic = geobr::read_municipality() %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(name_muni = stringi::stri_trans_general(name_muni, "Latin-ASCII"),
                    name_muni = tolower(name_muni)) %>%
      dplyr::rename(uf = code_state)

    ## Fuzzy String Matching

    df = df %>%
      dplyr::mutate(name_muni = stringi::stri_trans_general(municipali,'Latin-ASCII'),
                    name_muni = tolower(name_muni),
                    name_muni = dplyr::recode(name_muni,
                      "eldorado dos carajas" = "eldorado do carajas",
                      "poxoreo" = "poxoreu",
                      "santa isabel do para" = "santa izabel do para"
                      )
                    )

    df <- df %>%
      dplyr::left_join(munic, by = c("uf", "name_muni")) #%>%
      # dplyr::select(-.data$Municipio) %>%
      # dplyr::rename(CodIBGE = .data$CD_MUN)






    # Adding IBGE municipality codes
    # removing accents and making everything lower-case to match up the names

    # IBGE <- legal_amazon %>%
    #   dplyr::mutate(Municipio = stringi::stri_trans_general(.data$NM_MUN, "Latin-ASCII") %>% tolower()) %>%
    #   dplyr::select(-c(.data$NM_REGIAO, .data$CD_UF, .data$NM_UF, .data$NM_MUN, .data$AMZ_LEGAL)) %>%
    #   dplyr::rename(UF = .data$SIGLA)

    # df <- df %>% dplyr::mutate(Municipio = stringi::stri_trans_general(.data$MUNICIPALI, "Latin-ASCII") %>% tolower())

    # cities with names spelt two different ways in the datasets:
    # df$Municipio <- df$Municipio %>% dplyr::recode(
    #   "eldorado dos carajas" = "eldorado do carajas",
    #   "poxoreo" = "poxoreu",
    #   "santa isabel do para" = "santa izabel do para"
    # )


    # df <- df %>%
    #   dplyr::left_join(IBGE, by = c("UF", "Municipio")) %>%
    #   dplyr::select(-.data$Municipio) %>%
    #   dplyr::rename(CodIBGE = .data$CD_MUN)
  }

  ######################
  ## Time Aggregation ##
  ######################

  ## Data is already aggregated by month-geo_unit at previous step

  time_aggregation <- tolower(time_aggregation)

  if (time_aggregation == "year") {
    df <- df %>%
      #dplyr::ungroup(.data$Mes) %>%
      #dplyr::group_by(.data$CLASSNAME, .add = TRUE) %>%
      dplyr::group_by(classname) %>%
      dplyr::summarise(
        #dplyr::across(-c(.data$Mes, .data$AREAUCKM, .data$AREAMUNKM)),
        area_uc_km = sum(area_uc_km),
        area_geo_km = sum(area_geo_km)) #%>%
      #dplyr::distinct()
  } else if (time_aggregation != "month") {
    warning("Invalid time aggregation, grouping by month.")
  }


  ################### ### -------------------- Need to Work
  ## Renaming Data ##
  ###################

  df <- df %>%
    dplyr::rename_with(dplyr::recode,
                       CLASSNAME = "Classe",
                       AREAUCKM = "Area_em_UC_km2",
                       AREAMUNKM = "Area_em_Municipio_km2",
                       MUNICIPALI = "Municipio",
    )

  df$Classe <- df$Classe %>% dplyr::recode(
    CICATRIZ_DE_QUEIMADA = "Cicatriz de Queimada",
    CS_DESORDENADO = "Corte Seletivo Desordenado",
    CS_GEOMETRICO = "Corte Seletivo Geometrico",
    DEGRADACAO = "Degradacao",
    DESMATAMENTO_CR = "Desmatamento Corte Raso",
    DESMATAMENTO_VEG = "Desmatamento com Vegetacao",
    MINERACAO = "Mineracao",
    CORTE_SELETIVO = "Corte Seletivo"
  )

  ###########################
  ## Translating Variables ##
  ###########################

  language <- tolower(language)

  if (language == "eng") {

    ## Translate

    df$Classe <- df$Classe %>% dplyr::recode(
      "Cicatriz de Queimada" = "Fire Scar",
      "Corte Seletivo Desordenado" = "Unorganized Selection Cutting",
      "Corte Seletivo Geometrico" = "Geometric Selection Cutting",
      "Degradacao" = "Degradation",
      "Desmatamento Corte Raso" = "Clear Cut Deforestation",
      "Desmatamento com Vegetacao" = "Vegetation Remains Deforestation",
      "Mineracao" = "Mining",
      "aviso" = "Warning",
      "Corte Seletivo" = "Selection Cutting"
    )

    ## Data Engineering

    df <- df %>%
      dplyr::rename_with(dplyr::recode,
                         Classe = "Class",
                         UC = "ConservationUnit",
                         Area_em_UC_km2 = "Area_in_CU_km2",
                         Area_em_Municipio_km2 = "Area_in_Municipality_km2",
                         Municipio = "Municipality",
                         Mes = "Month",
                         Ano = "Year"
      )

  }
  else if (language != "pt") {
    warning("Selected language not supported. Proceeding with Portuguese.")
  }

  ###############
  ## Returning ##
  ###############

  return(df)
}
