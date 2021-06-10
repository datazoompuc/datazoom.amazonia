load_deter <- function(source = "amazonia",
                       space_aggregation = "municipality", time_aggregation = "year", language = "eng") {

  ## Apply the functions below sequentially

  df <- load_deter_raw(source)

  treat_deter_data(df, space_aggregation, time_aggregation, language)
}

load_deter_raw <- function(source = "amazonia") {

  ###################
  ## Download Data ##
  ###################

  if (tolower(source) %in% c("amazonia", "cerrado")) {
    if (tolower(source) == "amazonia") {
      source <- "amz"
    }

    url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", source, "/shape")

    dir <- tempdir()

    temp <- tempfile(fileext = ".zip", tmpdir = dir)

    ## Extraction through Curl Requests

    f <- RCurl::CFILE(temp, mode = "wb")

    RCurl::curlPerform(url = url, writedata = f@ref, noprogress = FALSE)

    RCurl::close(f)

    utils::unzip(temp, "deter_public.dbf", exdir = dir)

    df <- foreign::read.dbf(paste(dir, "deter_public.dbf", sep = "/"), as.is = TRUE) %>%
      tibble::as_tibble()

    unlink(temp)

    Encoding(df$MUNICIPALI) <- "UTF-8"

    return(df)
  }
  # As the data is contained in a .zip file also containing other files, downloading to a tempfile provides a way to extract only the .dbf file we're interested in.


  else if (file.exists(source)) {
    df <- foreign::read.dbf(utils::unzip(source, "deter_public.dbf")) %>% # If source is a valid path, the data is just pulled from there.
      tibble::as_tibble()

    Encoding(df$MUNICIPALI) <- "UTF-8"

    return(df)
  }


  else {
    warning("Invalid source.")
  }
}

treat_deter_data <- function(df, space_aggregation, time_aggregation, language) {

  ######################
  ## Data Engineering ##
  ######################

  space_aggregation <- tolower(space_aggregation)

  df <- df %>%
    dplyr::select(-c(.data$QUADRANT, .data$PATH_ROW, .data$SENSOR, .data$SATELLITE)) %>%
    dplyr::mutate(Ano = lubridate::year(.data$VIEW_DATE), Mes = lubridate::month(.data$VIEW_DATE)) %>%
    tidyr::drop_na(.data$MUNICIPALI)

  if (!(space_aggregation %in% c("state", "municipality"))) {
    warning("Aggregation level not supported. Proceeding with municipality.")
  }

  ## State Level

  else if (space_aggregation == "state") {
    df <- df %>%
      dplyr::select(-.data$MUNICIPALI, -.data$UC) %>%
      dplyr::group_by(.data$UF, .data$Ano, .data$Mes, .data$CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM))
  }

  ## Municipality Level

  else {
    df <- df %>%
      dplyr::select(-.data$UC) %>%
      dplyr::group_by(.data$MUNICIPALI, .data$Ano, .data$Mes, .data$CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM)) %>%
      dplyr::distinct()

    # Adding IBGE municipality codes
    # removing accents and making everything lower-case to match up the names
    IBGE <- legal_amazon %>%
      dplyr::mutate(Municipio = stringi::stri_trans_general(.data$NM_MUN, "Latin-ASCII") %>% tolower()) %>%
      dplyr::select(-c(.data$NM_REGIAO, .data$CD_UF, .data$NM_UF, .data$NM_MUN, .data$AMZ_LEGAL)) %>%
      dplyr::rename(UF = .data$SIGLA)

    df <- df %>% dplyr::mutate(Municipio = stringi::stri_trans_general(.data$MUNICIPALI, "Latin-ASCII") %>% tolower())

    # cities with names spelt two different ways in the datasets:
    df$Municipio <- df$Municipio %>% dplyr::recode(
      "eldorado dos carajas" = "eldorado do carajas",
      "poxoreo" = "poxoreu",
      "santa isabel do para" = "santa izabel do para"
    )


    df <- df %>%
      dplyr::left_join(IBGE, by = c("UF", "Municipio")) %>%
      dplyr::select(-.data$Municipio) %>%
      dplyr::rename(CodIBGE = .data$CD_MUN)
  }

  time_aggregation <- tolower(time_aggregation)

  if (time_aggregation == "year") {
    df <- df %>%
      dplyr::ungroup(.data$Mes) %>%
      dplyr::group_by(.data$CLASSNAME, .add = TRUE) %>%
      dplyr::summarise(dplyr::across(-c(.data$Mes, .data$AREAUCKM, .data$AREAMUNKM)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM)) %>%
      dplyr::distinct()
  }
  else if (time_aggregation != "month") {
    warning("Invalid time aggregation, grouping by month.")
  }


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

  language <- tolower(language)

  if (language == "eng") {
    df <- translate_deter_to_english(df)
  }
  else if (language != "pt") {
    warning("Selected language not supported. Proceeding with Portuguese.")
  }

  return(df)
}

translate_deter_to_english <- function(df) {

  ###############
  ## Translate ##
  ###############

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

  ######################
  ## Data Engineering ##
  ######################

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
