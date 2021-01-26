#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Loads and cleans INPE data on areas with deforestation warnings
#'
#' @inheritParams load_deter_raw
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "Municipality", the default alternative, or
#'  also "State".
#' @param time_aggregation A string defining whether data will be aggregated by "month" or by "year".
#' @param language A string that indicates in which language the data will be returned. The default is "eng", so your data will be returned in English.
#'   The other option is "pt" for Portuguese.
#'
#' @return A \code{tibble}.
#'
#' @seealso [load_deter_raw] for loading raw data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_deter()
#' \dontrun{
#' load_deter("path/to/deter.zip", aggregation_level = "municipality", time_aggregation = "year")
#' }
#'
#' load_deter(
#'   source = "cerrado",
#'   aggregation_level = "state",
#'   time_aggregation = "month",
#'   language = "pt"
#' )
load_deter <- function(source = "amazonia", aggregation_level = "municipality", time_aggregation = "year", language = "eng") {
  df <- load_deter_raw(source)

  treat_deter_data(df, aggregation_level, time_aggregation, language)
}




#' Loads INPE data on areas with deforestation warnings
#'
#' @param source A string indicating where the data will be drawn from.
#'
#' It can be "Amazonia" or "Cerrado", in which case the data will be pulled from the INPE website for the corresponding biome. Set to "Amazonia" by default.
#'
#' It can also be a path to a .zip file as is obtained from the INPE website, containing a file named "deter_public.dbf".
#'
#' @return A \code{tibble}
#'
#' @seealso [load_deter], for loading and treating the data.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' load_deter_raw()
#'
#' load_deter_raw("cerrado")
#' \dontrun{
#' load_deter_raw("path/to/deter.zip")
#' }
#'
load_deter_raw <- function(source = "amazonia") {
  if (tolower(source) %in% c("amazonia", "cerrado")) {
    if (tolower(source) == "amazonia") {
      source <- "amz"
    }

    url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", source, "/shape")

    temp <- tempfile(fileext = ".zip")

    utils::download.file(url, temp, mode = "wb")

    data <- utils::unzip(temp, "deter_public.dbf")

    df <- foreign::read.dbf(data, as.is = TRUE) %>%
      tibble::as_tibble()

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


treat_deter_data <- function(df, aggregation_level, time_aggregation, language) {
  stopifnot(tibble::is_tibble(df))

  aggregation_level <- tolower(aggregation_level)

  df <- df %>%
    dplyr::select(-c(.data$QUADRANT, .data$PATH_ROW, .data$SENSOR, .data$SATELLITE)) %>%
    dplyr::mutate(Ano = lubridate::year(.data$VIEW_DATE), Mes = lubridate::month(.data$VIEW_DATE)) %>%
    tidyr::drop_na(.data$MUNICIPALI)

  if (!(aggregation_level %in% c("state", "municipality"))) {
    warning("Aggregation level not supported. Proceeding with municipality.")
  }

  else if (aggregation_level == "state") {
    df <- df %>%
      dplyr::select(-.data$MUNICIPALI, -.data$UC) %>%
      dplyr::group_by(.data$UF, .data$Ano, .data$Mes, .data$CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM))
  }


  else {
    df <- df %>%
      dplyr::select(-.data$UC) %>%
      dplyr::group_by(.data$MUNICIPALI, .data$Ano, .data$Mes, .data$CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM)) %>%
      dplyr::distinct()

    # Adding IBGE municipality codes
    # removing accents and making everything lower-case to match up the names
    IBGE <- CodIBGE %>%
      dplyr::mutate(Municipio = stringi::stri_trans_general(.data$Municipio, "Latin-ASCII") %>% tolower()) %>%
      dplyr::mutate(CodUF = as.numeric(CodIBGE) %/% 100000)

    IBGE <- IBGE %>%
      dplyr::left_join(EstadosIBGE, by = "CodUF") %>% # assigning state abbreviations to match INPE data
      dplyr::select(-c(.data$CodUF, .data$Estado))

    df <- df %>% dplyr::mutate(Municipio = stringi::stri_trans_general(.data$MUNICIPALI, "Latin-ASCII") %>% tolower())

    # cities with names spelt two different ways in the datasets:
    df$Municipio <- df$Municipio %>% dplyr::recode(
      "eldorado dos carajas" = "eldorado do carajas",
      "poxoreo" = "poxoreu",
      "santa isabel do para" = "santa izabel do para"
    )


    df <- df %>%
      dplyr::left_join(IBGE, by = c("UF", "Municipio")) %>%
      dplyr::select(-.data$Municipio)
  }

  time_aggregation <- tolower(time_aggregation)

  if (time_aggregation == "year") {
    df <- df %>%
      dplyr::ungroup(.data$Mes) %>%
      dplyr::summarise(dplyr::across(-c(.data$Mes, .data$AREAUCKM,.data$AREAMUNKM)), AREAUCKM = sum(.data$AREAUCKM), AREAMUNKM = sum(.data$AREAMUNKM)) %>%
      dplyr::distinct()
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
    MINERACAO = "Mineracao"
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
  df$Classe <- df$Classe %>% dplyr::recode(
    "Cicatriz de Queimada" = "Fire Scar",
    "Corte Seletivo Desordenado" = "Unorganized Selection Cutting",
    "Corte Seletivo Geometrico" = "Geometric Selection Cutting",
    "Degradacao" = "Degradation",
    "Desmatamento Corte Raso" = "Clear Cut Deforestation",
    "Desmatamento com Vegetacao" = "Vegetation Remains Deforestation",
    "Mineracao" = "Mining",
    "aviso" = "Warning"
  )


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
