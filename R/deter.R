#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom foreign read.dbf
#' @importFrom tibble as_tibble
#' @importFrom lubridate year
NULL

#' Loads and cleans INPE data on areas with deforestation warnings.
#'
#' @inheritParams load_deter_raw
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by "Municipality", the default alternative, or
#'  also "State".
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
#' load_deter("amazonia")
#'
#' load_deter("C:\\...\\'deter'.zip", aggregation_level = "municipality")
#'
#' load_deter(
#' source = "cerrado",
#' aggregation_level = "state",
#' language = "pt"
#' )
#'
load_deter = function(source, aggregation_level = "municipality",  language = "eng") {

  df <- load_deter_raw(source)

  treat_deter_data(df, aggregation_level, language)

}




#' Loads INPE data on areas with deforestation warnings.
#'
#' @param source A string indicating where the data will be drawn from.
#'
#'It can be "Amazonia" or "Cerrado", in which case the data will be pulled from the INPE website for the corresponding biome.
#'
#'It can also be a path to a .zip file as is obtained from the INPE website, containing a file named "deter_public.dbf".
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
#' load_deter_raw("amazonia")
#'
#' load_deter_raw("C:\\...\\'deter'.zip")
#'
load_deter_raw = function(source) {

  if (tolower(source) == "amazonia") {source <- "amz"}

  if (tolower(source) %in% c("amz", "cerrado")){
    url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", source, "/shape")

    temp <- tempfile(fileext = ".zip")

    download.file(url, temp, mode="wb")

    df <- read.dbf(unzip(temp, "deter_public.dbf"), as.is = TRUE) %>%
      as_tibble()
  }
  #As the data is contained in a .zip file also containing other files, downloading to a tempfile provides a way to extract only the .dbf file we're interested in.

  else if (file.exists(source)) {

    df <- foreign::read.dbf(unzip(source, "deter_public.dbf")) %>% #If source is a valid path, the data is just pulled from there.
      as_tibble()

  }

  else {df = warning("Invalid source.")}

  Encoding(df$MUNICIPALI) <- "UTF-8"

  return(df)
}


treat_deter_data = function(df, aggregation_level, language) {

  aggregation_level <- tolower(aggregation_level)

  df <- df %>%
    dplyr::select(-c(QUADRANT, PATH_ROW, SENSOR, SATELLITE)) %>%
    dplyr::mutate(Ano = lubridate::year(VIEW_DATE), Mês = lubridate::month(VIEW_DATE))

  if (aggregation_level != "state") {

    df <- df %>%
      dplyr::select(-UF, -UC) %>%
      dplyr::group_by(MUNICIPALI, Ano, Mês, CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(AREAUCKM, AREAMUNKM, VIEW_DATE)), AREAUCKM = sum(AREAUCKM), AREAMUNKM = sum(AREAMUNKM))

  }

  else if (aggregation_level == "state") {

    df <- df %>%
      dplyr::select(-MUNICIPALI, -UC) %>%
      dplyr::group_by(UF, Ano, Mês, CLASSNAME) %>%
      dplyr::summarise(dplyr::across(-c(AREAUCKM, AREAMUNKM, VIEW_DATE)), AREAUCKM = sum(AREAUCKM), AREAMUNKM = sum(AREAMUNKM))
  }

  else if (aggregation_level != "municipality") {
    warning("Aggregation level not supported. Proceeding with municipality.")
  }



  df <- df %>%
    dplyr::rename_with(dplyr::recode,

                       CLASSNAME = "Classe",
                       AREAUCKM = "Area_em_UC",
                       AREAMUNKM = "Area_em_Municipio",
                       MUNICIPALI = "Municipio",

    )

  df$Classe <- df$Classe %>% dplyr::recode(CICATRIZ_DE_QUEIMADA = "Cicatriz de Queimada",
                              CS_DESORDENADO = "Corte Seletivo Desordenado",
                              CS_GEOMETRICO = "Corte Seletivo Geométrico",
                              DEGRADACAO = "Degradação",
                              DESMATAMENTO_CR = "Desmatamento Corte Raso",
                              DESMATAMENTO_VEG = "Desmatamento com Vegetação",
                              MINERACAO = "Mineração"
                              )

  language <- tolower(language)

  if(language == "eng") {
    df <- translate_deter_to_english(df)
  }
  else if(language != "pt"){
    warning("Selected language not supported. Proceeding with Portuguese.")
  }

  return(df)
}



translate_deter_to_english <- function(df) {

  df$Classe <- df$Classe %>% dplyr::recode('Cicatriz de Queimada' = "Fire Scar",
                                          'Corte Seletivo Desordenado' = "Unorganized Selection Cutting",
                                          'Corte Seletivo Geométrico' = "Geometric Selection Cutting",
                                          'Degradação' = "Degradation",
                                          'Desmatamento Corte Raso' = "Clear Cut Deforestation",
                                          'Desmatamento com Vegetação' = "Vegetation Remains Deforestation",
                                          'Mineração' = "Mining",
                                          'aviso' = "Warning"
                                          )


  df <- df %>%
    dplyr::rename_with(dplyr::recode,

                       Classe = "Class",
                       UC = "ConservationUnit",
                       Area_em_UC = "Area_in_CU",
                       Area_em_Municipio = "Area_in_Municipality",
                       Municipio = "Municipality",
                       Mês = "Month",
                       Ano = "Year"

    )


}
