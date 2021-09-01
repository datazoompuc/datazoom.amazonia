#' @title DETER - Forest Degradation in the Brazilian Amazon
#'
#' @description Loads information on change in forest cover in the Amazon. Survey is done at state or municipal level. See \url{http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter}
#'
#' @param dataset A dataset name ("deter_amz", "deter_cerrado") with information about both Amazon and Cerrado
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # download raw data from all years from Amazonia
#' deter_amz <- load_deter(dataset = 'deter_amz',
#'                         raw_data = TRUE,
#'                         time_period = "all")
#' }


load_deter <- function(dataset = NULL, raw_data,
                       time_period, language = 'eng') {

  ## Dataset can be either Amazonia or Cerrado
  # Default is all time-periods

  ###########################
  ## Bind Global Variables ##
  ###########################

  ## Bind Global Variables

  quadrant <- NULL
  path_row <- NULL
  sensor <- NULL
  satellite <- NULL
  view_date <- NULL
  municipali <- NULL
  uc <- NULL
  uf <- NULL
  ano <- NULL
  mes <- NULL
  classname <- NULL
  areauckm <- NULL
  areamunkm <- NULL
  name_muni <- NULL
  code_state <- NULL
  area_uc_km <- NULL
  area_geo_km <- NULL
  survey <- link <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$time_period = time_period
  param$language = language
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

  # dat = as.list(param$time_period) %>%
  #   purrr::map(
  #     function(t){external_download(dataset = param$dataset,
  #                                   source='degrad',year = t,
  #                                   geo_level = param$geo_level) %>%
  #         janitor::clean_names()
  #     }
  #   )

  dat = external_download(dataset = param$dataset,source = 'deter')

  dat = dat %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Engineering ##
  ######################

  # geo_level <- tolower(geo_level)
  #
  # # Can we extract information from the variables
  # # we are dropping below
  #
  # # quadrant,path_row,sensor,satellite
  # # uc is unidade de conservacao - we potentially want to create a dummy from it
  #
  # df = df %>%
  #   dplyr::select(-c(quadrant,path_row,sensor,satellite)) %>%
  #   dplyr::mutate(ano = lubridate::year(view_date),
  #                 mes = lubridate::month(view_date)) %>%
  #   tidyr::drop_na(municipali)

  #######################################
  ## Aggregate at the Geographic Level ##
  #######################################

  # if (!(geo_level %in% c("state", "municipality"))) {
  #   warning("Aggregation level not supported. Proceeding with municipality.")
  # }

  ## State Level

  # if (geo_level == "state") {
  #   df <- df %>%
  #     dplyr::select(-municipali,-uc) %>%
  #     dplyr::group_by(uf,ano,mes,classname) %>%
  #     dplyr::summarise(
  #       #dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)),
  #       area_uc_km = sum(areauckm),
  #       area_geo_km = sum(areamunkm))
  # }

  ## Municipality Level

  # if (geo_level == "municipality") {
  #   df <- df %>%
  #     dplyr::select(-uc) %>%
  #     dplyr::group_by(uf,municipali,ano,mes,classname) %>%
  #     dplyr::summarise(
  #       #dplyr::across(-c(.data$AREAUCKM, .data$AREAMUNKM, .data$VIEW_DATE)),
  #       area_uc_km = sum(areauckm),
  #       area_geo_km = sum(areamunkm)) %>%
  #     #dplyr::distinct() %>%
  #     dplyr::ungroup()
  #
  # }

  ###########################
  ## Add Municipality Code ##
  ###########################

  # if (geo_level == 'municipality'){
  #
  #   munic = geobr::read_municipality() %>%
  #     sf::st_drop_geometry() %>%
  #     dplyr::mutate(name_muni = stringi::stri_trans_general(name_muni, "Latin-ASCII"),
  #                   name_muni = tolower(name_muni)) %>%
  #     dplyr::rename(uf = code_state)
  #
  #   ## Fuzzy String Matching
  #
  #   df = df %>%
  #     dplyr::mutate(name_muni = stringi::stri_trans_general(municipali,'Latin-ASCII'),
  #                   name_muni = tolower(name_muni),
  #                   name_muni = dplyr::recode(name_muni,
  #                                             "eldorado dos carajas" = "eldorado do carajas",
  #                                             "poxoreo" = "poxoreu",
  #                                             "santa isabel do para" = "santa izabel do para"
  #                   )
  #     )
  #
  #   df <- df %>%
  #     dplyr::left_join(munic, by = c("uf", "name_muni")) #%>%
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
  #}

  ######################
  ## Time Aggregation ##
  ######################

  ## Data is already aggregated by month-geo_unit at previous step

  # time_aggregation <- tolower(time_aggregation)
  #
  # if (time_aggregation == "year") {
  #   df <- df %>%
  #     #dplyr::ungroup(.data$Mes) %>%
  #     #dplyr::group_by(.data$CLASSNAME, .add = TRUE) %>%
  #     dplyr::group_by(classname) %>%
  #     dplyr::summarise(
  #       #dplyr::across(-c(.data$Mes, .data$AREAUCKM, .data$AREAMUNKM)),
  #       area_uc_km = sum(area_uc_km),
  #       area_geo_km = sum(area_geo_km)) #%>%
  #   #dplyr::distinct()
  # } else if (time_aggregation != "month") {
  #   warning("Invalid time aggregation, grouping by month.")
  # }


  ################### ### -------------------- Need to Work
  ## Renaming Data ##
  ###################

  # df <- df %>%
  #   dplyr::rename_with(dplyr::recode,
  #                      CLASSNAME = "Classe",
  #                      AREAUCKM = "Area_em_UC_km2",
  #                      AREAMUNKM = "Area_em_Municipio_km2",
  #                      MUNICIPALI = "Municipio",
  #   )
  #
  # df$Classe <- df$Classe %>% dplyr::recode(
  #   CICATRIZ_DE_QUEIMADA = "Cicatriz de Queimada",
  #   CS_DESORDENADO = "Corte Seletivo Desordenado",
  #   CS_GEOMETRICO = "Corte Seletivo Geometrico",
  #   DEGRADACAO = "Degradacao",
  #   DESMATAMENTO_CR = "Desmatamento Corte Raso",
  #   DESMATAMENTO_VEG = "Desmatamento com Vegetacao",
  #   MINERACAO = "Mineracao",
  #   CORTE_SELETIVO = "Corte Seletivo"
  # )

  ###########################
  ## Translating Variables ##
  ###########################

  # language <- tolower(language)
  #
  # if (language == "eng") {
  #
  #   ## Translate
  #
  #   df$Classe <- df$Classe %>% dplyr::recode(
  #     "Cicatriz de Queimada" = "Fire Scar",
  #     "Corte Seletivo Desordenado" = "Unorganized Selection Cutting",
  #     "Corte Seletivo Geometrico" = "Geometric Selection Cutting",
  #     "Degradacao" = "Degradation",
  #     "Desmatamento Corte Raso" = "Clear Cut Deforestation",
  #     "Desmatamento com Vegetacao" = "Vegetation Remains Deforestation",
  #     "Mineracao" = "Mining",
  #     "aviso" = "Warning",
  #     "Corte Seletivo" = "Selection Cutting"
  #   )
  #
  #   ## Data Engineering
  #
  #   df <- df %>%
  #     dplyr::rename_with(dplyr::recode,
  #                        Classe = "Class",
  #                        UC = "ConservationUnit",
  #                        Area_em_UC_km2 = "Area_in_CU_km2",
  #                        Area_em_Municipio_km2 = "Area_in_Municipality_km2",
  #                        Municipio = "Municipality",
  #                        Mes = "Month",
  #                        Ano = "Year"
  #     )
  #
  # }
  # else if (language != "pt") {
  #   warning("Selected language not supported. Proceeding with Portuguese.")
  # }

  #################
  ## Return Data ##
  #################

  ## Apply the functions below sequentially

  # df = load_deter_raw(source)
  #
  # df = treat_deter_data(df, geo_level, time_aggregation, language)
  #
  # return(df)

}

