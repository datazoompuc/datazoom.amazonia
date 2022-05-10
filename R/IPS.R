#' @title IPS - Amazon Social Progress Index
#'
#' @description Loads information on the social and environmental performance of the Legal Amazon. Survey is done at the municipal level and data is available in 2014 and 2018. See \url{http://www.ipsamazonia.org.br/}
#'
#' @param dataset A dataset name ("all")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese and English are supported.
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples \dontrun{
#' # download raw data from 2014
#' ips <- load_ips(dataset = "ips", raw_data = TRUE, time_period = 2014)
#' }
load_ips <- function(dataset = "ips", raw_data,
                     time_period, language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- .data <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$time_period <- time_period
  param$language <- language
  # param$time_id = time_id
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

  ## TEMPORARY!! WHILE RAW_DATA= FALSE & language == eng IS BEING FIXED
  if (raw_data == FALSE & language == "eng") {
    stop("the combination raw_data = FALSE & language == eng argument for IPS is being improved at the moment!")
  }


  ##############
  ## Download ##
  ##############

  dat <- as.list(param$time_period) %>%
    purrr::map(
      function(t) {
        external_download(dataset = param$dataset, source = "ips", year = t)
      }
    )

  ##########
  # labels #
  ##########

  labels <- tibble::tribble(
    ~original_name, ~new_pt, ~original_eng,
    "Codigo IBGE do municipio", "Codigo IBGE do municipio", "IBGE city code",
    "Municipio", "Municipio", "Municipality",
    "Estado", "UF", "State",
    "IPS", "Indice de Progresso Social", "Social Progress Index",
    "Ranking IPS", "Ranking IPS","Ranking IPS",
    "Necessidades Humanas Basicas", "Necessidades Humanas Basicas", "Basic human needs",
    "Fundamentos para o Bem-Estar", "Fundamentos para o Bem-Estar", "Well-being fundamentals",
    "Oportunidades", "Oportunidades", "Opportunities",
    "Nutricao e cuidados medicos basicos", "Nutricao e cuidados medicos basicos", "Nutrition and basic medical care",
    "Agua e saneamento", "Agua e saneamento", "Water and sanitation",
    "Moradia", "Moradia", "Habitation",
    "Seguranca pessoal", "Seguranca pessoal", "Personal safety",
    "Acesso ao conhecimento basico", "Acesso ao conhecimento basico", "Access to basic knowledge",
    "Acesso a informacao e comunicacao", "Acesso a informacao e comunicacao", "Access to information and communication",
    "Saude e bem-estar", "Saude e bem-estar", "Health and well-being",
    "Qualidade do meio ambiente", "Qualidade do meio ambiente", "Environment quality",
    "Direitos individuais", "Direitos individuais", "Individual rights",
    "Liberdade individual e de escolha", "Liberdade individual e de escolha", "Individual freedom of choice",
    "Tolerancia e inclusao", "Tolerancia e inclusao", "Tolerance and inclusion",
    "Acesso a educacao superior", "Acesso a educacao superior", "Access to higher education",
    "Mortalidade infantil ate 5 anos (Obitos por mil nascidos vivos)", "Mortalidade infantil ate 5 anos (Obitos por mil nascidos vivos)", "Infant mortality - until 5 years (Deaths per thousand live births)",
    "Mortalidade materna (Obitos maternos por 100 mil nascidos vivos)", "Mortalidade materna (Obitos maternos por 100 mil nascidos vivos)", "Maternal mortality (Maternal deaths per 100.000 live births)",
    "Mortalidade por desnutricao (Obitos por 100 mil habitantes)", "Mortalidade por desnutricao (Obitos por 100 mil habitantes)", "Malnutrition mortality (Deaths per 100.000 people)",
    "Mortalidade por doencas infecciosas (Obitos por 100 mil habitantes)", "Mortalidade por doencas infecciosas (Obitos por 100 mil habitantes)", "Infeccious diseases mortality (Deaths per 100.000 people)",
    "Subnutricao (% da populacao)", "Subnutricao (% da populacao)", "Malnutrition (% of population)",
    "Abastecimento de agua (% da populacao)", "Abastecimento de agua (% da populacao)", "Water supply (% of population)",
    "Esgoto adequado  (% da populacao)", "Esgoto adequado  (% da populacao)", "Sewage (% of population)",
    "Índice atendimento de água (% da população)","Índice atendimento de água (% da população)", "Index of water supply (% of population)",
    "Coleta de lixo (% da populacao)", "Coleta de lixo (% da populacao)", "Garbage collection (% of population)",
    "Moradia com luminacao adequada (% de domicilios)", "Moradia com luminacao adequada (% de domicilios)", "habitation with adequate illumination(% of habitations)",
    "Moradia com parede adequada (% de domicilios)", "Moradia com parede adequada (% de domicilios)", "habitation with adequate walls (% of habitations)",
    "Moradia com piso adequado (% da populacao)", "Moradia com piso adequado (% da populacao)", "habitation with adequate ground (% of habitations)",
    "Assassinatos de jovens (Obitos por 100 mil habitantes de 15 a 29 anos)","Assassinatos de jovens (Obitos por 100 mil habitantes de 15 a 29 anos)","Youth murders (Deaths per 100.000 people aged 15 to 29)",
    "Assassinatos de jovens Pontuados em uma escala de 1-6: 1 = 0 2 = 1 - 6 3 = 6 - 10 4 = 10 - 20 5 = 20 - 40 6 > 40)","Assassinatos de jovens. Escala 1-6", "Youth murders Scored on a 1-6 scale: 1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40",
    "Homicidios (Óbitos/100.000 habitantes)", "Homicidios (Óbitos/100.000 habitantes)", "Homicides (Deaths per 100.000 people)",
    "Homicidios Ponuados numa escala 1-6:1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40", "Homicidios. Escala 1-6", "Homicides Scored from 1-6::1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40 ",
    "Mortes por acidente no transito (Obitos por 100 mil habitantes)", "Mortes por acidente no transito (Obitos por 100 mil habitantes)", "Traffic accident deaths (Deaths per 100.000 people)",
    "Abandono escolar ensino fundamental (% de alunos)","Abandono escolar ensino fundamental (% de alunos)", "Elementary School Dropout (% students)",
    "Distorção idade-série ensino fundamental (% de alunos)", "Distorção idade-série ensino fundamental (% de alunos)"," Elementary School age-Grade distortion (% students)",
    "Distorção idade-série ensino médio (% de alunos)", "Distorção idade-série ensino médio (% de alunos)", "High-School age-Grade distortion (% students)",
    "Qualidade da educação Ideb ensino fundamental (escala de 0-10)", "Qualidade da educação Ideb ensino fundamental (escala de 0-10)", "Elementary education quality Ideb (0-10 scale)",
    "Reprovação escolar ensino fundamental (% de alunos)", "Reprovação escolar ensino fundamental (% de alunos)", "Students failing in elementary school (% students)",
    "Densidade internet banda-larga (nº de acessos/100 domicílios)", "Densidade internet banda-larga (nº de acessos/100 domicílios)", "Broadband internet (acesses per 100 houses)",
    "Densidade telefonia fixa (nº de acessos/100 domicílios)","Densidade telefonia fixa (nº de acessos/100 domicílios)"," Landline telephone (accesses per 100 houses)",
    "Densidade telefonia movel (nº de acessos/100 habitantes)", "Densidade telefonia movel (nº de acessos/100 habitantes)", "Mobile phone (access per 100 people)",
    "Densidade TV por assinatura (nº de acessos/100 domicílios)", "Densidade TV por assinatura (nº de acessos/100 domicílios)", "Number of houses with cable TV (for each 100 houses)",
    "Mortalidade por diabetes mellitus (Óbitos/100.000 habitantes)", "Mortalidade por diabetes mellitus (Óbitos/100.000 habitantes)", "Deaths by diabetes mellitus (deaths per 100.000 people)",
    "Mortalidade por câncer (Óbitos/100.000 habitantes)", "Mortalidade por câncer (Óbitos/100.000 habitantes)", "Deaths by cancer (deaths per 100.000 people)",
    "Mortalidade por doenças circulatórias (Óbitos/100.000 habitantes)", "Mortalidade por doenças circulatórias (Óbitos/100.000 habitantes)", "Deaths by circulatory diseases (deaths per 100.000 people)",
    "Mortalidade por doenças respiratórias (Óbitos/100.000 habitantes)", "Mortalidade por doenças respiratórias (Óbitos/100.000 habitantes)", "Deaths by respiratory desiases (deaths per 100.000 people)",
    "Mortalidade por suicídios (Óbitos/100.000 habitantes)", "Mortalidade por suicídios (Óbitos/100.000 habitantes)", "Suicides (Deaths per 100.000 people)",
    "area degradada (%)", "area degradada (%)", "Degraded area (%)",
    "areas protegidas (%)", "areas protegidas (%)", "Protected area (%)",
    "Desmatamento acumulado (%)", "Desmatamento acumulado (%)", "Accumulated deforestation (%)",
    "Desmatamento recente (%)", "Desmatamento recente (%)", "Recent deforestation(%)",
    "Emissoes CO2 (ton. CO2/habitante)", "Emissoes CO2 (ton. CO2/habitante)", "CO2 emission (ton. CO2/habitant)",
    "Focos de calor (nª de focos/1.000 habitantes)", "Focos de calor (nª de focos/1.000 habitantes)", "Hotspots (number of hotspots per 1.000 habitants)",
    "Diversidade Partidaria (% vereadores eleitos partidos diferentes)","Diversidade Partidaria (% vereadores eleitos partidos diferentes)", "Partisan diversity (%)",
    "Acesso a cultura, lazer e esporte (Categorica. Pontuado em: 0 = nenhuma estrutura; 1 = uma; 2 = duas; 3 = tres; 4 = todas as estruturas)", "Acesso a cultura, lazer e esporte: 0-4", "Access to culture, leisure, and sports (Cathegorical. Scored by: 0 = no structure; 1 = one; 2 = two; 3 = three; 4 = all structures",
    "Gravidez na infancia e adolescencia (% de mulheres de 15 a 17 anos que tiveram filhos)", "Gravidez na infancia e adolescencia (% de mulheres de 15 a 17 anos que tiveram filhos)", "Child of adolescence pregnancy (% of women aged 15-17 with children)",
    "Trabalho infantil (% da populacao entre 10 e 14 anos de idade)", "Trabalho infantil (% da populacao entre 10 e 14 anos de idade)", "Child labor (% of population aged 10-14)",
    "Vulnerabilidade familiar (% de filhos de mães solteiras)", "Vulnerabilidade familiar (% de filhos de mães solteiras)", "Family vulnerability (% of kids raised by single mothers)",
    "Violencia contra indigena (casos por mil indigenas", "Violencia contra indigena (casos por mil indigenas","Violence against indigenous people (cases by thousand indigenous people)",
    "Violência contra indígenas (Taxa pontuada em uma escala de 1-5: 1 = 0 / 2 = 0,1 - 2,7 / 3 = 2,7 - 8,8 / 4 = 8,8 - 20,8 / 5 = > 20,8)", "Grau de violência contra indígenas: 1-5", "Violence against indigenous people (Scored Scale 1-5: 1 = 0 / 2 = 0,1 - 2,7 / 3 = 2,7 - 8,8 / 4 = 8,8 - 20,8 / 5 = > 20,8)",
    "Violencia contra a mulher (casos por 100 mil mulheres)", "Violencia contra a mulher (casos por 100 mil mulheres)", "Violence against women (cases per 100.000 women)",
    "Violência infantil (nº de casos/100.000 pessoas de 0-14 anos)", "Violência infantil (nº de casos/100.000 pessoas de 0-14 anos)", "Violence against child (number of cases per 100.000 0-14 years old kids)",
    "Violência infantil (Escala de 1-5: 1 = 0 / 2 = 1,1 - 40,1 / 3 = 40,1 - 133,1 / 4 = 133,1 - 496,0 / 5 = > 496,0)", "Violência infantil (Escala de 1-5)", "Violence against child ( 1-5 Scale: 1 = 0 / 2 = 1,1 - 40,1 / 3 = 40,1 - 133,1 / 4 = 133,1 - 496,0 / 5 = > 496,0)",
    "Empregos ensino superior (% de empregos em relação ao total)", "Empregos ensino superior (% de empregos em relação ao total)", "Higher education Jobs(% compared to total jobs)",
    "Mulheres com empregos ensino superior (% de empregos em relação ao total)", "Mulheres com empregos ensino superior (% de empregos em relação ao total)", "Women with higher education jobs (% in relation to total jobs)"
  )

  #############
  # language ##
  #############

  if(raw_data == TRUE & language == "pt"){name <- labels$original_name}
  if(raw_data == TRUE & language == "eng"){name <- labels$original_eng}
  if(raw_data == FALSE & language == "pt"){name <- labels$new_pt}
  #if(raw_data == FALSE & language == "eng"){name <- labels$new_eng}

  #######################
  # organizing the data #
  #######################

  if (length(time_period) > 1) {
    colnames(dat[[1]]) <- name
    colnames(dat[[2]]) <- name

    df <- dplyr::bind_rows(dat[[1]],dat[[2]])

  } else {
    colnames(dat[[1]]) <- name

    df <- dplyr::bind_rows(
      dat[[1]],
    )
  }


  ############
  # datasets #
  ############

  if(dataset == "all"){
    df <- df
  }

  if(dataset == "life_quality"){
    df <- df[,c(1:3,6:13,15:19,57:62)]
  }

  if(dataset == "sanit_habit"){
    df <- df[,c(1:3,9:11,15,26:32)]
  }

  if(dataset == "violence"){
    df <- df[,c(1:3,33:37,63:67)]
  }

  if(dataset == "educ"){
    df <- df[,c(1:3,13,38:42,68,69)]
  }

  if(dataset == "communic"){
    df <- df[,c(1:3,43:46,14)]
  }

  if(dataset == "mortality"){
    df <- df[,c(1:3,21:25,47:51)]
  }

  if(dataset == "deforest"){
    df<- df[,c(1:3,52:56,16)]
  }

  return(df)
}
