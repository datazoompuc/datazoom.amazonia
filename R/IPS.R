#' @title IPS - Amazon Social Progress Index
#'
#' @description Loads information on the social and environmental performance of the Legal Amazon. Survey is done at the municipal level and data is available in 2014 and 2018.
#'
#' @param dataset A dataset name ("ips")
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
#' ips <- load_ips(dataset = 'ips', raw_data = TRUE, time_period = 2014)
#' }


load_ips = function(dataset = "ips", raw_data,
                    time_period, language = 'eng'){

  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- .data <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$time_period = time_period
  param$language = language
  # param$time_id = time_id
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

  ##############
  ## Download ##
  ##############

  dat = as.list(param$time_period) %>%
    purrr::map(
      function(t){external_download(dataset = param$dataset,source='ips',year = t) %>%
          dplyr::mutate(ano = t)}
    )

  raw <- dat

  dat <- dat %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}


  ## Data May Have Different Names, we Need to be careful

  col.names <- data.frame(
   portuguese = c(
     "cod_municipio", "municipio", "uf", "ano", "IPS",
     "D1", "D2", "D3",
     paste0("C", 1:12),
     paste0("V", 1:43)
   ),
   english = c(
     "municipality_code", "municipality", "state", "year", "SPI",
     "D1", "D2", "D3",
     paste0("C", 1:12),
     paste0("V", 1:43)
   )
  )
    ########
  labels <- tibble::tribble(
   ~original_name, ~new_pt.br, ~new_eng,
   "Codigo IBGE do municipio", "Codigo IBGE do municipio", "IBGE city code",
   "Municipio", "", "Municipality",
   "Estado", "UF", "State",
   "IPS", "Indice de Progresso Social", "Social Progress Index",
   "Necessidades Humanas Basicas", "", "Basic human needs",
   "Fundamentos para o Bem-Estar", "", "Well-being fundamentals",
   "Oportunidades", "", "Opportunities",
   "Nutricao e cuidados medicos basicos", "", "Nutrition and basic medical care",
   "Agua e saneamento", "", "Water and sanitation",
   "Moradia", "", "Habitation",
   "Seguranca pessoal", "", "Personal safety",
   "Acesso ao conhecimento basico", "", "Access to basic knowledge",
   "Acesso a informacao e comunicacao", "", "Access to information and communication",
   "Saude e bem-estar", "", "Health and well-being",
   "Qualidade do meio ambiente", "", "Environment quality",
   "Direitos individuais", "", "Individual rights",
   "Liberdade individual e de escolha", "", "Individual freedom of choice",
   "Tolerancia e inclusao", "", "Tolerance and inclusion",
   "Acesso a educacao superior", "", "Access to higher education",
   "Mortalidade infantil ate 5 anos (Obitos por mil nascidos vivos)", "",
   "Infant mortality - until 5 years (Deaths per thousand live births)",
   "Mortalidade materna (Obitos maternos por 100 mil nascidos vivos)", "",
   "Maternal mortality (Maternal deaths per 100.000 live births)",
   "Mortalidade por desnutricao (Obitos por 100 mil habitantes)", "",
   "Malnutrition mortality (Deaths per 100.000 people)",
   "Mortalidade por doencas infecciosas (Obitos por 100 mil habitantes)", "",
   "Infeccious diseases mortality (Deaths per 100.000 people)",
   "Subnutricao (% da populacao)", "", "Malnutrition (% of population)",
   "Abastecimento de agua (% da populacao)", "", "Water supply (% of population)",
   "Esgotamento sanitario  (% da populacao)", "", "Sewage (% of population)",
   "Saneamento rural (diferenca entre a % da pop.
  Rural com acesso a agua em relacao a urbana)",
   "", "Rural sanitation (difference between % of rural pop. with access to water relative
  to urban population)",
   "Acesso a energia eletrica (% da populacao)", "", "Access to electricity (% da populacao)",
   "Coleta de lixo (% da populacao)", "", "Garbage collection (% of population)",
   "Moradia adequada (% da populacao)", "", "Adequate habitation (% of population)",
   "Assassinatos de jovens (Obitos por 100 mil habitantes de 15 a 24 anos).
  Pontuados em uma escala de 1-6: 1 = 0 2 = 1 - 6 3 = 6 - 10 4 = 10 - 20 5 = 20 - 40 6 > 40)",
   "", "Youth murders (Deaths per 100.000 people aged 15 to 24. Scored on a 1-6 scale:
  1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40",
   "Homicidios", "",
   "Homicides (Deaths per 100.000 people). Scored on a 1-6 scale:
  1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40",
   "Mortes por acidente no transito (Obitos por 100 mil habitantes)", "",
   "Traffic accident deaths (Deaths per 100.000 people)",
   "Acesso ao ensino fundamental (% de frequencia liquida ao ensino fundamental)",
   "", "Access to elementary school (% of net frequency in elementary school)",
   "Acesso ao ensino medio (% de frequencia liquida ao ensino medio)",
   "", "Access to high school (% of net frequency in middle school)",
   "Analfabetismo (% da populacao de 15 anos ou mais)", "",
   "Illiteracy (% of 15+ population)",
   "Qualidade da educacao Ideb (escala de 0-10)", "", "Education quality Ideb (0-10 scale)",
   "% de conexao efetuadas com sucesso. Pontuados em uma escala de 0-5.
  0 = 2% 1 = 2% - 79% 2 = 80% - 96% 3 = 96% - 98% 4 = 98% - 99% 5 = 99% - 100",
   "", "% of successful connections. Scored on a 0-5 scale. 0 = 2%; 1 = 2%-79%;
  2 = 80%-96%; 3 = 96%-98%; 4 = 98%-99%; 5 = 99%-100%",
   "Conexao de voz (% de ligacoes realizadas com sucesso. Pontuados em uma escala de
  1-5. 1 = 49% - 79% 2 = 80% - 96% 3 = 96% - 98% 4 = 98% - 99% 5 = 99% - 100)", "",
   "Voice connections (% of successful calls. Scored on a 1-5 scale. 1 = 49%-79%;
  2 = 80%-96%; 3 = 96%-98%; 4 = 98%-99%; 5 = 99%-100%",
   "Expectativa de vida ao nascer (numero de anos)", "", "Life expectancy at birth (years)",
   "Mortalidade por doencas crOnicas (Obitos por 100 mil habitantes)", "",
   "Infeccious diseases mortality (Deaths per 100.000 people)",
   "Mortalidade por doencas respiratOrias (Obitos por 100 mil habitantes)",
   "", "Respiratory diseases mortality (Deaths per 100.000 people)",
   "Obesidade (% da populacao)", "", "Obesity (% of population)",
   "Suicidio (Obitos por 100 mil habitantes)", "",
   "Suicides (Deaths per 100.000 people)",
   "area degradada (%)", "", "Degraded area (%)",
   "areas protegidas (%)", "", "Protected area (%)",
   "Desmatamento acumulado (%)", "", "Accumulated deforestation (%)",
   "Desmatamento recente (% do desmatamento de 2015, 2016, 2017 em relacao ao total)",
   "", "Recent deforestation",
   "Desperdicio de agua (%)", "", "Water waste (%)",
   "Diversidade partidaria (%)", "", "Partisan diversity (%)",
   "Mobilidade urbana (numero de Onibus por mil habitantes)",
   "", "Urban mobility (buses per thousand people)",
   "Pessoas ameacadas (numero de ameacados de morte por 100 mil habitantes)", "",
   "Threatened people (number of people threatened with death per thousand people)",
   "Acesso a cultura, lazer e esporte (CategOrica. Pontuado em:
  0 = nenhuma estrutura; 1 = uma; 2 = duas; 3 = tres; 4 = todas as estruturas)",
   "", "Access to culture, leisure, and sports (Cathegorical. Scored by:
  0 = no structure; 1 = one; 2 = two; 3 = three; 4 = all structures",
   "Gravidez na infancia e adolescencia (% de mulheres de 15 a 17 anos que tiveram filhos)",
   "", "Child of adolescence pregnancy (% of women aged 15-17 with children)",
   "Trabalho infantil (% da populacao entre 10 e 14 anos de idade)", "",
   "Child labor (% of population aged 10-14)",
   "Vulnerabilidade familia (% de maes)", "", "Family vulnerability (% of mothers)",
   "Desigualdade racial na educacao (% da populacao com 15 anos ou mais)", "",
   "Racial inequality in education (% da populacao com 15 anos e mais)",
   "Violencia contra a mulher (casos por 100 mil mulheres)", "",
   "Violence against women (cases per 100.000 women)",
   "Violencia contra indigena (casos por mil indigenas.
  Pontuados em uma escala de 1-3. 1 = 0 - 20  2 = 21 - 40  3 > 40)", "",
   "Violence against indigenous people (cases by thousand indigenous people.
  Scored on a 1-3 scale. 1 = 0-20; 2 = 21-40; 3 = > 40)",
   "Educacao feminina (% da populacao feminina com 15 anos ou mais)", "",
   "Female education (% of female population aged 15 or more)",
   "Frequencia ao ensino superior (% da populacao entre 18-24 anos)", "",
   "Attendance to higher education (% of population aged 18-24)",
   "Pessoas com ensino superior (% da populacao com mais de 25 anos)", "",
   "People with higher education (% of population aged 25+)",
   "Ano", "Ano", "Year",
  )

  #######
  if (length(time_period) > 1) {

    colnames(raw[[1]]) <- labels$original_name
    colnames(raw[[2]]) <- labels$original_name

    df <- dplyr::bind_rows(
      raw[[1]],
      raw[[2]]
    ) %>%
      dplyr::relocate(
        .data$`Codigo IBGE do municipio`, .data$Municipio,
        .data$Estado, .data$Ano, dplyr::everything()
      ) %>%
      dplyr::filter(.data$`Codigo IBGE do municipio` %in% legal_amazon$CD_MUN)

  } else {

    colnames(raw[[1]]) <- labels$original_name

    df <- dplyr::bind_rows(
      raw[[1]],
    ) %>%
      dplyr::relocate(
        .data$`Codigo IBGE do municipio`, .data$Municipio,
        .data$Estado, .data$Ano, dplyr::everything()
      ) %>%
      dplyr::filter(.data$`Codigo IBGE do municipio` %in% legal_amazon$CD_MUN)
  }

  if (language == "pt") {

    labels_key <- as.list(colnames(df)) %>%
      stats::setNames(colnames(df))

     df <- df %>%
       labelled::set_variable_labels(.labels = labels_key)

     colnames(df) <- col.names$portuguese

  } else {

   labels_key <- as.list(labels$new_eng) %>%
     stats::setNames(labels$original_name)

   df <- df %>%
     labelled::set_variable_labels(.labels = labels_key)

   colnames(df) <- col.names$english
  }

  return(df)


}
