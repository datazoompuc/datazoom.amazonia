#' @title IPS - Amazon Social Progress Index
#'
#' @description Loads information on the social and environmental performance of the Legal Amazon. Survey is done at the municipal level and data is available in 2014, 2018 and 2021. See \url{http://www.ipsamazonia.org.br/}
#'
#' @param dataset There are multiple datasets ("all", "life_quality", "sanit_habit", "violence", "educ", "communic", "mortality", "deforest")
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
#' ips <- load_ips(dataset = "all", raw_data = TRUE, time_period = 2014)
#' }
load_ips <- function(dataset = "all", raw_data,
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
  
  ##
  if (raw_data == TRUE & language == "eng") {
    stop("raw_data == TRUE only available in portuguese")
  }
  
  
  ##############
  ## Download ##
  ##############
  
  dat <- as.list(param$time_period) %>%
    purrr::map(
      function(t) {
        external_download(dataset = "ips", source = "ips", year = t)
      }
    )
  
  
  
  ####################
  # language = 'pt' ##
  ####################
  
  if(raw_data == FALSE){
    
    year_char <- c("_2012","_2013","_2014","_2015","_2016","_2017","_2018","_2019", "_2020","_2021")
    
    for (j in 1:length(time_period)) {
      
      for (i in 1:length(year_char)) {
        
        colnames(dat[[j]]) = colnames(dat[[j]]) %>%
          
          str_remove_all(year_char[i])
        
      }
      
    }
    
    dat_pt = dat
    
  }
  
  
  #####################
  # language = 'eng' ##
  #####################
  
  if(raw_data == FALSE & language == 'eng'){
    
    dat_eng = dat
    
    for (i in 1:length(time_period)) {
      
      dat_eng[[i]] <- dat_eng[[i]] %>%
        
        rename(
          
          c(code_ibge = codigo_ibge,
            municipality = municipio,
            state = estado,
            ips_amazon = ips_amazonia,
            basic_human_needs = necessidades_humanas_basicas,
            well_being_fundamentals = fundamentos_para_o_bem_estar,
            oportunities = oportunidades,
            nutrition_and_basic_medical_care = nutricao_e_cuidados_medicos_basicos,
            water_and_sanitation = agua_e_saneamento,
            habitation = moradia,
            personal_safety = seguranca_pessoal,
            access_to_basic_knowledge = acesso_ao_conhecimento_basico,
            access_to_info_and_comunication = acesso_a_informacao_e_comunicacao,
            health_and_well_being = saude_e_bem_estar,
            environment_quality = qualidade_do_meio_ambiente,
            individual_rights = direitos_individuais,
            individual_freedom_of_choice = liberdade_individual_e_de_escolha,
            tolerance_and_inclusion = tolerancia_e_inclusao,
            higher_education_access = acesso_a_educacao_superior,
            infant_mortality_until_5_years_deaths_1_000_live_births = mortalidade_infantil_ate_5_anos_obitos_1_000_nasc_vivos,
            maternal_mortality_deaths_100_000_live_births = mortalidade_materna_obitos_maternos_100_000_nascidos_vivos,
            malnutrition_mortality_death_100_000_people = mortalidade_por_desnutricao_obitos_100_000_habitantes,
            infeccious_diseases_mortality_deaths_100_000_people = mortalidade_por_doencas_infecciosas_obitos_100_000_habitantes,
            malnutrition_percent_population = subnutricao_percent_da_populacao,
            adaquate_water_supply_percent_population = abastecimento_de_agua_adequado_percent_da_populacao,
            adaquate_sewage_percent_population = esgoto_adequado_percent_da_populacao,
            index_water_services_percent_population = indice_atendimento_de_agua_percent_da_populacao,
            adaquate_garbage_collection_percent_houses = coleta_de_lixo_adequada_percent_de_domicilios,
            habitation_with_adequate_illumination_percent =  moradias_com_iluminacao_adequada_percent_de_domicilios,
            habitation_with_adequate_walls_percent = moradias_com_parede_adequada_percent_de_domicilios,
            habitation_with_adequate_floor_percent = moradias_com_piso_adequado_percent_de_domicilios,
            youth_murders_deaths_100_000_people_15_to_29_years_old = assassinatos_de_jovens_obitos_100_000_habitantes_de_15_a_29_anos,
            youth_murders_scored_on_1_to_6_scale_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40_plus = assassinatos_de_jovens_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40,
            homicides_deaths_100_000_people = homicidios_obitos_100_000_habitantes,
            homicides_scored_on_1_to_6_scale_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40_plus = homicidios_categorico_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40,
            traffic_accidents_deaths_100_000_people = mortes_por_acidente_no_transito_obitos_100_000_habitantes,
            elementary_school_dropout_percent_students = abandono_escolar_ensino_fundamental_percent_de_alunos,
            elementary_school_age_grade_distortion_percent_students = distorcao_idade_serie_ensino_fundamental_percent_de_alunos,
            high_school_age_grade_distortion_percent_students = distorcao_idade_serie_ensino_medio_percent_de_alunos,
            elementary_education_quality_ideb_0_to_10_scale = qualidade_da_educacao_ideb_ensino_fundamental,
            students_failing_elementary_school_percent = reprovacao_escolar_ensino_fundamental_percent_de_alunos,
            broadband_internet_acesses_per_100_houses = densidade_internet_banda_larga_no_de_acessos_100_domicilios,
            landline_telephone_accesses_per_100_houses = densidade_telefonia_fixa_no_de_acessos_100_domicilios,
            mobile_phone_access_per_100_people = densidade_telefonia_movel_no_de_acessos_100_habitantes,
            houses_with_cable_TV_for_each_100_houses = densidade_tv_por_assinatura_no_de_acessos_100_domicilios,
            deaths_by_diabetes_mellitus_per_100_000_people = mortalidade_por_diabetes_mellitus_obitos_100_000_habitantes,
            deaths_by_cancer_per_100_000_people = mortalidade_por_cancer_obitos_100_000_habitantes,
            deaths_by_circulatory_diseases_per_100_000_people = mortalidade_por_doencas_circulatorias_obitos_100_000_habitantes,
            deaths_by_respiratory_desiases_per_100_000_people = mortalidade_por_doencas_respiratorias_obitos_100_000_habitantes,
            suicide_deaths_per_100_000_people = mortalidade_por_suicidios_obitos_100_000_habitantes,
            protected_areas_percent_municipality_area = areas_protegidas_percent_area_total_do_municipio,
            accumulated_deforestation_percent_municipality_area =  desmatamento_acumulado_percent_area_total_do_municipio,
            recent_deforestation_percent_municipality_area = desmatamento_recente_percent_area_total_do_municipio,
            co2_emission_ton_of_co2_per_inhabitant = emissoes_co2_ton_co2_habitante,
            number_of_hotspots_per_1_000_inhabitant = focos_de_calor_na_de_focos_1_000_habitantes,
            partisan_diversity_percent_of_councilman_from_different_parties = diversidade_partidaria_percent_vereadores_eleitos_partidos_diferentes,
            public_transport_buses_per_1_000_inhabitants =  transporte_publico_no_de_onibus_e_micro_onibus_1_000_habitantes,
            access_culture_sport_leisure_categorical_1_to_10 = acesso_a_cultura_esporte_e_lazer_categorica_1_10,
            child_of_adolescence_pregnancy_percent_of_children_from_mom_aged_up_to_19_years_old = gravidez_na_infancia_e_adolescencia_percent_de_filhos_de_maes_com_ate_19_anos,
            child_labor_number_families_with_at_least_one_kid_working_per_1_000_families =  trabalho_infantil_no_de_familias_com_ao_menos_1_membro_em_trabalho_infantil_1_000_familias,
            family_vulnerability_percent_of_kids_raised_by_single_mothers = vulnerabilidade_familiar_percent_de_filhos_de_maes_solteiras,
            violence_against_indigenous_people_cases_by_1_000_indigenous_people = violencia_contra_indigenas_no_de_casos_1_000_indigenas,
            violence_against_indigenous_people_scale_from_1_to_5 = violencia_contra_indigenas_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_0_1_2_7_3_2_7_8_8_4_8_8_20_8_5_20_8,
            violence_against_women_per_100_000_women = violencia_contra_mulheres_no_de_casos_100_000_mulheres,
            violence_against_child_number_of_cases_per_100_000_kids_between_0_and_14_years_old = violencia_infantil_no_de_casos_100_000_pessoas_de_0_14_anos,
            violence_against_child_1_to_5_scale = violencia_infantil_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_1_1_40_1_3_40_1_133_1_4_133_1_496_0_5_496_0,
            higher_education_jobs_percent_compared_to_total_jobs = empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total,
            women_in_higher_education_jobs_percent_in_relation_to_total_jobs = mulheres_com_empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total
          ))
    }
    
  }
  
  ###############
  ##  datasets ##
  ###############
  
  
  for (i in 1:length(time_period)) {
    
    if(dataset == "life_quality"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               "necessidades_humanas_basicas",
               "fundamentos_para_o_bem_estar",
               "oportunidades",
               "nutricao_e_cuidados_medicos_basicos",
               "agua_e_saneamento",
               "moradia",
               "seguranca_pessoal",
               "acesso_ao_conhecimento_basico",
               "saude_e_bem_estar",
               "qualidade_do_meio_ambiente",
               "direitos_individuais",
               "liberdade_individual_e_de_escolha",
               "tolerancia_e_inclusao",
               str_subset(colnames(dat[[i]]), 'diversidade_partidaria'),
               str_subset(colnames(dat[[i]]), 'transporte_publico'),
               "acesso_a_cultura_esporte_e_lazer_categorica_1_10",
               str_subset(colnames(dat[[i]]), 'gravidez_na_infancia'),
               str_subset(colnames(dat[[i]]), 'trabalho_infantil'),
               str_subset(colnames(dat[[i]]), 'vulnerabilidade_familiar'))
    }
    
    if(dataset == "sanit_habit"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               "nutricao_e_cuidados_medicos_basicos",
               "saude_e_bem_estar",
               str_subset(colnames(dat[[i]]), 'agua'),
               str_subset(colnames(dat[[i]]), 'esgoto'),
               str_subset(colnames(dat[[i]]), 'indice_atendimento_de_agua'),
               str_subset(colnames(dat[[i]]), 'coleta_de_lixo'),
               str_subset(colnames(dat[[i]]), 'moradia'))
    }
    
    if(dataset == "violence"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               str_subset(colnames(dat[[i]]), 'assassinatos'),
               str_subset(colnames(dat[[i]]), 'homicidios'),
               "mortes_por_acidente_no_transito_obitos_100_000_habitantes")
    }
    
    if(dataset == "educ"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               "acesso_ao_conhecimento_basico",
               str_subset(colnames(dat[[i]]), 'ensino'))
    }
    
    if(dataset == "communic"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               str_subset(colnames(dat[[i]]), 'densidade'),
               "acesso_a_informacao_e_comunicacao")
    }
    
    if(dataset == "mortality"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               str_subset(colnames(dat[[i]]), 'mortalidade'),
               str_subset(colnames(dat[[i]]), 'subnutricao'))
    }
    
    
    if(dataset == "deforest"){
      
      dat[[i]] <- dat[[i]] %>%
        select("codigo_ibge",
               "municipio",
               "estado",
               "qualidade_do_meio_ambiente",
               str_subset(colnames(dat[[i]]), 'areas_protegidas'),
               str_subset(colnames(dat[[i]]), 'desmatamento'),
               str_subset(colnames(dat[[i]]), 'emissoes'),
               str_subset(colnames(dat[[i]]), 'focos_de_calor'))
    }
  }
  
  if(raw_data == TRUE){return(dat)}
  
  ###################################################################################
  # getting the indexes from the columns selected from the 'pt' data frame and     ##
  # using these indexes to select the equivalent columns from the 'eng' data frame ##
  ###################################################################################
  
  if(raw_data == FALSE & language == 'eng'){
    
    col_index <- match(colnames(dat[[i]]),colnames(dat_pt[[i]]))
    
    for (i in 1:length(time_period)) {
      
      dat_eng[[i]] <- dat_eng[[i]][,col_index]
      
    }
    
    dat = dat_eng
  }
  
  ##########################################################################################################################
  # organizing the data: bind columns from the different data frames in the list 'dat' and create column indicating years ##
  ##########################################################################################################################
  
  if(raw_data == FALSE){
    
    if(length(time_period) == 3) {
      
      df <- dplyr::bind_rows(dat[[1]],dat[[2]], dat[[3]])
      
      year <- c(rep(time_period[1], nrow(dat[[1]])),
                rep(time_period[2], nrow(dat[[2]])),
                rep(time_period[3], nrow(dat[[3]]))) %>%
        as.data.frame()
    }
    
    
    if (length(time_period) == 2) {
      
      
      df <- dplyr::bind_rows(dat[[1]],dat[[2]])
      
      year <- c(rep(time_period[1], nrow(dat[[1]])),
                rep(time_period[2], nrow(dat[[2]]))) %>%
        as.data.frame()
      
    } else {
      
      df <- dplyr::bind_rows(
        dat[[1]],
      )
      
      year <- c(rep(time_period, nrow(dat[[1]]))) %>%
        as.data.frame()
      
    }
    
    colnames(year) <- 'year'
    df <- bind_cols(year,df)
    
    return(df)
  }
}




