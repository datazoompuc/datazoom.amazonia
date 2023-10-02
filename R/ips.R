#' @title IPS - Amazon Social Progress Index
#'
#' @description Loads information on the social and environmental performance of the Legal Amazon.
#'
#' @param dataset A dataset name ("all", "life_quality", "sanit_habit", "violence", "educ", "communic", "mortality", or "deforest")
#' @param time_period Year to download. Can be 2014, 2018, 2021, 2023, or a vector with some combination thereof
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examples \dontrun{
#' # Download raw data from 2014
#' data <- load_ips(dataset = "all", raw_data = TRUE, time_period = 2014)
#'
#' # Download treated deforest data from 2018 in portuguese
#' data <- load_ips(
#'   dataset = "deforest", raw_data = FALSE,
#'   time_period = 2018, language = "pt"
#' )
#' }
#'
#' @export

load_ips <- function(dataset = "all", raw_data = FALSE,
                     time_period = c(2014, 2018, 2021, 2023), language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  survey <- link <- .data <- NULL
  codigo_ibge <- municipio <- estado <- ips_amazonia <- NULL
  necessidades_humanas_basicas <- fundamentos_para_o_bem_estar <- NULL
  oportunidades <- agua_e_saneamento <- moradia <- seguranca_pessoal <- NULL
  acesso_ao_conhecimento_basico <- acesso_a_informacao_e_comunicacao <- NULL
  saude_e_bem_estar <- qualidade_do_meio_ambiente <- direitos_individuais <- NULL
  liberdade_individual_e_de_escolha <- tolerancia_e_inclusao <- acesso_a_educacao_superior <- NULL
  mortalidade_infantil_ate_5_anos_obitos_1_000_nasc_vivos <- NULL
  mortalidade_materna_obitos_maternos_100_000_nascidos_vivos <- NULL
  mortalidade_por_desnutricao_obitos_100_000_habitantes <- NULL
  mortalidade_por_doencas_infecciosas_obitos_100_000_habitantes <- NULL
  subnutricao_percent_da_populacao <- abastecimento_de_agua_adequado_percent_da_populacao <- NULL
  esgoto_adequado_percent_da_populacao <- indice_atendimento_de_agua_percent_da_populacao <- NULL
  coleta_de_lixo_adequada_percent_de_domicilios <- moradias_com_iluminacao_adequada_percent_de_domicilios <- NULL
  moradias_com_parede_adequada_percent_de_domicilios <- NULL
  moradias_com_piso_adequado_percent_de_domicilios <- NULL
  assassinatos_de_jovens_obitos_100_000_habitantes_de_15_a_29_anos <- NULL
  assassinatos_de_jovens_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40 <- NULL
  homicidios_obitos_100_000_habitantes <- homicidios_categorico_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40 <- NULL
  mortes_por_acidente_no_transito_obitos_100_000_habitantes <- abandono_escolar_ensino_fundamental_percent_de_alunos <- NULL
  distorcao_idade_serie_ensino_fundamental_percent_de_alunos <- distorcao_idade_serie_ensino_medio_percent_de_alunos <- NULL
  qualidade_da_educacao_ideb_ensino_fundamental <- reprovacao_escolar_ensino_fundamental_percent_de_alunos <- NULL
  densidade_internet_banda_larga_no_de_acessos_100_domicilios <- NULL
  densidade_telefonia_fixa_no_de_acessos_100_domicilios <- NULL
  densidade_telefonia_movel_no_de_acessos_100_habitantes <- NULL
  densidade_tv_por_assinatura_no_de_acessos_100_domicilios <- NULL
  mortalidade_por_diabetes_mellitus_obitos_100_000_habitantes <- NULL
  mortalidade_por_cancer_obitos_100_000_habitantes <- NULL
  mortalidade_por_doencas_circulatorias_obitos_100_000_habitantes <- NULL
  mortalidade_por_doencas_respiratorias_obitos_100_000_habitantes <- NULL
  mortalidade_por_suicidios_obitos_100_000_habitantes <- NULL
  areas_protegidas_percent_area_total_do_municipio <- NULL
  desmatamento_acumulado_percent_area_total_do_municipio <- NULL
  desmatamento_recente_percent_area_total_do_municipio <- NULL
  emissoes_co2_ton_co2_habitante <- focos_de_calor_na_de_focos_1_000_habitantes <- NULL
  diversidade_partidaria_percent_vereadores_eleitos_partidos_diferentes <- NULL
  transporte_publico_no_de_onibus_e_micro_onibus_1_000_habitantes <- NULL
  acesso_a_cultura_esporte_e_lazer_categorica_1_10 <- NULL
  gravidez_na_infancia_e_adolescencia_percent_de_filhos_de_maes_com_ate_19_anos <- NULL
  trabalho_infantil_no_de_familias_com_ao_menos_1_membro_em_trabalho_infantil_1_000_familias <- NULL
  vulnerabilidade_familiar_percent_de_filhos_de_maes_solteiras <- NULL
  violencia_contra_indigenas_no_de_casos_1_000_indigenas <- NULL
  violencia_contra_indigenas_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_0_1_2_7_3_2_7_8_8_4_8_8_20_8_5_20_8 <- NULL
  violencia_contra_mulheres_no_de_casos_100_000_mulheres <- NULL
  violencia_infantil_no_de_casos_100_000_pessoas_de_0_14_anos <- NULL
  violencia_infantil_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_1_1_40_1_3_40_1_133_1_4_133_1_496_0_5_496_0 <- NULL
  empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total <- NULL
  mulheres_com_empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total <- NULL
  nutricao_e_cuidados_medicos_basicos <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # Picking which sheet to download

  sheet_list <- c(
    "2014" = "2014",
    "2018" = "2018 ",
    "2021" = "2021",
    "2023" = "2023"
  )

  sheets <- param$time_period %>%
    {purrr::quietly(dplyr::recode)}(!!!sheet_list) %>%
    purrr::pluck("result")

  if(any(is.na(sheets))) {
    stop("Some of the years you request aren't available. Check documentation for time availability.")
  }

  ##############
  ## Download ##
  ##############

  dat <- external_download(
    dataset = "ips",
    source = "ips",
    sheet = sheets
  )

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # cleaning column names

  dat <- dat %>%
    purrr::map(
      janitor::clean_names
    )

  # removing years from column names to be able to match columns from different years

  strs_to_remove <- paste0("_", 2012:2023, collapse = "|")

  dat <- dat %>%
    purrr::map(
      function(df) {
        df %>%
          dplyr::rename_with(
            ~ stringr::str_remove_all(., pattern = strs_to_remove)
          )
      }
    )

  # creating year variable

  dat <- purrr::map2(
    dat, param$time_period,
    function(df, year) {
      df %>%
        dplyr::mutate(ano = year)
    }
  )

  # stacking all years

  dat <- dat %>%
    dplyr::bind_rows()

  ##############
  ## Datasets ##
  ##############

  # vectors with groups of variables that belong to each dataset

  if (param$dataset == "all") {
    var_list <- names(dat)
  }
  if (param$dataset == "life_quality") {
    var_list <- c(
      "ano", "codigo_ibge",
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
      "diversidade_partidaria",
      "transporte_publico",
      "acesso_a_cultura_esporte_e_lazer_categorica_1_10",
      "gravidez_na_infancia",
      "trabalho_infantil",
      "vulnerabilidade_familiar"
    )
  }
  if (dataset == "sanit_habit") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "nutricao_e_cuidados_medicos_basicos",
      "saude_e_bem_estar",
      "agua",
      "esgoto",
      "indice_atendimento_de_agua",
      "coleta_de_lixo",
      "moradia"
    )
  }
  if (dataset == "violence") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "assassinatos",
      "homicidios",
      "mortes_por_acidente_no_transito_obitos_100_000_habitantes"
    )
  }
  if (dataset == "educ") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "acesso_ao_conhecimento_basico",
      "ensino"
    )
  }

  if (dataset == "communic") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "densidade",
      "acesso_a_informacao_e_comunicacao"
    )
  }

  if (dataset == "mortality") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "mortalidade",
      "subnutricao"
    )
  }


  if (dataset == "deforest") {
    var_list <- c(
      "ano",
      "codigo_ibge",
      "municipio",
      "estado",
      "qualidade_do_meio_ambiente",
      "areas_protegidas",
      "desmatamento",
      "emissoes",
      "focos_de_calor"
    )
  }

  # keep only columns containing the strings in var_list

  dat <- dat %>%
    dplyr::select(dplyr::contains(var_list))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (language == "pt") {
    dat_mod <- dat
  }

  if (language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(dplyr::any_of(
        c(
          year = "ano",
          municipality_code = "codigo_ibge",
          municipality = "municipio",
          state = "estado",
          ips_amazon = "ips_amazonia",
          basic_human_needs = "necessidades_humanas_basicas",
          well_being_fundamentals = "fundamentos_para_o_bem_estar",
          oportunities = "oportunidades",
          nutrition_and_basic_medical_care = "nutricao_e_cuidados_medicos_basicos",
          water_and_sanitation = "agua_e_saneamento",
          habitation = "moradia",
          personal_safety = "seguranca_pessoal",
          access_to_basic_knowledge = "acesso_ao_conhecimento_basico",
          access_to_info_and_comunication = "acesso_a_informacao_e_comunicacao",
          health_and_well_being = "saude_e_bem_estar",
          environment_quality = "qualidade_do_meio_ambiente",
          individual_rights = "direitos_individuais",
          individual_freedom_of_choice = "liberdade_individual_e_de_escolha",
          tolerance_and_inclusion = "tolerancia_e_inclusao",
          higher_education_access = "acesso_a_educacao_superior",
          infant_mortality_until_5_years_deaths_1_000_live_births = "mortalidade_infantil_ate_5_anos_obitos_1_000_nasc_vivos",
          maternal_mortality_deaths_100_000_live_births = "mortalidade_materna_obitos_maternos_100_000_nascidos_vivos",
          malnutrition_mortality_death_100_000_people = "mortalidade_por_desnutricao_obitos_100_000_habitantes",
          infeccious_diseases_mortality_deaths_100_000_people = "mortalidade_por_doencas_infecciosas_obitos_100_000_habitantes",
          malnutrition_percent_population = "subnutricao_percent_da_populacao",
          adaquate_water_supply_percent_population = "abastecimento_de_agua_adequado_percent_da_populacao",
          adaquate_sewage_percent_population = "esgoto_adequado_percent_da_populacao",
          index_water_services_percent_population = "indice_atendimento_de_agua_percent_da_populacao",
          adaquate_garbage_collection_percent_houses = "coleta_de_lixo_adequada_percent_de_domicilios",
          habitation_with_adequate_illumination_percent = "moradias_com_iluminacao_adequada_percent_de_domicilios",
          habitation_with_adequate_walls_percent = "moradias_com_parede_adequada_percent_de_domicilios",
          habitation_with_adequate_floor_percent = "moradias_com_piso_adequado_percent_de_domicilios",
          youth_murders_deaths_100_000_people_15_to_29_years_old = "assassinatos_de_jovens_obitos_100_000_habitantes_de_15_a_29_anos",
          youth_murders_scored_on_1_to_6_scale_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40_plus = "assassinatos_de_jovens_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40",
          homicides_deaths_100_000_people = "homicidios_obitos_100_000_habitantes",
          homicides_scored_on_1_to_6_scale_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40_plus = "homicidios_categorico_taxa_pontuada_em_uma_escala_de_1_6_1_0_2_1_6_3_6_10_4_10_20_5_20_40_6_40",
          traffic_accidents_deaths_100_000_people = "mortes_por_acidente_no_transito_obitos_100_000_habitantes",
          elementary_school_dropout_percent_students = "abandono_escolar_ensino_fundamental_percent_de_alunos",
          elementary_school_age_grade_distortion_percent_students = "distorcao_idade_serie_ensino_fundamental_percent_de_alunos",
          high_school_age_grade_distortion_percent_students = "distorcao_idade_serie_ensino_medio_percent_de_alunos",
          elementary_education_quality_ideb_0_to_10_scale = "qualidade_da_educacao_ideb_ensino_fundamental",
          students_failing_elementary_school_percent = "reprovacao_escolar_ensino_fundamental_percent_de_alunos",
          broadband_internet_acesses_per_100_houses = "densidade_internet_banda_larga_no_de_acessos_100_domicilios",
          landline_telephone_accesses_per_100_houses = "densidade_telefonia_fixa_no_de_acessos_100_domicilios",
          mobile_phone_access_per_100_people = "densidade_telefonia_movel_no_de_acessos_100_habitantes",
          houses_with_cable_TV_for_each_100_houses = "densidade_tv_por_assinatura_no_de_acessos_100_domicilios",
          deaths_by_diabetes_mellitus_per_100_000_people = "mortalidade_por_diabetes_mellitus_obitos_100_000_habitantes",
          deaths_by_cancer_per_100_000_people = "mortalidade_por_cancer_obitos_100_000_habitantes",
          deaths_by_circulatory_diseases_per_100_000_people = "mortalidade_por_doencas_circulatorias_obitos_100_000_habitantes",
          deaths_by_respiratory_desiases_per_100_000_people = "mortalidade_por_doencas_respiratorias_obitos_100_000_habitantes",
          suicide_deaths_per_100_000_people = "mortalidade_por_suicidios_obitos_100_000_habitantes",
          protected_areas_percent_municipality_area = "areas_protegidas_percent_area_total_do_municipio",
          accumulated_deforestation_percent_municipality_area = "desmatamento_acumulado_percent_area_total_do_municipio",
          recent_deforestation_percent_municipality_area = "desmatamento_recente_percent_area_total_do_municipio",
          co2_emission_ton_of_co2_per_inhabitant = "emissoes_co2_ton_co2_habitante",
          number_of_hotspots_per_1_000_inhabitant = "focos_de_calor_na_de_focos_1_000_habitantes",
          partisan_diversity_percent_of_councilman_from_different_parties = "diversidade_partidaria_percent_vereadores_eleitos_partidos_diferentes",
          public_transport_buses_per_1_000_inhabitants = "transporte_publico_no_de_onibus_e_micro_onibus_1_000_habitantes",
          access_culture_sport_leisure_categorical_1_to_10 = "acesso_a_cultura_esporte_e_lazer_categorica_1_10",
          child_of_adolescence_pregnancy_percent_of_children_from_mom_aged_up_to_19_years_old = "gravidez_na_infancia_e_adolescencia_percent_de_filhos_de_maes_com_ate_19_anos",
          child_labor_number_families_with_at_least_one_kid_working_per_1_000_families = "trabalho_infantil_no_de_familias_com_ao_menos_1_membro_em_trabalho_infantil_1_000_familias",
          family_vulnerability_percent_of_kids_raised_by_single_mothers = "vulnerabilidade_familiar_percent_de_filhos_de_maes_solteiras",
          violence_against_indigenous_people_cases_by_1_000_indigenous_people = "violencia_contra_indigenas_no_de_casos_1_000_indigenas",
          violence_against_indigenous_people_scale_from_1_to_5 = "violencia_contra_indigenas_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_0_1_2_7_3_2_7_8_8_4_8_8_20_8_5_20_8",
          violence_against_women_per_100_000_women = "violencia_contra_mulheres_no_de_casos_100_000_mulheres",
          violence_against_child_number_of_cases_per_100_000_kids_between_0_and_14_years_old = "violencia_infantil_no_de_casos_100_000_pessoas_de_0_14_anos",
          violence_against_child_1_to_5_scale = "violencia_infantil_taxa_pontuada_em_uma_escala_de_1_5_1_0_2_1_1_40_1_3_40_1_133_1_4_133_1_496_0_5_496_0",
          higher_education_jobs_percent_compared_to_total_jobs = "empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total",
          women_in_higher_education_jobs_percent_in_relation_to_total_jobs = "mulheres_com_empregos_ensino_superior_percent_de_empregos_em_relacao_ao_total"
        )
      ))
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
