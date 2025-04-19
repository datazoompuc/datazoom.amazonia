# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

teste1 <- load_datasus(
  dataset = "datasus_sinasc",
  time_period = 2023,
  language = "eng",
  raw_data = FALSE,
  states = "AC"
  )

testes <- load_datasus(
  dataset = "datasus_sih_rd",
  time_period = 2023,
  raw_data = FALSE,
  language = "pt",
  states = "AC"
  )


load_datasus("datasus_sinasc", time_period = 2023, states = "AC", raw_data = FALSE) %>% View()

"uf_zi", "uf_gestor", "uf_manager", "municipio do gestor", "municipality of the manager",
"ano_cmpt", "ano_processamento", "processing_year", "ano de processamento da aih", "aih processing year",
"mes_cmpt", "mes_processamento", "processing_month", "mes de processamento da aih", "aih processing month",
"espec", "especialidade_leito", "bed_specialty", "especialidade do leito", "bed specialty",
"cgc_hosp", "cnpj_estabelecimento", "establishment_cnpj", "cnpj do estabelecimento", "establishment cnpj",
"n_aih", "numero_aih", "aih_number", "numero da aih", "aih number",
"ident", "tipo_aih", "aih_type", "identificacao do tipo da aih", "aih type identification",
"cep", "cep_paciente", "patient_zipcode", "cep do paciente", "patient zipcode",
"municip_res", "municipio_residencia", "residence_municipality", "municipio de residencia do paciente", "patient's residence municipality",
"nasc", "data_nascimento", "birth_date", "data de nascimento do paciente", "patient's birth date",
"sexo", "sexo_paciente", "patient_gender", "sexo do paciente", "patient gender",
"uti_mes_in", "uti_mes_in", "uti_month_in", "zerado", "zeroed",
"uti_mes_an", "uti_mes_an", "uti_month_an", "zerado", "zeroed",
"uti_mes_al", "uti_mes_al", "uti_month_al", "zerado", "zeroed",
"uti_mes_to", "dias_uti_mes", "icu_days_month", "quantidade de dias de uti no mes", "number of icu days in the month",
"marca_uti", "tipo_uti", "icu_type", "indica qual o tipo de uti utilizada pelo paciente", "indicates the type of icu used by the patient",
"uti_int_in", "uti_int_in", "intermediate_care_in", "zerado", "zeroed",
"uti_int_an", "uti_int_an", "intermediate_care_an", "zerado", "zeroed",
"uti_int_al", "uti_int_al", "intermediate_care_al", "zerado", "zeroed",
"uti_int_to", "dias_unidade_intermediaria", "intermediate_care_days", "quantidade de diarias em unidade intermediaria", "number of days in intermediate care unit",
"diar_acom", "diarias_acompanhante", "companion_days", "quantidade de diarias de acompanhante", "number of companion days",
"qt_diarias", "quantidade_diarias", "number_of_days", "quantidade de diarias", "number of days",
"proc_solic", "procedimento_solicitado", "requested_procedure", "procedimento solicitado", "requested procedure",
"proc_rea", "procedimento_realizado", "performed_procedure", "procedimento realizado", "performed procedure",
"val_sh", "valor_servicos_hospitalares", "hospital_services_value", "valor de servicos hospitalares", "value of hospital services",
"val_sp", "valor_servicos_profissionais", "professional_services_value", "valor de servicos profissionais", "value of professional services",
"val_sadt", "valor_sadt", "sadt_value", "zerado", "zeroed",
"val_rn", "valor_rn", "rn_value", "zerado", "zeroed",
"val_acomp", "valor_acompanhante", "companion_value", "zerado", "zeroed",
"val_ortp", "valor_ortopedia", "orthopedics_value", "zerado", "zeroed",
"val_sangue", "valor_sangue", "blood_value", "zerado", "zeroed",
"val_sadtsr", "valor_sadtsr", "sadtsr_value", "zerado", "zeroed",
"val_transp", "valor_transporte", "transport_value", "zerado", "zeroed",
"val_obsang", "valor_obsang", "obsang_value", "zerado", "zeroed",
"val_ped1ac", "valor_ped1ac", "ped1ac_value", "zerado", "zeroed",


"val_tot", "valor_total", "total_value", "valor total da aih", "total value of the aih",
"val_uti", "valor_uti", "icu_value", "valor de uti", "icu value",
"us_tot", "valor_total_dolar", "total_value_usd", "valor total, em dolar", "total value in dollars",
"di_inter", "data_internacao", "admission_date", "data de internacao no formato aaammdd", "admission date in the format yymmdd",
"dt_saida", "data_saida", "discharge_date", "data de saida, no formato aaaammdd", "discharge date in the format yyyymmdd",
"diag_princ", "cid_diagnostico_principal", "main_diagnosis_cid", "codigo do diagnostico principal (cid10)", "main diagnosis code (cid10)",
"diag_secun", "cid_diagnostico_secundario", "secondary_diagnosis_cid", "codigo do diagnostico secundario (cid10)", "secondary diagnosis code (cid10)",
"cobranca", "motivo_saida_permanencia", "discharge_reason", "motivo de saida/permanencia", "reason for discharge/stay",
"natureza", "natureza_juridica_hospital", "hospital_legal_nature", "natureza juridica do hospital", "legal nature of the hospital",
"nat_jur", "natureza_juridica_estabelecimento", "establishment_legal_nature", "natureza juridica do estabelecimento", "legal nature of the establishment",
"destao", "tipo_gestao_hospital", "hospital_management_type", "indica o tipo de gestao do hospital", "indicates the type of hospital management",
"rubrica", "rubrica", "rubric", "zerado", "zeroed",
"ind_vdrl", "exame_vdrl", "vdrl_test", "indica exame vdrl", "indicates vdrl test",
"municip_mov", "municipio_estabelecimento", "establishment_municipality", "municipio do estabelecimento", "establishment municipality",
"cod_idade", "unidade_medida_idade", "age_unit", "unidade de medida da idade", "age measurement unit",
"idade", "idade_paciente", "patient_age", "idade", "age",
"dias_perm", "dias_permanencia", "days_of_stay", "dias de permanencia", "length of stay",
"morte", "indicador_obito", "death_indicator", "indica obito", "indicates death",
"nacional", "codigo_nacionalidade", "nationality_code", "codigo da nacionalidade do paciente", "patient's nationality code",
"num_proc", "numero_procedimento", "procedure_number", "zerado", "zeroed",
"car_int", "carater_internacao", "admission_type", "carater da internacao", "admission type",
"tot_pt_sp", "total_pt_sp", "total_pt_sp", "zerado", "zeroed",

"cpf_aut", "cpf_autorizador", "authorizer_cpf", "zerado", "zeroed",
"homonimo", "indicador_homonimo", "homonym_indicator", "indicador se o paciente da aih e homonimo do paciente de outra aih", "indicator if the aih patient is a homonym of another aih patient",
"num_filhos", "numero_filhos", "number_of_children", "numero de filhos do paciente", "number of patient's children",
"instru", "grau_instrucao", "education_level", "grau de instrucao do paciente", "patient's education level",
"cid_notif", "cid_notificacao_obrigatoria", "mandatory_reporting_cid", "cid de doenca de notificacao obrigatoria", "cid of mandatory notifiable disease",
"complex", "complexidade_procedimento", "procedure_complexity", "complexidade do procedimento", "procedure complexity",
"financ", "tipo_financiamento", "funding_type", "tipo de financiamento", "type of funding",
"faec_tp", "faec_tipo", "faec_type", "tipo de procedimento faec", "type of faec procedure",

"regct", "regiao_contratualizacao", "contract_region", "regiao de contratualizacao", "contracting region",
"munic_pac", "municipio_paciente", "patient_municipality", "municipio de residencia do paciente", "patient's residence municipality",
"estbl_pac", "estabelecimento_paciente", "patient_establishment", "codigo do estabelecimento do paciente", "patient's establishment code",
"codibge", "codigo_ibge_municipio_residencia", "ibge_code_residence_municipality", "codigo ibge do municipio de residencia do paciente", "ibge code of patient's residence municipality",
"val_spa", "valor_spa", "spa_value", "zerado", "zeroed",
"val_sh_fed", "valor_sh_federal", "federal_sh_value", "zerado", "zeroed",
"val_sp_fed", "valor_sp_federal", "federal_sp_value", "zerado", "zeroed",
"val_sh_ges", "valor_sh_gestor", "manager_sh_value", "zerado", "zeroed",
"val_sp_ges", "valor_sp_gestor", "manager_sp_value", "zerado", "zeroed",
"val_sh_uni", "valor_sh_unidade", "unit_sh_value", "zerado", "zeroed",
"val_sp_uni", "valor_sp_unidade", "unit_sp_value", "zerado", "zeroed",
"IDADE",      "idade_fake",  "age_fake",    "Idade falsa",     "Fake age",
"SEXO",       "sexo_fake",   "sex_fake",    "Sexo falso",      "Fake sex",
"MUNIC_RES",  "mun_fake",    "mun_fake",    "Município fake",  "Fake city",
"UF_RES",     "uf_fake",     "uf_fake",     "UF fake",         "Fake UF",
"RACA_COR",   "raca_fake",   "race_fake",   "Raça fake",       "Fake race",

