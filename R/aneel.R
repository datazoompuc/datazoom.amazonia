#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("cde_budget_expenses")
#' @param language Only available in Portuguese ("pt") as of now
#' @param raw_data Only treated data (\code{False}) is available
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016 (takes a long time to download)
#' clean_aneel <- load_aneel(
#'   raw_data = FALSE,
#'   time_period = 2016
#' )
#' }
#'
#' @export

load_aneel <- function(dataset, raw_data = FALSE, time_period = 2013:2022,
                       language = "pt") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  ano <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$time_period <- time_period

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    source = "ANEEL",
    dataset = param$dataset,
    year = param$time_period
  )

  ######################
  ## Data Engineering ##
  ######################

  if (param$raw_data) {
    return(dat)
  }

    if (param$dataset == "cde_budget_expenses"){

       dat <- dat %>%
       janitor::clean_names() %>%
        dplyr::filter(ano %in% param$time_period)


            ################################
            ## Harmonizing Variable Names ##
            ################################


       if (param$language == "eng"){

         dat <- dat %>%
           dplyr::rename(
             "year" = "ano",
             "type_of_expenses" = "tipo_de_despesa",
             "value" = "soma_de_valor",
             "share_of_total_amount" = "participacao"
           )
       }

       if (param$language == "pt"){

         dat <- dat %>%
           dplyr::rename(
             "ano" = "ano",
             "tipo_de_despesa" = "tipo_de_despesa",
             "total" = "soma_de_valor",
             "participacao_no_total" = "participacao"
           )
       }
    }

    if (param$dataset == "distributed_generation_ventures"){
        dat <- NULL
    }

    if (param$dataset == "aneel_generation_information_system"){

      ###############################
      ## Harmonizing variable name ##
      ###############################

      dat <- dat %>%
        janitor::clean_names() %>%
        dplyr::mutate_if(is.character, function(var) {
          stringi::stri_trans_general(str = var, id = "Latin-ASCII")}) %>%
          dplyr::rename("municipios" = "municipio_s",
                        "potencia_outorgada_kw" = "potencia_outorgada_k_w",
                        "potencia_fiscalizada_kw" = "potencia_fiscalizada_kw",
                        "garantia_fisica_k_w" = "garantia_fisica_kw",
                        "proprietario_ou_regime_de_exploracao" = "proprietario_regime_de_exploracao") %>%
          dplyr::mutate(fonte = dplyr::case_when(fonte == "CGH" ~ "CGH - Central geradora hidreletrica",
                                                  fonte == "CGU" ~ "CGU - Central geradora undi-eletrica",
                                                  fonte == "EOL" ~ "EOL - Central geradora eolica",
                                                  fonte == "PCH" ~ "PCH - Pequena central hidreletrica",
                                                  fonte == "UFV" ~ "UFV - Central geradora solar fotovoltaica",
                                                  fonte == "UHE" ~ "UHE - Usina hidreletrica",
                                                  fonte == "UTE" ~ "UTE - Usina termeletrica",
                                                  fonte == "UTN" ~ "UTN - Usina termonuclear"))

      if(param$language == "eng"){
        dat <- dat %>%
          dplyr::rename("venture" = "empreendimento",
                 "ceg" = "ceg",
                 "uf" = "uf",
                 "source" = "fonte",
                 "phase" = "fase",
                 "origin" = "origem",
                 "type" = "tipo",
                 "type_of_action" = "tipo_de_atuacao", #maybe type_of_permission is more appropriate
                 "final_fuel" = "combustivel_final",
                 "operation_start" = "entrada_em_operacao",
                 "granted_potency_kw" = "potencia_outorgada_k_w",
                 "fiscalized_potency_kw" = "potencia_fiscalizada_k_w",
                 "fisical_guarantee_kw" = "garantia_fisica_k_w",
                 "qualified_generation" = "geracao_qualificada",
                 "decimal_latitude" = "latitude_decimal",
                 "decimal_longitude" = "longitude_decimal",
                 "validity_start" = "inicio_vigencia",
                 "validity_end" = "fim_vigencia",
                 "proprietary_or_exploration_regime" = "proprietario_regime_de_exploracao",
                 "sub_basin" = "sub_bacia",
                 "municipalities" = "municipio_s") %>%
          dplyr::mutate(source = dplyr::case_when( source == "CGH" ~ "CGH - Hydroelectric generator center",
                                                   source == "CGU" ~ "CGU - Undi-elctric generator center",
                                                   source == "EOL" ~ "EOL - Eolic generator center",
                                                   source == "PCH" ~ "PCH - Small hydroelectric center",
                                                   source == "UFV" ~ "UFV - Photovoltaic solar generator center",
                                                   source == "UHE" ~ "UHE - Hydroelectric power plant",
                                                   source == "UTE" ~ "UTE - Thermoelectric power plant",
                                                   source == "UTN" ~ "UTN - Thermonuclear power plant"),
                        phase = dplyr::case_when( phase == "Operacao" ~ "Operation",
                                                  phase == "Construcao nao iniciada" ~ "Construction not initiated",
                                                  phase == "Construcao" ~ "Construction"),
                        origin = dplyr::case_when( origin == "Solar" ~ "Solar",
                                                   origin == "Hidrica" ~ "Hydric",
                                                   origin == "Fossil" ~ "Fossil",
                                                   origin == "Biomassa" ~ "Biomass",
                                                   origin == "Eolica" ~ "Eolic",
                                                   origin == "Nuclear" ~ "Nuclear"),
                        type = dplyr::case_when( type == "Radiacao solar" ~ "Solar radiation",
                                                 type == "Potencial hidraulico" ~ "Hydraulic potential",
                                                 type == "Petroleo" ~ "Petroleum",
                                                 type == "Floresta" ~ "Forest",
                                                 type == "Cinetica do vento" ~ "Wind kinetcs",
                                                 type == "Carvao mineral" ~ "Mineral coal",
                                                 type == "Agroindustriais" ~ "Agroindustrial",
                                                 type == "Gas natural" ~ "Natural gas",
                                                 type == "Residuos animais" ~ "Animal waste",
                                                 type == "Uranio" ~ "Uranium",
                                                 type == "Residuos solidos urbanos" ~ "Urban solid waste",
                                                 type == "Biocombustiveis liquidos" ~ "Liquid biofuels",
                                                 type == "Outros Fosseis" ~ "Other Fossils"),
                        type_of_action = dplyr::case_when( type_of_action == "Registro" ~ "Registration",
                                                           type_of_action == "Concessao" ~ "Concession",
                                                           type_of_action == "Autorizacao" ~ "Authorization"),
                        final_fuel = dplyr::case_when( final_fuel == "Radiacao solar" ~ "Solar Radiation",
                                                       final_fuel == "Potencial hidraulico" ~ "Hydraulic Potential",
                                                       final_fuel == "Oleo Diesel" ~ "Diesel Oil",
                                                       final_fuel == "Residuos Florestais" ~ "Forest Residues",
                                                       final_fuel == "Cinetica do vento" ~ "Wind Kinetics",
                                                       final_fuel == "Gas de Alto Forno - CM" ~ "Blast Furnace Gas - CM",
                                                       final_fuel == "Biogas-AGR" ~ "Biogas-AGR",
                                                       final_fuel == "Gas Natural" ~ "Natural Gas",
                                                       final_fuel == "Biogas - RA" ~ "Biogas - RA",
                                                       final_fuel == "Lenha" ~ "Firewood",
                                                       final_fuel == "Uranio" ~ "Uranium" ,
                                                       final_fuel == "Carvao Mineral" ~ "Mineral Coal",
                                                       final_fuel == "Biogas - RU" ~ "Biogas - RU",
                                                       final_fuel == "Oleo Combustivel" ~ "Fuel Oil",
                                                       final_fuel == "Licor Negro" ~ "Black Liquor",
                                                       final_fuel == "Casca de Arroz" ~ "Rice Husk",
                                                       final_fuel == "Carvao Vegetal" ~ "Charcoal",
                                                       final_fuel == "Oleos vegetais" ~ "Vegetable Oils",
                                                       final_fuel == "Capim Elefante" ~ "Elephant Grass",
                                                       final_fuel == "Residuos Solidos Urbanos - RU" ~ "Urban Solid Waste - RU",
                                                       final_fuel == "Outros Energeticos de Petroleo" ~ "Other Petroleum Energetics",
                                                       final_fuel == "Gas de Alto Forno - Biomassa" ~ "Blast Furnace Gas - Biomass",
                                                       final_fuel == "Gas de Refinaria" ~ "Refinery Gas",
                                                       final_fuel == "Calor de Processo - OF" ~ "Process Heat - OF",
                                                       final_fuel == "Carvao - RU" ~ "Coal - RU",
                                                       final_fuel == "Calor de Processo - GN" ~ "Process Heat - GN",
                                                       final_fuel == "Calor de Processo - CM" ~ "Process Heat - CM",
                                                       final_fuel == "Biogas - Floresta" ~ "Biogas - Forest",
                                                       final_fuel == "Gas de Alto Forno - PE" ~ "Blast Furnace Gas - PE",
                                                       final_fuel == "Etanol" ~ "Ethanol"))
        dat <- dat %>%
                dplyr::mutate(proprietary_or_exploration_regime = dplyr::case_when(stringr::str_detect(dat$proprietary_or_exploration_regime,
                                                                                                       pattern = "% para") ~
                                                                                      stringr::str_replace_all(dat$proprietary_or_exploration_regime,
                                                                                                                pattern = "% para",
                                                                                                                replacement = "% for"),
                                                                                   dat$proprietary_or_exploration_regime == "Nao Informado" ~
                                                                                    "Not Informed"),
                              sub_basin = dplyr::case_when(stringr::str_detect(dat$sub_basin,
                                                                                pattern = "e outros") ~
                                                            stringr::str_replace_all(dat$sub_basin,
                                                                                     pattern = "e outros",
                                                                                     replacement = "and others"),
                                                           TRUE ~
                                                            dat$sub_basin))


      }

      return(dat)
    }



  return(dat)
}
