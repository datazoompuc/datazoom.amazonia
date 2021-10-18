#' @title Greenhouse gases emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases
#'
#' @param dataset A dataset name ("seeg_farming", "seeg_industry", "seeg_energy", "seeg_land" and "seeg_residuals").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download farming state raw data
#' seeg <- load_seeg(dataset = "seeg_farming",
#'                   raw_data = TRUE,
#'                   geo_level = "state")
#'
#' # download energy country treated data
#' seeg = load_seeg(dataset = "seeg_energy",
#'                  raw_data = FALSE,
#'                  geo_level = "country)
#'
#' #  download residuals municipality treated data
#' seeg = load_seeg(dataset = "seeg_residuals",
#'                  raw_data = FALSE,
#'                  geo_level = "municipality)
#' }

load_seeg <- function(dataset = "seeg", raw_data,
                      geo_level, language = "eng"){


  survey <- link <- id_code <- x1970 <- x2019 <- nivel_1_setor <- nivel_2 <- nivel_3 <- nivel_4 <- nivel_5 <- nivel_6 <- produto <- atividade_economica <- Valor <- Ano <- estado <- setor <- processos_geradores_emissoes <- fonte_de_emissoes <- emissores <- gas <- emissao_remocao_bunker <- producao_emissores <- categorias_emissao <- atividade_geradora <- categorias_processos_geradores <- year <- state <- sector <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$language = language
  param$raw_data = raw_data

  if (!is.numeric(param$dataset)){
    param$code = datasets_seeg() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(id_code) %>%
      unlist() %>%
      as.numeric()
  } else {param$code = param$dataset}


  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}


  ##############
  ## Download ##
  ##############

  if (param$geo_level == "municipality"){message("Please follow the steps from `googledrive` package to download the data. This may take a while.")}

  dat <- external_download(dataset = param$dataset,
                           source = 'seeg',
                           geo_level = param$geo_level)

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})


  ## Return Raw Data
  if (raw_data == TRUE){return(dat)}

if (param$dataset == "seeg_farming"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Agropecuaria") %>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           tipo_emissao = nivel_4,
           emissores_diretos_e_indiretos = nivel_3,
           fonte_de_emissoes = nivel_5,
           emissores = nivel_6)%>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "PEC" ~ "Pecuaria",
                                           atividade_economica == "AGR" ~ "Agricultura"))%>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "CAR" ~ "Carne",
                               produto == "CAR/LEI" ~ "Carne/Leite",
                               produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carne/Leite/Alimentos/Bebidas",
                               produto == "LEI" ~ "Leite"))

  dat = dat%>%
    dplyr::relocate(Ano, estado, setor, processos_geradores_emissoes, fonte_de_emissoes, emissores, gas, atividade_economica, produto,
             Valor)


}

if (param$dataset == "seeg_industry"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Processos Industriais") %>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           producao_emissores = nivel_3,
           emissores = nivel_4,
           emissao_bunker = emissao_remocao_bunker)%>%
  dplyr::select(-c(nivel_6, nivel_5)) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "CIM" ~ "Industria Cimenteira",
                                           atividade_economica == "ENE_ELET" ~ "Industria Energia Eletrica",
                                           atividade_economica == "MET" ~ "Industria Metaleira",
                                           atividade_economica == "Outra_IND" ~ "Outra Industria",
                                           atividade_economica == "HFC" ~ "HFC"))%>%
    dplyr::mutate(estado = dplyr::case_when(estado== "BA" ~ "BA",
                                            estado == "ES"~"ES",
                                            estado == "MA"~ "MA",
                                            estado == "NA" ~ as.character(NA),
                                            estado == "PA" ~ "PA",
                                            estado == "RJ" ~ "RJ",
                                            estado == "RS" ~ "RS",
                                            estado == "SP" ~ "SP"))%>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALU" ~ "Aluminio",
                                             produto == "ACO"~ "Aco"))
  dat = dat %>%
    dplyr::relocate(Ano, estado, setor, processos_geradores_emissoes, producao_emissores, emissores, gas, atividade_economica, produto, Valor)


}

if (param$dataset == "seeg_energy"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Energia")%>%
    dplyr::rename(setor = nivel_1_setor, tipo_emissao = nivel_2,
           processos_geradores_emissoes = nivel_3,
           atividade_geradora = nivel_4,
           fonte_energetica = nivel_5,
           emissores = nivel_6)%>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "ENE_ELET" ~ "Energia Eletrica",
                               produto == "ALU" ~ "Aluminio")) %>%
      dplyr::select(-atividade_economica)

  dat = dat%>%
    dplyr::relocate(Ano, estado, setor)

}

if (param$dataset == "seeg_land"){
  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Mudanca de Uso da Terra e Floresta") %>%
    dplyr::filter(!is.na(Valor)) %>%
    dplyr::rename(setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
           bioma = nivel_3,
           area_bioma = nivel_4,
           tipo_atividade_geradora = nivel_5,
           local_atividade_geradora = nivel_6) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "AGROPEC" ~ "Agropecuaria",
                                                         atividade_economica == "Conservacao" ~ "Conservacao")) %>%
    dplyr::select(-produto)

  dat = dat %>%
    dplyr::relocate(Ano, estado, setor, processos_geradores_emissoes, atividade_economica)


}

if (param$dataset == "seeg_residuals"){

  ## Create Longer Data - Years as a Variable

  dat = dat %>%
    tidyr::pivot_longer(
      cols = x1970:x2019,
      names_to = 'Ano',
      names_prefix = 'x',
      values_to = 'Valor'
    )
  ## Changing column name, filtering by the specific sector and harmonizing variables
  dat = dat %>%
    dplyr::filter(nivel_1_setor == "Residuos") %>%
    dplyr::rename(setor = nivel_1_setor, categorias_emissao = nivel_2,
           processos_geradores_emissoes = nivel_3,
           atividade_geradora = nivel_4,
           categorias_processos_geradores = nivel_5,
           emissao_bunker = emissao_remocao_bunker) %>%
    dplyr::select(-nivel_6) %>%
    dplyr::mutate(atividade_economica = dplyr::case_when(atividade_economica == "PEC" ~ "Pecuaria",
                                           atividade_economica == "Outra_IND" ~ "Outra Industria",
                                           atividade_economica == "SANEAMENTO" ~ "Saneamento"
                                           )) %>%
    dplyr::mutate(produto = dplyr::case_when(produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
                               produto == "LEI" ~ "Leite",
                               produto == "CAR" ~ "Carne",
                               produto == "CAR/LEI" ~ "Carne/Leite",
                               produto == "ALU" ~ "Aluminio",
                               produto == "ENE_ELET" ~ "Energia Eletrica",
                               produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carne/Leite/Alimentos/Bebidas")) %>%
    dplyr::mutate(estado = dplyr::case_when(estado == "BA" ~ "BA",
                                            estado == "ES"~"ES",
                                            estado == "MA"~ "MA",
                                            estado == "NA" ~ as.character(NA),
                                            estado == "PA" ~ "PA",
                                            estado == "RJ" ~ "RJ",
                                            estado == "RS" ~ "RS",
                                            estado == "SP" ~ "SP",
                                            estado == "AC" ~ "AC",
                                            estado == "AL" ~ "AL",
                                            estado == "AP" ~ "AP",
                                            estado == "CE" ~ "CE",
                                            estado == "DF" ~ "DF",
                                            estado == "ES" ~ "ES",
                                            estado == "GO" ~ "GO",
                                            estado == "MG" ~ "MG",
                                            estado == "MS" ~ "MS",
                                            estado == "MT" ~ "MT",
                                            estado == "PB" ~ "PB",
                                            estado == "PE" ~ "PE",
                                            estado == "PI" ~ "PI",
                                            estado == "PR" ~ "PR",
                                            estado == "RN" ~ "RN",
                                            estado == "RO" ~ "RO",
                                            estado == "RR" ~ "RR",
                                            estado == "SC" ~ "SC",
                                            estado == "SE" ~ "SE",
                                            estado == "TO" ~ "TO",
                                            estado == "AM" ~ "AM"))

  dat = dat%>%
    dplyr::relocate(Ano, estado, setor, categorias_emissao, processos_geradores_emissoes, atividade_geradora, categorias_processos_geradores, atividade_economica, produto, Valor)
}

  if(param$dataset == "seeg_industry" & language == "eng"){

    dat = readxl::read_excel('./inst/extdata/industry_seeg.xlsx')

  }

  if(param$dataset == "seeg_farming" & language == "eng"){

    dat = readxl::read_excel('./inst/extdata/farming_seeg.xlsx')
  }

  if(param$dataset == "seeg_land" & language == "eng"){

    dat = readxl::read_excel('./inst/extdata/land_seeg.xlsx')
  }


  if(param$dataset == "seeg_residuals" & language == "eng"){

    dat = readxl::read_excel('./inst/extdata/residuals_seeg.xlsx')
  }

  if(param$dataset == "seeg_energy" & language == "eng"){

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2019,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value')

    dat = dat %>%
      dplyr::filter(nivel_1_setor == "Energia")%>%
      dplyr::rename(sector = nivel_1_setor, state = estado,
                    emission_type = nivel_2,
                    emissions_generating_processes = nivel_3,
                    generating_activity = nivel_4,
                    energetic_source = nivel_5,
                    emitters = nivel_6,
                    emission_bunker = emissao_remocao_bunker,
                    product = produto)%>%
      dplyr::mutate(product = dplyr::case_when(product == "ALIM_BEBIDAS" ~ "Food/Beverages",
                                               product == "ENE_ELET" ~ "Eletric energy",
                                               product == "ALU" ~ "Aluminum")) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Energia" ~ "Energy")) %>%
      dplyr::mutate(emission_type = dplyr::case_when(emission_type == "Emissoes Fugitivas" ~ "Fugitive emissions",
                                                    emission_type == "Emissoes pela Queima de Combustiveis" ~ "Emissions from fuel burning")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(emissions_generating_processes == "Agropecuario" ~ "Farming",
                                                                      emissions_generating_processes == "Geracao de Eletricidade (Servico Publico)" ~ "Eletricity generation (public service)",
                                                                      emissions_generating_processes == "Nao Identificado" ~ "Not Identified",
                                                                      emissions_generating_processes == "Publico" ~ "Public",
                                                                      emissions_generating_processes == "Transportes" ~ "Transports",
                                                                      emissions_generating_processes == "Comercial" ~ "Business",
                                                                      emissions_generating_processes == "Industrial" ~ "Industrial",
                                                                      emissions_generating_processes == "Producao de Combustiveis" ~ "Fuel production",
                                                                      emissions_generating_processes == "Residencial" ~ "Residential")) %>%
      dplyr::mutate(generating_activity = dplyr::case_when(generating_activity == "Aereo" ~ "Air",
                                                           generating_activity == "Cimento" ~ "Cement",
                                                           generating_activity == "Ferro Ligas" ~ "Iron garters",
                                                           generating_activity == "Mineracao e pelotizacao" ~ "Mining and pelletizing",
                                                           generating_activity == "Outras industrias" ~ "Other industries",
                                                           generating_activity == "Producao de carvao mineral e outros" ~ " Mineral coal production and others",
                                                           generating_activity == "Refino de petroleo" ~ "Oil refining",
                                                           generating_activity == "Transporte de gas natural" ~ "Natural gas transport",
                                                           generating_activity == "Alimentos e bebidas" ~ "Food and beverages",
                                                           generating_activity == "Exploracao de petroleo e gas natural" ~ "Oil and natural gas exploration",
                                                           generating_activity == "Ferroviario" ~ "Railroad",
                                                           generating_activity == "Nao ferrosos e outros da metalurgia" ~ "Non-ferrous and other metallurgy",
                                                           generating_activity == "Papel e celulose" ~ "Paper and Cellulose",
                                                           generating_activity == "Producao de carvao vegetal" ~ "Charcoal production",
                                                           generating_activity == "Rodoviario" ~ "Road",
                                                           generating_activity == "Ceramica" ~ "Ceramics",
                                                           generating_activity == "Ferro gusa e aco" ~ "Pig iron and steel",
                                                           generating_activity == "Hidroviario" ~ "Waterway",
                                                           generating_activity == "NAO SE APLICA" ~ "Not Applicable",
                                                           generating_activity == "Producao de Alcool" ~ "Alcohol production",
                                                           generating_activity == "Quimica" ~ "Chemical",
                                                           generating_activity == "Textil" ~ "Textile")) %>%
      dplyr::mutate(energetic_source = dplyr::case_when(energetic_source == "Alcatrao" ~ "Tar",
                                                        energetic_source == "Biogas" ~ "Biogas",
                                                        energetic_source == "Carvao vapor 3300" ~ "Steam coal 3300",
                                                        energetic_source == "Carvao vapor 4500" ~ "Steam coal 4500",
                                                        energetic_source == "Carvao vapor 5900" ~ "Steam coal 5900",
                                                        energetic_source == "Carvao vegetal" ~ "Charcoal",
                                                        energetic_source == "Diesel de petroleo" ~ "Petroleum diesel",
                                                        energetic_source == "Gas de coqueria" ~ "Coke oven gas",
                                                        energetic_source == "Gas natural seco" ~ "Dry natural gas",
                                                        energetic_source == "Gasolina C" ~ "Gasoline C",
                                                        energetic_source == "Lenha" ~ "Firewood",
                                                        energetic_source == "Nafta" ~ "Naphta",
                                                        energetic_source == "Outras biomassas" ~ "Other biomasses",
                                                        energetic_source == "Petroleo" ~ "Oil",
                                                        energetic_source == "Querosene iluminante" ~ "Illuminating kerosene",
                                                        energetic_source == "Petroleo e gas natural" ~ "Oil and natural gas",
                                                        energetic_source == "Outras nao renovaveis" ~ "Other non-renewable",
                                                        energetic_source == "Oleo combustivel" ~ "Fuel oil",
                                                        energetic_source == "Lenha carvoejamento" ~ "Charcoal firewood",
                                                        energetic_source == "Gasolina de aviacao" ~ "Aviation gasoline",
                                                        energetic_source == "Gas natural umido" ~ "Umid natural gas",
                                                        energetic_source == "Gas de refinaria" ~ "Refinery gas",
                                                        energetic_source == "Gas canalizado RJ" ~ "Piped gas RJ",
                                                        energetic_source == "Coque de carvao mineral" ~ "Coal coke",
                                                        energetic_source == "Carvao vapor 6000" ~ "Steam coal 6000",
                                                        energetic_source == "Carvao vapor 4700" ~ "Steam coal 4700",
                                                        energetic_source == "Carvao vapor 3700" ~ "Steam coal 3700",
                                                        energetic_source == "Carvao mineral" ~ "Mineral coal",
                                                        energetic_source == "Alcool hidratado" ~ "Hydrated alcohol",
                                                        energetic_source == "Bagaco de cana" ~ "Sugarcane bagasse",
                                                        energetic_source == "Carvao vapor 3100" ~ "Steam coal 3100",
                                                        energetic_source == "Carvao vapor 4200" ~ "Steam coal 4200",
                                                        energetic_source == "Carvao vapor 5200" ~ "Steam coal 5200",
                                                        energetic_source == "Carvao vapor sem especificacao" ~ "Steam coal without specification",
                                                        energetic_source == "Coque de petroleo" ~ "Oil coke",
                                                        energetic_source == "Gas canalizadp SP" ~ "Piped gas SP",
                                                        energetic_source == "Gas natural" ~ "Natural gas",
                                                        energetic_source == "Gasolina autmotiva" ~ "Automotive gasoline",
                                                        energetic_source == "GLP" ~ "Liquefied oil gas",
                                                        energetic_source == "Lixivia" ~ "Bleach",
                                                        energetic_source == "Oleo diesel" ~ "Diesel oil",
                                                        energetic_source == "Outros energeticos de petroleo" ~ "Other petroleum energy",
                                                        energetic_source == "Querosene de aviacao" ~ "Aviation kerosene")) %>%
      dplyr::mutate(emitters = dplyr::case_when(emitters == "Aeronaves" ~ "Airplanes",
                                         emitters == "Carvoarias" ~ "Charcoals",
                                         emitters == "Comerciais Leves" ~ "Light commercials",
                                         emitters == "Locomotivas" ~ "Locomotives",
                                         emitters == "Onibus" ~ "Bus",
                                         emitters == "Automoveis" ~ "Automobiles",
                                         emitters == "Centrais Eletricas Autoprodutoras" ~ "Self-producing power plants",
                                         emitters == "Consumo Final Energetico" ~ "Final energy consumption",
                                         emitters == "Motocicletas" ~ "Motorbikes",
                                         emitters == "Caminhoes" ~ "Trucks",
                                         emitters == "Centrais Eletricas de Servico Publico" ~ "Public service power plants",
                                         emitters == "Embarcacoes" ~ "Vessels",
                                         emitters == "NAO SE APLICA" ~ "Not Applicable")) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(emission_bunker == "Emissao" ~ "Emission",
                                                       emission_bunker == "Bunker" ~ "Bunker")) %>%

dplyr::select(-atividade_economica)

    dat = dat %>%
      dplyr::relocate(year, state, sector)
  }

  return(dat)
}


