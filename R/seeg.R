#' @title Greenhouse gas emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases
#'
#' @param dataset A dataset name ("seeg", seeg_farming", "seeg_industry", "seeg_energy", "seeg_land", "seeg_residuals"). On which "seeg" contains all five sectors (only works with raw_data = TRUE) and the others are filtered specifically by a main source of emission.
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#' @export
#'
#' @examples \dontrun{
#' # Download raw data (raw_data = TRUE) of greenhouse gases (dataset = "seeg")
#' # by state (geo_level = "state")
#' data <- load_seeg(
#'   dataset = "seeg",
#'   raw_data = TRUE,
#'   geo_level = "state"
#' )
#'
#' # Download treated data (raw_data = FALSE) of industry greenhouse gases (dataset = "seeg_industry")
#' data <- load_seeg(
#'   dataset = "seeg_industry",
#'   raw_data = FALSE,
#'   geo_level = "state"
#' )
#' }
load_seeg <- function(dataset, raw_data = FALSE,
                      geo_level, language = "eng") {


  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- ibge <- x2000 <- x2018 <- id_code <- tipo_de_emissao <- NULL
  city <- state <- nivel_1 <- municipio <- territorio <- x1970 <- x2021 <- NULL
  nivel_1_setor <- nivel_2 <- nivel_3 <- nivel_4 <- nivel_5 <- nivel_6 <- NULL
  produto <- atividade_economica <- Valor <- Ano <- estado <- setor <- NULL
  processos_geradores_emissoes <- fonte_de_emissoes <- emissores <- gas <- NULL
  emissao_remocao_bunker <- producao_emissores <- categorias_emissao <- NULL
  atividade_geradora <- categorias_processos_geradores <- year <- state <- NULL
  sector <- emitters_production <- emitters <- economic_activity <- product <- NULL
  value <- emissions_category <- activity <- generating_processes_categories <- NULL
  biome <- biome_area <- transition_type <- emission_removal_bunker <- NULL
  emissions_sources <- emissions_type <- emissions_generating_processes <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  if (!is.numeric(param$dataset)) {
    param$code <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(link) %>%
      unlist()
  } else {
    param$code <- param$dataset
  }

  ## Dataset

  if (param$dataset == "seeg" & param$raw_data == FALSE) {
    stop("This dataset only works with raw_data = TRUE")
  }
  if (param$dataset == "seeg_farming" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_energy" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_industry" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_land" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_residuals" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }

  # Picking which sheet to download

  sheet_list <- c(
    "country" = "GEE Brasil",
    "state" = "GEE Estados",
    "municipality" = "BD GEE Municipios GWP-AR5"
  )

  sheet <- param$geo_level %>%
    dplyr::recode(!!!sheet_list)

  ##############
  ## Download ##
  ##############

  dat <- external_download(
    dataset = param$dataset,
    source = "seeg",
    geo_level = param$geo_level,
    sheet = sheet
  )


  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })


  ## Return Raw Data

  if (param$dataset == "seeg" & param$raw_data) {
    return(dat)
  }


  ## Raw Data = FALSE

  if (param$dataset == "seeg_farming" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Agropecuaria") %>%
      dplyr::rename(
        setor = nivel_1, processos_geradores_emissoes = nivel_2,
        tipo_emissao = nivel_4,
        emissores_diretos_e_indiretos = nivel_3,
        fonte_de_emissoes = nivel_5,
        emissores = nivel_6,
        emissao_remocao = tipo_de_emissao
      ) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "PEC" ~ "Pecuaria",
        atividade_economica == "AGR" ~ "Agricultura"
      )) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
        produto == "CAR" ~ "Carne",
        produto == "CAR/LEI" ~ "Carne/Leite",
        produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carne/Leite/Alimentos/Bebidas",
        produto == "LEI" ~ "Leite"
      ))
    dat <- dat %>%
      dplyr::relocate(
        Ano, municipio, territorio, ibge, setor, processos_geradores_emissoes, fonte_de_emissoes, emissores, gas, atividade_economica, produto,
        Valor
      )

    dat <- dat %>%
      dplyr::mutate(municipio = dplyr::case_when(
        municipio == "NA" ~ NA_character_,
        TRUE ~ municipio
      )) %>%
      dplyr::mutate(territorio = dplyr::case_when(
        territorio == "NA" ~ NA_character_,
        TRUE ~ territorio
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }


  if (param$dataset == "seeg_farming" & param$geo_level %in% c("country", "state") & param$language == "pt") {

    ## Create Longer Data - Years as a Variable

    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Agropecuaria") %>%
      dplyr::rename(
        setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
        tipo_emissao = nivel_4,
        emissores_diretos_e_indiretos = nivel_3,
        fonte_de_emissoes = nivel_5,
        emissores = nivel_6
      ) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "PEC" ~ "Pecuaria",
        atividade_economica == "AGR" ~ "Agricultura"
      )) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
        produto == "CAR" ~ "Carne",
        produto == "CAR/LEI" ~ "Carne/Leite",
        produto == "CAR/LEI/ALIM_BEBIDAS" ~ "Carne/Leite/Alimentos/Bebidas",
        produto == "LEI" ~ "Leite"
      ))

    dat <- dat %>%
      dplyr::relocate(
        Ano, estado, setor, processos_geradores_emissoes, fonte_de_emissoes, emissores, gas, atividade_economica, produto,
        Valor
      )
    dat <- dat %>%
      dplyr::mutate(estado = dplyr::case_when(
        estado == "NA" ~ NA_character_,
        TRUE ~ estado
      ))
  }


  if (param$dataset == "seeg_industry" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Processos Industriais") %>%
      dplyr::rename(
        setor = nivel_1, processos_geradores_emissoes = nivel_2,
        producao_emissores = nivel_3,
        emissores = nivel_4,
        emissao = tipo_de_emissao
      ) %>%
      dplyr::select(-c(nivel_6, nivel_5)) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "CIM" ~ "Industria Cimenteira",
        atividade_economica == "ENE_ELET" ~ "Industria Energia Eletrica",
        atividade_economica == "MET" ~ "Industria Metaleira",
        atividade_economica == "Outra_IND" ~ "Outra Industria",
        atividade_economica == "HFC" ~ "HFC"
      ))

    dat <- dat %>%
      dplyr::relocate(Ano, municipio, territorio, setor, processos_geradores_emissoes, producao_emissores, emissores, gas, atividade_economica, produto, Valor)

    dat <- dat %>%
      dplyr::mutate(municipio = dplyr::case_when(
        municipio == "NA" ~ NA_character_,
        TRUE ~ municipio
      )) %>%
      dplyr::mutate(territorio = dplyr::case_when(
        territorio == "NA" ~ NA_character_,
        TRUE ~ territorio
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }


  if (param$dataset == "seeg_industry" & param$geo_level %in% c("country", "state") & param$language == "pt") {

    ## Create Longer Data - Years as a Variable

    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Processos Industriais") %>%
      dplyr::rename(
        setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
        producao_emissores = nivel_3,
        emissores = nivel_4,
        emissao_bunker = emissao_remocao_bunker
      ) %>%
      dplyr::select(-c(nivel_6, nivel_5)) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "CIM" ~ "Industria Cimenteira",
        atividade_economica == "ENE_ELET" ~ "Industria Energia Eletrica",
        atividade_economica == "MET" ~ "Industria Metaleira",
        atividade_economica == "Outra_IND" ~ "Outra Industria",
        atividade_economica == "HFC" ~ "HFC"
      )) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALU" ~ "Aluminio",
        produto == "ACO" ~ "Aco"
      ))
    dat <- dat %>%
      dplyr::relocate(Ano, estado, setor, processos_geradores_emissoes, producao_emissores, emissores, gas, atividade_economica, produto, Valor)

    dat <- dat %>%
      dplyr::mutate(estado = dplyr::case_when(
        estado == "NA" ~ NA_character_,
        TRUE ~ estado
      ))
  }

  if (param$dataset == "seeg_energy" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Energia") %>%
      dplyr::rename(
        setor = nivel_1, tipo_emissao = nivel_2,
        processos_geradores_emissoes = nivel_3,
        atividade_geradora = nivel_4,
        fonte_energetica = nivel_5,
        emissores = nivel_6,
        emission_bunker = tipo_de_emissao
      ) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
        produto == "ENE_ELET" ~ "Energia Eletrica",
        produto == "ALU" ~ "Aluminio"
      )) %>%
      dplyr::select(-atividade_economica)

    dat <- dat %>%
      dplyr::relocate(Ano, municipio, territorio, setor)

    dat <- dat %>%
      dplyr::mutate(municipio = dplyr::case_when(
        municipio == "NA" ~ NA_character_,
        TRUE ~ municipio
      )) %>%
      dplyr::mutate(territorio = dplyr::case_when(
        territorio == "NA" ~ NA_character_,
        TRUE ~ territorio
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }


  if (param$dataset == "seeg_energy" & param$geo_level %in% c("country", "state") & param$language == "pt") {

    ## Create Longer Data - Years as a Variable

    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Energia") %>%
      dplyr::rename(
        setor = nivel_1_setor, tipo_emissao = nivel_2,
        processos_geradores_emissoes = nivel_3,
        atividade_geradora = nivel_4,
        fonte_energetica = nivel_5,
        emissores = nivel_6
      ) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
        produto == "ENE_ELET" ~ "Energia Eletrica",
        produto == "ALU" ~ "Aluminio"
      )) %>%
      dplyr::select(-atividade_economica)



    dat <- dat %>%
      dplyr::relocate(Ano, estado, setor)

    dat <- dat %>%
      dplyr::mutate(estado = dplyr::case_when(
        estado == "NA" ~ NA_character_,
        TRUE ~ estado
      ))
  }


  if (param$dataset == "seeg_land" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Mudanca de Uso da Terra e Floresta") %>%
      dplyr::filter(!is.na(Valor)) %>%
      dplyr::rename(
        setor = nivel_1, processos_geradores_emissoes = nivel_2,
        bioma = nivel_3,
        area_bioma = nivel_4,
        local_atividade_geradora = nivel_5,
        atividade_geradora = nivel_6,
        emissao_remocao = tipo_de_emissao
      ) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "AGROPEC" ~ "Agropecuaria",
        atividade_economica == "Conservacao" ~ "Conservacao"
      )) %>%
      dplyr::select(-produto)

    dat <- dat %>%
      dplyr::relocate(Ano, municipio, territorio, setor, processos_geradores_emissoes, atividade_economica)

    dat <- dat %>%
      dplyr::mutate(municipio = dplyr::case_when(
        municipio == "NA" ~ NA_character_,
        TRUE ~ municipio
      )) %>%
      dplyr::mutate(territorio = dplyr::case_when(
        territorio == "NA" ~ NA_character_,
        TRUE ~ territorio
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }

  if (param$dataset == "seeg_land" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    ## Create Longer Data - Years as a Variable

    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Mudanca de Uso da Terra e Floresta") %>%
      dplyr::filter(!is.na(Valor)) %>%
      dplyr::rename(
        setor = nivel_1_setor, processos_geradores_emissoes = nivel_2,
        bioma = nivel_3,
        area_bioma = nivel_4,
        local_atividade_geradora = nivel_5,
        atividade_geradora = nivel_6
      ) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "AGROPEC" ~ "Agropecuaria",
        atividade_economica == "Conservacao" ~ "Conservacao"
      )) %>%
      dplyr::select(-produto)

    dat <- dat %>%
      dplyr::relocate(Ano, estado, setor, processos_geradores_emissoes, atividade_economica)

    dat <- dat %>%
      dplyr::mutate(estado = dplyr::case_when(
        estado == "NA" ~ NA_character_,
        TRUE ~ estado
      ))
  }

  if (param$dataset == "seeg_residuals" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Residuos") %>%
      dplyr::rename(
        setor = nivel_1, categorias_emissao = nivel_2,
        processos_geradores_emissoes = nivel_3,
        atividade_geradora = nivel_4,
        categorias_processos_geradores = nivel_5,
        emissao_bunker = tipo_de_emissao
      ) %>%
      dplyr::select(-nivel_6) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "PEC" ~ "Pecuaria",
        atividade_economica == "Outra_IND" ~ "Outra Industria",
        atividade_economica == "SANEAMENTO" ~ "Saneamento"
      ))

    dat <- dat %>%
      dplyr::mutate(municipio = dplyr::case_when(
        municipio == "NA" ~ NA_character_,
        TRUE ~ municipio
      )) %>%
      dplyr::mutate(territorio = dplyr::case_when(
        territorio == "NA" ~ NA_character_,
        TRUE ~ territorio
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))


    dat <- dat %>%
      dplyr::relocate(Ano, municipio, territorio, ibge, setor, categorias_emissao, processos_geradores_emissoes, atividade_geradora, categorias_processos_geradores, atividade_economica, produto, Valor)
  }


  if (param$dataset == "seeg_residuals" & param$geo_level %in% c("country", "state") & param$language == "pt") {

    ## Create Longer Data - Years as a Variable

    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Residuos") %>%
      dplyr::rename(
        setor = nivel_1_setor, categorias_emissao = nivel_2,
        processos_geradores_emissoes = nivel_3,
        atividade_geradora = nivel_4,
        categorias_processos_geradores = nivel_5,
        emissao_bunker = emissao_remocao_bunker
      ) %>%
      dplyr::select(-nivel_6) %>%
      dplyr::mutate(atividade_economica = dplyr::case_when(
        atividade_economica == "PEC" ~ "Pecuaria",
        atividade_economica == "Outra_IND" ~ "Outra Industria",
        atividade_economica == "SANEAMENTO" ~ "Saneamento"
      )) %>%
      dplyr::mutate(produto = dplyr::case_when(
        produto == "ALIM_BEBIDAS" ~ "Alimentos/Bebidas",
        produto == "CAR" ~ "Carne"
      )) %>%
      dplyr::mutate(estado = dplyr::case_when(
        estado == "NA" ~ NA_character_,
        TRUE ~ estado
      ))

    dat <- dat %>%
      dplyr::relocate(Ano, estado, setor, categorias_emissao, processos_geradores_emissoes, atividade_geradora, categorias_processos_geradores, atividade_economica, produto, Valor)
  }


  if (param$dataset == "seeg_energy" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )

    dat <- dat %>%
      dplyr::filter(nivel_1 == "Energia") %>%
      dplyr::rename(
        sector = nivel_1,
        emission_type = nivel_2,
        emissions_generating_processes = nivel_3,
        activity = nivel_4,
        energetic_source = nivel_5,
        emitters = nivel_6,
        emission_bunker = tipo_de_emissao,
        product = produto,
        city = municipio,
        state = territorio
      ) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "ENE_ELET" ~ "Eletric energy",
        product == "ALU" ~ "Aluminum"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Energia" ~ "Energy")) %>%
      dplyr::mutate(emission_type = dplyr::case_when(
        emission_type == "Emissoes Fugitivas" ~ "Fugitive emissions",
        emission_type == "Emissoes pela Queima de Combustiveis" ~ "Emissions from fuel burning"
      )) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Agropecuario" ~ "Farming",
        emissions_generating_processes == "Geracao de Eletricidade (Servico Publico)" ~ "Eletricity generation (public service)",
        emissions_generating_processes == "Nao Identificado" ~ "Not Identified",
        emissions_generating_processes == "Publico" ~ "Public",
        emissions_generating_processes == "Transportes" ~ "Transports",
        emissions_generating_processes == "Comercial" ~ "Business",
        emissions_generating_processes == "Industrial" ~ "Industrial",
        emissions_generating_processes == "Producao de Combustiveis" ~ "Fuel production",
        emissions_generating_processes == "Residencial" ~ "Residential"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        activity == "Aereo" ~ "Air",
        activity == "Cimento" ~ "Cement",
        activity == "Ferro Ligas" ~ "Iron garters",
        activity == "Mineracao e pelotizacao" ~ "Mining and pelletizing",
        activity == "Outras industrias" ~ "Other industries",
        activity == "Producao de carvao mineral e outros" ~ " Mineral coal production and others",
        activity == "Refino de petroleo" ~ "Oil refining",
        activity == "Transporte de gas natural" ~ "Natural gas transport",
        activity == "Alimentos e bebidas" ~ "Food and beverages",
        activity == "Exploracao de petroleo e gas natural" ~ "Oil and natural gas exploration",
        activity == "Ferroviario" ~ "Railroad",
        activity == "Nao ferrosos e outros da metalurgia" ~ "Non-ferrous and other metallurgy",
        activity == "Papel e celulose" ~ "Paper and Cellulose",
        activity == "Producao de carvao vegetal" ~ "Charcoal production",
        activity == "Rodoviario" ~ "Road",
        activity == "Ceramica" ~ "Ceramics",
        activity == "Ferro gusa e aco" ~ "Pig iron and steel",
        activity == "Hidroviario" ~ "Waterway",
        activity == "NAO SE APLICA" ~ "Not Applicable",
        activity == "Producao de Alcool" ~ "Alcohol production",
        activity == "Quimica" ~ "Chemical",
        activity == "Textil" ~ "Textile"
      )) %>%
      dplyr::mutate(energetic_source = dplyr::case_when(
        energetic_source == "Alcatrao" ~ "Tar",
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
        energetic_source == "Querosene de aviacao" ~ "Aviation kerosene"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Aeronaves" ~ "Airplanes",
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
        emitters == "NAO SE APLICA" ~ "Not Applicable"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(
        emission_bunker == "Emissao" ~ "Emission",
        emission_bunker == "Bunker" ~ "Bunker"
      )) %>%
      dplyr::select(-atividade_economica)

    dat <- dat %>%
      dplyr::relocate(year, city, state, sector)

    dat <- dat %>%
      dplyr::mutate(city = dplyr::case_when(
        city == "NA" ~ NA_character_,
        TRUE ~ city
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }

  if (param$dataset == "seeg_energy" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )

    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Energia") %>%
      dplyr::rename(
        sector = nivel_1_setor, state = estado,
        emission_type = nivel_2,
        emissions_generating_processes = nivel_3,
        activity = nivel_4,
        energetic_source = nivel_5,
        emitters = nivel_6,
        emission_bunker = emissao_remocao_bunker,
        product = produto
      ) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "ENE_ELET" ~ "Eletric energy",
        product == "ALU" ~ "Aluminum"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Energia" ~ "Energy")) %>%
      dplyr::mutate(emission_type = dplyr::case_when(
        emission_type == "Emissoes Fugitivas" ~ "Fugitive emissions",
        emission_type == "Emissoes pela Queima de Combustiveis" ~ "Emissions from fuel burning"
      )) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Agropecuario" ~ "Farming",
        emissions_generating_processes == "Geracao de Eletricidade (Servico Publico)" ~ "Eletricity generation (public service)",
        emissions_generating_processes == "Nao Identificado" ~ "Not Identified",
        emissions_generating_processes == "Publico" ~ "Public",
        emissions_generating_processes == "Transportes" ~ "Transports",
        emissions_generating_processes == "Comercial" ~ "Business",
        emissions_generating_processes == "Industrial" ~ "Industrial",
        emissions_generating_processes == "Producao de Combustiveis" ~ "Fuel production",
        emissions_generating_processes == "Residencial" ~ "Residential"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        activity == "Aereo" ~ "Air",
        activity == "Cimento" ~ "Cement",
        activity == "Ferro Ligas" ~ "Iron garters",
        activity == "Mineracao e pelotizacao" ~ "Mining and pelletizing",
        activity == "Outras industrias" ~ "Other industries",
        activity == "Producao de carvao mineral e outros" ~ " Mineral coal production and others",
        activity == "Refino de petroleo" ~ "Oil refining",
        activity == "Transporte de gas natural" ~ "Natural gas transport",
        activity == "Alimentos e bebidas" ~ "Food and beverages",
        activity == "Exploracao de petroleo e gas natural" ~ "Oil and natural gas exploration",
        activity == "Ferroviario" ~ "Railroad",
        activity == "Nao ferrosos e outros da metalurgia" ~ "Non-ferrous and other metallurgy",
        activity == "Papel e celulose" ~ "Paper and Cellulose",
        activity == "Producao de carvao vegetal" ~ "Charcoal production",
        activity == "Rodoviario" ~ "Road",
        activity == "Ceramica" ~ "Ceramics",
        activity == "Ferro gusa e aco" ~ "Pig iron and steel",
        activity == "Hidroviario" ~ "Waterway",
        activity == "NAO SE APLICA" ~ "Not Applicable",
        activity == "Producao de Alcool" ~ "Alcohol production",
        activity == "Quimica" ~ "Chemical",
        activity == "Textil" ~ "Textile"
      )) %>%
      dplyr::mutate(energetic_source = dplyr::case_when(
        energetic_source == "Alcatrao" ~ "Tar",
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
        energetic_source == "Querosene de aviacao" ~ "Aviation kerosene"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Aeronaves" ~ "Airplanes",
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
        emitters == "NAO SE APLICA" ~ "Not Applicable"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(
        emission_bunker == "Emissao" ~ "Emission",
        emission_bunker == "Bunker" ~ "Bunker"
      )) %>%
      dplyr::select(-atividade_economica)

    dat <- dat %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      ))

    dat <- dat %>%
      dplyr::relocate(year, state, sector)
  }

  if (param$dataset == "seeg_industry" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Processos Industriais") %>%
      dplyr::rename(
        sector = nivel_1, emissions_generating_processes = nivel_2,
        emitters_production = nivel_3,
        emitters = nivel_4,
        emission_bunker = tipo_de_emissao,
        city = municipio,
        state = territorio,
        economic_activity = atividade_economica,
        product = produto
      ) %>%
      dplyr::select(-c(nivel_6, nivel_5)) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Processos Industriais" ~ "Industrial Processes")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Emissoes de HFCs" ~ "HFC Emissions",
        emissions_generating_processes == "Producao de Metais" ~ "Metal Production",
        emissions_generating_processes == "Uso de SF6" ~ "SF6 Use",
        emissions_generating_processes == "Industria Quimica" ~ "Chemical Industry",
        emissions_generating_processes == "Produtos Minerais" ~ "Mineral Products",
        emissions_generating_processes == "Uso Nao-Energetico de Combustiveis e Uso de Solventes" ~ "Non-Energy Use of Fuels and Use of Solvents"
      )) %>%
      dplyr::mutate(emitters_production = dplyr::case_when(
        emitters_production == "Consumo de Barrilha" ~ "Barrel Consumption",
        emitters_production == "Equipamentos Eletricos" ~ "Eletric Equipment",
        emitters_production == "Producao de ABS" ~ "ABS Plastic Production",
        emitters_production == "Producao de Acido Fosforico" ~ "Production of Phosphoric Acid",
        emitters_production == "Producao de Acrilonitrila" ~ "Acrylonitrile Production",
        emitters_production == "Producao de Amonia" ~ "Ammonia Production",
        emitters_production == "Producao de Borracha de butadieno estireno (SBR)" ~ "Production of Styrene Butadiene Rubber",
        emitters_production == "Producao de Caprolactama" ~ "Production of Caprolactam",
        emitters_production == "Producao de Cimento" ~ "Cement Production",
        emitters_production == "Producao de Coque de Petroleo Calcinado" ~ "Production of Calcined Petroleum Coke",
        emitters_production == "Producao de Estireno" ~ "Styrene production",
        emitters_production == "Producao de Etilbenzeno" ~ "Ethylbenzene production",
        emitters_production == "Producao de Ferroligas" ~ "Ferroalloy Production",
        emitters_production == "Producao de Magnesio" ~ "Magnesium Production",
        emitters_production == "Producao de Negro-de-fumo" ~ "Carbon Black Production",
        emitters_production == "Producao de Oxido de Eteno" ~ "Ethylene Oxide Production",
        emitters_production == "Producao de Polietileno PEAD" ~ "HDPE Polyethylene Production",
        emitters_production == "Producao de Polietileno PELBD" ~ "LLDPE Polyethylene Production",
        emitters_production == "Producao de Propeno" ~ "Propylene Production",
        emitters_production == "Producao de Vidro" ~ "Glass Production",
        emitters_production == "Producao de PVC" ~ "PVC Production",
        emitters_production == "Producao de Polipropileno" ~ "Polypropylene Production",
        emitters_production == "Producao de Polietileno PEBD" ~ "LDPE Polyethylene Production",
        emitters_production == "Producao de Poliestireno" ~ "Polystyrene Production",
        emitters_production == "Producao de Outros Nao-Ferrosos" ~ "Production of Other Non-Ferrous",
        emitters_production == "Producao de Metanol" ~ "Methanol Production",
        emitters_production == "Producao de Formaldeido" ~ "Formaldehyde Production",
        emitters_production == "Producao de Ferro Gusa e Aco" ~ "Pig Iron and Steel Production",
        emitters_production == "Producao de Eteno" ~ "Ethene Production",
        emitters_production == "Producao de Dicloroetano" ~ "Dichloroethane Production",
        emitters_production == "Producao de Cloreto de Vinila" ~ "Vinyl Chloride Production",
        emitters_production == "Producao de Carbureto de Calcio" ~ "Calcium Carbide Production",
        emitters_production == "Producao de Cal" ~ "Lime Production",
        emitters_production == "Producao de Anidrido Ftalico" ~ "Production of Phthalic Anhydride",
        emitters_production == "Producao de Aluminio" ~ "Aluminum Production",
        emitters_production == "Producao de Acido Nitrico" ~ "Nitric Acid Production",
        emitters_production == "Producao de Acido Adipico" ~ "Adipic Acid Production",
        emitters_production == "NAO SE APLICA" ~ "Not Applicable",
        emitters_production == "Consumo Final Nao Energetico" ~ "Non-Energy Final Consumption"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Cal Calcitica" ~ "Calcitic lime",
        emitters == "Consumo de Calcario" ~ "Limestone Consumption",
        emitters == "Consumo em Outros Setores" ~ "Consumption in Other Sectors",
        emitters == "Tecnologia Soderberg" ~ "Soderberg Technology",
        emitters == "Cal Dolomitica" ~ "Dolomitic Lime",
        emitters == "Consumo de Combustiveis Redutores" ~ "Consumption of Reducing Fuels",
        emitters == "NAO SE APLICA" ~ "Not Applicable",
        emitters == "Uso de SF6" ~ "SF6 Use",
        emitters == "Cal Magnesiana" ~ "Magnesian Lime",
        emitters == "Consumo de Dolomita" ~ "Dolomite Consumption",
        emitters == "Tecnologia Prebaked Anode" ~ "Prebaked Anode Technology"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(emission_bunker == "Emissao" ~ "Emission")) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "CIM" ~ "Cement Industry",
        economic_activity == "ENE_ELET" ~ "Eletric Power Industry",
        economic_activity == "MET" ~ "Metal Industry",
        economic_activity == "Outra_IND" ~ "Other Industry",
        economic_activity == "HFC" ~ "HFC"
      ))
    dat <- dat %>%
      dplyr::mutate(city = dplyr::case_when(
        city == "NA" ~ NA_character_,
        TRUE ~ city
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALU" ~ "Aluminum",
        product == "ACO" ~ "Steel"
      ))
    dat <- dat %>%
      dplyr::relocate(year, city, state, sector, emissions_generating_processes, emitters_production, emitters, economic_activity, gas, product, value)
  }


  if (param$dataset == "seeg_industry" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Processos Industriais") %>%
      dplyr::rename(
        sector = nivel_1_setor, emissions_generating_processes = nivel_2,
        emitters_production = nivel_3,
        emitters = nivel_4,
        emission_bunker = emissao_remocao_bunker,
        economic_activity = atividade_economica,
        product = produto,
        state = estado
      ) %>%
      dplyr::select(-c(nivel_6, nivel_5)) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Processos Industriais" ~ "Industrial Processes")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Emissoes de HFCs" ~ "HFC Emissions",
        emissions_generating_processes == "Producao de Metais" ~ "Metal Production",
        emissions_generating_processes == "Uso de SF6" ~ "SF6 Use",
        emissions_generating_processes == "Industria Quimica" ~ "Chemical Industry",
        emissions_generating_processes == "Produtos Minerais" ~ "Mineral Products",
        emissions_generating_processes == "Uso Nao-Energetico de Combustiveis e Uso de Solventes" ~ "Non-Energy Use of Fuels and Use of Solvents"
      )) %>%
      dplyr::mutate(emitters_production = dplyr::case_when(
        emitters_production == "Consumo de Barrilha" ~ "Barrel Consumption",
        emitters_production == "Equipamentos Eletricos" ~ "Eletric Equipment",
        emitters_production == "Producao de ABS" ~ "ABS Plastic Production",
        emitters_production == "Producao de Acido Fosforico" ~ "Production of Phosphoric Acid",
        emitters_production == "Producao de Acrilonitrila" ~ "Acrylonitrile Production",
        emitters_production == "Producao de Amonia" ~ "Ammonia Production",
        emitters_production == "Producao de Borracha de butadieno estireno (SBR)" ~ "Production of Styrene Butadiene Rubber",
        emitters_production == "Producao de Caprolactama" ~ "Production of Caprolactam",
        emitters_production == "Producao de Cimento" ~ "Cement Production",
        emitters_production == "Producao de Coque de Petroleo Calcinado" ~ "Production of Calcined Petroleum Coke",
        emitters_production == "Producao de Estireno" ~ "Styrene production",
        emitters_production == "Producao de Etilbenzeno" ~ "Ethylbenzene production",
        emitters_production == "Producao de Ferroligas" ~ "Ferroalloy Production",
        emitters_production == "Producao de Magnesio" ~ "Magnesium Production",
        emitters_production == "Producao de Negro-de-fumo" ~ "Carbon Black Production",
        emitters_production == "Producao de Oxido de Eteno" ~ "Ethylene Oxide Production",
        emitters_production == "Producao de Polietileno PEAD" ~ "HDPE Polyethylene Production",
        emitters_production == "Producao de Polietileno PELBD" ~ "LLDPE Polyethylene Production",
        emitters_production == "Producao de Propeno" ~ "Propylene Production",
        emitters_production == "Producao de Vidro" ~ "Glass Production",
        emitters_production == "Producao de PVC" ~ "PVC Production",
        emitters_production == "Producao de Polipropileno" ~ "Polypropylene Production",
        emitters_production == "Producao de Polietileno PEBD" ~ "LDPE Polyethylene Production",
        emitters_production == "Producao de Poliestireno" ~ "Polystyrene Production",
        emitters_production == "Producao de Outros Nao-Ferrosos" ~ "Production of Other Non-Ferrous",
        emitters_production == "Producao de Metanol" ~ "Methanol Production",
        emitters_production == "Producao de Formaldeido" ~ "Formaldehyde Production",
        emitters_production == "Producao de Ferro Gusa e Aco" ~ "Pig Iron and Steel Production",
        emitters_production == "Producao de Eteno" ~ "Ethene Production",
        emitters_production == "Producao de Dicloroetano" ~ "Dichloroethane Production",
        emitters_production == "Producao de Cloreto de Vinila" ~ "Vinyl Chloride Production",
        emitters_production == "Producao de Carbureto de Calcio" ~ "Calcium Carbide Production",
        emitters_production == "Producao de Cal" ~ "Lime Production",
        emitters_production == "Producao de Anidrido Ftalico" ~ "Production of Phthalic Anhydride",
        emitters_production == "Producao de Aluminio" ~ "Aluminum Production",
        emitters_production == "Producao de Acido Nitrico" ~ "Nitric Acid Production",
        emitters_production == "Producao de Acido Adipico" ~ "Adipic Acid Production",
        emitters_production == "NAO SE APLICA" ~ "Not Applicable",
        emitters_production == "Consumo Final Nao Energetico" ~ "Non-Energy Final Consumption"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Cal Calcitica" ~ "Calcitic lime",
        emitters == "Consumo de Calcario" ~ "Limestone Consumption",
        emitters == "Consumo em Outros Setores" ~ "Consumption in Other Sectors",
        emitters == "Tecnologia Soderberg" ~ "Soderberg Technology",
        emitters == "Cal Dolomitica" ~ "Dolomitic Lime",
        emitters == "Consumo de Combustiveis Redutores" ~ "Consumption of Reducing Fuels",
        emitters == "NAO SE APLICA" ~ "Not Applicable",
        emitters == "Uso de SF6" ~ "SF6 Use",
        emitters == "Cal Magnesiana" ~ "Magnesian Lime",
        emitters == "Consumo de Dolomita" ~ "Dolomite Consumption",
        emitters == "Tecnologia Prebaked Anode" ~ "Prebaked Anode Technology"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(emission_bunker == "Emissao" ~ "Emission")) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "CIM" ~ "Cement Industry",
        economic_activity == "ENE_ELET" ~ "Eletric Power Industry",
        economic_activity == "MET" ~ "Metal Industry",
        economic_activity == "Outra_IND" ~ "Other Industry",
        economic_activity == "HFC" ~ "HFC"
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALU" ~ "Aluminum",
        product == "ACO" ~ "Steel"
      ))


    dat <- dat %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      ))

    dat <- dat %>%
      dplyr::relocate(year, state, sector, emissions_generating_processes, emitters_production, emitters, economic_activity, gas, product, value)
  }

  if (param$dataset == "seeg_residuals" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )

    dat <- dat %>%
      dplyr::filter(nivel_1 == "Residuos") %>%
      dplyr::rename(
        sector = nivel_1, emissions_category = nivel_2,
        emissions_generating_processes = nivel_3,
        activity = nivel_4,
        generating_processes_categories = nivel_5,
        emission_bunker = tipo_de_emissao,
        product = produto,
        economic_activity = atividade_economica,
        city = municipio,
        state = territorio
      ) %>%
      dplyr::select(-nivel_6) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "PEC" ~ "Livestock",
        economic_activity == "Outra_IND" ~ "Other industry",
        economic_activity == "SANEAMENTO" ~ "Sanitation"
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "CAR" ~ "Meat"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Residuos" ~ "Waste")) %>%
      dplyr::mutate(emissions_category = dplyr::case_when(
        emissions_category == "Efluentes Liquidos" ~ "Liquid Effluents",
        emissions_category == "Residuos Solidos" ~ "Solid Waste"
      )) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Disposicao Final de Residuos Solidos" ~ "Final Disposal of Solid Waste",
        emissions_generating_processes == "Efluentes Liquidos Industriais" ~ "Industrial Liquid Effluents",
        emissions_generating_processes == "Tratamento Biologico de Residuos Solidos" ~ "Biological Treatment of Solid Waste",
        emissions_generating_processes == "Efluentes Liquidos Domesticos" ~ "Domestic Liquid Effluents",
        emissions_generating_processes == "Incineracao ou queima a ceu aberto" ~ "Incineration or open burning"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        activity == "Producao de Carne Bovina" ~ "Beef Production",
        activity == "NAO SE APLICA" ~ "Not Applicable",
        activity == "Producao de Cerveja" ~ "Beer Production",
        activity == "Producao de Leite Cru" ~ "Raw Dairy Production",
        activity == "Producao de Carne Suina" ~ "Prok Production",
        activity == "Producao de Carne Avicola" ~ "Poultry Meat Production",
        activity == "Producao de Celulose" ~ "Cellulose Production",
        activity == "Producao de Leite Pasteurizado" ~ "Pasteurized Dairy Production",
        activity == "Residuos Solidos Urbanos" ~ "Urban Solid Waste",
        activity == "Residuos de Servicos de Saude" ~ "Health Services Waste",
        activity == "Queima de Residuos a Ceu Aberto" ~ "Open-air Waste Burning",
        activity == "Tratamento de Residuos por Incineracao" ~ "Waste Treatment by Incineration",
        activity == "Lodo de ETE" ~ "Sewage Treatment Plant Sludge"
      )) %>%
      dplyr::mutate(generating_processes_categories = dplyr::case_when(
        generating_processes_categories == "Compostagem" ~ "Compost",
        generating_processes_categories == "Residuos de Servicos de Saude" ~ "Health Services Waste",
        generating_processes_categories == "Disposicao em Aterro Sanitario" ~ "Landfill Disposal",
        generating_processes_categories == "Disposicao em Aterro Controlado ou Lixao" ~ "Disposal in Controlled Landfill or Landfill",
        generating_processes_categories == "NAO SE APLICA" ~ "Not Applicable",
        generating_processes_categories == "Residuos Solidos Urbanos" ~ "Urban Solid Waste"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(emission_bunker == "Emissao" ~ "Emission")) %>%
      dplyr::mutate(city = dplyr::case_when(
        city == "NA" ~ NA_character_,
        TRUE ~ city
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
    dat <- dat %>%
      dplyr::relocate(year, city, state, sector, emissions_category, emissions_generating_processes, activity, generating_processes_categories, economic_activity, product, value)
  }


  if (param$dataset == "seeg_residuals" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )

    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Residuos") %>%
      dplyr::rename(
        sector = nivel_1_setor, emissions_category = nivel_2,
        emissions_generating_processes = nivel_3,
        activity = nivel_4,
        generating_processes_categories = nivel_5,
        emission_bunker = emissao_remocao_bunker,
        product = produto,
        economic_activity = atividade_economica,
        state = estado
      ) %>%
      dplyr::select(-nivel_6) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "PEC" ~ "Livestock",
        economic_activity == "Outra_IND" ~ "Other industry",
        economic_activity == "SANEAMENTO" ~ "Sanitation"
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "CAR" ~ "Meat"
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "BA" ~ "BA",
        state == "ES" ~ "ES",
        state == "MA" ~ "MA",
        state == "NA" ~ as.character(NA),
        state == "PA" ~ "PA",
        state == "RJ" ~ "RJ",
        state == "RS" ~ "RS",
        state == "SP" ~ "SP",
        state == "AC" ~ "AC",
        state == "AL" ~ "AL",
        state == "AP" ~ "AP",
        state == "CE" ~ "CE",
        state == "DF" ~ "DF",
        state == "ES" ~ "ES",
        state == "GO" ~ "GO",
        state == "MG" ~ "MG",
        state == "MS" ~ "MS",
        state == "MT" ~ "MT",
        state == "PB" ~ "PB",
        state == "PE" ~ "PE",
        state == "PI" ~ "PI",
        state == "PR" ~ "PR",
        state == "RN" ~ "RN",
        state == "RO" ~ "RO",
        state == "RR" ~ "RR",
        state == "SC" ~ "SC",
        state == "SE" ~ "SE",
        state == "TO" ~ "TO",
        state == "AM" ~ "AM"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Residuos" ~ "Waste")) %>%
      dplyr::mutate(emissions_category = dplyr::case_when(
        emissions_category == "Efluentes Liquidos" ~ "Liquid Effluents",
        emissions_category == "Residuos Solidos" ~ "Solid Waste"
      )) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Disposicao Final de Residuos Solidos" ~ "Final Disposal of Solid Waste",
        emissions_generating_processes == "Efluentes Liquidos Industriais" ~ "Industrial Liquid Effluents",
        emissions_generating_processes == "Tratamento Biologico de Residuos Solidos" ~ "Biological Treatment of Solid Waste",
        emissions_generating_processes == "Efluentes Liquidos Domesticos" ~ "Domestic Liquid Effluents",
        emissions_generating_processes == "Incineracao ou queima a ceu aberto" ~ "Incineration or open burning"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        activity == "Producao de Carne Bovina" ~ "Beef Production",
        activity == "NAO SE APLICA" ~ "Not Applicable",
        activity == "Producao de Cerveja" ~ "Beer Production",
        activity == "Producao de Leite Cru" ~ "Raw Dairy Production",
        activity == "Producao de Carne Suina" ~ "Prok Production",
        activity == "Producao de Carne Avicola" ~ "Poultry Meat Production",
        activity == "Producao de Celulose" ~ "Cellulose Production",
        activity == "Producao de Leite Pasteurizado" ~ "Pasteurized Dairy Production",
        activity == "Residuos Solidos Urbanos" ~ "Urban Solid Waste",
        activity == "Residuos de Servicos de Saude" ~ "Health Services Waste",
        activity == "Queima de Residuos a Ceu Aberto" ~ "Open-air Waste Burning",
        activity == "Tratamento de Residuos por Incineracao" ~ "Waste Treatment by Incineration",
        activity == "Lodo de ETE" ~ "Sewage Treatment Plant Sludge"
      )) %>%
      dplyr::mutate(generating_processes_categories = dplyr::case_when(
        generating_processes_categories == "Compostagem" ~ "Compost",
        generating_processes_categories == "Residuos de Servicos de Saude" ~ "Health Services Waste",
        generating_processes_categories == "Disposicao em Aterro Sanitario" ~ "Landfill Disposal",
        generating_processes_categories == "Disposicao em Aterro Controlado ou Lixao" ~ "Disposal in Controlled Landfill or Landfill",
        generating_processes_categories == "NAO SE APLICA" ~ "Not Applicable",
        generating_processes_categories == "Residuos Solidos Urbanos" ~ "Urban Solid Waste"
      )) %>%
      dplyr::mutate(emission_bunker = dplyr::case_when(emission_bunker == "Emissao" ~ "Emission"))

    dat <- dat %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      ))

    dat <- dat %>%
      dplyr::relocate(year, state, sector, emissions_category, emissions_generating_processes, activity, generating_processes_categories, economic_activity, product, value)
  }


  if (param$dataset == "seeg_land" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Mudanca de Uso da Terra e Floresta") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(
        sector = nivel_1, emissions_generating_processes = nivel_2,
        biome = nivel_3,
        biome_area = nivel_4,
        transition_type = nivel_5,
        category = nivel_6,
        emission_removal_bunker = tipo_de_emissao,
        economic_activity = atividade_economica,
        city = municipio,
        state = territorio
      ) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Mudanca de Uso da Terra e Floresta" ~ "Land and Forest Use Change")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Alteracoes de Uso do Solo" ~ "Changes in Soil Use",
        emissions_generating_processes == "Remocao por Vegetacao Secundaria" ~ "Removal by Secondary Vegetation",
        emissions_generating_processes == "Remocao em Areas Protegidas" ~ "Removal in Protected Areas",
        emissions_generating_processes == "Residuos Florestais" ~ "Forest Waste",
        emissions_generating_processes == "Remocao por Mudanca de Uso da Terra" ~ "Removal for Land Use Change"
      )) %>%
      dplyr::mutate(biome = dplyr::case_when(
        biome == "Amazonia" ~ "Amazon",
        biome == "Caatinga" ~ "Caatinga",
        biome == "Cerrado" ~ "Cerrado",
        biome == "Mata Atlantica" ~ "Mata Atlantica",
        biome == "Pampa" ~ "Pampa",
        biome == "Pantanal" ~ "Pantanal"
      )) %>%
      dplyr::mutate(biome_area = dplyr::case_when(
        biome_area == "em Area Protegida" ~ "In Protected Area",
        biome_area == "fora de Area Protegida" ~ "Outside Protected Area",
        biome_area == "NAO SE APLICA" ~ "Not Applicable"
      )) %>%
      dplyr::mutate(transition_type = dplyr::case_when(
        transition_type == "Desmatamento" ~ "Deforestation",
        transition_type == "Regeneracao" ~ "Regeneration",
        transition_type == "NAO SE APLICA" ~ "Not Applicable",
        transition_type == "Vegetacao nativa estavel" ~ "Stable Native Vegetation",
        transition_type == "Outras Mudancas de uso da terra" ~ "Other Land Use Changes"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        category == "Area sem vegetacao -- Floresta secundaria" ~ "Area without Vegetation -- Secondary Forest",
        category == "Area sem vegetacao -- Silvicultura" ~ "Area without Vegetation -- Forestry",
        category == "Area sem vegetacao -- Uso agropecuario" ~ "Area without Vegetation -- Farming Use",
        category == "Area sem vegetacao -- Vegetacao nao florestal secundaria" ~ "Area without Vegetation -- Secondary Non-Forest Vegetation",
        category == "Floresta primaria -- Area sem vegetacao" ~ "Primary Forest -- Area without Vegetation",
        category == "Floresta primaria -- Floresta primaria" ~ "Primary Forest -- Primary Forest",
        category == "Floresta primaria -- Silvicultura" ~ "Primary Forest -- Forestry",
        category == "Floresta primaria -- Uso agropecuario" ~ "Primary Forest -- Farming Use",
        category == "Floresta secundaria -- Area sem vegetacao" ~ "Secondary Forest -- Area without Vegetation",
        category == "Floresta secundaria -- Floresta secundaria" ~ "Secondary Forest -- Secondary Forest",
        category == "Floresta secundaria -- Silvicultura" ~ "Secondary Forest -- Forestry",
        category == "Floresta secundaria -- Uso agropecuario" ~ "Secondary Forest -- Farming Use",
        category == "NAO SE APLICA" ~ "Not Applicable",
        category == "Silvicultura -- Area sem vegetacao" ~ "Forestry -- Area without Vegetation",
        category == "Silvicultura -- Floresta Secundaria" ~ "Forestry -- Secondary Forest",
        category == "Silvicultura -- Uso agropecuario" ~ "Forestry -- Farming Use",
        category == "Silvicultura -- Vegetacao nao florestal secundaria" ~ "Forestry -- Secondary non-Forest Vegetation",
        category == "Uso agropecuario -- Area sem vegetacao" ~ "Farming Use -- Area without vegetation",
        category == "Uso agropecuario -- Floresta secundaria" ~ "Farming Use -- Secondary Forest",
        category == "Uso agropecuario -- Silvicultura" ~ "Farming Use -- Forestry",
        category == "Uso agropecuario -- Uso agropecuario" ~ "Farming Use -- Farming Use",
        category == "Uso agropecuario -- Vegetacao nao florestal secundaria" ~ "Farming Use -- Secondary non-Forest Vegetation",
        category == "Vegetacao nao florestal primaria -- Area sem vegetacao" ~ "Primary non-Forest Vegetation -- Area without Vegetation",
        category == "Vegetacao nao florestal primaria -- Silvicultura" ~ "Primary Non-Forest Vegetation -- Forestry",
        category == "Vegetacao nao florestal primaria -- Uso agropecuario" ~ "Primary non-Forest Vegetation -- Farming use",
        category == "Vegetacao nao florestal primaria -- Vegetacao nao florestal primaria" ~ "Primary non-Forest Vegetation -- Primary non-Forest Vegetation",
        category == "Vegetacao nao florestal secundaria -- Area sem vegetacao" ~ "Secondary non-Forest Vegetation -- Area without Vegetation",
        category == "Vegetacao nao florestal secundaria -- Silvicultura" ~ "Secondary non-Forest Vegetation -- Forestry",
        category == "Vegetacao nao florestal secundaria -- Uso agropecuario" ~ "Secondary non-Forest Vegetation -- Farming use",
        category == "Vegetacao nao florestal secundaria -- Vegetacao nao florestal secundaria" ~ "Secondary non-Forest Vegetation"
      )) %>%
      dplyr::mutate(emission_removal_bunker = dplyr::case_when(
        emission_removal_bunker == "Emissao" ~ "Emission",
        emission_removal_bunker == "Remocao" ~ "Removal",
        emission_removal_bunker == "Remocao proxy" ~ "Proxy Removal"
      )) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "AGROPEC" ~ "Farming",
        economic_activity == "Conservacao" ~ "Conservation"
      )) %>%
      dplyr::select(-produto)

    dat <- dat %>%
      dplyr::mutate(city = dplyr::case_when(
        city == "NA" ~ NA_character_,
        TRUE ~ city
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))

    dat <- dat %>%
      dplyr::relocate(year, city, state, sector, emissions_generating_processes, economic_activity, biome, biome_area, transition_type, emission_removal_bunker)
  }

  if (param$dataset == "seeg_land" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Mudanca de Uso da Terra e Floresta") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(
        sector = nivel_1_setor, emissions_generating_processes = nivel_2,
        biome = nivel_3,
        biome_area = nivel_4,
        transition_type = nivel_5,
        category = nivel_6,
        emission_removal_bunker = emissao_remocao_bunker,
        economic_activity = atividade_economica,
        state = estado
      ) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Mudanca de Uso da Terra e Floresta" ~ "Land and Forest Use Change")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Alteracoes de Uso do Solo" ~ "Changes in Soil Use",
        emissions_generating_processes == "Remocao por Vegetacao Secundaria" ~ "Removal by Secondary Vegetation",
        emissions_generating_processes == "Remocao em Areas Protegidas" ~ "Removal in Protected Areas",
        emissions_generating_processes == "Residuos Florestais" ~ "Forest Waste",
        emissions_generating_processes == "Remocao por Mudanca de Uso da Terra" ~ "Removal for Land Use Change"
      )) %>%
      dplyr::mutate(biome = dplyr::case_when(
        biome == "Amazonia" ~ "Amazon",
        biome == "Caatinga" ~ "Caatinga",
        biome == "Cerrado" ~ "Cerrado",
        biome == "Mata Atlantica" ~ "Mata Atlantica",
        biome == "Pampa" ~ "Pampa",
        biome == "Pantanal" ~ "Pantanal"
      )) %>%
      dplyr::mutate(biome_area = dplyr::case_when(
        biome_area == "em Area Protegida" ~ "In Protected Area",
        biome_area == "fora de Area Protegida" ~ "Outside Protected Area",
        biome_area == "NAO SE APLICA" ~ "Not Applicable"
      )) %>%
      dplyr::mutate(transition_type = dplyr::case_when(
        transition_type == "Desmatamento" ~ "Deforestation",
        transition_type == "Regeneracao" ~ "Regeneration",
        transition_type == "NAO SE APLICA" ~ "Not Applicable",
        transition_type == "Vegetacao nativa estavel" ~ "Stable Native Vegetation",
        transition_type == "Outras Mudancas de uso da terra" ~ "Other Land Use Changes"
      )) %>%
      dplyr::mutate(activity = dplyr::case_when(
        category == "Area sem vegetacao -- Floresta secundaria" ~ "Area without Vegetation -- Secondary Forest",
        category == "Area sem vegetacao -- Silvicultura" ~ "Area without Vegetation -- Forestry",
        category == "Area sem vegetacao -- Uso agropecuario" ~ "Area without Vegetation -- Farming Use",
        category == "Area sem vegetacao -- Vegetacao nao florestal secundaria" ~ "Area without Vegetation -- Secondary Non-Forest Vegetation",
        category == "Floresta primaria -- Area sem vegetacao" ~ "Primary Forest -- Area without Vegetation",
        category == "Floresta primaria -- Floresta primaria" ~ "Primary Forest -- Primary Forest",
        category == "Floresta primaria -- Silvicultura" ~ "Primary Forest -- Forestry",
        category == "Floresta primaria -- Uso agropecuario" ~ "Primary Forest -- Farming Use",
        category == "Floresta secundaria -- Area sem vegetacao" ~ "Secondary Forest -- Area without Vegetation",
        category == "Floresta secundaria -- Floresta secundaria" ~ "Secondary Forest -- Secondary Forest",
        category == "Floresta secundaria -- Silvicultura" ~ "Secondary Forest -- Forestry",
        category == "Floresta secundaria -- Uso agropecuario" ~ "Secondary Forest -- Farming Use",
        category == "NAO SE APLICA" ~ "Not Applicable",
        category == "Silvicultura -- Area sem vegetacao" ~ "Forestry -- Area without Vegetation",
        category == "Silvicultura -- Floresta Secundaria" ~ "Forestry -- Secondary Forest",
        category == "Silvicultura -- Uso agropecuario" ~ "Forestry -- Farming Use",
        category == "Silvicultura -- Vegetacao nao florestal secundaria" ~ "Forestry -- Secondary non-Forest Vegetation",
        category == "Uso agropecuario -- Area sem vegetacao" ~ "Farming Use -- Area without vegetation",
        category == "Uso agropecuario -- Floresta secundaria" ~ "Farming Use -- Secondary Forest",
        category == "Uso agropecuario -- Silvicultura" ~ "Farming Use -- Forestry",
        category == "Uso agropecuario -- Uso agropecuario" ~ "Farming Use -- Farming Use",
        category == "Uso agropecuario -- Vegetacao nao florestal secundaria" ~ "Farming Use -- Secondary non-Forest Vegetation",
        category == "Vegetacao nao florestal primaria -- Area sem vegetacao" ~ "Primary non-Forest Vegetation -- Area without Vegetation",
        category == "Vegetacao nao florestal primaria -- Silvicultura" ~ "Primary Non-Forest Vegetation -- Forestry",
        category == "Vegetacao nao florestal primaria -- Uso agropecuario" ~ "Primary non-Forest Vegetation -- Farming use",
        category == "Vegetacao nao florestal primaria -- Vegetacao nao florestal primaria" ~ "Primary non-Forest Vegetation -- Primary non-Forest Vegetation",
        category == "Vegetacao nao florestal secundaria -- Area sem vegetacao" ~ "Secondary non-Forest Vegetation -- Area without Vegetation",
        category == "Vegetacao nao florestal secundaria -- Silvicultura" ~ "Secondary non-Forest Vegetation -- Forestry",
        category == "Vegetacao nao florestal secundaria -- Uso agropecuario" ~ "Secondary non-Forest Vegetation -- Farming use",
        category == "Vegetacao nao florestal secundaria -- Vegetacao nao florestal secundaria" ~ "Secondary non-Forest Vegetation"
      )) %>%
      dplyr::mutate(emission_removal_bunker = dplyr::case_when(
        emission_removal_bunker == "Emissao" ~ "Emission",
        emission_removal_bunker == "Remocao" ~ "Removal",
        emission_removal_bunker == "Remocao proxy" ~ "Proxy Removal"
      )) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "AGROPEC" ~ "Farming",
        economic_activity == "Conservacao" ~ "Conservation"
      )) %>%
      dplyr::select(-produto)

    dat <- dat %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      ))

    dat <- dat %>%
      dplyr::relocate(year, state, sector, emissions_generating_processes, economic_activity, biome, biome_area, transition_type, emission_removal_bunker)
  }

  if (param$dataset == "seeg_farming" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2018,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1 == "Agropecuaria") %>%
      dplyr::rename(
        sector = nivel_1, emissions_generating_processes = nivel_2,
        emissions_type = nivel_4,
        direct_indirect_emitters = nivel_3,
        emissions_sources = nivel_5,
        emitters = nivel_6,
        economic_activity = atividade_economica,
        product = produto,
        emission_removal_bunker = tipo_de_emissao,
        city = municipio,
        state = territorio
      ) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "PEC" ~ "Farming",
        economic_activity == "AGR" ~ "Agriculture"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Agropecuaria" ~ "Farming")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Cultivo do Arroz" ~ "Rice Cultivation",
        emissions_generating_processes == "Fermentacao Enterica" ~ "Enteric Fermentation",
        emissions_generating_processes == "Manejo de Dejetos Animais" ~ "Animal Waste Management",
        emissions_generating_processes == "Queima de Residuos Agricolas" ~ "Agricultural Waste Burning",
        emissions_generating_processes == "Solos Manejados" ~ "Managed Soils"
      )) %>%
      dplyr::mutate(emissions_sources = dplyr::case_when(
        emissions_sources == "Animal" ~ "Animal",
        emissions_sources == "Vegetal" ~ "Plant",
        emissions_sources == "Outros" ~ "Others"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Algodao" ~ "Cottage",
        emitters == "Arroz" ~ "Rice",
        emitters == "Aves" ~ "Birds",
        emitters == "Cana de Acucar" ~ "Sugar Cane",
        emitters == "Equino" ~ "Equine",
        emitters == "Fertilizantes Sinteticos" ~ "Synthetic Fertilizers",
        emitters == "Gado de Corte" ~ "Beef Cattle",
        emitters == "Lavouras Cultivadas sob Sistema Convencional" ~ "Crops Cultivated under Conventional System",
        emitters == "Mandioca" ~ "Cassava",
        emitters == "Muar" ~ "Mule",
        emitters == "Outros" ~ "Others",
        emitters == "Pastagem" ~ "Pasture",
        emitters == "Pastagem Degradada" ~ "Degraded Pasture",
        emitters == "Soja" ~ "Soil",
        emitters == "Suinos" ~ "Swine",
        emitters == "Trigo" ~ "Wheat",
        emitters == "Vinhaca" ~ "Vinasse",
        emitters == "Uso de Calcario" ~ "Limestone Usage",
        emitters == "Torta de Filtro" ~ "Filter Cake",
        emitters == "Solos organicos" ~ "Organic Soils",
        emitters == "Sistemas Integrados Lavoura-Pecuaria-Floresta" ~ "Integrated Crop-Livestock-Forestry Systems",
        emitters == "Pastagem Bem Manejada" ~ "Well Managed Pasture",
        emitters == "Ovino" ~ "Sheep",
        emitters == "Outras Culturas" ~ "Other Cultures",
        emitters == "Milho" ~ "Corn",
        emitters == "Lavouras Cultivadas sob Sistema Plantio Direto" ~ "Crops Cultivated under no-till System",
        emitters == "Gado de Leite" ~ "Dairy Cattle",
        emitters == "Florestas Plantadas" ~ "Planted Forests",
        emitters == "Feijao" ~ "Beans",
        emitters == "Caprino" ~ "Goat",
        emitters == "Bubalino" ~ "Buffalo",
        emitters == "Asinino" ~ "Asinine",
        emitters == "Aplicacao de Ureia" ~ "Urea Application"
      )) %>%
      dplyr::mutate(direct_indirect_emitters = dplyr::case_when(
        direct_indirect_emitters == "Diretas" ~ "Direct",
        direct_indirect_emitters == "Indiretas" ~ "Indirect"
      )) %>%
      dplyr::mutate(emissions_type = dplyr::case_when(
        emissions_type == "Aplicacao de residuos organicos" ~ "Application of Organic Waste",
        emissions_type == "Deposicao de dejetos em pastagem" ~ "Pasture Waste Disposal",
        emissions_type == "Lixiviacao" ~ "Leaching",
        emissions_type == "Outros" ~ "Others",
        emissions_type == "Solos organicos" ~ "Organic Soils",
        emissions_type == "Deposicao Atmosferica" ~ "Atmospheric Deposition",
        emissions_type == "Fertilizantes Sinteticos" ~ "Synthetic Fertilizers",
        emissions_type == "Mineralizacao de N associado a perda de C no solo" ~ "Mineralization of N Associated with Loss of C in the Soil",
        emissions_type == "Residuos Agricolas" ~ "Agricultural Waste",
        emissions_type == "Variacao dos Estoques de Carbono no Solo" ~ "Changes in Carbon Soil Stocks"
      )) %>%
      dplyr::mutate(emission_removal_bunker = dplyr::case_when(
        emission_removal_bunker == "Emissao" ~ "Emission",
        emission_removal_bunker == "Emissao NCI" ~ "NCI Emission",
        emission_removal_bunker == "Remocao NCI" ~ "NCI Removal"
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "CAR" ~ "Meat",
        product == "CAR/LEI" ~ "Meat/Dairy",
        product == "CAR/LEI/ALIM_BEBIDAS" ~ "Meat/Dairy/Food/Beverages",
        product == "LEI" ~ "Dairy"
      ))

    dat <- dat %>%
      dplyr::relocate(
        year, city, state, sector, emissions_generating_processes, emissions_sources, emitters, emissions_type, gas, economic_activity, product,
        value
      )

    dat <- dat %>%
      dplyr::mutate(city = dplyr::case_when(
        city == "NA" ~ NA_character_,
        TRUE ~ city
      )) %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      )) %>%
      dplyr::mutate(ibge = dplyr::case_when(
        ibge == "NA" ~ NA_character_,
        TRUE ~ ibge
      ))
  }

  if (param$dataset == "seeg_farming" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = x1970:x2021,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      )
    ## Changing column name, filtering by the specific sector and harmonizing variables
    dat <- dat %>%
      dplyr::filter(nivel_1_setor == "Agropecuaria") %>%
      dplyr::rename(
        sector = nivel_1_setor, emissions_generating_processes = nivel_2,
        emissions_type = nivel_4,
        direct_indirect_emitters = nivel_3,
        emissions_sources = nivel_5,
        emitters = nivel_6,
        economic_activity = atividade_economica,
        product = produto,
        emission_removal_bunker = emissao_remocao_bunker,
        state = estado
      ) %>%
      dplyr::mutate(economic_activity = dplyr::case_when(
        economic_activity == "PEC" ~ "Farming",
        economic_activity == "AGR" ~ "Agriculture"
      )) %>%
      dplyr::mutate(sector = dplyr::case_when(sector == "Agropecuaria" ~ "Farming")) %>%
      dplyr::mutate(emissions_generating_processes = dplyr::case_when(
        emissions_generating_processes == "Cultivo do Arroz" ~ "Rice Cultivation",
        emissions_generating_processes == "Fermentacao Enterica" ~ "Enteric Fermentation",
        emissions_generating_processes == "Manejo de Dejetos Animais" ~ "Animal Waste Management",
        emissions_generating_processes == "Queima de Residuos Agricolas" ~ "Agricultural Waste Burning",
        emissions_generating_processes == "Solos Manejados" ~ "Managed Soils"
      )) %>%
      dplyr::mutate(emissions_sources = dplyr::case_when(
        emissions_sources == "Animal" ~ "Animal",
        emissions_sources == "Vegetal" ~ "Plant",
        emissions_sources == "Outros" ~ "Others"
      )) %>%
      dplyr::mutate(emitters = dplyr::case_when(
        emitters == "Algodao" ~ "Cottage",
        emitters == "Arroz" ~ "Rice",
        emitters == "Aves" ~ "Birds",
        emitters == "Cana de Acucar" ~ "Sugar Cane",
        emitters == "Equino" ~ "Equine",
        emitters == "Fertilizantes Sinteticos" ~ "Synthetic Fertilizers",
        emitters == "Gado de Corte" ~ "Beef Cattle",
        emitters == "Lavouras Cultivadas sob Sistema Convencional" ~ "Crops Cultivated under Conventional System",
        emitters == "Mandioca" ~ "Cassava",
        emitters == "Muar" ~ "Mule",
        emitters == "Outros" ~ "Others",
        emitters == "Pastagem" ~ "Pasture",
        emitters == "Pastagem Degradada" ~ "Degraded Pasture",
        emitters == "Soja" ~ "Soil",
        emitters == "Suinos" ~ "Swine",
        emitters == "Trigo" ~ "Wheat",
        emitters == "Vinhaca" ~ "Vinasse",
        emitters == "Uso de Calcario" ~ "Limestone Usage",
        emitters == "Torta de Filtro" ~ "Filter Cake",
        emitters == "Solos organicos" ~ "Organic Soils",
        emitters == "Sistemas Integrados Lavoura-Pecuaria-Floresta" ~ "Integrated Crop-Livestock-Forestry Systems",
        emitters == "Pastagem Bem Manejada" ~ "Well Managed Pasture",
        emitters == "Ovino" ~ "Sheep",
        emitters == "Outras Culturas" ~ "Other Cultures",
        emitters == "Milho" ~ "Corn",
        emitters == "Lavouras Cultivadas sob Sistema Plantio Direto" ~ "Crops Cultivated under no-till System",
        emitters == "Gado de Leite" ~ "Dairy Cattle",
        emitters == "Florestas Plantadas" ~ "Planted Forests",
        emitters == "Feijao" ~ "Beans",
        emitters == "Caprino" ~ "Goat",
        emitters == "Bubalino" ~ "Buffalo",
        emitters == "Asinino" ~ "Asinine",
        emitters == "Aplicacao de Ureia" ~ "Urea Application"
      )) %>%
      dplyr::mutate(direct_indirect_emitters = dplyr::case_when(
        direct_indirect_emitters == "Diretas" ~ "Direct",
        direct_indirect_emitters == "Indiretas" ~ "Indirect"
      )) %>%
      dplyr::mutate(emissions_type = dplyr::case_when(
        emissions_type == "Aplicacao de residuos organicos" ~ "Application of Organic Waste",
        emissions_type == "Deposicao de dejetos em pastagem" ~ "Pasture Waste Disposal",
        emissions_type == "Lixiviacao" ~ "Leaching",
        emissions_type == "Outros" ~ "Others",
        emissions_type == "Solos organicos" ~ "Organic Soils",
        emissions_type == "Deposicao Atmosferica" ~ "Atmospheric Deposition",
        emissions_type == "Fertilizantes Sinteticos" ~ "Synthetic Fertilizers",
        emissions_type == "Mineralizacao de N associado a perda de C no solo" ~ "Mineralization of N Associated with Loss of C in the Soil",
        emissions_type == "Residuos Agricolas" ~ "Agricultural Waste",
        emissions_type == "Variacao dos Estoques de Carbono no Solo" ~ "Changes in Carbon Soil Stocks"
      )) %>%
      dplyr::mutate(emission_removal_bunker = dplyr::case_when(
        emission_removal_bunker == "Emissao" ~ "Emission",
        emission_removal_bunker == "Emissao NCI" ~ "NCI Emission",
        emission_removal_bunker == "Remocao NCI" ~ "NCI Removal"
      )) %>%
      dplyr::mutate(product = dplyr::case_when(
        product == "ALIM_BEBIDAS" ~ "Food/Beverages",
        product == "CAR" ~ "Meat",
        product == "CAR/LEI" ~ "Meat/Dairy",
        product == "CAR/LEI/ALIM_BEBIDAS" ~ "Meat/Dairy/Food/Beverages",
        product == "LEI" ~ "Dairy"
      ))

    dat <- dat %>%
      dplyr::relocate(
        year, state, sector, emissions_generating_processes, emissions_sources, emitters, emissions_type, gas, economic_activity, product,
        value
      )

    dat <- dat %>%
      dplyr::mutate(state = dplyr::case_when(
        state == "NA" ~ NA_character_,
        TRUE ~ state
      ))
  }

  return(dat)
}
