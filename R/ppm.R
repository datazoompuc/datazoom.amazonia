#' @title load_ppm_ibge
#'
#' @description Download Municipal Livestock Production (Producao Pecuaria Municipal - IBGE)
#'
#'
#' @param type A \code{string} with the desired PPM Dataset (see \url{https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019}). Accept the IBGE PPM codes or names. 94 ~ "cow_farm", 3939 ~ "cattle_number", 74 ~ "animal_orig_prod", 3940 ~ "water_orig_prod"
#' @param years A \code{sequence} of integers in the form year_begin:year_end in which both are numeric. Available time frame is from 1974 to 2019
#' @param aggregation_level A \code{string} containing the data aggregation level of the output. Can be 'country', 'region', 'state' or 'municipality'
#' @param language A \code{string} with the language of the desired output. Can be 'eng' or 'pt' for English or Portuguese.
#'
#' @return A panel (\code{tibble} format) with N x T observations in which N is the number of geographical units and T is the number of years selected.
#' @export load_ppm
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @examples \dontrun{load_ppm(type=3939,years = 2018:2019,aggregation_level = 'municipality',language='eng')}

load_ppm = function(type=NULL,years=2019,aggregation_level = "municipality",language = 'pt'){

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  param$years = years

  ## Dataset

  if (is.null(type)){stop('Missing Dataset!')}

  if (as.numeric(type) == 94 | type == 'cow_farm'){
    param$type = 94
    param$data_name = 'Cow Farming (Vacas ordenhadas - Cabecas)'
  }
  if (as.numeric(type) == 95 | type == 'sheep_farm'){
    param$type = 95
    param$data_name = 'Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas)'
    stop('Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas) is empty in this dataset! Available in 74 (Animal Origin Production)')
  }
  if (as.numeric(type) == 3939 | type == 'cattle_number'){
    param$type = 3939
    param$data_name = 'Cattle Number (Efetivo dos rebanhos)'
  }
  if (as.numeric(type) == 74 | type == 'animal_orig_prod'){
    param$type = 74
    param$data_name = 'Animal Origin Production (Producao de origem animal, por tipo de produto)'
  }
  if (as.numeric(type) == 3940 | type == 'water_orig_prod'){
    param$type = 3940
    param$data_name = 'Water Origin Production - Fish and others (Producao da aquicultura - peixes e outros)'
  }

  ## Aggregation Level

  if (aggregation_level == 'country'){param$geo_reg = 'Brazil'}
  if (aggregation_level == 'region'){param$geo_reg = 'Region'}
  if (aggregation_level == 'state'){param$geo_reg = 'State'}
  if (aggregation_level == 'municipality'){param$geo_reg = 'City'}

  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################

  input_df = expand.grid(
    x=param$uf,
    y=param$years
  )

  input = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))

  ###############
  ## Load Data ##
  ###############

  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid

  if (aggregation_level %in% c('country','region','state')){
    dat = input %>%
      purrr::pmap(function(x,y) sidrar::get_sidra(param$type,geo=param$geo_reg,period = y))
  }

  if (aggregation_level == 'municipality'){
    dat = input %>%
      purrr::pmap(function(x,y) sidrar::get_sidra(param$type,geo=param$geo_reg,period = y,geo.filter = list("State" = x)))
  }

  dat = dat%>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(x){gsub('[^ -~]', '', x)}) ## Remove Special Characters -- NOT BEST SOLUTION!!

  ######################
  ## Data Enginnering ##
  ######################

  ## We bind the datasets stored as elements of a list above, clean the names,
  ## drop unnecessary (duplication of already existing variables) variables and produce a tibble

  if (param$type == 74){
    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_de_origem_animal_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor)) %>%
      dplyr::as_tibble()
  }
  if (param$type == 94){
    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor)) %>%
      dplyr::as_tibble()
  }
  if (param$type == 3939){
    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_rebanho_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor)) %>%
      dplyr::as_tibble()
  }
  if (param$type == 3940){
    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_da_aquicultura_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor)) %>%
      dplyr::as_tibble()
  }

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  if(aggregation_level == 'country'){
    dat$geo_id = dat$brasil
    dat = dplyr::select(dat,-'brasil_codigo',-'brasil')
  }
  if (aggregation_level == 'region'){
    dat$geo_id = dat$grande_regiao
    dat = dplyr::select(dat,-'grande_regiao_codigo',-'grande_regiao')
  }
  if (aggregation_level == 'state'){
    dat$geo_id = dat$unidade_da_federacao_codigo
    dat = dplyr::select(dat,-'unidade_da_federacao_codigo',-'unidade_da_federacao')
  }
  if (aggregation_level == 'municipality'){
    dat$geo_id = dat$municipio_codigo
    dat = dplyr::select(dat,-'municipio',-'municipio_codigo')
  }

  #########################################################
  ## Comment Specific To "74	Producao de origem animal" ##
  #########################################################

  ## The current state of the data is: Production quantities and values are stacked together -- Producao de Origem Animal e Valor da Producao under one variable
  ## We will undo this to make visualization compatible with styling guides
  ## There is also a very important decision to be made here: Data contain duplicates, which we can choose to either sum, take the mean or drop
  ## The code below show this problem in action:

  # dat = dat %>% select(-unidade_de_medida) %>% pivot_wider(id_cols = geo_id:ano,
  #                                                      names_from = variavel:tipo_de_produto_de_origem_animal,
  #                                                      values_from=valor,
  #                                                      names_sep = '_x_',
  #                                                      values_fn = length)

  #################################
  ## Simplify names of variables ##
  #################################

  if (param$type == 74){

    if (language == 'pt'){
      dat$variavel = base::ifelse(dat$variavel == 'Produo de origem animal','producao_kg','valor_brl')
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'L'] = 'la'
    }

    if (language == 'eng'){
      dat$variavel = base::ifelse(dat$variavel == 'Produo de origem animal','production_kg','value_brl')
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'L'] = 'wool'
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'Casulos do bicho-da-seda'] = 'silk_worm_cocoons'
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'Leite'] = 'milk'
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'Mel de abelha'] = 'bee_honey'
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'Ovos de codorna'] = 'quail_eggs'
      dat$tipo_de_produto_de_origem_animal[dat$tipo_de_produto_de_origem_animal == 'Ovos de galinha'] = 'chicken_eggs'
    }

  }

  if (param$type == 94){

    if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'Vacas ordenhadas','milked_cows','')}
    if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'Vacas ordenhadas','vacas_ordenhadas','')}

  }

  if (param$type == 3939){

    if(language == 'pt'){
      dat$variavel = base::ifelse(dat$variavel == 'Efetivo dos rebanhos','gado_cabeca','')
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Galinceos - galinhas'] = 'galinaceos_galinha'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Galinceos - total'] = 'galinaceos_total'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Suno - matrizes de sunos'] = 'suino_matrizes'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Suno - total'] = 'suino_total'
    }

    if (language == 'eng'){
      dat$variavel = base::ifelse(dat$variavel == 'Efetivo dos rebanhos','cattle_number','')
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Galinceos - galinhas'] = 'gallinaceous_chicken'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Galinceos - total'] = 'gallinaceous_total'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Suno - matrizes de sunos'] = 'swine_sows'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Suno - total'] = 'swine_total'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Bovino'] = 'bovine'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Bubalino'] = 'buffalo'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Caprinos'] = 'goat'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Codornas'] = 'quail'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Equino'] = 'equine'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'Ovino'] = 'ovine'
    }
  }

  if (param$type == 3940){

    if (language == 'pt'){
      dat$variavel = base::ifelse(dat$variavel == 'Produo da aquicultura','producao_kg','valor_brl')
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Outros peixes'] = 'peixes_outros'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Camaro'] = 'camarao'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Larvas e ps-larvas de camaro'] = 'larvas_camarao'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Ostras, vieiras e mexilhes'] = 'ostra_mexilhoes'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Outros produtos (r, jacar, siri, caranguejo, lagosta, etc)'] = 'outros'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Sementes de moluscos'] = 'molusco_semente'
    }

    if (language == 'eng'){
      dat$variavel = base::ifelse(dat$variavel == 'Produo da aquicultura','production_kg','value_brl')
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Outros peixes'] = 'fish_others'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Camaro'] = 'shrimp'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Larvas e ps-larvas de camaro'] = 'shrimp_larvae'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Ostras, vieiras e mexilhes'] = 'oyester_mussels'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Outros produtos (r, jacar, siri, caranguejo, lagosta, etc)'] = 'others'
      dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Sementes de moluscos'] = 'shellfish_seeds'
    }

    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Matrinx'] = 'matrinxa'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Pacu e patinga'] = 'pacu'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Pintado, cachara, cachapira e pintachara, surubim'] = 'pintado'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Tambacu, tambatinga'] = 'tambacu'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Tilpia'] = 'tilapia'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Trara e trairo'] = 'traira'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Curimat, curimbat'] = 'curimata'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Jatuarana, piabanha e piracanjuba'] = 'jatuarana'
    dat$tipo_de_produto_da_aquicultura[dat$tipo_de_produto_da_aquicultura == 'Piau, piapara, piauu, piava'] = 'piau'
  }

  ######################
  ## Harmonize Values ##
  ######################

  ## Convert values to something comparable across different units (Dozens and Units to Kg based on product type)

  # Valor is always in 1k R$ and production can be in 1k dozens, 1k liters or kgs. We standardize to make comparison possible
  # Dozen is used for eggs, with a mean size of 50 to 54g per unit (https://www.ovoonline.com.br)
  # Liter is used for milk, with 1,032 g/ml https://www.agencia.cnptia.embrapa.br/Agencia8/AG01/arvore/
  # Milheiro is used for recent-born fish - We use 2g per unit (https://www.revistas.ufg.br/vet/article/view/1472/8597)

  dat$valor = base::as.numeric(dat$valor)

  dat$valor = base::ifelse(dat$unidade_de_medida == 'Mil dzias', (((dat$valor/(12*1e3))*52)/1e3),dat$valor) # 1k Dozen to Kg
  dat$valor = base::ifelse(dat$unidade_de_medida == 'Mil litros',((dat$valor/1e3)*1.032) ,dat$valor) # 1k Liters to Kg
  dat$valor = base::ifelse(dat$unidade_de_medida == 'Milheiros', dat$valor*1e3*2/1e3 ,dat$valor) # 1 Milheiro to Kg

  #############################
  ## Create Long Format Data ##
  #############################

  ## The Output is a tibble with unit and year identifiers + production and/or value of each item

  if (param$type == 74){ ## Animal Origin Production
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel:tipo_de_produto_de_origem_animal,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  if (param$type == 94){ ## Cow Farming
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  if (param$type == 3939){ ## Cattle Number
    dat = dat %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel:tipo_de_rebanho,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  if (param$type == 3940){ ## Fish Production
    dat = dat %>%
      dplyr::filter(unidade_de_medida != 'Nenhuma' & unidade_de_medida != '') %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel:tipo_de_produto_da_aquicultura,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  #####################
  ## Change Language ##
  #####################

  if (language == 'eng'){names(dat)[which(names(dat) == 'ano')] = 'year'}

  ############
  ## Return ##
  ############

  return(dat)


}
