#' @title load_ppm_ibge
#'
#' @description Download Municipal Livestock Production (Producao Pecuaria Municipal - IBGE)
#'
#'
#' @param type A \code{string} with the desired PPM Dataset (see \url{https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019}). Accept the IBGE PPM codes or names. 94 ~ "cow_farm", 3939 ~ "cattle_number", 74 ~ "animal_orig_prod", 3940 ~ "water_orig_prod"
#' @param time_period A \code{sequence} of integers in the form year_begin:year_end in which both are numeric. Available time frame is from 1974 to 2019
#' @param geo_level A \code{string} containing the data aggregation level of the output. Can be 'country', 'region', 'state' or 'municipality'
#' @param language A \code{string} with the language of the desired output. Can be 'eng' or 'pt' for English or Portuguese.
#'
#' @return A panel (\code{tibble} format) with N x T observations in which N is the number of geographical units and T is the number of time_period selected.
#' @export load_ppm
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @examples \dontrun{load_ppm(type=3939,time_period = 2018:2019,geo_level = 'municipality',language='eng')}

load_ppm = function(type=NULL,geo_level = "municipality",time_period=2019,language = 'pt'){


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  param$time_period = time_period

  ## Dataset

  if (is.null(type)){stop('Missing Dataset!')}

  if (as.numeric(type) == 94 | type == 'cow_farm'){
    param$type = 94
    param$data_name = 'Cow Farming (Vacas ordenhadas - Cabecas)'
  }
  if (as.numeric(type) == 95 | type == 'sheep_farm'){
    param$type = 95
    param$data_name = 'Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas)'
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

  if (geo_level == 'country'){param$geo_reg = 'Brazil'}
  if (geo_level == 'region'){param$geo_reg = 'Region'}
  if (geo_level == 'state'){param$geo_reg = 'State'}
  if (geo_level == 'municipality'){param$geo_reg = 'City'}

  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################

  input_df = expand.grid(
    x=param$uf,
    y=param$time_period
  )

  input_munic = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))
  input_other = list(x = as.list(as.character(param$time_period)))

  ###############
  ## Load Data ##
  ###############

  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid

  get_sidra_safe = purrr::safely(sidrar::get_sidra)

  ## We will need to check for the ones not considered in the loop afterwards

  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid

  if (geo_level %in% c('country','region','state')){

    base::message(base::cat('Downloading Data:',length(param$time_period),'API requests')) ## Show Message

    ## Download

    dat = input_other %>%
      purrr::pmap(function(x) get_sidra_safe(param$type,geo=param$geo_reg,period = x))

    ## Completion!

    base::message(base::cat('Download Completed! Starting Data Processing...'))

  }

  if (geo_level == 'municipality'){
    dat = input %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y,geo.filter = list("State" = x)))
  }

  ## Capture Errors

  ## Daniel Comment: The main point here is that the Sidra API has a limit for requests. Thus, for states with a large number
  ## of municipalities the request may break. We used the purrr::safely to not break the loop (or map), but we will need to
  ## deal with these problems in this step here. Now its under construction =)

  dat = lapply(dat,"[[", 1)

  boolean_downloaded = unlist(lapply(dat,is.data.frame))

  prob_data = input_df[!boolean_downloaded,]

  ## We need to check the municipalities in these UFs

  #######################
  ## Cleaning Function ##
  #######################


  clean_custom = function(var){
    var = stringr::str_replace_all(string=var,pattern=' - ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern=', ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='ª',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_to_lower(string=var)
    return(var)
  }

  ## Binding Rows

  dat  = dat[boolean_downloaded] %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()


  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    dplyr::mutate_all(clean_custom)

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  if(geo_level == 'country'){
    dat$geo_id = dat$brasil
    dat = dplyr::select(dat,-'brasil_codigo',-'brasil')
  }
  if (geo_level == 'region'){
    dat$geo_id = dat$grande_regiao
    dat = dplyr::select(dat,-'grande_regiao_codigo',-'grande_regiao')
  }
  if (geo_level == 'state'){
    dat$geo_id = dat$unidade_da_federacao_codigo
    dat = dplyr::select(dat,-'unidade_da_federacao_codigo',-'unidade_da_federacao')
  }
  if (geo_level == 'municipality'){
    dat$geo_id = dat$municipio_codigo
    dat = dplyr::select(dat,-'municipio',-'municipio_codigo')
  }

  ######################
  ## Data Enginnering ##
  ######################

  ## 1
    ## We bind the datasets stored as elements of a list above, clean the names,
    ## drop unnecessary (duplication of already existing variables) variables and produce a tibble

  ## 2 Variable Value Translation
    ##
    ##

  ## Cattle Number

    ## Rewrite in Tidyverse Syntax!!

  if (param$type == 3939){

    ## Selecting Variables

    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_rebanho_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor))

    ## Translating Names

    if(language == 'pt'){
      dat$variavel = base::ifelse(dat$variavel == 'efetivo_dos_rebanhos','gado_cabeca','')
    }

    if (language == 'eng'){
      dat$variavel = base::ifelse(dat$variavel == 'efetivo_dos_rebanhos','cattle_number','')
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'galinaceos_galinhas'] = 'gallinaceous_chicken'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'galinaceos_total'] = 'gallinaceous_total'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'suino_matrizes_de_suinos'] = 'swine_sows'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'suino_total'] = 'swine_total'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'bovino'] = 'bovine'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'bubalino'] = 'buffalo'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'caprino'] = 'goat'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'codornas'] = 'quail'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'equino'] = 'equine'
      dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'ovino'] = 'ovine'
    }
  }

  ## Sheep Farm

    ## Include number as "cabecas" measure

  if (param$type == 95){
    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor))

    if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','sheep_farmed','')}
    if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','ovinos_tosquiados','')}

  }

  ## Animal Origin Production

  if (param$type == 74){

    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_de_origem_animal_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor))

    if (language == 'pt'){

      dat = dat %>%
        dplyr::mutate(

          variavel = dplyr::case_when(
            variavel == 'producao_de_origem_animal' ~ 'producao_kg',
            variavel == 'valor_da_producao' ~ 'valor_brl')
        )

    }

    if (language == 'eng'){

      dat = dat %>%
        dplyr::mutate(

          variavel = dplyr::case_when(
            variavel == 'producao_de_origem_animal' ~ 'production_kg',
            variavel == 'valor_da_producao' ~ 'value_brl'),

          tipo_de_produto_de_origem_animal = dplyr::case_when(
            tipo_de_produto_de_origem_animal == 'la' ~ 'wool',
            tipo_de_produto_de_origem_animal == 'casulos_do_bichodaseda' ~ 'silk_worm_cocoons',
            tipo_de_produto_de_origem_animal == 'leite' ~ 'milk',
            tipo_de_produto_de_origem_animal == 'mel_de_abelha' ~ 'bee_honey',
            tipo_de_produto_de_origem_animal == 'ovos_de_codorna' ~ 'quail_eggs',
            tipo_de_produto_de_origem_animal == 'ovos_de_galinha' ~ 'chicken_eggs')

          )

    }

  }

  ## Milked Cows

  if (param$type == 94){

    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor))


    if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','number_milked_cows','')}
    if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','vacas_ordenhadas_num','')}

  }

  ## Water Origin Production

    ## We need to finish the names translation!

  if (param$type == 3940){

    ## Selecting variables

    dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
                                -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_da_aquicultura_codigo',-'unidade_de_medida_codigo') %>%
      dplyr::mutate(valor = as.numeric(valor))

    ## Translation

    if (language == 'pt'){

      dat = dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            variavel == 'producao_da_aquicultura' ~  'producao_kg',
            variavel == 'valor_da_producao' ~ 'valor_brl',
            TRUE ~ variavel
          )
        )
    }

    if (language == 'eng'){

      dat = dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            variavel == 'producao_da_aquicultura' ~  'production_kg',
            variavel == 'valor_da_producao' ~ 'value_brl',
            TRUE ~ variavel
        ),
        tipo_de_produto_da_aquicultura = dplyr::case_when(
          tipo_de_produto_da_aquicultura == 'outros_peixes' ~ 'fish_others',
          tipo_de_produto_da_aquicultura == 'camarao' ~ 'shrimp',
          tipo_de_produto_da_aquicultura == 'larvas_e_poslarvas_de_camarao' ~ 'shrimp_larvae',
          tipo_de_produto_da_aquicultura == 'ostras_vieiras_e_mexilhoes' ~ 'oyester_mussels',
          tipo_de_produto_da_aquicultura == 'outros_produtos_ra_jacare_siri_caranguejo_lagosta_etc' ~ 'others',
          tipo_de_produto_da_aquicultura == 'sementes_de_moluscos' ~ 'shellfish_seeds',
          TRUE ~ tipo_de_produto_da_aquicultura
        )

      )

    }
  }

  ################################
  ## Harmonize Variables Values ##
  ################################

  ## Convert values to something comparable across different units (Dozens and Units to Kg based on product type)

  # Valor is always in 1k R$ and production can be in 1k dozens, 1k liters or kgs. We standardize to make comparison possible
  # Dozen is used for eggs, with a mean size of 50 to 54g per unit (https://www.ovoonline.com.br)
  # Liter is used for milk, with 1,032 g/ml https://www.agencia.cnptia.embrapa.br/Agencia8/AG01/arvore/
  # Milheiro is used for recent-born fish - We use 2g per unit (https://www.revistas.ufg.br/vet/article/view/1472/8597)

  ## Animal Origin Production

  if (param$type == 74){

    dat$valor = base::ifelse(dat$unidade_de_medida == 'mil_duzias', (((dat$valor/(12*1e3))*52)/1e3),dat$valor) # 1k Dozen to Kg
    dat$valor = base::ifelse(dat$unidade_de_medida == 'mil_litros',((dat$valor/1e3)*1.032) ,dat$valor) # 1k Liters to Kg
  }

  ## Water Origin Production

  if (param$type == 3940){

    dat$valor = base::ifelse(dat$unidade_de_medida == 'milheiros', dat$valor*1e3*2/1e3 ,dat$valor) # 1 Milheiro to Kg

  }

  #############################
  ## Create Long Format Data ##
  #############################

  # ==> The Output is a tibble with unit and year identifiers + production and/or value of each item

  # Warning: A double check on this aggregation (despite the unique identifiers established) would be welcomed!

  ## Cattle Number

  if (param$type == 3939){
    dat = dat %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = c(tipo_de_rebanho,variavel),
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  ## Sheep Farm

  if (param$type == 95){
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  ## Animal Origin Production

  if (param$type == 74){ ## Animal Origin Production
    dat = dat %>%
      dplyr::filter(unidade_de_medida != "") %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = c(tipo_de_produto_de_origem_animal,variavel),
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  ## Milked Cows

  if (param$type == 94){ ## Milked Cows
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  ## Water Origin Production
    ## We need to create a dictionary for this!

  if (param$type == 3940){
    dat = dat %>%
      dplyr::filter(unidade_de_medida != '') %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = c(tipo_de_produto_da_aquicultura,variavel),
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()
  }

  ########################
  ## Changing Year Name ##
  ########################

  if (language == 'eng'){names(dat)[which(names(dat) == 'ano')] = 'year'}

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)


}
