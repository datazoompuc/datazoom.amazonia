#' @title load_ppm_ibge
#'
#' @description Download Municipal Livestock Production (Producao Pecuaria Municipal - IBGE)
#'
#'
#' @param dataset A code or name indicating the dataset to be downloaded (ADD MORE INFO)
#' @param geo_level A \code{string} containing the data aggregation level of the output. Can be 'country', 'region', 'state' or 'municipality'
#' @param time_period A \code{sequence} of integers in the form year_begin:year_end in which both are numeric. Available time frame is from 1974 to 2019
#' @param language A \code{string} with the language of the desired output. Can be 'eng' or 'pt' for English or Portuguese.
#'
#' @return A panel (\code{tibble} format) with N x T observations in which N is the number of geographical units and T is the number of time_period selected.
#' @export load_ppm
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @examples \dontrun{load_ppm(type=3939,time_period = 2018:2019,geo_level = 'municipality',language='eng')}

load_ppm = function(dataset=NULL,geo_level = "municipality",time_period=2019,language = 'pt'){

  ## Bind Global Variables

  sidra_code <- NULL
  nivel_territorial_codigo<- NULL
  nivel_territorial<- NULL
  unidade_de_medida_codigo<- NULL
  variavel_codigo<- NULL
  ano_codigo<- NULL
  valor<- NULL
  geo_id<- NULL
  ano<- NULL
  tipo_de_rebanho<- NULL
  variavel<- NULL
  unidade_de_medida<- NULL
  tipo_de_produto_de_origem_animal<- NULL
  tipo_de_produto_da_aquicultura<- NULL

  # Adjust Code Based on State Level Information
  # Check if any observation geo-time at the final data have multiple NA entries -- this would mean the data is "wrong"
  # Double Check Translation

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language

  if (!is.numeric(param$dataset)){
    param$code = datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {param$code = param$dataset}

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}

  if (param$code == 94){
    param$data_name = 'Cow Farming (Vacas ordenhadas - Cabecas)'
  }
  if (param$code == 95){
    param$data_name = 'Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas)'
  }
  if (param$code == 3939){
    param$data_name = 'Cattle Number (Efetivo dos rebanhos)'
  }
  if (param$code == 74){
    param$data_name = 'Animal Origin Production (Producao de origem animal, por tipo de produto)'
  }
  if (param$code == 3940){
    param$data_name = 'Water Origin Production - Fish and others (Producao da aquicultura - peixes e outros)'
  }

  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  dat = as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num){
      #suppressMessages(
      sidra_download(sidra_code = param$code,year = year_num,geo_level = param$geo_level)
      #)
    }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

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



  ######################
  ## Data Enginnering ##
  ######################

  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    dplyr::mutate_all(clean_custom)

  ## The Code Below Depends on the type of data

  dat = dat %>%
    dplyr::select(-c(nivel_territorial_codigo,nivel_territorial,
                     unidade_de_medida_codigo,variavel_codigo,
                     ano_codigo)) %>%
    dplyr::mutate(valor=as.numeric(valor))




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



  #############################################
  ## Adding Measure Information to Variables ##
  #############################################

  ################################
  ## Harmonizing Variable Names ##
  ################################

  ########################
  ## Measure Conversion ##
  ########################

  ## Convert values to something comparable across different units (Dozens and Units to Kg based on product dataset)

  # Valor is always in 1k R$ and production can be in 1k dozens, 1k liters or kgs. We standardize to make comparison possible
  # Dozen is used for eggs, with a mean size of 50 to 54g per unit (https://www.ovoonline.com.br)
  # Liter is used for milk, with 1,032 g/ml https://www.agencia.cnptia.embrapa.br/Agencia8/AG01/arvore/
  # Milheiro is used for recent-born fish - We use 2g per unit (https://www.revistas.ufg.br/vet/article/view/1472/8597)

  ## Animal Origin Production

  if (param$code == 74){

    dat$valor = base::ifelse(dat$unidade_de_medida == 'mil_duzias', (((dat$valor/(12*1e3))*52)/1e3),dat$valor) # 1k Dozen to Kg
    dat$valor = base::ifelse(dat$unidade_de_medida == 'mil_litros',((dat$valor/1e3)*1.032) ,dat$valor) # 1k Liters to Kg
  }

  ## Water Origin Production

  if (param$code == 3940){

    dat$valor = base::ifelse(dat$unidade_de_medida == 'milheiros', dat$valor*1e3*2/1e3 ,dat$valor) # 1 Milheiro to Kg

  }




  #################
  ## Translation ##
  #################

  # I NEED TO RENAME THE VARIABLES!!!!

  ## 1
    ## We bind the datasets stored as elements of a list above, clean the names,
    ## drop unnecessary (duplication of already existing variables) variables and produce a tibble

  ## 2 Variable Value Translation
    ##
    ##

  ## Cattle Number

    ## Rewrite in Tidyverse Syntax!!

  if (param$code == 3939){

    ## Selecting Variables

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_rebanho_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

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

  if (param$code == 95){
    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

    if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','sheep_farmed','')}
    if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','ovinos_tosquiados','')}

  }

  ## Animal Origin Production

  if (param$code == 74){

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_de_origem_animal_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

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

  if (param$code == 94){

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))


    if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','number_milked_cows','')}
    if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','vacas_ordenhadas_num','')}

  }

  ## Water Origin Production

    ## We need to finish the names translation!

  if (param$code == 3940){

    ## Selecting variables

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_da_aquicultura_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

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




  #############################
  ## Create Long Format Data ##
  #############################

  # ==> The Output is a tibble with unit and year identifiers + production and/or value of each item

  # Warning: A double check on this aggregation (despite the unique identifiers established) would be welcomed!

  ## Cattle Number

  if (param$code == 3939){
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

  if (param$code == 95){
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

  if (param$code == 74){ ## Animal Origin Production
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

  if (param$code == 94){ ## Milked Cows
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

  if (param$code == 3940){
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
