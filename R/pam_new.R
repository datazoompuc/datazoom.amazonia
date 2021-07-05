

load_pam = function(dataset=NULL,geo_level = "municipality", time_period = 2017:2018, language = "eng") {


  ## Bind Global Variables

  sidra_code <- nivel_territorial_codigo <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida_codigo <- variavel_codigo <- ano_codigo <- valor <- NULL
  produto_das_lavouras_temporarias_e_permanentes_codigo  <- NULL
  produto_das_lavouras_temporarias_e_permanentes <- NULL
  produto_das_lavouras_permanentes_codigo <- NULL
  produto_das_lavouras_permanentes <- NULL
  produto_das_lavouras_temporarias_codigo <- NULL
  produto_das_lavouras_temporarias <- NULL
  geo_id <- ano <- variavel <- produto_das_lavouras <- NULL

  # Measure Conversion Before Translation
  # Dataset can be either sidra number or name
  # Need to add translation
  # Need to add measure translation
  # Check for old years
  # Check if any observation geo-time at the final data have multiple NA entries -- this would mean the data is "wrong"

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

  if (is.null(dataset)){stop('Missing Dataset!')}

  if (param$code == 1612){ ## This is a subset of all crops
    param$data_name = 'Temporary Crops (Lavouras Temporarias)'
  }

  if (param$code == 1613){ ## This is a subset of all crops
    param$data_name = 'Permanent Crops (Lavouras Permanente)'
  }

  if (param$code == 5457){
    param$data_name = 'All Crops (Lavouras)'
  }


  if (param$code %in% c(839,1001,1000,1002)){
    param$data_name = 'Crop with more than one harvest (Corn, Potato, Peanut or Beans)'
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
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\u00aa',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_replace_all(string=var,pattern='__',replacement='_')
    # var = stringr::str_replace_all(string=var,pattern='.',replacement='_')
    # var = stringr::str_replace_all(string=var,pattern=',',replacement='_')
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

    dat = dat %>%
      dplyr::mutate(variavel = dplyr::case_when(
                      (variavel == 'area_colhida') ~ 'area_colhida_ha',
                      (variavel == 'area_plantada_ou_destinada_a_colheita') ~ 'area_plantada_ou_destinada_colheita_ha',
                      (variavel == 'area_plantada') ~ 'area_plantada_ha',
                      (variavel == 'area_destinada_a_colheita') ~ 'area_destinada_colheita_ha',
                      (variavel == 'quantidade_produzida') ~ 'quant_prod_ton',
                      (variavel == 'rendimento_medio_da_producao') ~ 'rend_medio_prod_kg_per_ha',
                      ## Needs to be adjusted according to different currency systems in Brazil
                      (variavel == 'valor_da_producao') ~ 'valor_da_producao_brl'
                      )
      )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$code == 5457){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_e_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_temporarias_e_permanentes)
  }

  if (param$code == 1613){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_permanentes)
  }


  if (param$code %in% c(839,1000,1001,1002,1612)){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_codigo,
                  produto_das_lavouras = produto_das_lavouras_temporarias)
  }

  #################
  ## Translation ##
  #################


  ########################
  ## Measure Conversion ##
  ########################




  #############################
  ## Create Long Format Data ##
  #############################

  ## The Output is a tibble with unit and year identifiers + production and/or value of each item

    dat = dat %>%
      dplyr::select(-'unidade_de_medida',-'produto_das_lavouras_codigo') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel:produto_das_lavouras,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = 0) %>%
      janitor::clean_names()


  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)

}
