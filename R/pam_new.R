

load_pam = function(type=NULL,geo_level = "municipality", time_period = 2017:2018, language = "eng") {

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$uf = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,
               50,51,52,53)
  param$time_period = time_period

  ## Dataset

  if (is.null(type)){stop('Missing Dataset!')}

  if (as.numeric(type) == 1612){ ## This is a subset of all crops

    param$type = as.numeric(type)
    param$data_name = 'Temporary Crops (Lavouras Temporárias)'
  }

  if (as.numeric(type) == 1613){ ## This is a subset of all crops

    param$type = as.numeric(type)
    param$data_name = 'Permanent Crops (Lavouras Permanente)'
  }

  if (as.numeric(type) == 5457){
    param$type = 5457
    param$data_name = 'All Crops (Lavouras)'
  }


  if (as.numeric(type) %in% c(839,1001,1000,1002)){
    param$type = as.numeric(type)
    param$data_name = 'Crop with more than one harvest (Corn, Potato, Peanut or Beans)'
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
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='ª',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_to_lower(string=var)
    return(var)
  }

  ## Binding Rows

  dat  = dat[boolean_downloaded] %>%
          dplyr::bind_rows() %>%
          tibble::as_tibble()

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

  if (param$type == 5457){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_e_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_temporarias_e_permanentes)
  }

  if (param$type == 1613){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_permanentes)
  }


  if (param$type %in% c(839,1000,1001,1002,1612)){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_codigo,
                  produto_das_lavouras = produto_das_lavouras_temporarias)
  }


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
