
download_br_data = function(data=NULL,dataset=NULL,source='ibge',geo_id='municipality',time_id=2017:2018){

  ## Initialize Parameters

  param = list()
  param$dataset = data
  param$source = source
  param$geo_level

  if (param$source == 'ibge'){

    ###################
    ## Municipal GDP ##
    ###################

    # https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas

    ################
    ## Population ##
    ################

    # https://sidra.ibge.gov.br/pesquisa/estimapop/tabelas

    #######################
    ## Labor Market Info ##
    #######################

    # CEMPRE - https://sidra.ibge.gov.br/pesquisa/cempre/quadros/brasil/2018

    ######################
    ## Demographic Info ##
    ######################

    # https://sidra.ibge.gov.br/pesquisa/censo-demografico/series-temporais/series-temporais/


    #########
    ## PAM ##
    #########

    # https://sidra.ibge.gov.br/pesquisa/pam/tabelas

    if (param$dataset == 'pam_all_crops'){}
    if (param$dataset == 'pam_permanent_crops'){}
    if (param$dataset == 'pam_temporary_crops'){}
    if (param$dataset == 'pam_corn'){}
    if (param$dataset == 'pam_potato'){}
    if (param$dataset == 'pam_peanut'){}
    if (param$dataset == 'pam_beans'){}

    #########
    ## PPM ##
    #########

    # https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019

    ##########
    ## PEVS ##
    ##########

    # https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019


    ############
    ## Call Function that make the download
    ###


  }

  ##

  if (param$source != 'ibge'){

    ## Download from Site

    ## We need to have a database with the official links of the sites in which the data is stored

    ###########
    ## COMEX ##
    ###########

    if (param$dataset == 'comex_import_product'){}

    if (param$dataset == 'comex_import_municipality'){}

    if (param$dataset == 'comex_export_firm_product'){}

    if (param$dataset == 'comex_export_firm_municipality'){}

    ##########
    ## INPE ##
    ##########

    # http://www.dpi.inpe.br/DPI/

    # http://terrabrasilis.dpi.inpe.br/downloads/

    #http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/prodes

    if (param$dataset == 'prodes'){}

    # www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad/acesso-ao-dados-do-degrad
    # Degrad was discontinued in 2016 - Check Deter-B

    # http://www.inpe.br/cra/projetos_pesquisas/deter.php

    #http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter

    if (param$dataset == 'deter'){}

    if (param$dataset == 'degrad'){

    cat('Do you want degradation data? Try using the Deter function!')

    }

    ###############
    ## MapBiomas ##
    ###############

    ###########
    ## IBAMA ##
    ###########

    # https://servicos.ibama.gov.br/ctf/publico/areasembargadas/ConsultaPublicaAreasEmbargadas.php

    # Enviromental Fines

    ##########
    ## SEEG ##
    ##########

    #########
    ## IPS ##
    #########

    # http://www.ipsamazonia.org.br/assets/IPS_Tabela_Completa-8bb3b841e46c8fb17b0331d8ea92bef3.xlsx

    #############
    ## Sigmine ##
    #############

    ##########
    ## RAIS ##
    ##########

    # ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/












  }

  ##

}

sidra_download = function(sidra_code = NULL,time_period,geo_id = 'municipality'){

  # ----------------------------------------------------

  ## Download from Sidra IBGE

  ## Include Progress Bar
  ## Omit Warnings
  ## We should include support for microregion/mesoregion

  # ------------------------------------------------------

  if (geo_id == 'country'){geo_reg = 'Brazil'}
  if (geo_id == 'region'){geo_reg = 'Region'}
  if (geo_id == 'state'){geo_reg = 'State'}
  if (geo_id == 'municipality'){geo_reg = 'City'}

  ##########################
  ## Get Brazilian States ##
  ##########################

  uf = geobr::read_state(year=2010,simplified=TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(code_state) %>%
    tibble::as_tibble()

  uf = uf$code_state

  #################
  ## Downloading ##
  #################

  ## If data is requested:
    ## i) At the country, region or state level, we just download all the units together.
    ## ii) At the municipality level, we attempt to download all the municipalities in a given
    ## state, mesoregion and microregion, respectively


  if (geo_id %in% c('country','region','state')){


    ## Download all units together

  }

  if (geo_id == 'municipality'){

    ## 1) Filter by UF

    ## 2) Filter by MesoRegion

    ## 3) Filter by MicroRegion

  }


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
    dat = input_munic %>%
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

  ## Binding Rows

  dat  = dat[boolean_downloaded] %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  #############################
  ## Downloading Second Data ##
  #############################

  ## Download MesoRegion Info

  meso = geobr::read_meso_region(year=2010,simplified=TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(code_meso,code_state) %>%
    tibble::as_tibble()

  ## Selecting Municipalities we need to download manually

  new_download = meso[meso$code_state %in% as.character(unique(prob_data$x)), ]

  ## Creating Grid

  input_df = expand.grid(
    x=unique(new_download$code_meso),
    y=param$time_period
  )

  input_munic = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))

  if (geo_level == 'municipality'){
    new_dat = input_munic %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y,geo.filter = list("MesoRegion" = x)))
  }

  new_dat = lapply(new_dat,"[[", 1)

  boolean_downloaded = unlist(lapply(new_dat,is.data.frame))

  prob_data = input_df[!boolean_downloaded,]

  new_dat  = new_dat[boolean_downloaded] %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  dat = dplyr::bind_rows(dat,new_dat)

  rm(new_dat)


  ################
  ## Third Data ##
  ################

  ## Download MicroRegion Info

  micro = geobr::read_micro_region(year=2010,simplified=TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(code_micro,code_state) %>%
    tibble::as_tibble()

  ## Selecting Municipalities we need to download manually

  uf_need = unique(new_download$code_state[new_download$code_meso %in% unique(prob_data$x)])

  new_download = micro[micro$code_state %in% uf_need, ]

  ## Creating Grid

  input_df = expand.grid(
    x=unique(new_download$code_micro),
    y=param$time_period
  )

  input_munic = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))

  if (geo_level == 'municipality'){
    new_dat2 = input_munic %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y,geo.filter = list("MicroRegion" = x)))
  }

  new_dat2 = lapply(new_dat2,"[[", 1)

  boolean_downloaded = unlist(lapply(new_dat2,is.data.frame))

  prob_data = input_df[!boolean_downloaded,]

  new_dat2  = new_dat2[boolean_downloaded] %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  dat = dplyr::bind_rows(dat,new_dat2)

  rm(new_dat2)

}

external_download = function()

datasets_link = function(){

  link = tibble::tribble(~survey,~dataset,~sidra_code,~available_time,~available_geo,~link,
                         'PAM-IBGE','pam_all_crops',5457,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_permanent_crops',1613,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_temporary_crops',1612,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_corn',839,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_potato',1001,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_peanut',1000,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',
                         'PAM-IBGE','pam_beans',1002,NA,NA,'https://sidra.ibge.gov.br/pesquisa/pam/tabelas',

                         'PPM-IBGE','ppm_livestock_inventory',3939,NA,NA,'https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019',
                         'PPM-IBGE','ppm_cow_farming',94,NA,NA,'https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019',
                         'PPM-IBGE','ppm_sheep_farming',74,NA,NA,'https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019',
                         'PPM-IBGE','ppm_animal_origin_production',3939,NA,NA,'https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019',
                         'PPM-IBGE','ppm_aquaculture',3940,NA,NA,'https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019',

                         'PEVS-IBGE','pevs_forestry',289,'1986-2019','Country, State, Municipality','https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019',
                         'PEVS-IBGE','pevs_silviculture',291,'1986-2019',NA,'https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019',
                         'PEVS-IBGE','pevs_forest_area',5930,'2013-2019',NA,'https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019'
                         )

  return(link)

}
