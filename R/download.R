
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

  ##############################
  ## Setting Basic Parameters ##
  ##############################

  param = list()

  param$sidra_code = sidra_code
  param$time_period = time_period

  if (geo_id == 'country'){param$geo_reg = 'Brazil'}
  if (geo_id == 'region'){param$geo_reg = 'Region'}
  if (geo_id == 'state'){param$geo_reg = 'State'}
  if (geo_id == 'municipality'){param$geo_reg = 'City'}

  ##########################
  ## Get Brazilian States ##
  ##########################

  uf = geobr::read_state(year=2010,simplified=TRUE,showProgress = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(code_state) %>%
    tibble::as_tibble()

  uf = uf$code_state

  param$uf = uf


  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################

  input_df = expand.grid(
    geo=param$uf,
    time=param$time_period
  )

  input_munic = list(geo = as.list(input_df$geo),time = as.list(as.character(input_df$time)))
  input_other = list(geo = as.list(as.character(param$time_period)))

  ###############
  ## Load Data ##
  ###############

  get_sidra_safe = purrr::safely(sidrar::get_sidra)

  ## We will need to check for the ones not considered in the loop afterwards

  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid

  if (param$geo_reg %in% c('Brazil','Region','State')){

    ## Download all units together

    base::message(base::cat('Downloading Data:',length(param$time_period),'API requests')) ## Show Message

    ## Download

    dat = input_other %>%
      purrr::pmap(function(geo) get_sidra_safe(param$type,geo=param$geo_reg,period = geo))

    ## Completion!

    base::message(base::cat('Download Completed! Starting Data Processing...'))

  }

  if (param$geo_reg == 'City'){

    # ----------------------------

    ## 1) Filter by UF

    ## 2) Filter by MesoRegion

    ## 3) Filter by MicroRegion

    # ----------------------------

    #base::message(base::cat('Downloading Data:',length(param$time_period),'API requests'))
    base::message(base::cat('Attempting to download data at the state level:',length(param$uf),'API requests'))

    ## Download UF

    dat_uf = input_munic %>%
      purrr::pmap(function(geo,time){

        base::suppressMessages(get_sidra_safe(x = param$sidra_code,
                                                    geo = param$geo_reg, # Municipality Level - City
                                                    period = time,
                                                    geo.filter = list("State" = geo)))
        base::message(base::cat(which(geo == unique(input_df$geo)),'of',length(unique(input_df$geo))))
      })

    dat_uf = base::lapply(dat_uf,"[[", 1)

    missed_uf = param$uf[which(unlist(lapply(dat_uf,is.data.frame)) == FALSE)]

    if (length(missed_uf) > 0){
      base::message(base::cat(length(missed_uf),
                              'missed API requests at the state level. Moving to more disaggregated requests...'))
    }
    if (length(missed_uf) == 0){base::message(base::cat('Download Succesfully Completed!'))}

    dat_uf = dat_uf[unlist(lapply(dat_uf,is.data.frame))] %>% ## Filter for only found dataframes
      dplyr::bind_rows() %>%
      tibble::as_tibble()

    ###################
    ## Run if needed ##
    ###################

    if (length(missed_uf) > 0){

      # ## Meso Region
      #
      # meso = geobr::read_meso_region(code_meso = missed_uf,year=2010,simplified=TRUE) %>%
      #   sf::st_drop_geometry() %>%
      #   dplyr::select(code_state,code_meso) %>%
      #   tibble::as_tibble()
      #
      # input_meso = expand.grid(
      #   geo=meso$code_meso,
      #   time=param$time_period
      # )
      #
      # input_meso = list(geo = as.list(input_meso$geo),time = as.list(as.character(input_meso$time)))
      #
      # dat_meso = input_meso %>%
      #   purrr::pmap(function(geo,time) get_sidra_safe(x = param$sidra_code,
      #                                                 geo = param$geo_reg, # Municipality Level - City
      #                                                 period = time,
      #                                                 geo.filter = list("MesoRegion" = geo))) %>%
      #   base::lapply("[[", 1)
      #
      # missed_meso = meso[which(unlist(lapply(dat_meso,is.data.frame)) == FALSE),]
      #
      # dat_meso = dat_meso[unlist(lapply(dat_meso,is.data.frame))] %>% ## Filter for only found dataframes
      #   dplyr::bind_rows() %>%
      #   tibble::as_tibble()



    }

    #if (length(missed_meso) > 0){

    if (length(missed_uf) > 0){

      ## Micro Region

      micro = geobr::read_micro_region(code_micro = missed_uf,year=2010,simplified=TRUE) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(code_state,code_micro) %>%
        tibble::as_tibble()

      base::message(base::cat('Attempting to download data at the Micro Region Level:\n',length(micro$code_micro),'API requests'))

      input_micro = expand.grid(
        geo=micro$code_micro,
        time=param$time_period
      )

      input_micro = list(geo = as.list(input_micro$geo),time = as.list(as.character(input_micro$time)))

      dat_micro = input_micro %>%
        purrr::pmap(function(geo,time){

          base::suppressMessages(get_sidra_safe(x = param$sidra_code,
                                                geo = param$geo_reg, # Municipality Level - City
                                                period = time,
                                                geo.filter = list("MicroRegion" = geo))
          )
          base::message(base::cat(which(geo == micro$code_micro),'of',length(micro$code_micro)))
        }) %>%
        base::lapply("[[", 1)

      missed_micro = micro[which(unlist(lapply(dat_micro,is.data.frame)) == FALSE),]

      if (length(missed_micro) > 0){
        base::message(base::cat(length(missed_micro),
                                'missed API requests at the Micro Region level. Please report this problem to package developers...'))
      }
      if (length(missed_micro) == 0){base::message(base::cat('Download Succesfully Completed!'))}

      dat_micro = dat_micro[unlist(lapply(dat_micro,is.data.frame))] %>% ## Filter for only found dataframes
        dplyr::bind_rows() %>%
        tibble::as_tibble()

      dat = dplyr::bind_rows(dat_uf,dat_micro)

      rm(dat_uf,dat_micro)



      # micro = geobr::read_micro_region(code_micro = unique(missed_meso$code_state),year=2010,simplified=TRUE) %>%
      #   sf::st_drop_geometry() %>%
      #   dplyr::select(code_state,code_micro) %>%
      #   tibble::as_tibble()
      #
      # input_micro = expand.grid(
      #   geo=micro$code_micro,
      #   time=param$time_period
      # )
      #
      # input_micro = list(geo = as.list(input_micro$geo),time = as.list(as.character(input_micro$time)))
      #
      # dat_micro = input_micro %>%
      #   purrr::pmap(function(geo,time) get_sidra_safe(x = param$sidra_code,
      #                                                 geo = param$geo_reg, # Municipality Level - City
      #                                                 period = time,
      #                                                 geo.filter = list("microRegion" = geo))) %>%
      #   base::lapply("[[", 1)
      #
      # missed_micro = micro[which(unlist(lapply(dat_micro,is.data.frame)) == FALSE),]
      #
      # dat_micro = dat_micro[unlist(lapply(dat_micro,is.data.frame))] %>% ## Filter for only found dataframes
      #   dplyr::bind_rows() %>%
      #   tibble::as_tibble()



    }

    ################
    ## Next Steps ##
    ################

    if (length(missed_uf == 0)){dat = dat_uf}


  }

  #################
  ## Downloading ##
  #################

  ## If data is requested:
  ## i) At the country, region or state level, we just download all the units together.
  ## ii) At the municipality level, we attempt to download all the municipalities in a given
  ## state, mesoregion and microregion, respectively





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
