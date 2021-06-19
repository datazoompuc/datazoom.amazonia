
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

sidra_download = function(sidra_code = NULL,year,geo_id = 'municipality'){

  # Obs: Sometimes there are non-catched municipalieis - user should check on IBGE SIDRA
  # f = geo %>% filter(!(code_muni %in% unique(dat_uf$`Município (Código)`)))

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
  param$year = year

  if (geo_id == 'country'){param$geo_reg = 'Brazil'}
  if (geo_id == 'region'){param$geo_reg = 'Region'}
  if (geo_id == 'state'){param$geo_reg = 'State'}
  if (geo_id == 'municipality'){param$geo_reg = 'City'}

  ##################################
  ## Get Geographical Information ##
  ##################################

  geo = base::suppressMessages(geobr::lookup_muni('all'))

  # uf = list('RO' = 11,'AC' = 12,'AM' = 13,'RR' = 14,'PA' = 15,'AP' = 16,'TO' = 17,
  #           'MA' = 21,'PI' = 22,'CE' = 23,'RN' = 24,'PB' = 25,'PE' = 26,'AL' = 27,'SE' = 28,'BA' = 29,
  #           'MG' = 31,'ES' = 32,'RJ' = 33,'SP' = 35,
  #           'PR' = 41,'SC' = 42,'RS' = 43,
  #           'MS' = 50,'MT' = 51,'GO' = 52,'DF' = 53)

  ###############
  ## Load Data ##
  ###############

  get_sidra_safe = purrr::safely(sidrar::get_sidra)


  if (param$geo_reg %in% c('Brazil','Region','State')){

    base::message(base::cat('Downloading Data at the',param$geo_reg,'level')) ## Show Message

    ## Download

    dat = get_sidra_safe(x = param$sidra_code,
                         geo = param$geo_reg,
                         period = as.character(param$year))

    base::message(base::cat('Download Succesfully Completed!'))

  }

  if (param$geo_reg == 'City'){

    ##############################
    ## Download at the UF Level ##
    ##############################

    uf_list = geo %>%
      dplyr::select(code_state) %>%
      unlist() %>%
      unique() %>%
      as.list()

    names(uf_list) = uf_list

    dat_raw_uf = purrr::map(uf_list,function(uf){

        base::message(base::cat(which(uf == uf_list),'in',length(uf_list),'states...\n'))

        suppressMessages(
          get_sidra_safe(x = param$sidra_code,
                         geo = param$geo_reg,
                         period = as.character(param$year),
                         geo.filter = list("State" = uf))
        )

      }
    )

    dat_mod_uf = base::lapply(dat_raw_uf,"[[", 1)

    dat_uf = dat_mod_uf[unlist(lapply(dat_mod_uf,is.data.frame))] %>% ## Filter for only found dataframes
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    ###########################################
    ## Checking for Completeness of Download ##
    ###########################################

    missed_uf = dat_mod_uf[!unlist(lapply(dat_mod_uf,is.data.frame))] %>% names()

    rm(dat_mod_uf)

    if (length(missed_uf) > 0){
      base::message(base::cat('Download at the State Level Completed!',length(missed_uf),'failed.\n',
                                                       'Attempting to Download at the MesoRegion Level...'))
    } else if (length(missed_uf) == 0){

      base::message(base::cat('Download Succesfully Completed!'))

      return(dat_uf)

      }

    #################
    ## Meso Region ##
    #################

    if (length(missed_uf) > 0){

    geo_meso = geo %>%
      dplyr::filter(code_state %in% missed_uf)

    meso_reg_list = geo_meso %>%
      dplyr::select(code_meso) %>%
      unlist() %>%
      unique() %>%
      as.list()

    names(meso_reg_list) = meso_reg_list

    dat_raw_meso = purrr::map(meso_reg_list,function(meso_reg){

      base::message(base::cat(which(meso_reg == meso_reg_list),'in',length(meso_reg_list),'meso regions...\n'))

      base::suppressMessages(
        get_sidra_safe(x = param$sidra_code,
                      geo = param$geo_reg,
                      period = as.character(param$year),
                      geo.filter = list("MesoRegion" = meso_reg))
        )

      }
    )

    dat_mod_meso = base::lapply(dat_raw_meso,"[[", 1)

    dat_meso = dat_mod_meso[unlist(lapply(dat_mod_meso,is.data.frame))] %>% ## Filter for only found dataframes
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    dat_uf = dat_uf %>%
      dplyr::bind_rows(dat_meso)

    ###########################################
    ## Checking for Completeness of Download ##
    ###########################################

    missed_meso = dat_mod_meso[!unlist(lapply(dat_mod_meso,is.data.frame))] %>% names()

    rm(dat_mod_meso)

    if (length(missed_meso) > 0){
      base::message(base::cat('Download at the Meso Region Level Completed!',length(missed_meso),'failed.\n',
                              'Attempting to Download at the MicroRegion Level...'))
    } else if (length(missed_meso) == 0){

      base::message(base::cat('Download Succesfully Completed!'))

      return(dat_uf)

    }

    ##################
    ## Micro Region ##
    ##################

    if (length(missed_meso) > 0){

    geo_micro = geo %>%
      dplyr::filter(code_meso %in% missed_meso)

    micro_reg_list = geo_micro %>%
      dplyr::select(code_micro) %>%
      unlist() %>%
      unique() %>%
      as.list()

    names(micro_reg_list) = micro_reg_list

    dat_raw_micro = purrr::map(micro_reg_list,function(micro_reg){

      base::message(base::cat(which(micro_reg == micro_reg_list),'in',length(micro_reg_list),'micro regions...\n'))

      base::suppressMessages(
        get_sidra_safe(x = param$sidra_code,
                      geo = param$geo_reg,
                      period = as.character(param$year),
                      geo.filter = list("MicroRegion" = micro_reg))
      )
    }
    )

    dat_mod_micro = base::lapply(dat_raw_micro,"[[", 1)

    dat_micro = dat_mod_micro[unlist(lapply(dat_mod_micro,is.data.frame))] %>% ## Filter for only found dataframes
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    dat_uf = dat_uf %>%
      dplyr::bind_rows(dat_micro)

    ###########################################
    ## Checking for Completeness of Download ##
    ###########################################

    missed_micro = dat_mod_micro[!unlist(lapply(dat_mod_micro,is.data.frame))] %>% names()

    rm(dat_mod_micro)

    if (length(missed_micro) > 0){
      base::message(base::cat(length(missed_micro),
                              'missed API requests at the Micro Region level.
                              Please report this problem to package developers...'))
    }
    if (length(missed_micro) == 0){

      base::message(base::cat('Download Succesfully Completed!'))

      return(dat_uf)

      }

    } # End of if Meso

    } # End of if Uf

    } # End of If - Download at the Municipality Level



}

external_download = function(data){

  ## Get Download Link

  ###########################
  ## Initiate Curl Process ##
  ###########################

  ## Defining URL from TerraBrasilis

  url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", source, "/shape")

  ## Define Empty Directory and Files For Download

  dir <- tempdir()

  temp <- tempfile(fileext = ".zip", tmpdir = dir)

  ## Extraction through Curl Requests
  ## Investigate a bit more on how Curl Requests work

  proc <- RCurl::CFILE(temp, mode = "wb")

  RCurl::curlPerform(url = url, writedata = proc@ref, noprogress = FALSE)

  RCurl::close(proc)

  ## This Data Opening Part Should Include the .Shp, not DBF

  utils::unzip(temp, exdir = dir)

  df = sf::read_sf(paste(dir, "deter_public.shp", sep = "/"))





}

terrabrasilis_download = function(geo = 'amazon_biome',type='cumulative_deforestation'){

  ###########################
  ## Amazon Biome - PRODES ##
  ###########################

  # amz_prodes
  # legal_amz_prodes

    ## Cumulative Deforestation
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/accumulated_deforestation_1988_2007_biome.zip
    ## Annual Forest
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/forest_biome.zip
    ## Hidrography
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/hydrography_biome.zip
    ## Annual Increase in Deforestation
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/yearly_deforestation_biome.zip
    ## Yearly Cloudes
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/cloud_biome.zip
    ## Not Forest
      ## http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/no_forest_biome.zip


  amz_base = 'http://terrabrasilis.dpi.inpe.br/download/dataset'

  cum_deforestation = 'accumulated_deforestation_1988_2007_biome.zip'
  forest_biome = 'forest_biome.zip'




  # if (geo == 'amazon_biome'){
  #   if (type == 'cumulative_deforestation')
  # }



}

datasets_link = function(){

  ## Add file type at the end in order to set the Curl Process

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
