
## We want to download both imports and exports data

## Type: By NCM, Município da empresa exportadora/importadora e Posição do Sistema Harmonizado (SH4),
##

## https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

load_br_trade = function(type='exports',geo_level,time_period =2012,
                         time_id='year',language='eng',dic='isic'){

  ## There are two main dissagregated data levels in the COMEX website:
    ## 1 - Dissagregated by Classification: NCM
    ## 2 - Disagreggated by Exporter/Importer Municipality

  #######################
  ## Define Parameters ##
  #######################

  param = list()
  # param$data_type = type
  # param$geo_level = NULL
  # param$time_period = time_period
  # param$time_id = time_id
  # param$language = language
  # param$dic = dic
  param$path = 'https://balanca.economia.gov.br/balanca/bd/'

  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2012_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2021_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncmv2/IMP_2012_V2.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2010.csv


  time = 2012

  # if (param$type == 'exports'){
    param$download_path = paste(param$path,'comexstat-bd/mun/EXP_',time,'_MUN.csv',sep='')
  # }
  # if (param$type == 'imports'){
  #   param$download_path = paste(param$path,'comexstat-bd/ncmv2/IMP_',time,'_V2.csv',sep='')
  # }

  ##############
  ## Download ##
  ##############

  ## We need to download new and old version

  download_trade = function(path){

    temp_file = base::tempfile()
    utils::download.file(url = param$download_path, destfile = temp_file)

    #dat = readr::read_csv(temp_file)

    dat = data.table::fread(temp_file)

    return(dat)

  }

  # dat = readr::read_delim(param$download_path,delim=';',
  #                       col_types = readr::cols(.default = readr::col_character()),
  #                       progress = TRUE)

  dat = download_trade(path = param$download_path)

  ###############
  ## Edit Data ##
  ##############

  dat = dat %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  dat = dat %>%
    dplyr::rename(year = co_ano, month = co_mes,hs4_code = sh4,
                  country = co_pais, uf = sg_uf_mun, munic_code = co_mun,
                  quantity_net_kg = kg_liquido, fob_usd = vl_fob) %>%
    dplyr::filter(!(munic_code %in% c(9999999,9300000)))

  dat = dat %>%
    dplyr::filter(quantity_net_kg > 0) %>%
    dplyr::filter(fob_usd > 0)

  ## Load Dictionary

  dic = load_trade_dic(type = 'hs')
  dic = dic %>%
    dplyr::select(hs4_code,hs2_code)

  non_dup = !duplicated(dic)

  dic = dic %>%
    dplyr::filter(non_dup)

  dat = dat %>%
    dplyr::mutate(hs4_code = as.character(hs4_code)) %>%
    dplyr::left_join(dic,by='hs4_code')

  ## Be careful with other info

  dat = dat %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(year,munic_code,hs2_code) %>%
    dplyr::summarise(quantity_net_kg = sum(quantity_net_kg),
                     fob_usd = sum(fob_usd)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(usd_per_kg = as.numeric(fob_usd)/as.numeric(quantity_net_kg)) %>%
    tibble::as_tibble()

  ## Long Format

  dat = dat %>%
    tidyr::pivot_wider(id_cols = c(munic_code,year),
                       names_from = hs2_code,
                       values_from=quantity_net_kg:usd_per_kg,
                       names_sep = '_',
                       values_fn = sum,
                       values_fill = NA) %>%
    janitor::clean_names()

  return(dat)

}

load_trade_dic = function(type = 'hs'){

  path = 'https://balanca.economia.gov.br/balanca/bd/'

  ################

  # ncm = paste(path,'tabelas/NCM.csv',sep='')

  ## Download Dictionary

  # dat = readr::read_delim(ncm,delim=';',locale = locale(encoding='Latin1'),progress=TRUE)

  ######################

  final = paste(path,'tabelas/NCM_SH.csv',sep='')

  dic = readr::read_delim(final,delim=';',locale = locale(encoding='Latin1'),progress=TRUE)

  ## Convert Dictionary

  dic = dic %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    janitor::clean_names()

  ## Pick Variables

  dic = dic %>%
    dplyr::select(co_sh6,co_sh4,co_sh2,co_ncm_secrom,no_sh6_ing,no_sh4_ing,no_sh2_ing,no_sec_ing) %>%
    dplyr::rename(hs6_code = co_sh6,hs4_code = co_sh4,hs2_code = co_sh2,ncm_secrom_code = co_ncm_secrom)

  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_CUCI.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_ISIC.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/ISIC_CUCI.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_CGCE.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_FAT_AGREG.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_PPE.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_PPI.csv
  #
  # https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_UNIDADE.csv

  return(dic)


}
