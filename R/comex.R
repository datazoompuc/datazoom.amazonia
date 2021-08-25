#' @title Comex - External Trade
#'
#' @description Loads information on both imports and exports data. Data is available from 1997 to 2021 for most datasets. See \url{https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta}.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("comex_export_mun", "comex_import_mun", "comex_export_prod" or "comex_import_prod").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#' @param prod_class A string indicating the classification to be downloaded, chosen between "hs" (SH - Sistema Harmonizado), "cuci" (CUCI - Classificação Uniforme do Comércio Internacional), "isic" (ISIC - Classificação Internacional Padrão por Atividade Econômica), "cgce" (CGCE - Classificação por Grandes Categorias Econômicas). Defaults to "hs".
#'
#' @return A \code{tibble} consisting of imports or exports data.
#'
#'
#' @examples
#' \dontrun{
#' # download treated exports data by municipality from 1997 to 2021
#' exp_mun <- load_br_trade(dataset = "comex_export_mun",
#'                          raw_data = FALSE, time_period = 1997:2021)
#'
#' # download raw imports data by municipality from 1997 to 2021
#' raw_imp_mun <- load_br_trade(dataset = "comex_import_mun",
#'                              raw_data = TRUE, time_period = 1997:2021)
#'
#' # download treated imports data by municipality from 1997 to 2021 using "CUCI" classification
#' imp_mun_cuci <- load_br_trade(dataset = "comex_import_mun",
#'                               raw_data = FALSE, time_period = 1997:2021,
#'                               prod_class = "cuci")
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export

load_br_trade <- function(dataset = NULL, raw_data,
                          time_period, language = 'eng',
                          prod_class = 'hs'){

  ## We want to download both imports and exports data

  ## Type: By NCM, Município da empresa exportadora/importadora e Posição do Sistema Harmonizado (SH4),
  ##

  ## https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

  ## https://www.m2vconsultoria.com.br/noticias/a-importancia-da-correta-classificacao-fiscal-de-mercadorias-para-os-processos-aduaneiros/
  ## http://siscomex.gov.br/balanca-comercial-traz-agora-classificacao-de-produtos-por-setor-de-atividade-economica/
  # https://wits.worldbank.org/WITS/WITS/Restricted/Login.aspx
  # http://www.cepii.fr/cepii/en/bdd_modele/bdd.asp
  # https://www.usitc.gov/data/gravity/data_faq.htm
  # http://siscomex.gov.br/balanca-comercial-traz-agora-classificacao-de-produtos-por-setor-de-atividade-economica/



  ## To-Do:
    ## Include Labels

  ###########################
  ## Bind Global Variables ##
  ###########################

  co_ano <- co_mes <- sh4 <- co_pais <- sg_uf_mun <- co_mun <- kg_liquido <- vl_fob <- munic_code <- NULL
  quantity_net_kg <- fob_usd <- hs4_code <- hs2_code <- year <- usd_per_kg <- NULL
  survey <- link <- co_sh4 <- co_sh2 <- quant_net_kg <- NULL
  geo_level <- NULL


  ## There are two main dissagregated data levels in the COMEX website:
    ## 1 - Dissagregated by Classification: NCM
    ## 2 - Disagreggated by Exporter/Importer Municipality

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language
  param$prod_class = prod_class
  param$raw_data = raw_data

  param$survey_name = datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(survey) %>%
      unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}

  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2012_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2021_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncmv2/IMP_2012_V2.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2010.csv

  ##############
  ## Download ##
  ##############

  dat = as.list(param$time_period) %>%
    purrr::map(
      function(t){external_download(dataset = param$dataset,
                                    source='comex',year = t,
                                    geo_level = param$geo_level)
      }
    )

  dat = dat %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ######################
  ## Data Engineering ##
  ######################

  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

  ## Change Data Type

  dat = dat %>%
    dplyr::mutate_if(is.numeric,as.double)

  ## Returning Raw Data

    ## Just Add Translation

  if (param$raw_data == TRUE){return(dat)}

  ## ---------------------------------------------------------------------------##

  ## Double Check all the info below depending on the database we want to analyze

  dat = dat %>%
    dplyr::filter(!(co_mun %in% c(9999999,9300000)))

  dat = dat %>%
    dplyr::filter(kg_liquido > 0) %>%
    dplyr::filter(vl_fob > 0)

  #####################
  ## Load Dictionary ##
  #####################

  ## This Needs to Load the Dictionary Depending on param$prod_class

    # HS (2,4,6) - https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv
    # NCM - https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv
    # CUCI- https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_CUCI.csv
    # CGCe - https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_CGCE.csv


  dic = suppressMessages(load_trade_dic(type = param$prod_class))
  dic = dic %>%
    dplyr::select(co_sh4,co_sh2)

  non_dup = !duplicated(dic)

  dic = dic %>%
    dplyr::filter(non_dup)

  #######################
  ## Add Variable Name ##
  #######################

  ## Data at Comex Export - Municipalities come at the HS4 level
    ## We merge HS2 Information (More Aggregate)

  dat = dat %>%
    dplyr::rename(co_sh4 = sh4) %>%
    dplyr::mutate(co_sh4 = as.character(co_sh4)) %>%
    dplyr::left_join(dic,by='co_sh4')

  dat = dat %>%
    dplyr::filter(!is.na(co_sh2)) %>%
    dplyr::mutate(co_sh2 = as.numeric(co_sh2))

  ## Be careful with other info

  ##############################################
  ## Aggregate to Desired Geo-Year-Code Level ##
  ##############################################

  dat_mod = dat %>%
    dplyr::group_by(co_ano,co_mun,co_sh2) %>%
    dplyr::summarise(kg_liquido = sum(kg_liquido),
                     vl_fob = sum(vl_fob)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(usd_per_kg = vl_fob/kg_liquido) %>%
    tibble::as_tibble()

  ## wide Format

  if (param$language == 'pt'){

  dat_mod = dat_mod %>%
    dplyr::arrange(co_sh2,co_ano) %>%
    tidyr::pivot_wider(id_cols = c(co_mun,co_ano),
                       names_from = co_sh2,
                       values_from=kg_liquido:usd_per_kg,
                       names_sep = '_',
                       values_fn = sum,
                       values_fill = NA) %>%
    janitor::clean_names()

  }

  if (param$language == 'eng'){

    dat_mod = dat_mod %>%
      dplyr::arrange(co_sh2,co_ano) %>%
      dplyr::rename(year = co_ano, munic_code = co_mun,
                    quant_net_kg = kg_liquido, fob_usd = vl_fob) %>%
      tidyr::pivot_wider(id_cols = c(munic_code,year),
                         names_from = co_sh2,
                         values_from=quant_net_kg:usd_per_kg,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()

  }


  return(dat)

}


load_trade_dic <- function(type = 'hs'){

  # Bind Global Variables

  locale <-co_sh6 <-co_sh4 <-co_sh2 <-co_ncm_secrom <-no_sh6_ing <-no_sh4_ing <-no_sh2_ing <-no_sec_ing <- NULL

  #########################
  ## Download Dictionary ##
  #########################

  path = 'https://balanca.economia.gov.br/balanca/bd/'

  if (type == 'hs'){final = paste(path,'tabelas/NCM_SH.csv',sep='')} # Harmonized System
  if (type == 'cuci'){final = paste(path,'tabelas/NCM_CUCI.csv',sep='')}
  if (type == 'isic'){final = paste(path,'tabelas/NCM_ISIC.csv',sep='')}
  if (type == 'cgce'){final = paste(path,'tabelas/NCM_CGCE.csv',sep='')}
  # if (type == 'aggreg'){final = paste(path,'tabelas/NCM_FAT_AGREG.csv',sep='')}
  # if (type == 'ppe'){final = paste(path,'tabelas/NCM_PPE.csv',sep='')}
  # if (type == 'ppi'){final = paste(path,'tabelas/NCM_PPI.csv',sep='')}

  dic = readr::read_delim(final,delim=';',locale = readr::locale(encoding='Latin1'),progress=TRUE)

  #####################
  ## Data Processing ##
  #####################

  dic = dic %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    janitor::clean_names()

  #################
  ## Translation ##
  #################

  # if (language == 'eng'){
  #   dic = dic %>%
  #     dplyr::select(co_sh6,co_sh4,co_sh2,co_ncm_secrom,no_sh6_ing,no_sh4_ing,no_sh2_ing,no_sec_ing) %>%
  #     dplyr::rename(hs6_code = co_sh6,hs4_code = co_sh4,hs2_code = co_sh2,ncm_secrom_code = co_ncm_secrom)
  # }
  #
  # if (language == 'pt'){
  #   dic = dic %>%
  #     dplyr::select(co_sh6,co_sh4,co_sh2,co_ncm_secrom,no_sh6_por,no_sh4_por,no_sh2_por,no_sec_por)
  # }

  return(dic)


}
