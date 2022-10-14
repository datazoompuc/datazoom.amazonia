#' @title Comex - Brazilian external trade
#'
#' @description Loads data on all products imported to or exported from Brazil.
#'
#' @param dataset A dataset name ("comex_export_mun", "comex_import_mun", "comex_export_prod" or "comex_import_prod").
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated (raw_data = FALSE) exports data by municipality (dataset = "comex_export_mun")
#' # from 2020 to 2021 (time_period = 2020:2021)
#' data <- load_br_trade(
#'   dataset = "comex_export_mun",
#'   raw_data = FALSE,
#'   time_period = 2020:2021
#' )
#' # download treated(raw_data = FALSE) imports data by municipality (dataset = "comex_import_mun")
#' # from 2020 to 2021 (time_period = 2020:2021)
#' data <- load_br_trade(
#'   dataset = "comex_import_mun",
#'   raw_data = FALSE,
#'   time_period = 2020:2021
#' )
#' }
#'
#' @export

load_br_trade <- function(dataset, raw_data = FALSE,
                          time_period, language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  co_ano <- co_mes <- sh4 <- co_pais <- sg_uf_mun <- co_mun <- kg_liquido <- vl_fob <- munic_code <- NULL
  quantity_net_kg <- fob_usd <- hs4_code <- hs2_code <- year <- usd_per_kg <- NULL
  survey <- link <- co_sh4 <- co_sh2 <- quant_net_kg <- NULL
  geo_level <- NULL
  no_sh4_por <- no_sh4_ing <- no_sh4 <- ano <- mes <- cod_municipio <- cod_pais <- NULL
  nome_sh4 <- month <- municipality_code <- country_code <- hs4_name <- NULL
  co_ncm <- no_ncm_por <- no_ncm_ing <- sg_uf_ncm <- no_ncm <- co_via <- co_unid <- NULL
  co_urf <- qt_estat <- uf <- nome_ncm <- state <- ncm_name <- NULL


  ## There are two main dissagregated data levels in the COMEX website:
  ## 1 - Dissagregated by Classification: NCM
  ## 2 - Disagreggated by Exporter/Importer Municipality

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2012_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2021_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncmv2/IMP_2012_V2.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2010.csv

  ##############
  ## Download ##
  ##############

  dat <- as.list(param$time_period) %>%
    purrr::map(
      function(t) {
        external_download(
          dataset = param$dataset,
          source = "comex", year = t,
          geo_level = param$geo_level
        )
      }
    )

  dat <- dat %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  ## Change Data Type

  dat <- dat %>%
    dplyr::mutate_if(is.numeric, as.double)

  ## ---------------------------------------------------------------------------##

  #####################
  ## Load Dictionary ##
  #####################

  ## This Needs to Load the Dictionary Depending on param$prod_class

  # HS (2,4,6) - https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv
  # NCM - https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv

  if (param$dataset == "comex_export_mun" | param$dataset == "comex_import_mun") {
    dic <- suppressMessages(load_trade_dic(type = "hs"))

    if (param$language == "pt") {
      dic <- dic %>%
        dplyr::select(co_sh4, no_sh4 = no_sh4_por)
    }
    if (param$language == "eng") {
      dic <- dic %>%
        dplyr::select(co_sh4, no_sh4 = no_sh4_ing)
    }

    non_dup <- !duplicated(dic)

    dic <- dic %>%
      dplyr::filter(non_dup)

    #######################
    ## Add Variable Name ##
    #######################

    dat <- dat %>%
      dplyr::rename(co_sh4 = sh4) %>%
      dplyr::mutate(co_sh4 = formatC(co_sh4, width = 4, format = "d", flag = "0")) %>%
      dplyr::left_join(dic, by = "co_sh4")


    ## Translation

    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::select(
          ano = co_ano, mes = co_mes,
          cod_pais = co_pais, uf = sg_uf_mun,
          cod_municipio = co_mun,
          cod_sh4 = co_sh4, nome_sh4 = no_sh4,
          kg_liquido, valor_fob = vl_fob
        ) %>%
        dplyr::arrange(ano, mes, cod_municipio, cod_pais, nome_sh4)
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::select(
          year = co_ano, month = co_mes,
          country_code = co_pais, state = sg_uf_mun,
          municipality_code = co_mun,
          hs4_code = co_sh4, hs4_name = no_sh4,
          kg_net = kg_liquido, value_fob = vl_fob
        ) %>%
        dplyr::arrange(year, month, municipality_code, country_code, hs4_name)
    }
  }

  if (param$dataset == "comex_export_prod" | param$dataset == "comex_import_prod") {
    dic <- suppressMessages(load_trade_dic(type = "ncm"))

    if (param$language == "pt") {
      dic <- dic %>%
        dplyr::select(co_ncm, no_ncm = no_ncm_por)
    }
    if (param$language == "eng") {
      dic <- dic %>%
        dplyr::select(co_ncm, no_ncm = no_ncm_ing)
    }

    non_dup <- !duplicated(dic)

    dic <- dic %>%
      dplyr::filter(non_dup)

    #######################
    ## Add Variable Name ##
    #######################

    dat <- dat %>%
      dplyr::mutate(co_ncm = formatC(co_ncm, width = 8, format = "d", flag = "0")) %>%
      dplyr::left_join(dic, by = "co_ncm")


    ## Translation

    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::select(
          ano = co_ano, mes = co_mes,
          cod_pais = co_pais, uf = sg_uf_ncm,
          cod_ncm = co_ncm, nome_ncm = no_ncm,
          cod_transporte = co_via, cod_unidade = co_unid,
          cod_urf = co_urf, qtd_estatistica = qt_estat,
          kg_liquido, valor_fob = vl_fob
        ) %>%
        dplyr::arrange(ano, mes, uf, cod_pais, nome_ncm)
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::select(
          year = co_ano, month = co_mes,
          country_code = co_pais, state = sg_uf_ncm,
          ncm_code = co_ncm, ncm_name = no_ncm,
          transport_code = co_via, unit_code = co_unid,
          urf_code = co_urf, statistical_qt = qt_estat,
          kg_net = kg_liquido, value_fob = vl_fob
        ) %>%
        dplyr::arrange(year, month, state, country_code, ncm_name)
    }
  }

  return(dat_mod)
}


load_trade_dic <- function(type) {

  # Bind Global Variables

  locale <- co_sh6 <- co_sh4 <- co_sh2 <- co_ncm_secrom <- no_sh6_ing <- no_sh4_ing <- no_sh2_ing <- no_sec_ing <- NULL

  #########################
  ## Download Dictionary ##
  #########################

  path <- "https://balanca.economia.gov.br/balanca/bd/"

  if (type == "hs") {
    final <- paste(path, "tabelas/NCM_SH.csv", sep = "")
  } # Harmonized System
  if (type == "ncm") {
    final <- paste(path, "tabelas/NCM.csv", sep = "")
  }
  # if (type == 'cuci'){final = paste(path,'tabelas/NCM_CUCI.csv',sep='')}
  # if (type == 'isic'){final = paste(path,'tabelas/NCM_ISIC.csv',sep='')}
  # if (type == 'cgce'){final = paste(path,'tabelas/NCM_CGCE.csv',sep='')}
  # if (type == 'aggreg'){final = paste(path,'tabelas/NCM_FAT_AGREG.csv',sep='')}
  # if (type == 'ppe'){final = paste(path,'tabelas/NCM_PPE.csv',sep='')}
  # if (type == 'ppi'){final = paste(path,'tabelas/NCM_PPI.csv',sep='')}

  dic <- readr::read_delim(final, delim = ";", locale = readr::locale(encoding = "Latin1"), progress = TRUE)

  #####################
  ## Data Processing ##
  #####################

  dic <- dic %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    }) %>%
    janitor::clean_names()

  return(dic)
}
