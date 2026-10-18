#' @title Comex - Brazilian foreign trade
#'
#' @description Loads data on all products imported to or exported from Brazil.
#'
#' @param dataset A dataset name ("export_mun", "import_mun", "export_prod" or "import_prod").
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated exports data by municipality from 2020 to 2021
#' data <- load_br_trade(
#'   dataset = "export_mun",
#'   raw_data = FALSE,
#'   time_period = 2020:2021,
#'   language = "eng"
#' )
#'
#' # download treated imports data by municipality from 2020 to 2021
#' data <- load_br_trade(
#'   dataset = "import_mun",
#'   raw_data = FALSE,
#'   time_period = 2020:2021,
#'   language = "eng"
#' )
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
  param$source <- "comex"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2012_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2021_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncmv2/IMP_2012_V2.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2010.csv
  #
  # These example URLs predate the host move (the manifest rows now use
  # balanca.mdic.gov.br) and the `ncmv2/` one is dead: checked live
  # 2026-10-17, every `ncmv2/{EXP,IMP}_<year>_V2.csv` (1995-2027) answers the
  # server's catch-all placeholder, not a file -- `ncm/` is the real folder
  # for every year. See actions/scrapers/resolve_comex.R's header.

  # check if dataset and time_period are valid

  check_params(param)

  ##############
  ## Download ##
  ##############

  dat <- as.list(param$time_period) %>%
    purrr::map(
      function(t) {
        external_download(
          dataset = param$dataset,
          source = param$source, year = t,
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

  # HS (2,4,6) - https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv
  # NCM - https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM.csv

  if (param$dataset == "export_mun" | param$dataset == "import_mun") {
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

    # FRAGILE: the raw MUN files' column names are hard-coded here and in the
    # select() calls below (CO_ANO, CO_MES, SH4, CO_PAIS, SG_UF_MUN, CO_MUN,
    # KG_LIQUIDO, VL_FOB, lower-cased by clean_names()). Verified live
    # 2026-10-17: the header of export_mun/import_mun is exactly those 8
    # columns in 1997, 2025 and 2026, and actions/scrapers/resolve_comex.R
    # re-checks it for every year before publishing a range. formatC(format
    # = "d") also needs SH4 read as an integer, which fread() does on those
    # samples. A renamed column would stop here on the select(), not mismatch.
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

  if (param$dataset == "export_prod" | param$dataset == "import_prod") {
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

    # FRAGILE: same for the NCM files: CO_ANO, CO_MES, CO_NCM, CO_UNID, CO_PAIS,
    # SG_UF_NCM, CO_VIA, CO_URF, QT_ESTAT, KG_LIQUIDO, VL_FOB. Verified live
    # 2026-10-17 on the 1997, 2025 and 2026 headers (resolve_comex.R checks
    # every published year). The IMPORT files carry two more columns,
    # VL_FRETE and VL_SEGURO, that nothing below selects, so they are dropped
    # without a word (the ministry's own layout for imports lists them).
    # formatC(format = "d") needs CO_NCM as an integer: true of fread() on
    # those samples. QT_ESTAT / KG_LIQUIDO (and, in 2026, VL_FOB) come back as
    # integer64 whenever a column holds a value above int32 -- seen in the
    # export samples -- and the as.double step above turns them into doubles
    # (checked on 1997+2025 loads of the samples, not on a full file).
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

  # FRAGILE: these two dictionary URLs are hard-coded here, NOT read from the
  # manifest (NCM_SH.csv is also the manifest's baci_dic/product_codes row,
  # which still points at the old host; NCM.csv is in no manifest row), so
  # nothing resolves or probes them: the yearly data URLs move with
  # resolve_comex.R, this host does not. The host must be the one the
  # ministry's open-data page links, balanca.mdic.gov.br -- until 2026-10-18
  # this said balanca.economia.gov.br, whose TLS certificate expired
  # 2026-10-03, which made load_br_trade() fail at this download even though
  # the yearly files were fine. Verified live 2026-10-18: both files are
  # byte-identical on the two hosts (same MD5), are Latin-1, not UTF-8 (so
  # the Latin1 locale below is right), and carry the columns used here
  # (co_sh4 / no_sh4_por / no_sh4_ing for NCM_SH.csv; co_ncm / no_ncm_por /
  # no_ncm_ing for NCM.csv). If resolve_comex.R's COMEX_HOST ever changes
  # again, change this too (test-comex-resolver.R fails if they disagree).
  path <- "https://balanca.mdic.gov.br/balanca/bd/"

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
