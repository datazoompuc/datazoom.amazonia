#' @title BACI - External Trade
#'
#' @description provides disaggregated data on bilateral trade flows for more than 5000 products and 200 countries. See \url{http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=37}.
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("HS92").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be only one year at a time.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "pt".
#'
#' @return A \code{tibble} consisting of imports or exports data.
#'
#'
#' @examples
#' \dontrun{
#' # download treated trade data from 2018
#' exp_mun <- load_br_trade(
#'   dataset = "HS92",
#'   raw_data = FALSE, time_period = 2018
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom utils download.file read.table
#'
#' @export



load_baci <- function(dataset = "HS92", raw_data, time_period,
                      language = "pt") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  available_time <- i <- j <- k <- v <- co_sh6 <- no_sh6_por <- NULL
  produto <- no_sh6 <- valor <- product <- code <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$time_period <- time_period
  param$language <- language

  ## Check if year is acceptable

  year_check <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>%
    as.character() %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    as.numeric()

  if (min(time_period) < year_check[1]) {
    stop("Provided time period less than supported. Check documentation for time availability.")
  }
  if (max(time_period) > year_check[2]) {
    stop("Provided time period greater than supported. Check documentation for time availability.")
  }
  base::message(base::cat("This may take a few hours"))

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    source = "baci",
    dataset = param$dataset,
    year = year,
    skip_rows = 1
  )

  names(dat) <- c("year", "exporter", "importer", "product", "value", "quantity")

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "HS92" & param$language == "pt") {
    dat <- dat %>%
      dplyr::rename(
        ano = t,
        exportador = i,
        importador = j,
        produto = k,
        valor = v,
        quantidade = q
      )

    countries_dict <- load_dictionary(dataset = "baci")

    dat <- dat %>%
      dplyr::mutate(exportador = dplyr::case_when(

      ))


    dic <- suppressMessages(load_trade_dic(type = "hs"))

    dic <- dic %>%
      dplyr::select(co_sh6, no_sh6 = no_sh6_por)

    non_dup <- !duplicated(dic)

    dic <- dic %>%
      dplyr::filter(non_dup)

    dat <- dat %>%
      dplyr::rename(co_sh6 = produto) %>%
      dplyr::mutate(co_sh6 = formatC(co_sh6, width = 4, format = "d", flag = "0")) %>%
      dplyr::left_join(dic, by = "co_sh6")


    dat <- dat %>%
      dplyr::rename(
        cod_produto = co_sh6,
        nome_produto = no_sh6
      )

    dat <- dat %>%
      dplyr::mutate(valor = as.numeric(valor))

    dat <- dat %>%
      dplyr::mutate(nome_produto = dplyr::case_when(
        cod_produto == "080130" ~
          "Nozes comestiveis: castanhas de caju, frescas ou secas, mesmo descascadas ou peladas",
        cod_produto == "844350" ~ "Maquinas de impressao: do tipo NES na posicao 8443",
        cod_produto == "854380" ~ "Maquinas e aparelhos eletricos: com funcao propria, NES na posicao 8543",
        cod_produto == "847120" ~ "Maquinas para processamento de dados: automaticas digitais, contendo no mesmo involucro pelo menos uma unidade central de processamento e uma unidade de entrada e saida, combinadas ou nao",
        cod_produto == "903081" ~ "Instrumentos e aparelhos: com dispositivo de gravacao, especialmente concebido para telecomunicacoes",
        cod_produto == "560300" ~ "Falsos tecidos: mesmo impregnados, revestidos, recobertos ou estratificados",
        cod_produto == "847193" ~ "Maquinas de processamento de dados: unidades de armazenamento, apresentadas ou nao com o resto de um sistema",
        cod_produto == "852490" ~ "Midia gravadas: NES na posicao 8524 para fenomenos de gravacao de som ou similar, incluindo matrizes e mestres para a producao de registros",
        cod_produto == "080110" ~ "Nozes comestiveis: cocos, frescos ou secos, mesmo descascados ou pelados",
        cod_produto == "080710" ~ "Frutas comestiveis: meloes (incluindo melancias), frescos",
        TRUE ~ nome_produto
      ))
  }


  if (param$dataset == "HS92" & param$language == "eng") {
    dat <- dat %>%
      dplyr::rename(
        year = t,
        exporter = i,
        importer = j,
        product = k,
        value = v,
        quantity = q
      )


    dic <- suppressMessages(load_trade_dic_eng(type = "hs"))

    non_dup <- !duplicated(dic)

    dic <- dic %>%
      dplyr::filter(non_dup)

    dat <- dat %>%
      dplyr::rename(code = product) %>%
      dplyr::mutate(code = formatC(code, width = 4, format = "d", flag = "0")) %>%
      dplyr::left_join(dic, by = "code")
  }

  return(dat)
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


load_trade_dic_eng <- function(type) {
  code <- description <- NULL

  url <- "http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202201.zip"

  if (type == "hs") {
    temp <- tempfile()
    download.file(url, temp, mode = "wb")
    dic <- read.table(unz(temp, paste0("product_codes_HS92_V202201.csv")),
      fill = TRUE, header = FALSE, sep = ","
    )
  }

  dic <- dic %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    }) %>%
    janitor::clean_names()

  return(dic)
}
