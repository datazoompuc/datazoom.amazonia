#' @title BACI - Global international trade
#'
#' @description Loads disaggregated data on bilateral trade flows for more than 5000 products and 200 countries.
#'
#' @param dataset A dataset name ("HS92").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016 (takes a long time to download)
#' clean_baci <- load_baci(
#'   raw_data = FALSE,
#'   time_period = 2016
#' )
#' }
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export

load_baci <- function(dataset = "HS92", raw_data = FALSE, time_period,
                      language = "eng") {

  ###########################
  ## Bind Global Variables ##
  ###########################

  available_time <- i <- j <- k <- v <- co_sh6 <- no_sh6_por <- NULL
  produto <- no_sh6 <- valor <- product <- code <- NULL
  country <- country_code <- exporter <- importer <- no_sh6_por <- product_code <- NULL

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

  #################
  ## Downloading ##
  #################

  base::message(base::cat("The download can take some time (~10-30mins)"))

  dat <- external_download(
    source = "baci",
    dataset = param$dataset,
    year = param$time_period,
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # Binding elements
  dat <- dat %>%
    data.table::rbindlist() %>%
    tibble::as_tibble()

  ## Turning country codes into names

  # Transforming country codes into country names
  countries_dict <- load_dictionary(dataset = "baci_countries")

  # picking only country names in the chosen language
  countries_dict <- countries_dict %>%
    dplyr::select(c(country_code, "country" = paste0("country_", param$language)))

  # converting exporters ("i" column)
  dat <- dat %>%
    dplyr::left_join(countries_dict, by = c("i" = "country_code")) %>%
    dplyr::rename("exporter" = country)

  # converting importers ("j" column)
  dat <- dat %>%
    dplyr::left_join(countries_dict, by = c("j" = "country_code")) %>%
    dplyr::rename("importer" = country)

  # removing the old columns
  dat <- dat %>%
    dplyr::select(-c(i, j))

  ## Turning product codes into names

  dic <- load_baci_dic(language = param$language)

  dat <- dat %>%
    dplyr::left_join(dic, by = c("k" = "product_code"))


  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "year" = t,
        "product_code" = k,
        "value" = v,
        "quantity" = q,
        "product_name" = product
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "ano" = t,
        "cod_produto" = k,
        "valor" = v,
        "quantidade" = q,
        "exportador" = exporter,
        "importador" = importer,
        "nome_produto" = product
      )
  }

  return(dat_mod)
}



load_baci_dic <- function(language) {

  # Bind Global Variables

  locale <- co_sh6 <- co_sh4 <- co_sh2 <- co_ncm_secrom <- no_sh6_ing <- no_sh4_ing <- no_sh2_ing <- no_sec_ing <- NULL
  no_sh6_por <- product_code <- NULL

  #########################
  ## Download Dictionary ##
  #########################

  url <- "https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv"

  base::message("Downloading dictionary for product codes")

  dic <- base::suppressMessages(readr::read_delim(url, delim = ";", locale = readr::locale(encoding = "Latin1"), progress = TRUE))

  #####################
  ## Data Processing ##
  #####################

  dic <- dic %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    }) %>%
    janitor::clean_names()

  # Harmonizing names

  dic <- dic %>%
    dplyr::select(
      "product_code" = co_sh6,
      "product_pt" = no_sh6_por,
      "product_eng" = no_sh6_ing
    )

  dic <- dic %>%
    dplyr::select(c(product_code, "product" = paste0("product_", language)))

  dic <- dic %>%
    dplyr::mutate(dplyr::across(product_code, as.integer))

  return(dic)
}
