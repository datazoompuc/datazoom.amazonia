#' @title BACI - Global foreign trade
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
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated trade data for 2016 (HS92 classification)
#' trade_2016 <- load_baci(
#'   dataset = "HS92",
#'   raw_data = FALSE,
#'   time_period = 2016,
#'   language = "eng"
#' )
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
  param$source <- "baci"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$time_period <- time_period
  param$language <- language

  # check if dataset and time_period are valid

  check_params(param)

  #################
  ## Downloading ##
  #################

  base::message("The download can take some time (~10-30mins)")

  dat <- external_download(
    source = param$source,
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

  # FRAGILE: both rename blocks below assume BACI's raw column names are
  # exactly t/k/v/q (plus i/j/product from the joins above) -- CEPII's own
  # documented schema, but nothing here re-verifies it against the actual
  # downloaded file. A column rename or reorder upstream would make
  # dplyr::rename() error on a missing column (loud) or, worse, silently
  # rename the WRONG column if CEPII ever reused one of these single letters
  # for something else.
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

  # This table is Brazilian COMEX's own product-code dictionary
  # (balanca.economia.gov.br), not part of CEPII's BACI archive itself --
  # kept in the manifest under its own survey/dataset ("baci_dic"/
  # "product_codes") rather than under "baci" so it doesn't pass
  # check_params()'s dataset validation for load_baci() as if it were an
  # actual BACI dataset. Reading it via dataset_url() (instead of the
  # literal URL this used to hardcode) means the manifest's own resolver
  # pipeline can at least see and HTTP-probe this dependency, even though it
  # has no dedicated resolver of its own -- see resolve_epe.R's/
  # resolve_baci.R's headers for what a resolver for it would look like if
  # this URL ever needs one.
  url <- dataset_url(source = "baci_dic", dataset = "product_codes")

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

  # FRAGILE: co_sh6/no_sh6_por/no_sh6_ing are this off-manifest source's
  # (balanca.economia.gov.br's NCM_SH.csv, a Brazilian COMEX table, not
  # CEPII) column names after janitor::clean_names() -- if that table's
  # headers change, this select() errors on a missing column rather than
  # silently mismatching (dplyr::select() is at least loud about it), but
  # there's no check anywhere before this point that catches it earlier or
  # more clearly.
  dic <- dic %>%
    dplyr::select(
      "product_code" = co_sh6,
      "product_pt" = no_sh6_por,
      "product_eng" = no_sh6_ing
    )

  dic <- dic %>%
    dplyr::select(c(product_code, "product" = paste0("product_", language)))

  # FRAGILE: as.integer() on a non-numeric product_code (e.g. if the source
  # ever ships a code with a letter or leading zero lost) coerces to NA
  # silently, per R's usual as.integer() behavior -- no warning surfaces
  # here, and the row just fails to join later instead of erroring where the
  # actual problem is.
  dic <- dic %>%
    dplyr::mutate(dplyr::across(product_code, as.integer))

  return(dic)
}
