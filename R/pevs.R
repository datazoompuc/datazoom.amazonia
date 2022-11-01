#' @title PEVS - Forestry Activities
#'
#' @description Loads information on the amount and value of the production of the exploitation of native plant resources and planted forest massifs, as well as existing total and harvested areas of forest crops.
#'
#' @param dataset A dataset name ("pevs_forest_crops", "pevs_silviculture" or "pevs_silviculture_area"). You can also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/pevs/quadros/brasil/2019})
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "region", "state", or "municipality".
#'
#' @return A \code{tibble} consisting of geographic units that present positive values for any of the variables in the dataset.
#'
#' @examples \dontrun{
#' # Download treated (raw_data = FALSE) silviculture data (dataset = 'pevs_silviculture')
#' # by state (geo_level = 'state') from 2012 (time_period =  2012)
#' # in portuguese (language = "pt")
#' data <- load_pevs(
#'   dataset = "pevs_silviculture",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2012,
#'   language = "pt"
#' )
#'
#' # Download raw (raw_data = TRUE) forest crops data by region from 2012 to 2013 in english
#' data <- load_pevs(
#'   dataset = "pevs_forest_crops",
#'   raw_data = TRUE,
#'   geo_level = "region",
#'   time_period = 2012:2013
#' )
#' }
#'
#' @export

load_pevs <- function(dataset, raw_data = FALSE,
                      geo_level, time_period,
                      language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  sidra_code <- nivel_territorial_codigo <- nivel_territorial <- unidade_de_medida_codigo <- NULL
  unidade_de_medida <- tipo_de_produto_codigo <- variavel_codigo <- ano_codigo <- NULL
  valor <- tipo_de_produto_extrativo_codigo <- tipo_de_produto_da_silvicultura_codigo <- NULL
  tipo_de_produto_da_silvicultura <- especie_florestal_codigo <- especie_florestal <- NULL
  geo_id <- ano <- variavel <- tipo_de_produto <- tipo_de_produto_extrativo <- NULL
  available_time <- vars <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  if (!is.numeric(param$dataset)) {
    param$code <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {
    param$code <- param$dataset
  }

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

  ## Dataset

  if (param$code == 289) {
    param$data_name <- "Vegetal extraction quantity and value (Quantidade e valor da extracao vegetal)"
  }

  if (param$code == 291) {
    param$data_name <- "Forestry quantity and value (Quantidade e valor da silvicultura)"
  }

  if (param$code == 5930) {
    param$data_name <- "Forestry area (Area da silvicultura)"
  }

  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  dat <- as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num) {
      # suppressMessages(
      sidra_download(sidra_code = param$code, year = year_num, geo_level = param$geo_level)
      # )
    }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Enginnering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  # We need to check if this works for all data

  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor))

  ## Only Keep Valid Observations

  dat <- dat %>%
    dplyr::filter(!is.na(valor))

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  if (geo_level == "country") {
    dat$geo_id <- dat$brasil
    dat <- dplyr::select(dat, -"brasil_codigo", -"brasil")
  }

  if (geo_level == "region") {
    dat$geo_id <- dat$grande_regiao
    dat <- dplyr::select(dat, -"grande_regiao_codigo", -"grande_regiao")
  }

  if (geo_level == "state") {
    dat$geo_id <- dat$unidade_da_federacao_codigo
    dat <- dplyr::select(dat, -"unidade_da_federacao_codigo", -"unidade_da_federacao")
  }

  if (geo_level == "municipality") {
    dat$geo_id <- dat$municipio_codigo
    dat <- dplyr::select(dat, -"municipio", -"municipio_codigo")
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat <- dat %>%
    dplyr::select(-unidade_de_medida, -unidade_de_medida_codigo)

  ## Change Variable Names for Common Across Datasets

  if (param$code == 289) {
    dat <- dat %>%
      dplyr::rename(
        tipo_de_produto_codigo = tipo_de_produto_extrativo_codigo,
        tipo_de_produto = tipo_de_produto_extrativo
      )
  }

  if (param$code == 291) {
    dat <- dat %>%
      dplyr::rename(
        tipo_de_produto_codigo = tipo_de_produto_da_silvicultura_codigo,
        tipo_de_produto = tipo_de_produto_da_silvicultura
      )
  }

  if (param$code == 5930) {
    dat <- dat %>%
      dplyr::rename(
        tipo_de_produto_codigo = especie_florestal_codigo,
        tipo_de_produto = especie_florestal
      )
  }

  ## Translation

  if (language == "pt") {
    dat <- dat %>%
      dplyr::mutate(variavel = dplyr::case_when(
        (variavel_codigo == "142") ~ "quant", # Quantidade produzida na silvicultura
        (variavel_codigo == "144") ~ "quant", # Quantidade produzida na extracao vegetal
        (variavel_codigo == "143") ~ "valor", # Valor da producao na silvicultura
        (variavel_codigo == "145") ~ "valor", # Valor da produção na extração vegetal
        (variavel_codigo == "6549") ~ "area" # Area total existente em 31/12 dos efetivos da silvicultura
        # (variavel == 'valor_da_producao_na_silvicultura') ~ 'valor_da_prod_silvicultura_brl'
      ))
  }

  if (language == "eng") {
    dat <- dat %>%
      dplyr::mutate(variavel = dplyr::case_when(
        (variavel_codigo == "142") ~ "quant", # Quantidade produzida na silvicultura
        (variavel_codigo == "144") ~ "quant", # Quantidade produzida na extracao vegetal
        (variavel_codigo == "143") ~ "value", # Valor da producao na silvicultura
        (variavel_codigo == "145") ~ "value", # Valor da produção na extração vegetal
        (variavel_codigo == "6549") ~ "area" # Area total existente em 31/12 dos efetivos da silvicultura
      ))
  }

  #############################
  ## Create Long Format Data ##
  #############################

  ## The Output is a tibble with unit and year identifiers + production and/or value of each item

  dat <- dat %>%
    dplyr::select(-"tipo_de_produto") %>%
    dplyr::arrange(tipo_de_produto_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = c(geo_id, ano),
      names_from = variavel:tipo_de_produto_codigo,
      values_from = valor,
      names_sep = "_V",
      values_fn = sum,
      values_fill = NA
    ) %>%
    janitor::clean_names()

  ########################
  ## Changing Year Name ##
  ########################


  if (language == "eng") {
    dat <- dat %>%
      dplyr::rename(year = ano)
  }

  ###############
  ## Labelling ##
  ###############

  labelled <- function(x, label) {
    Hmisc::label(x) <- label
    x
  }

  label_data_eng <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$var_code == cols, "var_eng"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(cols)),
        ~ labelled(., as.character(dic[dic$var_code == cols, "var_eng"]))
      )

    return(df)
  }

  label_data_pt <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$var_code == cols, "var_pt"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(cols)),
        ~ labelled(., as.character(dic[dic$var_code == cols, "var_pt"]))
      )

    return(df)
  }

  ## Load Dictionary

  dic <- load_dictionary(param$dataset)

  types <- as.character(dic$var_code)
  types <- types[types != "0"] ## Remove 0


  if (language == "eng") {

    # f = dat %>%
    #   dplyr::mutate_at(vars(tidyr::matches(as.character(types[1]))),
    #                    ~ labelled::set_variable_labels(. = as.character(dic[dic$var_code == types[1],'var_eng']))
    #   )

    for (i in seq_along(types)) {
      dat <- label_data_eng(dat, cols = types[i], dic = dic)
    }
  }

  if (language == "pt") {
    for (i in seq_along(types)) {
      dat <- label_data_pt(dat, cols = types[i], dic = dic)
    }
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}
