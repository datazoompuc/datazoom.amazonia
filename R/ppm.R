#' @title PPM - Municipal Livestock Production
#'
#' @description Loads information on animal farming inventories and livestock products (IBGE).
#'
#' @param dataset A dataset name ("ppm_livestock_inventory", "ppm_sheep_farming", "ppm_animal_orig_production", "ppm_cow_farming" or "ppm_aquaculture". You can also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021})
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble} consisting of geographic units that present positive values for any of the variables in the dataset.
#'
#' @export
#'
#' @examples \dontrun{
#' # Download treated data (raw_data = FALSE) about aquaculture (dataset = "ppm_aquaculture")
#' # from 2013 to 2015 (time_period = 2013:2015) in english
#' # with the level of aggregation being the country (geo_level = "country").
#' data <- load_ppm(
#'   dataset = "ppm_aquaculture",
#'   raw_data = FALSE,
#'   geo_level = "country",
#'   time_period = 2013:2015
#' )
#'
#' # Download raw data about sheep farming by state from 1980 to 1995 in portuguese (language = "pt")
#' data <- load_ppm(
#'   dataset = "ppm_sheep_farming",
#'   raw_data = TRUE,
#'   geo_level = "state",
#'   time_period = 1980:1995,
#'   language = "pt"
#' )
#' }
load_ppm <- function(dataset, raw_data = FALSE,
                     geo_level, time_period,
                     language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  sidra_code <- nivel_territorial_codigo <- nivel_territorial <- unidade_de_medida_codigo <- NULL
  variavel_codigo <- ano_codigo <- valor <- geo_id <- ano <- tipo_de_rebanho <- NULL
  variavel <- unidade_de_medida <- tipo_de_produto_de_origem_animal <- NULL
  tipo_de_produto_da_aquicultura <- tipo_de_produto_de_origem_animal_codigo <- NULL
  available_time <- tipo_de_rebanho_codigo <- tipo_de_produto_da_aquicultura_codigo <- vars <- NULL

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

  if (param$code == 94) {
    param$data_name <- "Cow Farming (Vacas ordenhadas - Cabecas)"
  }
  if (param$code == 95) {
    param$data_name <- "Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas)"
  }
  if (param$code == 3939) {
    param$data_name <- "Cattle Number (Efetivo dos rebanhos)"
  }
  if (param$code == 74) {
    param$data_name <- "Animal Origin Production (Producao de origem animal, por tipo de produto)"
  }
  if (param$code == 3940) {
    param$data_name <- "Water Origin Production - Fish and others (Producao da aquicultura - peixes e outros)"
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

  ## The Code Below Depends on the type of data

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

  ##############################
  ## Translate Variable Names ##
  ##############################

  ## Cattle Number

  if (param$dataset == "ppm_livestock_inventory") {
    if (language == "pt") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "105") ~ "num"
        ) # Efetivo dos Rebanhos
      )
    }

    if (language == "eng") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "105") ~ "num"
        ) # Efetivo dos Rebanhos
      )
    }
  }

  ## Sheep Farm

  if (param$dataset == "ppm_sheep_farming") {
    if (language == "pt") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "108") ~ "num"
        ) # Ovinos Tosquiados
      )
    }

    if (language == "eng") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "108") ~ "num"
        ) # Ovinos Tosquiados
      )
    }
  }

  ## Animal Origin Production

  if (param$dataset == "ppm_animal_origin_production") {
    if (language == "pt") {
      dat <- dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == "106") ~ "quant", # Producao de origem animal
            (variavel_codigo == "215") ~ "valor", # Valor da producao
          )
        )
    }

    if (language == "eng") {
      dat <- dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == "106") ~ "quant", # Producao de origem animal
            (variavel_codigo == "215") ~ "value", # Valor da producao
          )
        )
    }
  }

  ## Milked Cows

  if (param$dataset == "ppm_cow_farming") {
    if (language == "pt") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "107") ~ "num"
        ) # Vacas ordenhadas
      )
    }

    if (language == "eng") {
      dat <- dat %>% dplyr::mutate(
        variavel = dplyr::case_when(
          (variavel_codigo == "107") ~ "num"
        ) # Vacas ordenhadas
      )
    }
  }

  ## Water Origin Production

  if (param$dataset == "ppm_aquaculture") {
    if (language == "pt") {
      dat <- dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == "4146") ~ "quant", # Producao da aquicultura
            (variavel_codigo == "215") ~ "valor", # Valor da producao
          )
        )
    }

    if (language == "eng") {
      dat <- dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == "4146") ~ "quant", # Producao da aquicultura
            (variavel_codigo == "215") ~ "value", # Valor da producao
          )
        )
    }
  }


  #############################
  ## Create Long Format Data ##
  #############################

  # ==> The Output is a tibble with unit and year identifiers + production and/or value of each item

  # Warning: A double check on this aggregation (despite the unique identifiers established) would be welcomed!

  ## Cattle Number

  if (param$dataset == "ppm_livestock_inventory") {
    dat <- dat %>%
      dplyr::select(-"unidade_de_medida") %>%
      tidyr::pivot_wider(
        id_cols = c(geo_id, ano),
        names_from = c(variavel, tipo_de_rebanho_codigo),
        values_from = valor,
        names_sep = "_V",
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }

  ## Sheep Farm

  if (param$dataset == "ppm_sheep_farming") {
    dat <- dat %>%
      dplyr::select(-"unidade_de_medida") %>%
      tidyr::pivot_wider(
        id_cols = c(geo_id, ano),
        names_from = c(variavel, variavel_codigo),
        values_from = valor,
        names_sep = "_V",
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }

  ## Animal Origin Production

  if (param$dataset == "ppm_animal_origin_production") { ## Animal Origin Production

    dat <- dat %>%
      dplyr::arrange(tipo_de_produto_de_origem_animal_codigo, variavel) %>%
      tidyr::pivot_wider(
        id_cols = c(geo_id, ano),
        names_from = variavel:tipo_de_produto_de_origem_animal_codigo,
        values_from = valor,
        names_sep = "_V",
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }

  ## Milked Cows

  if (param$dataset == "ppm_cow_farming") { ## Milked Cows

    dat <- dat %>%
      dplyr::select(-"unidade_de_medida") %>%
      tidyr::pivot_wider(
        id_cols = c(geo_id, ano),
        names_from = c(variavel, variavel_codigo),
        values_from = valor,
        names_sep = "_V",
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }

  ## Water Origin Production
  ## We need to create a dictionary for this!

  if (param$dataset == "ppm_aquaculture") {
    dat <- dat %>%
      tidyr::pivot_wider(
        id_cols = c(geo_id, ano),
        names_from = c(variavel, tipo_de_produto_da_aquicultura_codigo),
        values_from = valor,
        names_sep = "_V",
        values_fn = sum,
        values_fill = NA
      ) %>%
      janitor::clean_names()
  }


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
