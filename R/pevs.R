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
#' @examplesIf interactive()
#' ### Example 1: Forest Crop Production by State
#'
#' Analyze timber and forest product production across states:
#'
#' library(dplyr)
#'
#' # Download forest crops data at state level
#' forest_crops <- load_pevs(
#'   dataset = "pevs_forest_crops",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2019,
#'   language = "eng"
#' )
#'
#' # Top states by forest product production value
#' top_producers <- forest_crops %>%
#'   group_by(state) %>%
#'   summarize(
#'     total_production_value = sum(production_value, na.rm = TRUE),
#'     total_quantity = sum(quantity_produced, na.rm = TRUE),
#'     num_products = n_distinct(product_type),
#'     .groups = 'drop'
#'   ) %>%
#'   arrange(desc(total_production_value)) %>%
#'   head(15)
#'
#' print(top_producers)
#'
#' ### Example 2: Silviculture Area Expansion
#'
#' Track the growth of forest plantations over time:
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # Load silviculture area data for multiple years
#' silvi_area <- load_pevs(
#'   dataset = "pevs_silviculture_area",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2013:2019,
#'   language = "eng"
#' )
#'
#' # National silviculture area trends
#' national_area <- silvi_area %>%
#'   group_by(year) %>%
#'   summarize(
#'     total_area = sum(area_hectares, na.rm = TRUE),
#'     num_states = n_distinct(state),
#'     .groups = 'drop'
#'   )
#'
#' # Visualize expansion
#' ggplot(national_area, aes(x = year, y = total_area)) +
#'   geom_line(size = 1) +
#'   geom_point(size = 3) +
#'   labs(title = "Total Silviculture Area in Brazil (2013-2019)",
#'        x = "Year",
#'        y = "Area (hectares)") +
#'   theme_minimal()
#'
#' # States with largest plantations in 2019
#' largest_states <- silvi_area %>%
#'   filter(year == 2019) %>%
#'   group_by(state) %>%
#'   summarize(total_area = sum(area_hectares, na.rm = TRUE)) %>%
#'   arrange(desc(total_area)) %>%
#'   head(10)
#'
#' ### Example 3: Regional Silviculture Production
#'
#' Compare silviculture production across regions:
#'
#' library(dplyr)
#'
#' # Regional silviculture data
#' silvi_prod <- load_pevs(
#'   dataset = "pevs_silviculture",
#'   raw_data = FALSE,
#'   geo_level = "region",
#'   time_period = 2019,
#'   language = "eng"
#' )
#'
#' # Production by region
#' regional_production <- silvi_prod %>%
#'   group_by(region) %>%
#'   summarize(
#'     total_production_value = sum(production_value, na.rm = TRUE),
#'     total_quantity = sum(quantity_produced, na.rm = TRUE),
#'     num_products = n_distinct(product_type),
#'     .groups = 'drop'
#'   ) %>%
#'   arrange(desc(total_production_value))
#'
#' print(regional_production)
#'
#' # Product specialization by region
#' region_products <- silvi_prod %>%
#'   group_by(region, product_type) %>%
#'   summarize(
#'     value = sum(production_value, na.rm = TRUE),
#'     .groups = 'drop'
#'   ) %>%
#'   pivot_wider(
#'     names_from = product_type,
#'     values_from = value,
#'     values_fill = 0
#'   )
#'
#' @export
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
  param$source <- "pevs"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset, geo_level, and time_period are supported

  check_params(param)

  if (!is.numeric(param$dataset)) {
    param$code <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {
    param$code <- param$dataset
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
    attr(x, "label") <- label
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
