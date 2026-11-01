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
#' ### DO NOT RUN ###
#' # download treated silviculture data by state from 2012 in portuguese
#' data <- load_pevs(
#'   dataset = "pevs_silviculture",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2012,
#'   language = "pt"
#' )
#'
#' # download raw forest crops data by region from 2012 to 2013
#' data <- load_pevs(
#'   dataset = "pevs_forest_crops",
#'   raw_data = TRUE,
#'   geo_level = "region",
#'   time_period = 2012:2013,
#'   language = "eng"
#' )
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
  param$source <- "pevs"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # A SIDRA table given as a number ("You can also use SIDRA codes") means the
  # manifest dataset that reads that table: load_pevs(dataset = 289) is
  # load_pevs(dataset = "pevs_forest_crops"), with the same classification, the
  # same treatment and the same checks. Only a number the manifest does not know
  # keeps the old behaviour below (sidra_download()'s defaults, raw data only).

  if (is.numeric(param$dataset)) {
    known <- sidra_dataset_of_table("pevs", param$dataset)
    if (!is.null(known)) param$dataset <- known
  }

  # check if dataset, geo_level, and time_period are supported

  check_params(param)

  # The manifest's sidra_code is "<table>/c<classification>/all" (e.g.
  # "289/c193/all"): the classification is spelled out so that sidra_download()
  # never asks sidrar to look it up -- see actions/scrapers/resolve_pevs.R.

  if (!is.numeric(param$dataset)) {
    parts <- datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::pull(sidra_code) %>%
      sidra_code_parts()

    param$code <- parts$table
    param$classific <- parts$classific
    param$category <- parts$category
  } else {
    param$code <- param$dataset
    param$classific <- "all"
    param$category <- "all"
  }

  ## Dataset

  # FRAGILE: the SIDRA table ids hardcoded here and below (289, 291, 5930) are
  # copies of the table in the manifest's sidra_code; resolve_pevs.R checks that the
  # manifest's code still exists in IBGE and has the classification it says, not
  # that it is still one of these ids. The datasets are told apart by table, not by
  # name: a manifest row moved to another table would skip the renames below and
  # stop at the pivot, which needs the tipo_de_produto_codigo column they create.
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
      sidra_download(
        sidra_code = param$code,
        classific = param$classific,
        category = param$category,
        year = year_num,
        geo_level = param$geo_level
      )
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

  # FRAGILE: these renames, the geographic columns above (brasil, grande_regiao,
  # unidade_da_federacao(_codigo), municipio(_codigo)) and the pivot below all name
  # columns as IBGE's labels are after sidrar and janitor::clean_names() -- "Tipo de
  # produto extrativo (Codigo)" -> tipo_de_produto_extrativo_codigo, "Tipo de
  # produto da silvicultura (Codigo)", "Especie florestal (Codigo)" -- and the pivot
  # also selects the column RANGE variavel:tipo_de_produto_codigo, so it depends on
  # sidrar's column order. A label IBGE rewords stops this with an "object not found"
  # error (loud); a reordering of sidrar's columns pivots on the wrong ones (not).
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

  # FRAGILE: each dataset names only the IBGE variable ids its table reports -- 144
  # and 145 (289), 142 and 143 (291), 6549 (5930) -- in its own "<dataset>_variables"
  # entry of load_dictionary() (R/dictionary.R). Until that entry replaced a
  # case_when() with no default, any other id would have become NA and ended up in a
  # column called na_v<category code> (the bug load_pam() had with 2313 until
  # 2026-10-21); now it becomes v<id> and warns. Checked against IBGE's metadata and
  # the data itself on 2026-10-31: these are every variable the three tables report.
  # resolve_pevs.R re-checks that on every run (PEVS_NAMED_VARIABLES there is a copy
  # of the entries' ids; tests/testthat/test-pevs-resolver.R fails if they differ).
  dat <- translate_sidra_variables(
    dat, paste0(param$dataset, "_variables"), language, "load_pevs()",
    columns = "v<id>_v<category code>"
  )

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

  # A column is named "<quant|value|valor|area>_v<category code>", so it is found by
  # that exact code, anchored at the end. Matching the code anywhere in the name
  # (tidyr::matches(cols), as this did until 2026-10-31) also hit the column of every
  # category whose code merely CONTAINS it: IBGE's 2025 categories 83410-83419 of
  # table 289 contain the codes 3410-3419, and came back labelled "Umbu (fruit)",
  # "Hevea (liquid latex)", ... -- the label of the shorter code.
  label_pattern <- function(code) paste0("_v", code, "$")

  label_data_eng <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$var_code == cols, "var_eng"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(label_pattern(cols))),
        ~ labelled(., as.character(dic[dic$var_code == cols, "var_eng"]))
      )

    return(df)
  }

  label_data_pt <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$var_code == cols, "var_pt"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(label_pattern(cols))),
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
