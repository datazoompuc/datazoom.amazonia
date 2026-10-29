#' @title PPM - Municipal Livestock Production
#'
#' @description Loads information on animal farming inventories and livestock products (IBGE).
#'
#' @param dataset A dataset name ("ppm_livestock_inventory", "ppm_sheep_farming", "ppm_animal_origin_production", "ppm_cow_farming" or "ppm_aquaculture". You can also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021})
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble} consisting of geographic units that present positive values for any of the variables in the dataset.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated aquaculture data at the country level from 2013 to 2015
#' data <- load_ppm(
#'   dataset = "ppm_aquaculture",
#'   raw_data = FALSE,
#'   geo_level = "country",
#'   time_period = 2013:2015,
#'   language = "eng"
#' )
#'
#' # download raw sheep farming data by state from 1980 to 1995 in portuguese
#' data <- load_ppm(
#'   dataset = "ppm_sheep_farming",
#'   raw_data = TRUE,
#'   geo_level = "state",
#'   time_period = 1980:1995,
#'   language = "pt"
#' )
#'
#' @export

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
  param$source <- "ppm"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # A SIDRA table given as a number ("You can also use SIDRA codes") means the
  # manifest dataset that reads that table: load_ppm(dataset = 94) is
  # load_ppm(dataset = "ppm_cow_farming"), with the same classification, the
  # same treatment and the same checks. Only a number the manifest does not know
  # keeps the old behaviour below (sidra_download()'s defaults, raw data only).

  if (is.numeric(param$dataset)) {
    known <- sidra_dataset_of_table("ppm", param$dataset)
    if (!is.null(known)) param$dataset <- known
  }

  # check if dataset, geo_level, and time_period are supported

  check_params(param)

  # The manifest's sidra_code is "<table>" for a table with no classification
  # (94, 95) and "<table>/c<classification>/all" for the others
  # (e.g. "74/c80/all"); see actions/scrapers/resolve_ppm.R.

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

  # FRAGILE: the SIDRA table ids hardcoded here (94, 95, 3939, 74, 3940) are
  # copies of the table in the manifest's sidra_code; resolve_ppm.R checks that
  # the manifest's code still exists in IBGE's table and has the classification
  # it says (none for 94 and 95), not that it is still one of these ids. The
  # datasets are told apart below by name, not by table.
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

  # FRAGILE: each dataset names only the IBGE variable ids its table reports --
  # 105 (3939), 108 (95), 107 (94), 106 and 215 (74), 4146 and 215 (3940) -- in
  # its own "<dataset>_variables" entry of load_dictionary() (R/dictionary.R).
  # Until that entry replaced a case_when() with no default, any other id would
  # have become NA and ended up in a column called na_v<category code> (the bug
  # load_pam() had with 2313 until 2026-10-21); now it becomes v<id> and warns.
  # Checked against IBGE's metadata and the data itself on 2026-10-22: these are
  # every variable the five tables report. resolve_ppm.R re-checks that on every
  # run (PPM_NAMED_VARIABLES there is a copy of the entries' ids;
  # tests/testthat/test-ppm-resolver.R fails if they differ).
  dat <- translate_sidra_variables(
    dat, paste0(param$dataset, "_variables"), language, "load_ppm()",
    columns = "v<id>_v<category code>"
  )


  #############################
  ## Create Long Format Data ##
  #############################

  # ==> The Output is a tibble with unit and year identifiers + production and/or value of each item

  # Warning: A double check on this aggregation (despite the unique identifiers established) would be welcomed!

  ## Cattle Number

  # FRAGILE: names_from uses columns that are IBGE's classification labels after
  # janitor::clean_names() ("Tipo de rebanho (Codigo)" -> tipo_de_rebanho_codigo,
  # "Tipo de produto de origem animal (Codigo)", "Tipo de produto da
  # aquicultura (Codigo)"); the origin-animal pivot also selects the column RANGE
  # variavel:tipo_de_produto_de_origem_animal_codigo, so it depends on sidrar's
  # column order. If IBGE rewords a label or sidrar reorders columns, pivot_wider()
  # stops with a missing-column error (loud) or, for the range, pivots on the
  # wrong columns (not).
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
