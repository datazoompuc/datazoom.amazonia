#' @title Population
#'
#' @description Loads information on (estimated) population
#'
#' @param dataset A dataset name ("population").
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download raw population data at the country level from 2008 to 2010
#' data <- load_population(
#'   raw_data = TRUE,
#'   geo_level = "country",
#'   time_period = 2008:2010,
#'   language = "eng"
#' )
#'
#' # download treated population data by state from 2008 to 2010 in portuguese
#' data <- load_population(
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2008:2010,
#'   language = "pt"
#' )
#'
#' @export

load_population <- function(dataset = "population", raw_data = FALSE,
                            geo_level, time_period, language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- geo_id <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida <- unidade_de_medida_codigo <- valor <- variavel <- variavel_codigo <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "population"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language

  # A SIDRA table given as a number ("You can also use SIDRA codes") means the
  # manifest dataset that reads that table: load_population(dataset = 6579) is
  # load_population(dataset = "population"), with the same requests, the same
  # treatment and the same checks. Only a number the manifest does not know keeps
  # the old behaviour below (sidra_download()'s defaults, no checks).

  if (is.numeric(param$dataset)) {
    known <- sidra_dataset_of_table("population", param$dataset)
    if (!is.null(known)) param$dataset <- known
  }

  # check if dataset, geo_level, and time_period are supported

  check_params(param)

  # The manifest's sidra_code is the bare table number ("6579"), and a bare code
  # means "this table has no classification": sidra_download() then sends the
  # request without one (sidra_get_table(), R/download.R) instead of letting sidrar
  # scrape IBGE's description of the table, a page behind a bot challenge from some
  # networks. See actions/scrapers/resolve_population.R.

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

  ##############
  ## Download ##
  ##############

  dat <- as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num) {
      request <- population_request(year_num, param$code, param$classific, param$category)

      one_year <- suppressWarnings(
        sidra_download(
          sidra_code = request$table,
          classific = request$classific,
          category = request$category,
          year = year_num,
          geo_level = param$geo_level
        )
      )

      # FRAGILE: the Censo 2022 table (4714) reports three variables -- the resident
      # population (93), the area of the territorial unit (6318) and the population
      # density (614) -- where the tables of 2007 and 2010 report only the population
      # (93) and the estimates only 9324, so the other two are left out: they would
      # become columns of 2022 alone, and the treatment below would pass them
      # through under IBGE's own names. A table IBGE adds a variable to is not
      # noticed here; resolve_population.R reports it.
      if (identical(request$table, 4714)) {
        one_year <- one_year[one_year$variavel_codigo == "93", ]
      }

      one_year
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

  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor)) %>%
    dplyr::select(unidade_de_medida_codigo:variavel)

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


  dat <- dat %>%
    dplyr::arrange(variavel_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = c(ano, geo_id),
      names_from = variavel:variavel_codigo,
      values_from = valor,
      names_sep = "_V",
      values_fn = sum,
      values_fill = NA
    ) %>%
    janitor::clean_names()

  # FRAGILE: only the two variable ids of the tables above are renamed (93, the
  # resident population of 2007, 2010 and 2022; 9324, the estimate of every other
  # year), and only in English: the Portuguese names are IBGE's own, cleaned. A
  # column recode() does not know passes through under IBGE's name instead of
  # turning NA, so a variable IBGE adds is not lost, just not renamed;
  # resolve_population.R reports it. The columns are named after sidrar's own labels
  # ("Populacao residente (Codigo)" and so on) once janitor::clean_names() has
  # written them.
  if (language == "eng") {
    dat <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "ano" = "year",
        "populacao_residente_v93" = "population_v93",
        "populacao_residente_estimada_v9324" = "estimated_population_v9324"
      )
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}

# The years the estimates table (6579) has no value for, and the table of each:
# the Contagem da Populacao of 2007 (793), the Censo Demografico of 2010 (1378)
# and the Censo Demografico of 2022 (4714). Each is the resident population of the
# year (variable 93) at the three levels load_population() offers, which is what
# table 6579 holds, as variable 9324 and estimated, for every other year. The
# requests are what the manifest's sidra_code would say if it could name more than
# one table: a table with no classification (793, 4714) is sent with none, and table
# 1378, which has four (situacao do domicilio, sexo, idade, condicao no domicilio), is
# asked for the Total of the first (c1, category 0) -- the others, left out, are
# answered with their Total too: Brazil's 190,755,799 of 2010.
#
# FRAGILE: this list is a copy of what IBGE publishes, and nothing in the manifest
# says it. 2022 was missing from it until 2026-11-12, so load_population(time_period
# = 2022) stopped with "Error in Download." (6579 has no 2022); 2023 is in neither
# this list nor 6579, and is no year of the series. resolve_population.R checks every
# run that each table below exists, has the classification the request assumes and a
# value for its year, that 6579 does not also publish that year, and writes the
# manifest's available_time from all of them (POPULATION_SPECIAL_TABLES there is a
# copy of this list, kept equal by tests/testthat/test-population-resolver.R). A
# census year IBGE adds -- 2032 -- is one more entry here and there.
population_special_tables <- list(
  `2007` = list(table = 793, classific = NULL, category = NULL),
  `2010` = list(table = 1378, classific = "c1", category = list(0)),
  `2022` = list(table = 4714, classific = NULL, category = NULL)
)

# What load_population() asks sidra_download() for in `year`: the table of the
# manifest's code (6579) with its classification, or the special table of that
# year. A table number the manifest does not know (any other `code`) is always
# asked for as it is.
population_request <- function(year, code, classific, category) {
  special <- if (identical(code, 6579)) population_special_tables[[as.character(year)]] else NULL
  if (!is.null(special)) {
    return(special)
  }
  list(table = code, classific = classific, category = category)
}
