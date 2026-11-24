#' @title CEMPRE - Central Register of Companies
#'
#' @description Loads information on companies and other organizations and their respective formally constituted local units, registered with the CNPJ - National Register of Legal Entities.
#'
#' @param dataset A dataset name ("cempre").
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#' @param sectors A \code{boolean} that defines if the data will be return separated by sectors (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return A \code{tibble}.
#'
#' @details IBGE publishes this series in two SIDRA tables: table 6449 for 2006 to 2021, which it has
#' closed ("serie encerrada em 2021"), and table 9418 for 2022 onwards. They have the same structure
#' and \code{load_cempre()} reads the one that has each year, but the two do not join up: Brazil's
#' number of companies goes from 5.75 million in 2021 to 9.43 million in 2022, so a comparison
#' across 2021 and 2022 is a comparison across two series.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated national-level data from 2008 to 2010
#' data <- load_cempre(
#'   raw_data = FALSE,
#'   geo_level = "country",
#'   time_period = 2008:2010,
#'   language = "eng"
#' )
#'
#' # download treated state-level data split by sector
#' data <- load_cempre(
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2008:2010,
#'   language = "pt",
#'   sectors = TRUE
#' )
#'
#' @export

load_cempre <- function(dataset = "cempre", raw_data = FALSE,
                        geo_level, time_period,
                        language = "eng", sectors = FALSE) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo <- NULL
  geo_id <- id_code <- nivel_territorial <- nivel_territorial_codigo <- valor <- NULL
  variavel <- unidade_de_medida <- unidade_de_medida_codigo <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "cempre"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language

  # A SIDRA table given as a number ("You can also use SIDRA codes") means the
  # manifest dataset that reads it: load_cempre(dataset = 9418) -- or 6449, the closed
  # table of 2006-2021 -- is load_cempre(dataset = "cempre"), with the same requests, the
  # same treatment and the same checks. Only a number the manifest does not know keeps
  # the old behaviour below (that table, the CNAE classification, no checks).

  if (is.numeric(param$dataset)) {
    known <- sidra_dataset_of_table("cempre", param$dataset)
    if (is.null(known) && param$dataset %in% vapply(cempre_closed_tables, function(closed) closed$table, numeric(1))) {
      known <- "cempre"
    }
    if (!is.null(known)) param$dataset <- known
  }

  # check if dataset, time_period, and geo_level are valid

  check_params(param)

  # FRAGILE: the series is in TWO tables. IBGE closed table 6449 with the 2021 data (its
  # name ends "serie encerrada em 2021") and publishes 2022 onwards in table 9418, which
  # has the same name, the same classification (c12762, the CNAE 2.0), the same 1,067
  # categories, the same four variables and the same territorial levels. The manifest's
  # sidra_code names the open one with the classification and the category sectors = FALSE
  # asks for, "9418/c12762/117897" (117897 is the Total); cempre_table_for_year() sends the
  # years of the closed one to it. If IBGE closes 9418 too, the manifest has to point to
  # its successor and 9418 join cempre_closed_tables (resolve_cempre.R reports a table
  # whose name says it is closed). The two series do not join up: Brazil's number of
  # companies goes from 5.75 million in 2021 (table 6449) to 9.43 million in 2022 (9418).
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
    param$classific <- "c12762"
    param$category <- list("117897")
  }

  # The table that has a year: the manifest dataset sends the years of a closed table to it; a
  # table number the manifest does not know is asked for every year, as it always was.
  table_of_year <- function(year) {
    if (is.numeric(param$dataset)) param$code else cempre_table_for_year(year, param$code)
  }

  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  if (sectors & geo_level == "municipality") {
    warning("This may take too long")
  }

  ## Download separate by sectors
  if (sectors) {
    year_cnaes <- purrr::cross2(as.character(param$time_period), as.list(cempre_sectors))

    dat <- year_cnaes %>%
      purrr::map(function(year_cnae) {
        sidra_download(
          sidra_code = table_of_year(year_cnae[[1]]),
          year = year_cnae[[1]],
          geo_level = param$geo_level,
          classific = param$classific,
          category = list(year_cnae[[2]])
        )
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

    ## Download only the total
  } else {
    dat <- as.list(as.character(param$time_period)) %>%
      purrr::map(function(year_num) {
        sidra_download(
          sidra_code = table_of_year(year_num),
          year = year_num,
          geo_level = param$geo_level,
          classific = param$classific,
          category = param$category
        )
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  }

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################


  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })


  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor))

  ## Only Keep Valid Observations

  dat <- dat %>%
    dplyr::filter(!is.na(valor))


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

  if (sectors == TRUE) {
    # FRAGILE: a column's name ends in "<sector code>_<k>", with k the place of the variable in
    # cempre_variable_index (2585 -> 1, 707 -> 2, 708 -> 3, 662 -> 4). This replaces a
    # case_when() of 88 branches (22 sectors x 4 variables) with no default, which was exactly
    # that -- every branch was the sector's code, an underscore and k, checked on 2026-11-23 --
    # and which gave an NA to a variable it did not list. A variable nobody indexed now gets
    # "v<id>" for k and a warning. The suffix is also what the dictionary's id_code, and so
    # the labels, are matched on.
    dat <- dat %>%
      dplyr::mutate(id_code = cempre_id_code(
        classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo, variavel_codigo
      ))
  }

  # FRAGILE: the columns this and the geography above name are IBGE's labels after sidrar and
  # janitor::clean_names() -- brasil, unidade_da_federacao(_codigo), municipio(_codigo),
  # classificacao_nacional_de_atividades_economicas_cnae_2_0(_codigo), variavel -- and the
  # pivot names a column "<variavel>_V<id_code>". A label IBGE rewords stops this with an
  # "object not found" error. With sectors = FALSE there is no id_code: the columns are the
  # variables' names alone and carry no label (the dictionary's rows are per id_code).
  dat <- dat %>%
    dplyr::select(-"classificacao_nacional_de_atividades_economicas_cnae_2_0") %>%
    dplyr::arrange(classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo, variavel) %>%
    tidyr::pivot_wider(
      id_cols = c(ano, geo_id),
      names_from = c(variavel, id_code),
      values_from = valor,
      names_sep = "_V",
      values_fn = sum,
      values_fill = NA
    ) %>%
    janitor::clean_names()

  labelled <- function(x, label) {
    attr(x, "label") <- label
    x
  }

  # A column is named "<variable>_v<sector code>_<k>", so it is found by that exact id, anchored at
  # the end. Matching the id anywhere in the name (tidyr::matches(cols), as this did until
  # 2026-11-23) cannot misfire today -- the 88 ids are all six digits, an underscore and one
  # digit, none contained in another, checked -- but it did in load_pevs(), where a new code
  # (83410) contained an old one (3410) and took its label.
  label_pattern <- function(id_code) paste0("_v", id_code, "$")

  label_data_eng <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$id_code == cols, "var_eng"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(label_pattern(cols))),
        ~ labelled(., as.character(dic[dic$id_code == cols, "var_eng"]))
      )

    return(df)
  }


  label_data_pt <- function(df, cols, dic) {
    label_value <- as.character(dic[dic$id_code == cols, "var_pt"])

    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(tidyr::matches(label_pattern(cols))),
        ~ labelled(., as.character(dic[dic$id_code == cols, "var_pt"]))
      )

    return(df)
  }



  ## Load Dictionary

  dic <- load_dictionary(param$dataset)

  types <- as.character(dic$id_code)
  types <- types[types != "0"] ## Remove 0


  if (language == "eng") {
    for (i in seq_along(types)) {
      dat <- label_data_eng(dat, cols = types[i], dic = dic)
    }
  }

  if (language == "pt") {
    for (i in seq_along(types)) {
      dat <- label_data_pt(dat, cols = types[i], dic = dic)
    }
  }

  remove_numbers <- function(string) {
    stringr::str_remove(string = string, pattern = "_\\d")
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}

# The Total (117897) and the 21 sections (A to U) of the CNAE 2.0 that sectors = TRUE asks
# for, one request each: the Total of the classification and every category of its first
# level.
#
# FRAGILE: a copy of IBGE's classification c12762, not read from it. A section IBGE adds
# would be missing from the sector-by-sector download and the dictionary; resolve_cempre.R
# checks every run that these 22 are in the table and that they are all its sections
# (CEMPRE_SECTORS there is a copy of this vector, kept equal by tests/testthat/
# test-cempre-resolver.R, which also keeps the dictionary's 88 id_codes equal to these
# sectors times the four variables of cempre_variable_index).
cempre_sectors <- c(
  "117897", "116830", "116880", "116910", "117296", "117307", "117329", "117363", "117484",
  "117543", "117555", "117608", "117666", "117673", "117714", "117774", "117788", "117810",
  "117838", "117861", "117888", "117892"
)

# The k of a column's "<sector code>_<k>" suffix, by IBGE's variable id (see the FRAGILE note
# in load_cempre()): 2585 number of companies and other organizations, 707 total employed
# persons, 708 employed persons on a payroll, 662 wages and other remunerations. They are the
# four variables tables 6449 and 9418 declare, apart from their "percentual do total geral"
# twins (1002585, 1000707, 1000708, 1000662), which "allxp" never returns.
cempre_variable_index <- c(`2585` = 1L, `707` = 2L, `708` = 3L, `662` = 4L)

# The tables IBGE has closed, with the last year each has: the series of the manifest's table
# starts the year after. See the FRAGILE note in load_cempre().
cempre_closed_tables <- list(list(table = 6449, last_year = 2021L))

# The table that has `year`: a closed one when the year is within its span, else `code` (the
# manifest's, the open one).
cempre_table_for_year <- function(year, code) {
  for (closed in cempre_closed_tables) {
    if (as.integer(year) <= closed$last_year) {
      return(closed$table)
    }
  }
  code
}

# The id_code of a column -- "<sector code>_<k>" -- from the sector's code and the variable's id
# (both vectors of one length). A variable cempre_variable_index does not list is not given an NA
# but "v<id>" for its k, and a warning.
cempre_id_code <- function(sector_code, variable_code) {
  index <- unname(cempre_variable_index[as.character(variable_code)])
  unknown <- is.na(index)
  if (any(unknown)) {
    warning(
      "IBGE variable(s) with no index in load_cempre(): ", paste(unique(variable_code[unknown]), collapse = ", "),
      ". Their columns end in _v<id> instead of _<k>. Please report it at ",
      "https://github.com/datazoompuc/datazoom.amazonia/issues so it can be indexed.",
      call. = FALSE
    )
  }
  paste0(sector_code, "_", ifelse(unknown, paste0("v", variable_code), index))
}
