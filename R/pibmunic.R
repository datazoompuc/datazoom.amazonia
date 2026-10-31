#' @title PIB MUNICIPAL - Municipal GDP
#'
#' @description Loads information on gross domestic product at current prices, taxes, net of subsidies, on products at current prices and gross value added at current prices, total and by economic activity, and respective shares.
#'
#' @param dataset A dataset name ("pibmunic") with Municipal GDP information. You can also use SIDRA codes (See \url{https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas})
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated municipal GDP data at the state level for 2010 to 2012
#' data <- load_pibmunic(
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   time_period = 2010:2012,
#'   language = "eng"
#' )
#'
#' @export

load_pibmunic <- function(dataset = "pibmunic", raw_data = FALSE,
                          geo_level, time_period,
                          language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  sidra_code <- available_time <- legal_amazon <- municipio_codigo <- ano <- NULL
  ano_codigo <- geo_id <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida <- unidade_de_medida_codigo <- valor <- variavel <- variavel_codigo <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "pibmunic"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # A SIDRA table given as a number ("You can also use SIDRA codes") means the
  # manifest dataset that reads that table: load_pibmunic(dataset = 5938) is
  # load_pibmunic(dataset = "pibmunic"), with the same request, the same
  # treatment and the same checks. Only a number the manifest does not know
  # keeps the old behaviour below (sidra_download()'s defaults, no checks).

  if (is.numeric(param$dataset)) {
    known <- sidra_dataset_of_table("pibmunic", param$dataset)
    if (!is.null(known)) param$dataset <- known
  }

  # check if dataset, geo_level, and time_period are supported

  check_params(param)

  # FRAGILE: the manifest's sidra_code is the bare table number ("5938"), and a
  # bare code MEANS "this table has no classification": sidra_download() then
  # sends the request without one (sidra_get_table(), R/download.R), which skips
  # the IBGE HTML page sidrar scrapes for classific = "all" -- a page behind a
  # Cloudflare bot challenge (HTTP 403) from some networks, where load_pibmunic()
  # failed with "Error in Download.". If IBGE gave table 5938 a classification,
  # that request would still succeed (HTTP 200) but return only the "Total" slice,
  # silently; resolve_pibmunic.R checks the manifest's code against the table's
  # metadata every week for exactly that.
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

  dat <- dat %>%
    dplyr::select(-c(nivel_territorial_codigo, nivel_territorial, ano_codigo)) %>%
    dplyr::mutate(valor = as.numeric(valor))

  ## Only Keep Valid Observations

  dat <- dat %>%
    dplyr::filter(!is.na(valor))

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  # FRAGILE: the geographic columns are named here as sidrar labels them in
  # IBGE's response and janitor::clean_names() writes them -- brasil,
  # unidade_da_federacao(_codigo), municipio(_codigo) -- along with the other
  # label-derived names selected above (nivel_territorial*, ano_codigo,
  # unidade_de_medida*, variavel*). A label IBGE renames stops this with an
  # "object not found" error; nothing checks it ahead of time.
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

  # FRAGILE: a column gets its label by tidyr::matches(<var_code>), a regular
  # expression on the column's name, which ends in "_v<variable id>". It only
  # works because no id of the "pibmunic" dictionary is the tail of another
  # except 571/570 inside 6571/6570, where the longer id comes later in the
  # dictionary and so overwrites the shorter one's label. A variable IBGE adds to
  # table 5938 has no label (resolve_pibmunic.R reports it); one whose id
  # contains an existing id would take that id's label, and a reordering of the
  # dictionary would swap 571/6571 and 570/6570.
  dic <- load_dictionary(param$dataset)

  types <- as.character(dic$var_code)
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

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}
