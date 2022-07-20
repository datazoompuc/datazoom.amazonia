


oad_ibama_multas <- function(dataset = NULL, raw_data = FALSE,
                     geo_level = "municipality",
                     uf = NULL, language = "pt") {

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data
  param$uf = uf

  param$survey_name <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ##############
  ## Download ##
  ##############

  geo <- municipalities %>%
    dplyr::select(
      code_muni,
      "municipio" = name_muni,
      code_state,
      "uf" = abbrev_state
    )

  uf <- geo %>%
    dplyr::filter(
      uf == param$uf
    ) %>%
    dplyr::select(uf) %>%
    dplyr::distinct() %>%
    unlist()


  dat <- external_download(
    dataset = param$dataset,
    source = "ibama_multas",
    geo_level = param$geo_level,
    uf = param$uf
  )

  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  ## Return Raw Data
  if (param$raw_data) {
    return(dat)
  }





}
