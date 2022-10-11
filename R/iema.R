#' @title IEMA - Institute of Environment and Water Resources
#'
#' @description Loads information on electric energy access at the municipality level in the Amazon region
#'
#' @param dataset A dataset name ("iema")
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # Download treated data
#' data <- load_iema(raw_data = FALSE)
#' }
#'
#' @export

load_iema <- function(dataset = "iema", raw_data = FALSE, language = "eng") {

  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  municipio <- uf <- populacao_nao_atendida <- NULL
  code_muni <- name_muni <- code_state <- abbrev_state <- legal_amazon <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  ##############
  ## Download ##
  ##############

  dat <- external_download(
    dataset = param$dataset,
    source = "iema"
  )

  ## Return Raw Data
  if (param$raw_data) {
    return(dat)
  }

  ##############
  ## Cleaning ##
  ##############

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate( # removing accents from municipality names
      dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
    ) %>%
    dplyr::mutate(dplyr::across(municipio, tolower)) # making all municipality names lowercase

  # All cities in the municipio column come in the form "city name (uf)"

  dat <- dat %>%
    # UFs come between parentheses e.g. (RJ), this grabs all strings in that form
    dplyr::mutate(uf = stringr::str_extract(municipio, "(?<=\\().+?(?=\\))")) %>%
    dplyr::mutate(uf = toupper(uf)) %>% # making them uppercase again
    tidyr::drop_na(uf)

  dat <- dat %>%
    dplyr::mutate( # removing those "(uf)" bits
      dplyr::across(municipio, ~ stringr::str_remove(., "\\([^()]+\\)")),
      dplyr::across(municipio, ~ stringr::str_trim(.))
    )

  geo <- datazoom.amazonia::municipalities %>%
    dplyr::select(
      code_muni,
      name_muni,
      code_state,
      "uf" = abbrev_state,
      legal_amazon
    )

  # Removing accents from the dataset with IBGE codes to make the city names compatible and merge

  geo <- geo %>%
    dplyr::mutate(
      dplyr::across(
        name_muni,
        ~ stringi::stri_trans_general(., id = "Latin-ASCII"),
        .names = "municipio"
      )
    ) %>%
    dplyr::mutate(dplyr::across(municipio, tolower))

  # Merging IEMA with IBGE municipalities

  dat <- dat %>%
    dplyr::left_join(geo, by = c("municipio", "uf"))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  # Removing lowercase municipalities
  dat <- dat %>%
    dplyr::select(-municipio)

  # Column positions
  dat <- dat %>%
    dplyr::relocate(
      code_muni, name_muni, uf, code_state, legal_amazon, populacao_nao_atendida
    )

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "cod_municipio" = code_muni,
        "municipio" = name_muni,
        "cod_uf" = code_state,
        "amazonia_legal" = legal_amazon
      )
  }


  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality_code" = code_muni,
        "municipality" = name_muni,
        "state" = uf,
        "state_code" = code_state,
        "population_without_electricity" = populacao_nao_atendida
      )
  }


  return(dat_mod)
}
