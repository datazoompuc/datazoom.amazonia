#' @title IEMA - Institute of Environment and Water Resources
#'
#' @description Loads information on electric energy access at the municipality level considering the Amazon region
#'
#' @param dataset A dataset name ("iema")
#' @param raw_data A \code{boolean} setting the return of processed data. Raw Data is not publicly available
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download processed data
#' iema_clean <- load_iema(
#'   dataset = "iema",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @export


load_iema <- function(dataset = "iema", raw_data = FALSE,
                      geo_level = "municipality", language = "pt") {

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
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  ##############
  ## Download ##
  ##############

  dat <- external_download(
    dataset = param$dataset,
    source = "iema",
    geo_level = param$geo_level
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

  geo <- municipalities %>%
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
