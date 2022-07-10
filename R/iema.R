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
  survey <- link <- municipio <- uf <- populacao_nao_atendida <- NULL
  code_muni <- name_muni <- code_state <- abbrev_state <- legal_amazon <- code_muni_6 <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

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

  dat <- external_download(
    dataset = param$dataset,
    source = "iema",
    geo_level = param$geo_level
  )


  ##############
  ## Cleaning ##
  ##############

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
    ) %>%
    dplyr::mutate(dplyr::across(municipio, tolower))

  dat <- dat %>%
    dplyr::mutate(uf = stringr::str_extract(municipio, "(?<=\\().+?(?=\\))")) %>%
    dplyr::mutate(uf = toupper(uf)) %>%
    tidyr::drop_na(uf)

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(municipio, ~ stringr::str_remove(., "\\([^()]+\\)")),
      dplyr::across(municipio, ~ stringr::str_trim(.))
    )

  geo <- municipalities %>%
    dplyr::select(
      code_muni,
      name_muni,
      code_state,
      abbrev_state,
      legal_amazon
    )

  geo <- geo %>%
    dplyr::mutate(code_muni_6 = as.integer(code_muni / 10)) %>%
    dplyr::distinct(code_muni_6, .keep_all = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    }) %>%
    dplyr::mutate(
      dplyr::across(name_muni, tolower)
    ) %>%
    dplyr::mutate(municipio = name_muni) %>%
    dplyr::select(-c(name_muni, abbrev_state, code_muni_6))


  dat <- dat %>%
    dplyr::left_join(geo, by = "municipio") %>%
    dplyr::relocate(
      uf, code_state, code_muni,
      legal_amazon, municipio
    )


  dat <- dat[c(-7, -8, -10, -30, -43, -53, -71, -109), ]

  dat <- dat[c(
    -102, -115, -137, -141, -147, -148, -149, -154,
    -155, -156, -177, -218, -261
  ), ]

  dat <- dat[c(-258, -284, -285, -290, -297, -307, -310, -338), ]



  ##################
  ### Language #####
  ##################


  if (param$language == "pt") {
    dat <- dat %>%
      dplyr::rename(
        cod_municipio = code_muni,
        cod_uf = code_state,
        amazonia_legal = legal_amazon
      )
  }


  if (param$language == "eng") {
    dat <- dat %>%
      dplyr::rename(
        city = municipio,
        population_without_electric_energy = populacao_nao_atendida,
        state = uf
      )
  }


  return(dat)
}
