#' Plataforma CIPÓ - Mappings on environmental crimes
#'
#' @param dataset A dataset name ("brazilian_actors", "international_cooperation", "forest_governance")
#' @inheritParams load_baci
#' @param search A \code{string} that filters entries containing it.
#'
#' @return A \code{tibble} of the chosen CIPÓ spreadsheet.
#'
#' @examples \dontrun{
#' # download the spreacdsheet on Brazilian actors involved in fighting environmental crimes
#' brazilian_actors <- load_cipo(dataset = "brazilian_actors")
#'
#' # searching only for entries containing IBAMA
#' actors_ibama <- load_cipo(
#'   dataset = "brazilian_actors",
#'   search = "ibama"
#' )
#'
#' # entries containing IBAMA or FUNAI
#' actors_ibama <- load_cipo(
#'   dataset = "brazilian_actors",
#'   search = "ibama|funai"
#' )
#' }
#'
#' @export
#'

load_cipo <- function(dataset = "brazilian_actors", raw_data = FALSE,
                      search = "") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  aux <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$dataset <- dataset
  param$raw_data <- raw_data
  param$search <- clean_text(search)

  ######################
  ## Downloading Data ##
  ######################

  # spreadsheet embedded into the page is sourced from google sheets

  dat <- external_download(
    source = "cipo",
    dataset = param$dataset
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # only modification is to filter rows that include the 'search' parameter

  dat <- dat %>%
    tidyr::unite(aux, sep = " ", remove = FALSE) %>%
    dplyr::mutate(dplyr::across(aux, clean_text)) %>%
    dplyr::filter(stringr::str_detect(aux, param$search)) %>%
    dplyr::select(-aux)

  # no translation as everything is text-based

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}

clean_text <- function(text) {
  text %>%
    tolower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII")
}
