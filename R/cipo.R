#' Title Plataforma CIPÓ - Mappings on environmental crimes
#'
#' @param dataset A dataset name ("brazilian_actors", "international_cooperation", "forest_governance")
#' @param search A \code{string} that filters entries containing it.
#'
#' @return A \code{tibble} of the chosen CIPÓ spreadsheet.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download the spreacdsheet on Brazilian actors involved in fighting environmental crimes
#' brazilian_actors <- load_cipo(dataset = "brazilian_actors")
#'
#' # searching only for entries containing IBAMA
#' actors_ibama <- load_cipo(dataset = "brazilian_actors",
#'                          search = "ibama")
#'
#' # entries containing IBAMA or FUNAI
#' actors_ibama <- load_cipo(dataset = "brazilian_actors",
#'                          search = "ibama|funai")
#' }
load_cipo <- function(dataset = "brazilian_actors",
                      search = ""){

param <- list()

param$dataset <- dataset
param$search <- clean_text(search)

# spreadsheet embedded into the page is sourced from google sheets

  if (dataset == "brazilian_actors"){
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpRIu-paL_8rtXLpiT-kCTJRa2Tf_jCCPZxZBc3sjCwMHL8mkrhG2eqVeeIdWkxLTUKPru5uYAWG6g/"
    skip_rows <- 2
  }
  else if (dataset == "international_cooperation"){
    url <- "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vSpyBina4qr3GG-5ZlKW8_fjQwgIP3lq5lxanpO5_bUZenCVFO6N-WrF3bTkpokVzNVpRnob9Jhn8qe/"
    skip_rows <- 0
  }
  else if (dataset == "forest_governance"){
    url <- "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vTpnO9DEiy1mMRwBI5jAzBbYhFVBlcsX4TNRZyoDYBNUhEPZcLviexaynCJfY3JC-CCBGy00-Fs3jxu/"
    skip_rows <- 0
  }

url <- paste0(url, "pub?output=csv")

df <- readr::read_csv(url, skip = skip_rows) %>%
  tidyr::unite(aux, sep = " ", remove = FALSE) %>%
  mutate(across(aux, clean_text)) %>%
  filter(across(aux, ~ str_detect(., search))) %>%
  select(-aux)

df

}

clean_text <- function(text){
   text %>%
  tolower() %>%
  stringi::stri_trans_general(., id = "Latin-ASCII")
}
