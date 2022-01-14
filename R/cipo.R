load_cipo <- function(dataset = "environmental_crimes",
                      search = ""){

param <- list()

param$dataset <- dataset
param$search <- clean_text(search)

# spreadsheet embedded into the page is sourced from google sheets

  if (dataset == "environmental_crimes"){
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
