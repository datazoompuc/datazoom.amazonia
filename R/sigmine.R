#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads information the mines being explored legally in Brazil, including their location, status, product being mined and area in square meters etc. Survey is done at municipal and state level
#'
#' @param dataset A dataset name ("sigmine_active")
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} with the selected data.
#'
#' @encoding UTF-8
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' # download state raw data
#' sigmine_active <- load_sigmine(
#'   dataset = "sigmine_active",
#'   raw_data = TRUE
#' )
#' }
load_sigmine <- function(dataset = "sigmine_active",
                         raw_data = FALSE,
                         language = "eng") {

  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- nome <- uf <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  ######################
  ## Downloading Data ##
  ######################

  dat <- external_download(dataset = param$dataset, source = "sigmine") %>%
    janitor::clean_names()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  a <- dat %>%
    dplyr::mutate(dplyr::across(
      nome:uf,
      ~ ifelse(stringr::str_detect(.x, "DADO N.O CADASTRADO"),
        NA, .x
      )
    ))

  if (language == "pt") {
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "ult_evento"] <- "ultimo_evento"
    names(a)[names(a) == "nome"] <- "empresa"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "uso"
    names(a)[names(a) == "area_ha"] <- "area_m2"
  } else if (language == "eng") {
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "numero"] <- "number"
    names(a)[names(a) == "ult_evento"] <- "last_event"
    names(a)[names(a) == "uf"] <- "state"
    names(a)[names(a) == "ano"] <- "year"
    names(a)[names(a) == "processo"] <- "process"
    names(a)[names(a) == "id"] <- "id"
    names(a)[names(a) == "fase"] <- "phase"
    names(a)[names(a) == "nome"] <- "company"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "use"
    names(a)[names(a) == "area_ha"] <- "area_m2"
  }

  return(a)
}
