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


load_iema = function(dataset = "iema", raw_data = FALSE,
                     geo_level = "municipality", language = "pt") {



  survey <- link <- municipio <- uf <- populacao_nao_atendida <- NULL
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

dat = dat %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
  ) %>%
  dplyr::mutate(dplyr::across(municipio, tolower))

dat = dat %>%
  dplyr::mutate(uf = stringr::str_extract(municipio, "(?<=\\().+?(?=\\))")) %>%
  dplyr::mutate(uf = toupper(uf)) %>%
  tidyr::drop_na(uf)

dat = dat %>%
  dplyr::mutate(
    dplyr::across(municipio, ~ stringr::str_remove(., "\\([^()]+\\)")),
    dplyr::across(municipio, ~ stringr::str_trim(.))
  )


##################
### Language #####
##################


if(param$language == "eng"){

  dat = dat %>%
    dplyr::rename(city = municipio,
                  population_without_electric_energy = populacao_nao_atendida,
                  state = uf)
}


return(dat)

}


