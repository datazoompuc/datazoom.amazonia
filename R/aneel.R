#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("cde_budget_expenses")
#' @param language Only available in Portuguese ("pt") as of now
#' @param raw_data Only treated data (\code{False}) is available
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016 (takes a long time to download)
#' clean_aneel <- load_aneel(
#'   raw_data = FALSE,
#'   time_period = 2016
#' )
#' }
#'
#' @export

load_aneel <- function(dataset, raw_data = FALSE, time_period = 2013:2022,
                       language = "pt") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  Ano <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$time_period <- time_period

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    source = "ANEEL",
    dataset = param$dataset,
    year = param$time_period
  )

  ######################
  ## Data Engineering ##
  ######################

  if (param$raw_data) {
    base::message("No raw data available")
  } else {

    if (param$dataset == "cde_budget_expenses"){

       dat <- dat %>%
        dplyr::filter(Ano %in% param$time_period)


            ################################
            ## Harmonizing Variable Names ##
            ################################


       if (param$language == "eng"){

         dat <- dat %>%
           dplyr::rename(
             "year" = "Ano",
             "type_of_expenses" = "Tipo de Despesa",
             "value" = "Soma de Valor",
             "share_of_total_amount" = "Participação"
           )
       }

       if (param$language == "pt"){

         dat <- dat %>%
           dplyr::rename(
             "ano" = "Ano",
             "tipo_de_despesa" = "Tipo de Despesa",
             "total" = "Soma de Valor",
             "participacao_no_total" = "Participação"
           )
       }
    }

    if (param$dataset == "distributed_generation_ventures"){

}

  }
  return(dat)
}
