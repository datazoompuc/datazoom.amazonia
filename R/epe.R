#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016
#' clean_epe <- load_epe(
#'   dataset = "EPE",
#'   raw_data = FALSE,
#'   time_period = 2016
#' )
#' }
#'
#' @encoding UTF-8
#'
#' @importFrom magrittr %>%
#'
#' @export

load_epe <- function(dataset, raw_data = FALSE, time_period = 2004:2021,
                     language = "pt") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  Sistema <- ano <- Total <- Quantidade <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$time_period <- time_period

  dat <- external_download(
    source = "EPE",
    dataset = param$dataset,
    year = param$time_period
  )

  if (param$raw_data == TRUE) {
    return(dat)
  } else {
    # funcao para limpar dados de consumo -----------------------------------------------
    limpa_consumo <- function(sheet_name) {
      # troca nome das colunas
      df <- as.data.frame(dat[sheet_name])
      colnames(df) <- c("Sistema", 1:12, "Total")

      # limpa
      clean_df <- df %>%
        dplyr::filter(Sistema == "Sistemas Isolados") %>%
        tidyr::drop_na() %>%
        dplyr::slice(-1) %>% # remove ano corrente, para o qual dados sao parciais
        dplyr::mutate(ano = 2021:2004) %>%
        dplyr::mutate(ano = as.numeric(ano)) %>%
        dplyr::select(ano, Total) %>%
        dplyr::mutate(tipo = paste0("CONSUMO ", sheet_name)) %>% # adiciona coluna para tipo de consumo
        dplyr::rename("valor" = "Total")

      return(clean_df)
    }

    # funcao para limpar dados de consumidores -----------------------------------------------
    limpa_consumidores <- function(sheet_name) {
      df <- as.data.frame(dat[sheet_name])
      # troca nome das colunas
      colnames(df) <- c("Sistema", 1:12)

      # limpa
      clean_df <- df %>%
        dplyr::filter(Sistema == "Sistemas Isolados") %>%
        dplyr::slice(-1) %>% # remove ano corrente, para o qual dados sao parciais
        dplyr::mutate(ano = 2021:2004) %>%
        tidyr::pivot_longer(cols = c(2:13), names_to = "Mes", values_to = "Quantidade") %>%
        dplyr::select(ano, Quantidade) %>% # tira a coluna "Sistema", que tinha apenas valores iguais ("Sistemas Isolados") e "Mes"
        dplyr::mutate(ano = as.numeric(ano), Quantidade = as.numeric(Quantidade)) %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(media = mean(Quantidade)) %>%
        dplyr::mutate(tipo = sheet_name) # adiciona coluna para tipo de consumidor

      return(clean_df)
    }

    # roda funcao -----------------------------------------------------------------------
    # consumo total

    if (param$dataset == "CONSUMO") {
      db_consumo_total <- limpa_consumo("TOTAL")

      # consumo por residencial, industrial, comercial e outros
      db_consumo_residencial <- limpa_consumo("RESIDENCIAL")
      db_consumo_industrial <- limpa_consumo("INDUSTRIAL")
      db_consumo_comercial <- limpa_consumo("COMERCIAL")
      db_consumo_outros <- limpa_consumo("OUTROS")

      # bind all
      db_consumo <- dplyr::bind_rows(list(
        db_consumo_total, db_consumo_residencial,
        db_consumo_industrial, db_consumo_comercial,
        db_consumo_outros
      ))

      db_consumo <- dplyr::filter(db_consumo, ano %in% param$time_period)

      return(db_consumo)
    }
    # dados de consumidores -------------------------------------------------------------

    # roda funcao -----------------------------------------------------------------------
    # consumidores total
    if (param$dataset == "CONSUMIDOR") {
      db_consumidores_total <- limpa_consumidores("CONSUMIDORES TOTAIS")

      # consumidores por residencial, industrial, comercial e outros
      db_consumidores_residencial <- limpa_consumidores("CONSUMIDORES RESIDENCIAIS")

      # bind all
      db_consumidores <- dplyr::bind_rows(list(db_consumidores_total, db_consumidores_residencial))

      db_consumidores <- dplyr::filter(db_consumidores, ano %in% param$time_period)

      return(db_consumidores)
    }
  }
}
