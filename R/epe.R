#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name ("SIGA").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble}.
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

load_epe <- function(raw_data = FALSE, time_period,
                     language = "pt", geo_level = "state"){

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language


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
    colnames(raw_df) <- c("Sistema", 1:12, "Total")

    # limpa
    clean_df <- raw_df %>%
      filter(Sistema == "Sistemas Isolados") %>%
      drop_na() %>%
      slice(-1) %>% # remove ano corrente, para o qual dados sao parciais
      mutate(Ano = 2021:2004) %>%  mutate(Ano = as.numeric(Ano)) %>%
      select(Ano, Total) %>%
      mutate(tipo = paste0("CONSUMO ", sheet_name)) %>% # adiciona coluna para tipo de consumo
      rename("valor" = "Total")

    return(clean_df)

  }

     # funcao para limpar dados de consumidores -----------------------------------------------
    limpa_consumidores <- function(sheet_name) {

      # troca nome das colunas
      colnames(raw_df) <- c("Sistema", 1:12)

      # limpa
      clean_df <- raw_df %>%
        filter(Sistema == "Sistemas Isolados") %>%
        slice(-1) %>% #remove ano corrente, para o qual dados sao parciais
        mutate(Ano = 2021:2004)  %>%
        pivot_longer(cols = c(2:13), names_to = "Mes", values_to = "Quantidade") %>%
        select(Ano, Quantidade) %>% #tira a coluna "Sistema", que tinha apenas valores iguais ("Sistemas Isolados") e "Mes"
        mutate(Ano = as.numeric(Ano), Quantidade = as.numeric(Quantidade)) %>%
        group_by(Ano) %>%
        dplyr::summarise(media = mean(Quantidade)) %>%
        mutate(tipo = sheet_name) # adiciona coluna para tipo de consumidor

      return(clean_df)

    }

  # roda funcao -----------------------------------------------------------------------
  # consumo total

 if(param$dataset == "CONSUMO"){
   db_consumo_total <- limpa_consumo("TOTAL")

  # consumo por residencial, industrial, comercial e outros
  db_consumo_residencial <- limpa_consumo("RESIDENCIAL")
  db_consumo_industrial <- limpa_consumo("INDUSTRIAL")
  db_consumo_comercial <- limpa_consumo("COMERCIAL")
  db_consumo_outros <- limpa_consumo("OUTROS")

  # bind all
  db_consumo <- bind_rows(list(db_consumo_total, db_consumo_residencial,
                               db_consumo_industrial, db_consumo_comercial,
                               db_consumo_outros))
 return(db_consumo)
  }
  # dados de consumidores -------------------------------------------------------------

  # roda funcao -----------------------------------------------------------------------
  # consumidores total
 if(param$dataset == "CONSUMIDOR"){
   db_consumidores_total <- limpa_consumidores("CONSUMIDORES TOTAIS")

  # consumidores por residencial, industrial, comercial e outros
  db_consumidores_residencial <- limpa_consumidores("CONSUMIDORES RESIDENCIAIS")

  # bind all
  db_consumidores <- bind_rows(list(db_consumidores_total, db_consumidores_residencial))

  return(db_consumidores)
}
  }
}
