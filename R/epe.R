#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name, either ("CONSUMO") for consumption, or ("CONSUMIDOR") for consumers
#' @param language Only available in Portuguese ("pt") as of now
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016
#' clean_epe <- load_epe(
#'   dataset = "CONSUMO",
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

load_epe <- function(dataset, raw_data = FALSE, time_period = "default",
                     geo_level = "State", language = "pt") {
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
  param$geo_level <- geo_level


  #################
  ## Downloading ##
  #################
  dat <- external_download(
    source = "EPE",
    dataset = param$dataset,
    year = param$time_period
  )

  ##################
  ## Period Check ##
  ##################
if(param$time_period != "default"){
  year_check <- datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>%
    as.character() %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    as.numeric()


  if (min(time_period) < year_check[1]) {
    stop("Provided time period less than supported. Check documentation for time availability.")
  }
  if (max(time_period) > year_check[2]) {
    stop("Provided time period greater than supported. Check documentation for time availability.")
  }
}

  ######################
  ## Data Engineering ##
  ######################

  if (param$raw_data == TRUE) {
    return(dat)
  } else {

  if (param$dataset == "energy_consumption_per_class"){
    base::message("Almost done.")
    if(param$time_period == "default"){
      param$time_period <- 2004:2021
    }

    if (param$geo_level == "State"){

    #Select sheets with 'UF' in the name
    sheets_selected <- as.data.frame(all_sheets) %>%
      filter(str_detect(as.data.frame(all_sheets)[,1], "UF") == T | str_detect(as.data.frame(all_sheets)[,1], "POR") == T)

    #Function to tidy a single sheet
    manipular <- function(sheet_name){

      #Selecting a data.frame to be manipulated
      df <- as.data.frame(dat[sheet_name])

      #Make months as row names and have exclusive names
      #(ex.: When year is 2004, month is still "jan" | when year is 2005, month is "jan_2")
      df <- df %>% janitor::row_to_names(5) %>% janitor::clean_names()

      #Maket pivot longer
      df <- pivot_longer(df, cols = c(2:length(df)))

      #Rename Colunms
      df[1,4] <- NA
      names(df) <- c("Estado", "Mes", paste0(sheet_name), "Ano")

      #Make year observations by the numbers associated with the month variable
      #(ex.: when "jan_10" > year = "2013" | when "jan_11" > year = "2014" | when "jan" > year = 2004)
      for(i in 1:nrow(df)){
        if(str_detect(df[i,"Mes"], "_") == TRUE){
          df[i,"Ano"] <- 2003 + as.numeric(str_split(df[i,"Mes"], "_")[[1]][[2]])
        } else {
          df[i, "Ano"] <- 2004
        }

      }

      #Make month observations lose the nunmbers (ex.: turn "jan_10" into "jan")
      for(i in 1:nrow(df)){

        if(str_detect(df[i,"Mes"], "_") == TRUE){
          df[i,"Mes"] <- (str_split(df[i,"Mes"], "_")[[1]][[1]])
        } else {
          df[i, "Mes"] <- df[i, "Mes"]
        }
      }

      return(df)

    }

    #Define an empty data.frame that will receive the data
    final_dat <- data.frame(Estado = as.character(NULL),
                            Mes = as.character(NULL),
                            Ano = as.integer(NULL))[numeric(0), ]
    #Apply function to all sheets selected and merge the outputs
    for(i in 1:nrow(sheets_selected)){
      single_sheet <- manipular(paste0(sheets_selected[i,1]))
      final_dat <- right_join(final_dat, single_sheet, by = c("Estado", "Ano", "Mes"))
    }
   return(final_dat)
  }

    if (param$geo_level == "SubSystem"){
  # funcao para limpar dados de consumo ----------------------------------------------
  limpa_consumo <- function(sheet_name) {
      # troca nome das colunas
      df <- as.data.frame(dat[sheet_name])
      colnames(df) <- c("Sistema", 1:12, "Total")

      # limpa
      clean_df <- df %>%
        dplyr::filter(Sistema == "Sistemas Isolados") %>%
        tidyr::drop_na() %>%
        dplyr::mutate(ano = 2022:2004) %>%
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

      sheets_selected_consumo <- as.data.frame(all_sheets) %>%
        filter(str_detect(as.data.frame(all_sheets)[,1], "POR") == F &
               str_detect(as.data.frame(all_sheets)[,1], "CONSUMIDORES") == F &
               str_detect(as.data.frame(all_sheets)[,1], "GENERO") == F &
               str_detect(as.data.frame(all_sheets)[,1], "UF") == F)

      final_dat_consumo <- data.frame()

      for(i in 1:nrow(sheets_selected_consumo)){
        db <- limpa_consumo(paste0(sheets_selected_consumo[i,1]))
        final_dat_consumo <- dplyr::bind_rows(final_dat_consumo, db)
      }

      sheets_selected_consumidores <- as.data.frame(all_sheets) %>%
        filter(str_detect(as.data.frame(all_sheets)[,1], "POR") == F &
                 str_detect(as.data.frame(all_sheets)[,1], "UF") == F &
                 str_detect(as.data.frame(all_sheets)[,1], "CONSUMIDORES") == T)

      final_dat_consumidores <- data.frame()

      for(i in 1:nrow(sheets_selected_consumidores)){
        db <- limpa_consumidores(paste0(sheets_selected_consumidores[i,1]))
        final_dat_consumidores <- dplyr::bind_rows(final_dat_consumidores, db)
      }


      dat <- dplyr::full_join(final_dat_consumidores,final_dat_consumo)

      return(dat)
    }

    if (param$geo_level == "Region") {

    }
}

  if (param$dataset == "national_energy_balance") {

    if(param$time_period == "default"){
      param$time_period <- 2011:2022
    }

    dat <- dat %>%
      dplyr::rename(
      "amazonia_legal" = "amz legal"
      )

    dat <- dat %>%
      dplyr::filter(ano %in% param$time_period)
    return(dat)
  }
  }
}


