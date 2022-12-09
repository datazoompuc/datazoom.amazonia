#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name, ("energy_consumption_per_class") or ("national_energy_balance")
#' @param language Only available in Portuguese ("pt") as of now
#' @param geo_level A geographical level, ("State") or ("Subsystem"), only available for ("energy_consumption_per_class")
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data for 2016
#' clean_epe <- load_epe(
#'   dataset = "energy_consumption_per_class",
#'   geo_level = "State",
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
                     geo_level = "state", language = "pt") {
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

    ###########################
    ## Geo-Level = Subsystem ##
    ###########################

    if (param$geo_level == "Subsystem"){

  geolevel.pattern <- "SUBSISTEMA ELÉTRICO"

      ###########################
      ### Build limpa_consumo ###
      ###########################

  limpa_consumo <- function(sheet_name) {
      # troca nome das colunas
      df <- as.data.frame(dat[sheet_name])
      colnames(df) <- c("sistema", "jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez", "anual")

      #identify row numbers that have the geo_level we want
      id <- grep(geolevel.pattern, df$sistema)

      dat.final <- data.frame()
      row.x <- data.frame()
      for(i in 1:length(id)){

        #0:5 because there are only five subsystems. 0 is for SUBSISTEMA ELÉTRICO
      for(j in 1:5){

        #bind every row that is under SUBSISTEMA ELÉTRICO

        row.x <- df[(id[i]+j),] %>%
        mutate("ano" = as.character(2004 + length(id) - i)) %>%
        pivot_longer(c(2:ncol(df)))

      dat.final <- bind_rows(dat.final, row.x)

        }
      }
      names(dat.final) <- c("subsistema", "ano", "mes", "consumo")
      return(dat.final)

  }

      #########################
      ### Run limpa_consumo ###
      #########################

      sheets_selected_consumo <- as.data.frame(all_sheets) %>%
        filter(str_detect(as.data.frame(all_sheets)[,1], "POR") == F &
               str_detect(as.data.frame(all_sheets)[,1], "CONSUMIDORES") == F &
               str_detect(as.data.frame(all_sheets)[,1], "GENERO") == F &
               str_detect(as.data.frame(all_sheets)[,1], "UF") == F)

      final_dat_consumo <- data.frame(subsistema = as.character(NULL),
                                      ano = as.character(NULL),
                                      mes = as.character(NULL))

      for(i in 1:nrow(sheets_selected_consumo)){
        db <- limpa_consumo(paste0(sheets_selected_consumo[i,1]))
          names(db) <- c("subsistema","ano","mes",paste0("CONSUMO ", sheets_selected_consumo[i,1]))
        final_dat_consumo <- dplyr::full_join(final_dat_consumo, db, by = c("subsistema", "ano", "mes"))
      }

      ################################
      ### Build limpa_consumidores ###
      ################################

      limpa_consumidores <- function(sheet_name) {
        # troca nome das colunas
        df <- as.data.frame(dat[sheet_name])
        colnames(df) <- c("sistema", "jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")

        #identify row numbers that have the geo_level we want
        id <- grep(geolevel.pattern, df$sistema)

        dat.final <- data.frame()
        row.x <- data.frame()
        for(i in 1:length(id)){

          #0:5 because there are only five subsystems. 0 is for SUBSISTEMA ELÉTRICO
          for(j in 1:5){

            #bind every row that is under SUBSISTEMA ELÉTRICO

            row.x <- df[(id[i]+j),] %>%
              mutate("ano" = as.character(2004 + length(id) - i)) %>%
              pivot_longer(c(2:ncol(df)))

            dat.final <- bind_rows(dat.final, row.x)

          }
        }
        names(dat.final) <- c("subsistema", "ano", "mes", "consumidores")
        return(dat.final)

      }

      ##############################
      ### Run limpa_consumidores ###
      ##############################

      sheets_selected_consumidores <- as.data.frame(all_sheets) %>%
        filter(str_detect(as.data.frame(all_sheets)[,1], "POR") == F &
                 str_detect(as.data.frame(all_sheets)[,1], "UF") == F &
                 str_detect(as.data.frame(all_sheets)[,1], "CONSUMIDORES") == T)

      final_dat_consumidores <- data.frame(subsistema = as.character(NULL),
                                           ano = as.character(NULL),
                                           mes = as.character(NULL))

      for(i in 1:nrow(sheets_selected_consumidores)){
        db <- limpa_consumidores(paste0(sheets_selected_consumidores[i,1]))
        names(db) <- c("subsistema","ano","mes",paste0(sheets_selected_consumidores[i,1]))
        final_dat_consumidores <- dplyr::full_join(final_dat_consumidores, db, by = c("subsistema", "ano", "mes"))
      }


      #merge CONSUMO and CONSUMIDOR dataframes
      dat <- dplyr::full_join(final_dat_consumidores,final_dat_consumo, by = c("subsistema", "ano", "mes")) %>%
        arrange(ano) %>%
        janitor::clean_names() %>%
        filter(ano %in% param$time_period)

      if(param$language == "eng"){
        dat <- rename(dat,
                      "subsystem" = "subsistema",
                      "year" = "ano",
                      "month" = "mes",
                      "residential_consumers" = "consumidores_residenciais",
                      "total_consumers" = "consumidores_totais",
                      "total_consumption" = "consumo_total",
                      "residencial_consumption" = "consumo_residencial",
                      "industrial_consumption" = "consumo_industrial",
                      "comercial_consumption"= "consumo_comercial",
                      "other_consumption"= "consumo_outros",
                      "captive_consumption"= "consumo_cativo"
                      ) %>%
                mutate(subsystem = case_when(subsystem == "Sistemas Isolados" ~ "Isolated Systems",
                                             subsystem == "Norte" ~ "North",
                                             subsystem == "Nordeste" ~ "Northeast",
                                             subsystem == "Sudeste/C.Oeste" ~ "Southeast/Midwest",
                                             subsystem == "Sul" ~ "South"),
                       month = case_when(month == "jan" ~ "jan",
                                         month == "fev" ~ "feb",
                                         month == "mar" ~ "mar",
                                         month == "abr" ~ "apr",
                                         month == "mai" ~ "may",
                                         month == "jun" ~ "jun",
                                         month == "jul" ~ "jul",
                                         month == "ago" ~ "aug",
                                         month == "set" ~ "sep",
                                         month == "out" ~ "oct",
                                         month == "nov" ~ "nov",
                                         month == "dez" ~ "dec",
                                         month == "anual" ~ "yearly"
                                         ))

    }
      return(dat)
    }

    ########################
    ## Geo-Level = Region ##
    ########################

    if (param$geo_level == "Region") {
      geolevel.pattern <- "REGIÃO GEOGRÁFICA"

    }
}
########################################
## Data-Set = national_energy_balance ##
########################################
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


