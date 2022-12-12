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
  if (param$geo_level == "State"){
    sheets_selected <- c("CONSUMO POR UF",
                         "CONSUMO CATIVO POR UF",
                         "CONSUMO RESIDENCIAL POR UF",
                         "CONSUMO INDUSTRIAL POR UF",
                         "CONSUMO COMERCIAL POR UF",
                         "CONSUMO OUTROS POR UF",
                         "CONSUMIDORES RESIDENCIAIS POR F", #Ha um erro de escrita na planilha de origem: 'F' ao invés de 'UF'
                         "CONSUMIDORES INDUSTRIAIS POR UF",
                         "CONSUMIDORES COMERCIAIS POR UF",
                         "CONSUMIDORES OUTROS POR UF")
  }
  if (param$geo_level == "Region" | param$geo_level == "Subsystem"){

    sheets_selected <- c("TOTAL",
                         "RESIDENCIAL",
                         "INDUSTRIAL",
                         "COMERCIAL",
                         "OUTROS",
                         "CATIVO",
                         "CONSUMIDORES RESIDENCIAIS",
                         "CONSUMIDORES TOTAIS")
  }

  dat <- external_download(
    source = "EPE",
    dataset = param$dataset,
    year = param$time_period,
    sheet = sheets_selected
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



  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "energy_consumption_per_class"){
    base::message("Download complete. Almost done.")
    if(param$time_period == "default"){
      param$time_period <- 2004:2021
    }

    if (param$geo_level == "State"){

      #Define an empty data.frame that will receive the data
      final_dat <- data.frame(Estado = as.character(NULL),
                              Mes = as.character(NULL),
                              Ano = as.integer(NULL))[numeric(0), ]

    #Select sheets with 'UF' in the name
      for (s in 1:length(sheets_selected)){

      #Selecting a data.frame to be manipulated
      df <- as.data.frame(dat[paste0(sheets_selected[s])])

      #Make months as row names and have exclusive names
      #(ex.: When year is 2004, month is still "jan" | when year is 2005, month is "jan_2")
      df <- df %>% janitor::row_to_names(5) %>% janitor::clean_names()

      #Maket pivot longer
      df <- pivot_longer(df, cols = c(2:length(df)))

      #Rename Colunms
      df[1,4] <- NA
      names(df) <- c("Estado", "Mes", paste0(sheets_selected[s]), "Ano")

      #Make year observations by the numbers associated with the month variable
      #(ex.: when "jan_10" > year = "2013" | when "jan_11" > year = "2014" | when "jan" > year = 2004)
      for(r in 1:nrow(df)){
        if(stringr::str_detect(df[r,"Mes"], "_") == TRUE){
          df[r,"Ano"] <- 2003 + as.numeric(str_split(df[r,"Mes"], "_")[[1]][[2]])
        } else {
          df[r, "Ano"] <- 2004
        }

      }

      #Make month observations lose the nunmbers (ex.: turn "jan_10" into "jan")
      for(r in 1:nrow(df)){

        if(stringr::str_detect(df[r,"Mes"], "_") == TRUE){
          df[r,"Mes"] <- (str_split(df[r,"Mes"], "_")[[1]][[1]])
        } else {
          df[r, "Mes"] <- df[r, "Mes"]
        }
      }
      final_dat <- right_join(final_dat, df, by = c("Estado", "Ano", "Mes"))
    }

    final_dat <- final_dat %>% janitor::clean_names()

    if (param$language == "eng"){
      final_dat <- final_dat %>%
                    rename("state" = "estado",
                           "month" = "mes",
                           "year" = "ano",
                           "consumption_per_state" = "consumo_por_uf",
                           "captive_consumption_per_state" = "consumo_cativo_por_uf",
                           "residential_consumption_per_state" = "consumo_residencial_por_uf",
                           "industrial_consumption_per_state" = "consumo_industrial_por_uf",
                           "comercial_consumption_per_state" = "consumo_comercial_por_uf",
                           "other_consumption_per_state" = "consumo_outros_por_uf",
                           "residential_consumers_per_state" = "consumidores_residenciais_por_f", # Ha um erro na planilha de origem, por isso 'f' ao inves de 'uf'
                           "industrial_consumers_per_state" = "consumidores_industriais_por_uf",
                           "comercial_consumers_per_state" = "consumidores_comerciais_por_uf",
                           "other_consumers_per_state" = "consumidores_outros_por_uf"
                           )
    }
   return(final_dat)
  }

    ###########################
    ## Geo-Level = Subsystem ##
    ###########################

    if (param$geo_level == "Subsystem" | param$geo_level == "Region"){

      if (param$geo_level == "Subsystem"){

        geolevel.pattern <- "SUBSISTEMA ELÉTRICO"
        geolevel.names <- "subsistema"

        #define an empty data.frame for consumption data
        final_dat_consumo <- data.frame(subsistema = as.character(NULL),
                                        mes = as.character(NULL),
                                        ano = as.integer(NULL))[numeric(0), ]

        #define an empty data.frame for consumers data
        final_dat_consumidores <- data.frame(subsistema = as.character(NULL),
                                             mes = as.character(NULL),
                                             ano = as.integer(NULL))[numeric(0), ]
      }

      if (param$geo_level == "Region"){

        geolevel.pattern <- "REGIÃO GEOGRÁFICA"
        geolevel.names <- "regiao"

         #define an empty data.frame for consumption data
        final_dat_consumo <- data.frame(regiao = as.character(NULL),
                                        mes = as.character(NULL),
                                        ano = as.integer(NULL))[numeric(0), ]

        #define an empty data.frame for consumers data
        final_dat_consumidores <- data.frame(regiao = as.character(NULL),
                                             mes = as.character(NULL),
                                             ano = as.integer(NULL))[numeric(0), ]
      }

  #Select sheets with consumption data
  sheets_selected_consumo <- as.data.frame(sheets_selected) %>%
    dplyr::filter(stringr::str_detect(as.data.frame(sheets_selected)[,1], "CONSUMIDORES") == F)

  #Select sheets with consumers data
  sheets_selected_consumidores <- as.data.frame(sheets_selected) %>%
    dplyr::filter(stringr::str_detect(as.data.frame(sheets_selected)[,1], "CONSUMIDORES") == T)


        #################################
        ### cleaning consumption data ###
        #################################

  for (s in 1:nrow(sheets_selected_consumo)){

      #select a data.frame form the list
      df <- as.data.frame(dat[paste0(sheets_selected_consumo[s,1])])

      #change column names
      colnames(df) <- c(paste0(geolevel.names), "jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez", "anual")

      #identify row numbers that have the geo_level we want
      if (param$geo_level == "Subsystem"){id <- grep(geolevel.pattern, df$subsistema)}
      if (param$geo_level == "Region"){id <- grep(geolevel.pattern, df$regiao)}

      dat.mid <- data.frame()
      row.x <- data.frame()

      #Loop that repeats for every row with the gelevel.pattern
      for(i in 1:length(id)){

      #1:5 because there are exactly five subsystems. It starts at 1 because it skips the line with the geolevel.pattern, which doesn't have data.
      for(j in 1:5){

        #bind every row that is under geolevel.patern
        row.x <- df[(id[i]+j),] %>%
        dplyr::mutate("ano" = 2004 + length(id) - i) %>%
        tidyr::pivot_longer(c(2:ncol(df)))

      dat.mid <- dplyr::bind_rows(dat.mid, row.x)

        }
      }
      names(dat.mid) <- c(paste0(geolevel.names), "ano", "mes", paste0("consumo_",sheets_selected_consumo[s,1]))
      final_dat_consumo <- dplyr::right_join(final_dat_consumo, dat.mid, by = c(paste0(geolevel.names), "ano", "mes"))

  }

        ###############################
        ### cleaning consumers data ###
        ###############################

        for (s in 1:nrow(sheets_selected_consumidores)) {

        #select a data.frame form the list
        df <- as.data.frame(dat[paste0(sheets_selected_consumidores[s,1])])

        #change column names
        colnames(df) <- c(paste0(geolevel.names), "jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")

        #identify row numbers that have the geo_level we want
        if (param$geo_level == "Subsystem"){id <- grep(geolevel.pattern, df$subsistema)}
        if (param$geo_level == "Region"){id <- grep(geolevel.pattern, df$regiao)}

        #define empty data.frame for manipulating each row of 'df' (row.x) and binding them in a new data.frame(dat.mid)
        dat.mid <- data.frame()
        row.x <- data.frame()


        for(i in 1:length(id)){

          #1:5 because there are exactly five regions.
          for(j in 1:5){

            #bind every row that is under SUBSISTEMA ELÉTRICO

            row.x <- df[(id[i]+j),] %>%
              mutate("ano" = 2004 + length(id) - i) %>%
              pivot_longer(c(2:ncol(df)))

            dat.mid <- bind_rows(dat.mid, row.x)

          }

        }
        names(dat.mid) <- c(paste0(geolevel.names),"ano","mes",paste0(sheets_selected_consumidores[s,1]))
        final_dat_consumidores <- right_join(final_dat_consumidores, dat.mid, by = c(paste0(geolevel.names), "ano", "mes"))
      }

      #merge CONSUMO and CONSUMIDOR dataframes
      dat <- dplyr::full_join(final_dat_consumidores,final_dat_consumo, by = c(paste0(geolevel.names), "ano", "mes")) %>%
        dplyr::arrange(ano) %>%
        janitor::clean_names() %>%
        dplyr::filter(ano %in% param$time_period)

      if(param$language == "eng"){
        if(param$geo_level == "Subsystem"){
        dat <- dat %>%
               dplyr::rename("subsystem" = "subsistema",
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
                dplyr::mutate(subsystem = case_when(subsystem == "Sistemas Isolados" ~ "Isolated Systems",
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
        if(param$geo_level == "Region"){
          dat <- dat %>%
            dplyr::rename("region" = "regiao",
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
            dplyr::mutate(region = case_when(region == "Norte" ~ "North",
                                             region == "Nordeste" ~ "Northeast",
                                             region == "Sudeste" ~ "Southeast",
                                             region == "Centro-Oeste" ~ "Midwest",
                                             region == "Sul" ~ "South"),
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
    }
      return(dat)
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


