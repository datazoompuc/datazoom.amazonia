#' @title Energy
#'
#' @description Downloading Aneel - Electrical Energy National Agency  - data and others
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
#' # download treated data for 2016 (takes a long time to download)
#' clean_baci <- load_baci(
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

load_energy <- function(dataset = "SIGA", raw_data = FALSE, time_period,
                        language = "pt", geo_level = "state"){

  ###########################
  ## Bind Global Variables ##
  ###########################



  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$time_period <- time_period
  param$language <- language

  ## Check if year is acceptable

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

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    source = "Energy",
    dataset = param$dataset,
    year = param$time_period
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  } else {

    if (param$dataset == "CMEEC"){
      #If user selects state level
      if(geo_level == "State"){

        #Select sheets with 'UF' in the name

        sheets_selected <- as.data.frame(all_sheets) %>% filter(str_detect(all_sheets[,1], "UF") == T)

        #Function to tidy a single sheet
        manipular <- function(sheet_name){

          #df sera manipulado
          df <- as.data.frame(dat[sheet_name])

          #transformar nomes das colunas nos meses referentes (para usar pivot_longer)
          df <- df %>% janitor::row_to_names(5) %>% janitor::clean_names()

          #fazer o pivot longer
          df2 <- pivot_longer(df, cols = c(2:length(df)))

          #renomear colunas
          df2[1,4] <- NA
          names(df2) <- c("Estado", "Mes", "Valor", "Ano")

          #criar uma coluna de ano onde entra o ano de cada observacao de acordo com o numero associado ao mes
          for(i in 1:nrow(df2)){
            if(str_detect(df2[i,"Mes"], "2") == TRUE){
              df2[i,4] <- 2005
            } else if(str_detect(df2[i,"Mes"], "3") == TRUE){
              df2[i,4] <- 2006
            } else if(str_detect(df2[i,"Mes"], "4") == TRUE){
              df2[i,4] <- 2007
            } else if(str_detect(df2[i,"Mes"], "5") == TRUE){
              df2[i,4] <- 2008
            } else if(str_detect(df2[i,"Mes"], "6") == TRUE){
              df2[i,4] <- 2009
            } else if(str_detect(df2[i,"Mes"], "7") == TRUE){
              df2[i,4] <- 2010
            } else if(str_detect(df2[i,"Mes"], "8") == TRUE){
              df2[i,4] <- 2011
            } else if(str_detect(df2[i,"Mes"], "9") == TRUE){
              df2[i,4] <- 2012
            } else if(str_detect(df2[i,"Mes"], "10") == TRUE){
              df2[i,4] <- 2013
            } else if(str_detect(df2[i,"Mes"], "11") == TRUE){
              df2[i,4] <- 2014
            } else if(str_detect(df2[i,"Mes"], "12") == TRUE){
              df2[i,4] <- 2015
            } else if(str_detect(df2[i,"Mes"], "13") == TRUE){
              df2[i,4] <- 2016
            } else if(str_detect(df2[i,"Mes"], "14") == TRUE){
              df2[i,4] <- 2017
            } else if(str_detect(df2[i,"Mes"], "15") == TRUE){
              df2[i,4] <- 2018
            } else if(str_detect(df2[i,"Mes"], "16") == TRUE){
              df2[i,4] <- 2019
            } else if(str_detect(df2[i,"Mes"], "17") == TRUE){
              df2[i,4] <- 2020
            } else if(str_detect(df2[i,"Mes"], "18") == TRUE){
              df2[i,4] <- 2021
            } else if(str_detect(df2[i,"Mes"], "19") == TRUE){
              df2[i,4] <- 2022
            } else if(str_detect(df2[i,"Mes"], "20") == TRUE){
              df2[i,4] <- 2023
            } else if(str_detect(df2[i,"Mes"], "21") == TRUE){
              df2[i,4] <- 2024
            } else {
              df2[i,4] <- 2004
            }
          }

          #Fazer com que as observacoes dos meses nao tenham numeros
          for(i in 1:nrow(df2)){
            if(str_detect(df2[i,"Mes"], "jan") == TRUE){
              df2[i,"Mes"] <- "JAN"
            } else if(str_detect(df2[i,"Mes"], "fev") == TRUE){
              df2[i,"Mes"] <- "FEV"
            } else if(str_detect(df2[i,"Mes"], "mar") == TRUE){
              df2[i,"Mes"] <- "MAR"
            } else if(str_detect(df2[i,"Mes"], "abr") == TRUE){
              df2[i,"Mes"] <- "ABR"
            } else if(str_detect(df2[i,"Mes"], "mai") == TRUE){
              df2[i,"Mes"] <- "MAI"
            } else if(str_detect(df2[i,"Mes"], "jun") == TRUE){
              df2[i,"Mes"] <- "JUN"
            } else if(str_detect(df2[i,"Mes"], "jul") == TRUE){
              df2[i,"Mes"] <- "JUL"
            } else if(str_detect(df2[i,"Mes"], "ago") == TRUE){
              df2[i,"Mes"] <- "AGO"
            } else if(str_detect(df2[i,"Mes"], "set") == TRUE){
              df2[i,"Mes"] <- "SET"
            } else if(str_detect(df2[i,"Mes"], "out") == TRUE){
              df2[i,"Mes"] <- "OUT"
            } else if(str_detect(df2[i,"Mes"], "nov") == TRUE){
              df2[i,"Mes"] <- "NOV"
            } else if(str_detect(df2[i,"Mes"], "dez") == TRUE){
              df2[i,"Mes"] <- "DEZ"
            }
          }

          return(df2)

        }

        #Run function that tidies sheets and bind all sheets together

        final_dat <- data.frame(Estado = as.character(NULL),
                                Mes = as.character(NULL),
                                Ano = as.integer(NULL))[numeric(0), ]

        for(i in 1:length(sheets_selected)){
          single_sheet <- manipular(paste0(sheets_selected[i,1]))
          final_dat <- full_join(final_dat, single_sheet, by = c("Estado", "Mes", "Ano"))
        }

        #If user selects region level
      } else if (geo_level == "Region"){}

} else if (param$dataset == "SIGA"){}

  }
}
