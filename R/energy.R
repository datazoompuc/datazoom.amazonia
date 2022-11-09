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
          df <- pivot_longer(df, cols = c(2:length(df)))

          #renomear colunas
          df[1,4] <- NA
          names(df) <- c("Estado", "Mes", paste0(sheet_name), "Ano")

          #criar uma coluna de ano onde entra o ano de cada observacao de acordo com o numero associado ao mes
          for(i in 1:nrow(df2)){
            if(str_detect(df2[i,"Mes"], "_") == TRUE){
              df2[i,"Ano"] <- 2003 + as.numeric(str_split(df2[i,"Mes"], "_")[[1]][[2]])
            } else {
              df2[i, "Ano"] <- 2004
            }

          }

          #Fazer com que as observacoes dos meses nao tenham numeros
          for(i in 1:nrow(df)){
            if(str_detect(df[i,"Mes"], "jan") == TRUE){
              df[i,"Mes"] <- "JAN"
            } else if(str_detect(df[i,"Mes"], "fev") == TRUE){
              df[i,"Mes"] <- "FEV"
            } else if(str_detect(df[i,"Mes"], "mar") == TRUE){
              df[i,"Mes"] <- "MAR"
            } else if(str_detect(df[i,"Mes"], "abr") == TRUE){
              df[i,"Mes"] <- "ABR"
            } else if(str_detect(df[i,"Mes"], "mai") == TRUE){
              df[i,"Mes"] <- "MAI"
            } else if(str_detect(df[i,"Mes"], "jun") == TRUE){
              df[i,"Mes"] <- "JUN"
            } else if(str_detect(df[i,"Mes"], "jul") == TRUE){
              df[i,"Mes"] <- "JUL"
            } else if(str_detect(df[i,"Mes"], "ago") == TRUE){
              df[i,"Mes"] <- "AGO"
            } else if(str_detect(df[i,"Mes"], "set") == TRUE){
              df[i,"Mes"] <- "SET"
            } else if(str_detect(df[i,"Mes"], "out") == TRUE){
              df[i,"Mes"] <- "OUT"
            } else if(str_detect(df[i,"Mes"], "nov") == TRUE){
              df[i,"Mes"] <- "NOV"
            } else if(str_detect(df[i,"Mes"], "dez") == TRUE){
              df[i,"Mes"] <- "DEZ"
            }
          }

          return(df)

        }

        #Define an empty data.frame that will receive the data
        final_dat <- data.frame(Estado = as.character(NULL),
                                Mes = as.character(NULL),
                                Ano = as.integer(NULL))[numeric(0), ]

        #Run function that tidies sheets and bind all sheets together
          for(i in 1:nrow(sheets_selected)){
          single_sheet <- manipular(paste0(sheets_selected[i,1]))
          final_dat <- right_join(final_dat, single_sheet, by = c("Estado", "Ano", "Mes"))
          }
        }
        #If user selects Region geo_level
      }




    ##############################################
    ############   AINDA FALTA FAZER  ############
    ##############################################

      else if (geo_level == "Region"){}

      #If user selects SubSystem geo_level
      else if (geo_level == "SubSystem"){}

} else if (param$dataset == "SIGA"){}

  }
}
