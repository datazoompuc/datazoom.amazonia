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
    dataset = param$dataset
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

        sheets_selected <- all_sheets %>% filter(str_detect(all_sheets[,1], "UF") == T)

        #Function to tidy a single sheet
        manipular <- function(sheet_name){

        }

        #Run function that tidies sheets and bind all sheets together

        final_dat <- data.frame()
        for(i in 1:length(sheets_selected)){
          single_sheet <- manipular(dat[i])
          final_dat <- rbind(final_dat, single_sheet)
        }

        #If user selects region level
      } else if (geo_level == "Region"){}

} else if (param$dataset == "SIGA"){}

  }
}
