#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads data on deforestation in the Legal Amazon region.
#'
#' @param dataset A dataset name ("deforestation" or "cloud").
#' @param year Year of the dataset to load (only required for "cloud" dataset).
#' @param raw_data Logical indicating whether to return raw data or processed tibble.
#' @param language Language for variable names ("eng" for English, "pt" for Portuguese).
#'
#' @return A tibble with the selected data.
#'
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE)
#' # in Portuguese (language = 'pt').
#' data <- load_prodes(
#'   dataset = "deforestation",
#'   raw_data = FALSE,
#'   language = "pt"
#' )
#' }
#'
#' @export

load_prodes <- function(dataset, year = NULL, raw_data = FALSE, language = "eng") {
  
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  year <- municipio <- cod_ibge <- estado <- area_km2 <- increment <- NULL
  municipality <- municipality_code <- state <- deforestation <- desmatamento2000 <- NULL
  
  #############################
  ## Define Basic Parameters ##
  #############################
  
  param <- list()
  param$source <- "prodes"
  param$dataset <- dataset
  param$year <- year
  param$raw_data <- raw_data
  param$language <- language
  
  # Check if dataset and time_period are supported
  check_params(param)
  
  ###################
  ## Download Data ##
  ###################
  
  # Function to download from Google Drive based on year
  download_from_drive <- function(year) {
    # Authenticate with Google Drive
    drive_auth()
    
    # Construct download link based on year
    download_link <- switch(year,
                            "2016" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",
                            "2017" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",
                            "2018" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",  # Replace with actual link for 2018
                            "2019" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",  # Replace with actual link for 2019
                            "2020" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",  # Replace with actual link for 2020
                            "2021" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",  # Replace with actual link for 2021
                            "2022" = "https://drive.google.com/uc?id=1bMDRs5-EYLQu-GRj6NREh9ukk_72HqKz",  # Replace with actual link for 2022
                            stop("Year not supported.")
    )
    
    # Download the file
    temp_file <- tempfile(fileext = ".rds")
    download.file(download_link, destfile = temp_file, mode = "wb")
    
    # Return file path
    return(temp_file)
  }
  
  # Download the appropriate file based on dataset and year
  if (param$source == "prodes") {
    if (param$dataset == "cloud") {
      if (!is.null(param$year)) {
        # Download the file from Google Drive based on year
        local_file <- download_from_drive(param$year)
        
        # Load the downloaded data
        dat <- readRDS(local_file)
      } else {
        stop("Year must be specified for the 'cloud' dataset.")
      }
    } else {
      stop("Dataset not supported for 'prodes' source.")
    }
  } else {
    stop("Source not supported.")
  }
  
  ## Return Raw Data
  
  if (param$raw_data) {
    return(dat)
  }
  
  ######################
  ## Data Engineering ##
  ######################
  
  # Keep only relevant variables for 'cloud' dataset
  
  if (param$dataset == "cloud") {
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::select(
        municipio, cod_ibge, estado, area_km2, desmatamento2000, dplyr::starts_with("incremento")
      ) %>%
      dplyr::rename("incremento2000" = "desmatamento2000") %>%
      tidyr::pivot_longer(
        dplyr::starts_with("incremento"),
        names_prefix = "incremento",
        names_to = "year",
        values_to = "increment"
      ) %>%
      dplyr::arrange(municipio, year) %>%
      dplyr::mutate(
        deforestation = cumsum(increment),
        increment = dplyr::case_when(
          year == 2000 ~ NA,
          TRUE ~ increment
        )
      )
  }
  
  ############################
  ## Harmonizing Variable Names ##
  ############################
  
  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality" = municipio,
        "municipality_code" = cod_ibge,
        "state" = estado
      )
  } else if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "ano" = year,
        "cod_municipio" = "cod_ibge",
        "uf" = estado,
        "incremento" = increment,
        "desmatamento" = deforestation
      )
  } else {
    stop("Unsupported language. Supported options: 'eng' (English), 'pt' (Portuguese).")
  }
  
  return(dat_mod)
}
