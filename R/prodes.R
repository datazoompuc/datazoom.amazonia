#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads data on deforestation in the Legal Amazon region.
#'
#' @param dataset A dataset name ("deforestation", "cloud").
#' @inheritParams load_baci
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE)
#' # in portuguese (language = 'pt').
#' data <- load_prodes(
#'   raw_data = FALSE,
#'   language = "pt"
#' )
#' }
#'
#' @export

load_prodes <- function(dataset, year, raw_data = FALSE,
                        language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  municipio <- cod_ibge <- estado <- area_km2 <- increment <- NULL
  municipality <- municipality_code <- state <- deforestation <- desmatamento2000 <- NULL
  
  #############################
  ## Define Basic Parameters ##
  #############################
  
  param <- list()
  param$source <- "prodes"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$year <- year
  
  # check if dataset and time_period are supported
  
  check_params(param)
  
  ###################
  ## Download Data ##
  ###################

  ## Column Names come with numbers at the side - we need to clean those
  
  dat <- external_download(
    dataset = param$dataset,
    source = param$source,
    year = param$year
  )
  
  ## Return Raw Data
  
  if (param$raw_data) {
    return(dat)
  }
  
  ######################
  ## Data Engineering ##
  ######################
  
  # keep only deforestation-related variables if the dataset is "deforestation"
  
  if (dataset == "deforestation") {
    required_columns <- c("municipio", "cod_ibge", "estado", "area_km2", "desmatamento2000")
    
    # Check if the required columns are present
    missing_columns <- setdiff(required_columns, colnames(dat))
    if (length(missing_columns) > 0) {
      stop(paste("The following required columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
    }
    
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::select(
        municipio, cod_ibge, estado, area_km2, desmatamento2000, dplyr::starts_with("incremento")
      )
    
    # change to long format with increment variable
    dat <- dat %>%
      dplyr::rename("incremento2000" = "desmatamento2000") %>%
      tidyr::pivot_longer(
        dplyr::starts_with("incremento"),
        names_prefix = "incremento",
        names_to = "year",
        values_to = "increment"
      )
    
    # calculating cumulative deforestation
    dat <- dat %>%
      dplyr::arrange(municipio, year) %>%
      dplyr::mutate(
        deforestation = cumsum(increment),
        .by = municipio
      ) %>%
      dplyr::mutate(
        increment = dplyr::case_when(
          year == 2000 ~ NA,
          .default = increment
        )
      )
  }
  
  ################################
  ## Harmonizing Variable Names ##
  ################################
  
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
        "cod_municipio" = cod_ibge,
        "uf" = estado,
        "incremento" = increment,
        "desmatamento" = deforestation
      )
  }
  
  return(dat_mod)
}
