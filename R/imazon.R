# library(sf)
# library(tidyverse)
# library(googledrive)

#seeg tem google drive
#degrad, deter e terra climate tem shp

load_imazon= function(dataset = 'imazon_shp', raw_data = TRUE,
                      geo_level = 'municipality', language = "pt_br"){

  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data



  if (param$geo_level == "state" | param$raw_data == FALSE) {
    stop("This dataset is only available for geo_level = 'municipality' and raw_data = TRUE")
  }

  if (param$geo_level == "municipality") {
  message("Please follow the steps from `googledrive` package to download the data. This may take a while.")
  }


dat <- external_download(
  dataset = param$dataset,
  source = "imazon_shp",
  geo_level = param$geo_level
)

return(dat)

}
