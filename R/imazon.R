library(sf)
library(tidyverse)
library(googledrive)

#seeg tem google drive
#degrad, deter e terra climate tem shp

imazon_sf= read_sf("C:\\Users\\lugui\\Desktop\\DataZoom\\IMAZON\\Fronteiras_Amazonia\\Fronteiras_Amazonia_2020.shp")


load_imazon= function(dataset = 'imazon', raw_data,
                      geo_level = 'municipality', language = "eng"){


# Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }


  if (param$geo_level == "municipality") {
  message("Please follow the steps from `googledrive` package to download the data. This may take a while.")
  }

dat <- external_download(
  dataset = param$dataset,
  source = "imazon",
  geo_level = param$geo_level
)



}
