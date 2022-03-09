library(tidyverse)

## Importing all municipalities

municipalities <- geobr::read_municipality(year = 2020,
                                               simplified = FALSE) %>%
  sf::st_drop_geometry()

## Importing Legal Amazon municipalities

dir <- tempdir()
temp <- tempfile(tmpdir = dir)

url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2020/lista_de_municipios_Amazonia_Legal_2020.xls"

utils::download.file(url, destfile = temp, method = "curl")

legal_amazon <- readxl::read_xls(temp, range = "A1:L773")

legal_amazon_list <- legal_amazon$CD_MUN %>%
  as.numeric()

## Creating legal amazon dummy

municipalities <- municipalities %>%
  mutate(
    legal_amazon = case_when(
      code_muni %in% legal_amazon_list ~ 1,
      TRUE ~ 0
    )
  )

## Adding to sysdata

usethis::use_data(
  municipalities,
  internal = TRUE,
  overwrite = TRUE
)
