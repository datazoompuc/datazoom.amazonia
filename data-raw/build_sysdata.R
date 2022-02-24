library(tidyverse)

## Importing all municipalities

municipalities_2010 <- geobr::lookup_muni("all") %>%
  as_tibble()

geo_municipalities <- geobr::read_municipality(year = 2020,
                                               simplified = FALSE)

## Importing Legal Amazon municipalities

dir <- tempdir()
temp <- tempfile(tmpdir = dir)

url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2020/lista_de_municipios_Amazonia_Legal_2020.xls"

utils::download.file(url, destfile = temp, method = "curl")

legal_amazon <- readxl::read_xls(temp, range = "A1:L773")

legal_amazon <- legal_amazon$CD_MUN %>%
  as.numeric()

## Creating Legal Amazon map data

geo_amazon_municipalities <- geo_municipalities %>%
  filter(code_muni %in% legal_amazon)

## Final municipality datasets

municipalities <- geo_municipalities %>%
  sf::st_drop_geometry()

amazon_municipalities <- geo_amazon_municipalities %>%
  sf::st_drop_geometry()

## Aggregated map data by state

geo_states <- geo_municipalities %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarise()

geo_amazon_states <- geo_amazon_municipalities %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarise()

## Aggregated map data

geo_country <- geo_municipalities %>%
  summarise()

geo_amazon <- geo_amazon_municipalities %>%
  summarise()

## Adding to sysdata

usethis::use_data(
  municipalities_2010,
  municipalities,
  geo_municipalities,
  geo_states,
  geo_country,
  amazon_municipalities,
  geo_amazon_municipalities,
  geo_amazon_states,
  geo_amazon,
  internal = TRUE,
  overwrite = TRUE
)
