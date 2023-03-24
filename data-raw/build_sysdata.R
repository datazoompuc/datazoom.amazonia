library(tidyverse)

## Importing all municipalities

geo_municipalities <- geobr::read_municipality(
  year = 2020,
  simplified = T
)

municipalities <- geo_municipalities %>%
  sf::st_drop_geometry()

municipality_mapbiomas <- read_csv("data-raw/municipalities_mapbiomas.csv")
biome_munic_mapbiomas <- read_csv("data-raw/biome_munic_mapbiomas.csv")

## Importing Legal Amazon municipalities

dir <- tempdir()
temp <- tempfile(tmpdir = dir)
options(download.file.method = "curl", download.file.extra = "-k -L")

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

## Using lookup_muni to add micro and mesoregion codes

look <- geobr::lookup_muni("all") %>%
  select(code_muni, code_micro, name_micro, code_meso, name_meso)

municipalities <- municipalities %>%
  full_join(look, by = "code_muni")

# To avoid notes, convert all municipality names to ASCII

municipalities <- municipalities %>%
  dplyr::mutate(
    dplyr::across(
      contains("name"),
      ~ stringi::stri_trans_general(., id = "Latin-ASCII")
    )
  ) %>%
  dplyr::mutate(dplyr::across(contains("name"), tolower)) %>%
  left_join(municipality_mapbiomas, by = join_by(code_muni))


municipalities_biomes <- biome_munic_mapbiomas %>%
  dplyr::mutate(
    dplyr::across(
      is.character,
      ~ stringi::stri_trans_general(., id = "Latin-ASCII")
    )
  ) %>%
  right_join(municipalities %>% select(abbrev_state, municipality_mapbiomas, code_muni),
             multiple = "all",
             by = join_by(abbrev_state, municipality_mapbiomas))


## Adding to sysdata

usethis::use_data(
  municipalities,
  internal = FALSE,
  overwrite = TRUE
)

usethis::use_data(
  municipalities_biomes,
  internal = FALSE,
  overwrite = TRUE
)

## Writing shapefile to /data-raw/

write_rds(geo_municipalities, "./data-raw/geo_municipalities.rds", compress = "bz2")
