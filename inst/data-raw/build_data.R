

dados <- readxl::read_excel("C:/users/arthu/Desktop/finesIBAMA.xlsx",
                            skip = 6,
                            col_type = c(rep("guess", 13), "date", "guess", "date")
) %>%
  janitor::clean_names()

cities_amazon <- read.csv("C:/users/arthu/Desktop/legal_amazon.csv",
                          fileEncoding = "UTF-8"
)

data_amazon_fines <- dados %>%
  dplyr::filter(codigo_ibge_municipio_embargo %in% cities_amazon$CD_MUN)


usethis::use_data(data_amazon_fines, internal = TRUE)
