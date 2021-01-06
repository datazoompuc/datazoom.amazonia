

dados <- readxl::read_xls("C:/users/arthu/Desktop/fines.xls",
                            skip = 6,
                            col_type = c(rep("guess", 13), "date", "guess", "date")
) %>%
  janitor::clean_names()

cities_amazon <- read.csv("C:/users/arthu/Desktop/fines.csv",
                          fileEncoding = "UTF-8"
                          )

data_amazon_fines <- dados %>%
  dplyr::filter(codigo_ibge_municipio_embargo %in% cities_amazon$CD_MUN)


usethis::use_data(data_amazon_fines, internal = TRUE)




utils::download.file(
  url = 'https://servicos.ibama.gov.br/ctf/publico/areasembargadas/downloadListaAreasEmbargadas.php',
  destfile = './fines.rar',
  mode = "wb"
)


utils::unzip(
  zipfile = './fines.rar',
  exdir = './ibama_data'
)

files <- list.files('C:/users/arthu/Desktop/ibama_data',
                    full.names = TRUE)

file.rename(
  from = files,
  to = "C:/users/arthu/Desktop/teste.xlsx"
)

file.exists('./teste.xlsx')

dados <- readxl::read_excel(
  "./teste.xlsx",
  skip = 6,
  col_type = c(rep("guess", 13), "date", "guess", "date")
) %>%
  janitor::clean_names()
