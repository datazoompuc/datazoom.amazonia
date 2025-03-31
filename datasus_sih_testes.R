devtools::install_github("datazoompuc/datazoom.amazonia", force = TRUE)


# download data in a single tibble, with variable labels
data_pt <- load_datasus(
  dataset = "datasus_sih",
  time_period = 2015,
  states = "AM",
  raw_data = TRUE,
)


url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/ERAC2311.dbc"

destfile <- "ERAC2312.dbc"

download.file(url, destfile, mode = "wb")

df <- datazoom.amazonia:::read.dbc("ERAC2312.dbc")
