url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/DNAC2016.dbc"

temp <- tempfile()

download.file(url, temp, method = "curl")

dat <- datazoom.amazonia:::read.dbc(temp)

dat <- dat %>%
  janitor::clean_names()
