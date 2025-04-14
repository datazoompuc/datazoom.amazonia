# install.packages("devtools")
library(devtools)

devtools::install_github("datazoompuc/datazoom.amazonia")
devtools::load_all()

# Definir a URL do arquivo e o caminho de destino
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDAC2302.dbc"
destino <- "IT_SIHSUS_1603.pdf"  # Nome do arquivo no seu computador

# Baixar o arquivo
download.file(url, destfile = destino, mode = "wb")

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDDF2302.dbc"
destino <- "RDDF2302.dbc"

# Baixar o arquivo .dbc do SIHSUS
download.file(url, destfile = destino, mode = "wb")

# Verifica se baixou e lê o conteúdo usando o pacote certo
if (file.exists(destino)) {
  message("Download concluído com sucesso. Lendo o arquivo com datazoom.amazonia...")

  dados <- datazoom.amazonia:::read.dbc(destino)
  View(dados)
} else {
  message("Falha no download.")
}

datazoom.amazonia:::read.dbc()


#

data <- load_datasus("datasus_sih", 2010, states = "AC", raw_data = TRUE )
