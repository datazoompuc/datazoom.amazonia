devtools::load_all()

# Definir a URL do arquivo e o caminho de destino
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Doc/IT_SIHSUS_1603.pdf"
destino <- "IT_SIHSUS_1603.pdf"  # Nome do arquivo no seu computador

# Baixar o arquivo
download.file(url, destfile = destino, mode = "wb")

# Verificar se o download foi bem-sucedido
if (file.exists(destino)) {
  message("Download concluído com sucesso!")
} else {
  message("Falha no download.")
}


#

data <- load_datasus("datasus_sih", 2010, states = "AC", raw_data = TRUE )
