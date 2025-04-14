# install.packages("devtools")
library(devtools)

devtools::load_all()

baixar_dbc <- function(url, destino) {
  download.file(url, destfile = destino, mode = "wb")
  if (file.exists(destino)) {
    message(paste("Download concluído:", destino))
    return(datazoom.amazonia:::read.dbc(destino))
  } else {
    stop("Falha no download de ", destino)
  }
}

dados1 <- baixar_dbc("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDAC2302.dbc", "RDAC2302.dbc")
dados2 <- baixar_dbc("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDDF2302.dbc", "RDDF2302.dbc")
