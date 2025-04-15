# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

# Baixar e ler dados
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
View(dados1)

teste1 <- load_datasus(
  dataset = "datasus_sih_rd",
  time_period = 2023,
  states = "AC"
)

load_datasus(dataset = "datasus_sih_rd", time_period = 2023, states = "AC")


load_datasus("datasus_sih_rd", 2023, "AC")

