# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

teste1 <- load_datasus(
  dataset = "datasus_sih_er",
  time_period = 2023,
  states = "AC"
)

rj_data <- load_datasus(
  dataset = "datasus_sinasc",
  time_period = 2024,
  states = "AC",
  raw_data = TRUE)


load_datasus("datasus_sih_rd", 2023, "AC")

