# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

teste1 <- load_datasus(
  dataset = "datasus_sih_rd",
  time_period = 2023,
  states = "AC"
)

data <- load_datasus(dataset = "datasus_sih_rd", time_period = 2023, states = "AC")


load_datasus("datasus_sih_rd", 2023, "AC")

