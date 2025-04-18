# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

teste1 <- load_datasus(
  dataset = "datasus_sinasc",
  time_period = 2023,
  raw_data = TRUE,
  states = "AC"
)

testes <- load_datasus(
  dataset = "datasus_sih_rd",
  time_period = 2023,
  raw_data = FALSE,
  states = "AC")


load_datasus("datasus_sinasc", time_period = 2023, states = "AC", raw_data = FALSE) %>% View()

