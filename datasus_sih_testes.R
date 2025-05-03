# Só precisa rodar uma vez:
# devtools::install_github("datazoompuc/datazoom.amazonia")

# Carrega o pacote
# library(datazoom.amazonia)

devtools::load_all()

# SINASC

sinasc_pt <- load_datasus(
  dataset = "datasus_sinasc",
  time_period = 2023,
  language = "pt",
  raw_data = FALSE,
  states = "AC"
  )

sinasc_eng <- load_datasus(
  dataset = "datasus_sinasc",
  time_period = 2023,
  language = "eng",
  raw_data = FALSE,
  states = "AC"
)

# SIHSUS

datasus_sih_rd <- load_datasus(
  dataset = "datasus_sih_rd",
  time_period = 2023,
  raw_data = FALSE,
  language = "pt",
  states = "AC"
  )

# Pt
datasus_sih_er <-load_datasus(
  dataset = "datasus_sih_er",
  time_period = 2023,
  raw_data = FALSE,
  language = "pt",
  states = "AC"
)


datasus_sih_rj <-load_datasus(
  dataset = "datasus_sih_rj",
  time_period = 2023,
  raw_data = FALSE,
  language = "pt",
  states = "AC"
)

# Pt e Eng
datasus_sih_sp <-load_datasus(
  dataset = "datasus_sih_sp",
  time_period = 2023,
  raw_data = FALSE,
  language = "pt",
  states = "AC"
)

x <- load_datasus(
  dataset = "datasus_sih_sp",
  time_period = 2023,
  raw_data = FALSE,
  language = "eng",
  states = "AC"
)

