# datazoom.amazonia

datazoom.amazonia is an R package that facilitates access to official data regarding the Amazon. The package provides functions that download and pre-process selected datasets. Currently we support:
* INPE - PRODES: deforestation by municipality
* MDIC - Comex: exports and imports by municipality or state
* IBGE - PIB-Munic: gdp by municipality
* MAPBIOMAS: land covering by municipality or state
* INPE - DETER: deforestation warnings by municipality or state

<!-- badges: start -->
[![R build status](https://github.com/datazoompuc/datazoom.amazonia/workflows/R-CMD-check/badge.svg)](https://github.com/datazoompuc/datazoom.amazonia/actions)
<!-- badges: end -->

## Installation
The package can be installed using `devtools` like so:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.amazonia")
```

## Usage

```
library(datazoom.amazonia)

##INPE

# Downloads data

data <- load_prodes(c(2018, 2019))

data <- load_prodes(2017, aggregation_level = "state", language = "pt")

data <- load_deter("amazonia", space_aggregation = "state",
                   time_aggregation = "year", language = "pt")

# Loads data locally

data <- load_prodes("~/Downloads")

data <- load_deter("~/Downloads")

##Comex

# Downloads data

years <- c(2000:2009)

data <- load_comex(years, ncm = TRUE, exp = TRUE, imp = TRUE)

##PIB-Munic

data <- load_amazon_gdp(c(2014, 2015))

data <- load_amazon_gdp(2017, aggregation_level = "state", language = "pt")

##MAPBIOMAS

# Downloads data

data <- load_mapbiomas_cobertura(aggregation_level = 'municipality', path = NULL, code_state = "PA", code_mun = NULL, covering = 3,
                                  type = 'stacked', year_begin = 2000, year_end = 2010)

data <- load_mapbiomas_transicao(code_state = "PA", covering_from = 3, covering_to = 19, type = 'normal', year_diff = 5)

# Loads data locally

data <- load_mapbiomas_cobertura("~/Downloads")

data <- load_mapbiomas_transicao("~/Downloads")

```

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
