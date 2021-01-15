# datazoom.amazonia

datazoom.amazonia is an R package that facilitates access to official data regarding the Amazon. The package provides functions that download and pre-process selected datasets. Currently we support:
* INPE - PRODES: deforestation by municipality
* MDIC - Comex: exports and imports by municipality or state
* IBGE - PIB-Munic: gdp by municipality

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

# Loads data locally
data <- load_prodes("~/Downloads")

##Comex

#Downloads data

years <- c(2000:2009)

data <- load_comex(years, ncm = TRUE, exp = TRUE, imp = TRUE)

##PIB-Munic
data <- load_amazon_gdp(c(2014, 2015))

data <- load_amazon_gdp(2017, aggregation_level = "state", language = "pt")


```

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
