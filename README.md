# datazoom.amazonia

dz.amazonia is an R package that facilitates access to official data regarding the Amazon. The package provides functions that download and pre-process selected datasets. Currently we support:
* INPE - PRODES: deforestation by municipality

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/datazoompuc/dz.amazonia.svg?branch=master)](https://travis-ci.com/datazoompuc/dz.amazonia)
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

# Downloads data
data <- load_prodes(c(2018, 2019))

data <- load_prodes(2017, aggregation_level = "state", language = "pt")

# Loads data locally
data <- load_prodes("~/Downloads")

data <- load_prodes("~/Downloads/data.txt")

# Loads raw data
raw_data <- load_prodes_raw(2018)
```

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
