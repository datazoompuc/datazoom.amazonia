# dz.amazonia

dz.amazonia is an R package that facilitates access to official data regarding the Amazon. The package provides functions that download and pre-process selected datasets. Currently we support:
* INPE - PRODES: deforestation by municipality

## Installation
The package can be installed using `devtools` like so:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/dz.amazonia")
```

## Usage

```
library(dz.amazonia)

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
