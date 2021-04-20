# datazoom.amazonia

datazoom.amazonia is an R package that facilitates access to official data regarding the Amazon. The package provides functions that download and pre-process selected datasets. Data is in general provided at the municipality-year level (see the documentation for more information). Currently we support:
* INPE - PRODES: deforestation
* INPE - DETER: deforestation warnings
* INPE - DEGRAD: degradation
* MDIC - COMEX: exports and imports
* IBGE - PIB-Munic: gdp
* IBGE - CEMPRE: formal employment
* IBGE - Census: income
* IBGE - SIGMINE: mining area
* IBGE - PAM: agricultural data
* MAPBIOMAS: land covering
* IMAZON - IPS: Social Progress Index by municipality

<!-- badges: start -->
[![R build status](https://github.com/datazoompuc/datazoom.amazonia/workflows/R-CMD-check/badge.svg)](https://github.com/datazoompuc/datazoom.amazonia/actions)
<!-- badges: end -->

## Installation
The package can be installed using `devtools` like so:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.amazonia")
```

## Usage for INPE data

```
library(datazoom.amazonia)

# Downloads data

data <- load_prodes(c(2018, 2019))

data <- load_degrad(2016)

data <- load_prodes(2017, space_aggregation = "state", language = "pt")

data <- load_deter("amazonia", space_aggregation = "state",
                   time_aggregation = "year", language = "pt")

# Loads data locally

data <- load_prodes("~/Downloads")

data <- load_deter("~/Downloads")

```

## Usage for COMEX data

```
# Downloads data

years <- c(2000:2009)

data <- load_comex(years, ncm = TRUE, exp = TRUE, imp = TRUE)

```

## Usage for IBGE data

```
##PIB-Munic

data <- load_amazon_gdp(c(2014, 2015))

data <- load_amazon_gdp(2017, space_aggregation = "state", language = "pt")

##PAM

data <- load_pam_permanent(2013, aggregation_level = "region")

```

## Usage for MAPBIOMAS data

```
# Downloads data

data <- load_mapbiomas_covering(space_aggregation = 'municipality', path = NULL, covering = 3, years = c(2005:2015))

data <- load_mapbiomas_transition(space_aggregation = 'state', transition_interval = 5)



```

## Usage for IMAZON data

```
##IPS

data <- load_IPS(download_directory = getwd(), language = "pt")

```

## Usage for SIGMINE data

```
# Downloads data

data <- load_sigmine(space_aggregation = 'municipality', source = NULL, language = 'pt')

# Loads data locally

data <- load_sigmine(space_aggregation = 'state', source = "~/Downloads", language = 'eng')

```

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
