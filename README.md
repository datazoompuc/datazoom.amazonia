
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.amazonia

<!-- badges: start -->

[![R build
status](https://github.com/datazoompuc/datazoom.amazonia/workflows/R-CMD-check/badge.svg)](https://github.com/datazoompuc/datazoom.amazonia/actions)

<!-- badges: end -->

\[EDIT THIS\]

The goal of datazoom.amazonia is to facilitate access to official data
regarding the Amazon. The package provides functions that download and
pre-process selected datasets. Data is in general provided at the
municipality-year level (see the documentation for more information).

**May 4th - All functions are currently under review – please do not
download till further notice!**

``` r
tibble::tribble(~Data,~Source,~'Download Type',~'Geographical Units',~'Time Periods',~'Docs Link', ~'Download Link',
                'PRODES',NA,NA,NA,NA,NA,NA,
                'DETER',NA,NA,NA,NA,NA,NA,
                'DEGRAD',NA,NA,NA,NA,NA,NA,
                'COMEX',NA,NA,NA,NA,NA,NA,
                'GDP',NA,NA,NA,NA,NA,NA,
                'CEMPRE',NA,NA,NA,NA,NA,NA,
                'CENSUS',NA,NA,NA,NA,NA,NA,
                'SIGMINE',NA,NA,NA,NA,NA,NA,
                'PAM','IBGE','API - Sidra',NA,NA,NA,NA,
                'PEVS','IBGE','API - Sidra',NA,NA,NA,NA,
                'PPM','IBGE','API - Sidra',NA,NA,NA,NA,
                'MAPBIOMAS',NA,NA,NA,NA,NA,NA,
                'IPS',NA,NA,NA,NA,NA,NA,
                'SEEG',NA,NA,NA,NA,NA,NA,)
#> # A tibble: 14 x 7
#>    Data     Source `Download Type` `Geographical Uni~ `Time Periods` `Docs Link`
#>    <chr>    <chr>  <chr>           <lgl>              <lgl>          <lgl>      
#>  1 PRODES   <NA>   <NA>            NA                 NA             NA         
#>  2 DETER    <NA>   <NA>            NA                 NA             NA         
#>  3 DEGRAD   <NA>   <NA>            NA                 NA             NA         
#>  4 COMEX    <NA>   <NA>            NA                 NA             NA         
#>  5 GDP      <NA>   <NA>            NA                 NA             NA         
#>  6 CEMPRE   <NA>   <NA>            NA                 NA             NA         
#>  7 CENSUS   <NA>   <NA>            NA                 NA             NA         
#>  8 SIGMINE  <NA>   <NA>            NA                 NA             NA         
#>  9 PAM      IBGE   API - Sidra     NA                 NA             NA         
#> 10 PEVS     IBGE   API - Sidra     NA                 NA             NA         
#> 11 PPM      IBGE   API - Sidra     NA                 NA             NA         
#> 12 MAPBIOM~ <NA>   <NA>            NA                 NA             NA         
#> 13 IPS      <NA>   <NA>            NA                 NA             NA         
#> 14 SEEG     <NA>   <NA>            NA                 NA             NA         
#> # ... with 1 more variable: Download Link <lgl>
```

-   INPE - PRODES: deforestation
-   INPE - DETER: deforestation warnings
-   INPE - DEGRAD: degradation (Work In Progress)
-   MDIC - COMEX: exports and imports
-   IBGE - PIB-Munic: gdp
-   IBGE - CEMPRE: formal employment
-   IBGE - Census: income
-   IBGE - SIGMINE: mining area
-   IBGE - PAM: agricultural data
-   IBGE - PEVS: silviculture and plant extraction data
-   MAPBIOMAS: land covering
-   IMAZON - IPS: Social Progress Index by municipality
-   SEEG - Greenhouse gases emissions by municipality

## Installation

Currently you can only install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.amazonia")
```

## Usage for INPE data

``` r
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

``` r
# Downloads data

years <- c(2000:2009)

data <- load_comex(years, ncm = TRUE, exp = TRUE, imp = TRUE)
```

## Usage for IBGE data

``` r
##PIB-Munic

data <- load_amazon_gdp(c(2014, 2015))

data <- load_amazon_gdp(2017, space_aggregation = "state", language = "pt")

##SIGMINE

data <- load_sigmine(space_aggregation = 'municipality')

##PAM

data <- load_pam_permanent(2013, aggregation_level = "region")

##PEVS

data <- datazoom.amazonia::load_pevs_areasilv(2017, aggregation_level = "state", language = "eng")
```

## Usage for MAPBIOMAS data

``` r
# Downloads data

data <- load_mapbiomas_covering(space_aggregation = 'municipality', path = NULL, covering = 3, years = c(2005:2015))

data <- load_mapbiomas_transition(transition_interval = 5)

# Loads data locally

data <- load_mapbiomas_covering("~/Downloads")

data <- load_mapbiomas_transition("~/Downloads")
```

## Usage for IMAZON data

``` r
##IPS

data <- load_IPS(download_directory = getwd(), language = "pt")
```

## Usage for SEEG data

``` r
##SEEG

data <- load_seeg(language = "eng")
```

## Usage for SIGMINE data

``` r
# Downloads data

data <- load_sigmine(space_aggregation = 'municipality', source = NULL, language = 'pt')

# Loads data locally

data <- load_sigmine(space_aggregation = 'state', source = "~/Downloads", language = 'eng')
```

## Credits

DataZoom is developed by a team at Pontifícia Universidade Católica do
Rio de Janeiro (PUC-Rio), Department of Economics. Our official website
is at: <http://www.econ.puc-rio.br/datazoom>.
