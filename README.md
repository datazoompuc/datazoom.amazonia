
<a href="https://github.com/datazoompuc/datazoom.amazonia"><img src="https://raw.githubusercontent.com/datazoompuc/datazoom.amazonia/master/logo.png" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.amazonia

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/datazoom.amazonia?color=orange)](https://cran.r-project.org/package=datazoom.amazonia?style=flat)
[![R build
status](https://github.com/datazoompuc/datazoom.amazonia/workflows/R-CMD-check/badge.svg)](https://github.com/datazoompuc/datazoom.amazonia/actions?style=flat)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/datazoom.amazonia?color=blue)](https://cran.r-project.org/package=datazoom.amazonia?style=flat)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/datazoom.amazonia?color=lightgrey)](https://cran.r-project.org/package=datazoom.amazonia?style=flat)
![Languages](https://img.shields.io/github/languages/count/datazoompuc/datazoom.amazonia?style=flat)
![Commits](https://img.shields.io/github/commit-activity/y/datazoompuc/datazoom.amazonia?style=flat)
![Open
Issues](https://img.shields.io/github/issues-raw/datazoompuc/datazoom.amazonia?style=flat)
![Closed
Issues](https://img.shields.io/github/issues-closed-raw/datazoompuc/datazoom.amazonia?style=flat)
![Files](https://img.shields.io/github/directory-file-count/datazoompuc/datazoom.amazonia?style=flat)
![Followers](https://img.shields.io/github/followers/datazoompuc?style=flat)
<!-- badges: end -->

The datazoom.amazonia package facilitates access to official Brazilian
Amazon data, including agriculture, deforestation, production. The
package provides functions that download and pre-process selected
datasets.

## Installation

You can install the released version of `datazoom.amazonia` from
[CRAN](https://CRAN.R-project.org/package=datazoom.amazonia) with:

``` r
install.packages("datazoom.amazonia")
```

And the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.amazonia")
```

**[1 - Environmental data](#environmental-data)**

<table>

<tr>

<td>

|                       |                                        |
|-----------------------|----------------------------------------|
| **[PRODES](#prodes)** | *Yearly deforestation*                 |
| **[DETER](#deter)**   | *Alerts on forest cover changes*       |
| **[DEGRAD](#degrad)** | *Forest degradation*                   |
| **[Imazon](#imazon)** | *Deforestation pressure in the Amazon* |

</td>

<td>

|                                   |                                     |
|-----------------------------------|-------------------------------------|
| **[IBAMA](#ibama)**               | *Environmental fines*               |
| **[MapBiomas](#mapbiomas)**       | *Land cover and land use*           |
| **[TerraClimate](#terraclimate)** | *Climate data*                      |
| **[SEEG](#seeg)**                 | *Greenhouse gas emission estimates* |
| **[CENSOAGRO](#censoagro)**       | *Agriculture activities*            |

</td>

</tr>

</table>

**[2 - Social data](#social-data)**

<table>

<tr>

<td>

|  |  |
|----|----|
| **[IPS](#ips)** | *Amazon Social Progress Index* |
| **[IEMA](#iema)** | *Access to electricity in the Amazon region* |
| **[Population](#population)** | *Population* |

</td>

</tr>

</table>

**[3 - Economic data](#economic-data)**

<table>

<tr>

<td>

|                             |                                 |
|-----------------------------|---------------------------------|
| **[COMEX](#comex)**         | *Brazilian international trade* |
| **[BACI](#baci)**           | *Global international trade*    |
| **[PIB-Munic](#pib-munic)** | *Municipal GDP*                 |
| **[CEMPRE](#cempre)**       | *Central register of companies* |
| **[PAM](#pam)**             | *Agricultural production*       |

</td>

<td>

|                         |                           |
|-------------------------|---------------------------|
| **[PEVS](#pevs)**       | *Forestry and extraction* |
| **[PPM](#ppm)**         | *Livestock farming*       |
| **[SIGMINE](#sigmine)** | *Mining*                  |
| **[ANEEL](#aneel)**     | *Energy development*      |
| **[EPE](#epe)**         | *Energy consumption*      |

</td>

</tr>

</table>

**[4 - Other tools](#other-tools)**

<table>

<tr>

<td>

|  |  |
|----|----|
| **[Legal Amazon Municipalities](#legal-amazon-municipalities)** | *Dataset with brazilian cities and whether they belong to the Legal Amazon* |
| **[The ‘googledrive’ package](#googledrive)** | *Troubleshooting and information for downloads from Google Drive* |

</table>

# Environmental Data

## PRODES

The PRODES project uses satellites to monitor deforestation in Brazil’s
Legal Amazon. The raw data reports total and incremental (year-by-year)
low-cut deforested area at the municipality level, going back to the
year 2007.

INPE’s most recent data is now published at
[TerraBrasilis](http://terrabrasilis.dpi.inpe.br/downloads/). We read
their full [raster
data](https://terrabrasilis.dpi.inpe.br/geonetwork/srv/eng/catalog.search#/metadata/507294db-a789-42dd-9158-9dea77d9293f)
for the Legal Amazon region and extract values onto the map of Brazilian
municipalities.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"deforestation"`, `"residual_deforestation"`,
    `"native_vegetation"`, `"hydrography"`, `"non_forest"`, or
    `"clouds"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally, read as a
      SpatRaster.
    - `FALSE`: if you want the treated version of the data, measuring
      affected areas per municipality.

3.  **time_period**: picks the years for which the data will be
    downloaded, under the following constraints:

    - For dataset `"deforestation"`, it can be between 2007 and 2023.
      Deforestation for 2007 includes all cumulative deforestation up
      to 2007. For other years, deforestation is incremental;
    - For dataset `"residual_deforestation"`, it can be between 2010 and
      2023;
    - For all other datasets, only the year 2023 is available.

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data (raw_data = FALSE)
# in portuguese (language = 'pt').
data <- load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  time_period = 2020:2023,
  language = "pt"
)
```

## DETER

[DETER](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter)
uses satellite surveillance to detect and report changes in forest cover
across the Legal Amazon and the Cerrado biome. Each data point consists
of a warning, describing which type of change has affected a certain
area of forest at a given date. Broadly speaking, it makes a distinction
between events of deforestation, degradation and logging. The data
extracted here spans from 2016 onward in the Amazon, and from 2018
onward in the Cerrado.

The raw DETER data shows one warning per row, with each row also
containing a municipality. However, many warnings actually overlap with
2 or up to 4 municipalities, which are not shown in the original data.
Therefore, when the option `raw_data = FALSE` is selected, the original
spatial information is intersected with a municipalities map of Brazil,
and each warning can be split into more than one row, with each row
corresponding to a municipality.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are two options:
    - `"deter_amz"` for data from the Amazon
    - `"deter_cerrado"` for data from the Cerrado
2.  **raw_data**: there are two options:
    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.
3.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data (raw_data = FALSE) from Amazonia (dataset = "deter_amz")
deter_amz <- load_deter(
  dataset = "deter_amz",
  raw_data = FALSE
)
```

## DEGRAD

The [DEGRAD
project](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad)
uses satellites to monitor degradation of forest areas. Raw data is
available as simple features (sf) objects, read from shapefiles. The
project was substituted in 2016 by DETER-B. Accordingly, data is
available from 2007 up to 2016.

Original documentation for this data is very scarce, users beware. Some
things to keep in mind are:

Event data is organized through yearly editions (DEGRAD 2007-2016).
Inside a given edition however, there may be data from different years
(events that happened in 2015 inside DEGRAD 2016 for example).

This package provides degradation data with municipality identification.
It does this by intersecting DEGRAD geometries with IBGE’s municipality
geometries from the year 2019. CRS metadata however is missing from the
original data source. A best effort approach is used and a CRS is
assumed
`(proj4string: "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs")`.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"degrad"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: picks the years for which the data will be
    downloaded

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated data (raw_data = TRUE) related to forest degradation
# from 2010 to 2012 (time_period = 2010:2012).
data <- load_degrad(
  dataset = "degrad",
  raw_data = FALSE,
  time_period = 2010:2012
)
```

## Imazon

Loads data categorizing each municipality by the level of deforestation
pressure it faces. The categories used by Imazon have three levels,
ranging from 0 to 3.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"imazon_shp"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data
data <- load_imazon(raw_data = FALSE)
```

🔴 This function uses the `googledrive` package to download data. In
case of authentication errors, see [googledrive](#googledrive).

## IBAMA

The dataset is originally from the Brazilian Institute of Environment
and Renewable Natural Resources (Ibama), documenting environmental
embargoes and fines at the individual level from 2005 to the present
day. In addition, it is possible to download distributed and collected
fines from 1994 until the present day.

The function returns either the raw data or a data frame with aggregates
considering, for each time-location period, counts for total the number
of infractions, infractions that already went to trial, and number of
unique perpetrators of infractions. There are also two data frames
regarding distributed and collected fines across municipalities

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are three possible choices.

    - `"embargoed_areas"`: embargoed areas
    - `"distributed_fines"`: fines that have not been paid by
      individuals or corporations
    - `"collected_fines"`: fines that have been paid by individuals or
      corporations

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **states**: specifies for which states to download the data. It is
    “all” by default, but can be a single state such as `"AC"` or any
    vector such as `c("AC", "AM")`. Does not apply to the
    `"embargoed_areas"` dataset.

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
library(datazoom.amazonia)

# Download treated embargoes data (raw_data = FALSE) in english (language = "eng")
data <- load_ibama(
  dataset = "embargoed_areas", raw_data = FALSE,
  language = "eng"
)

# Download treated collected fines data from "BA"
data <- load_ibama(
  dataset = "collected_fines", raw_data = FALSE,
  states = "BA", language = "pt"
)
```

## MapBiomas

The MapBiomas project gathers data reporting the type of land covering
each year by area, that is, for example, the area used for a temporary
crop of soybeans. It also reports the transitions in land coverage
between years.

The data has yearly frequency and is available starting from the year
1985.

For all datasets, areas are measured in hectares (ha).

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are 7 possible choices:

    - `"mapbiomas_cover"`: types of land cover.
    - `"mapbiomas_transition"`: changes in land cover.
    - `"mapbiomas_deforestation_regeneration"`: deforestation and forest
      regeneration.
    - `"mapbiomas_irrigation"` (temporarily unavailable, a new
      collection will be soon delivered): irrigated areas.
    - `"mapbiomas_mining"`: areas used for mining.
    - `"mapbiomas_water"` (temporarily unavailable, a new collection
      will be soon delivered): areas of water surface.
    - `"mapbiomas_fire"`: areas of wildfire burn scars.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**:

    - For dataset `"mapbiomas_cover"`, can be `"municipality"` or
      `"indigenous_land"`
    - For dataset `"mapbiomas_transition"`, can be `"municipality"` or
      `"biome"` (faster download)
    - For dataset `"mapbiomas_deforestation_regeneration"`, can only be
      `"municipality"`
    - For dataset `"mapbiomas_mining"`, can be `"indigenous_land"` or
      `"municipality"`
    - For dataset `"mapbiomas_irrigation"`, can be `"state"` or
      `"biome"`
    - For dataset `"mapbiomas_water"`, can be `"municipality"`,
      `"state"` or `"biome"`
    - For dataset `"mapbiomas_fire"`, can only be `"state"`

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`.

------------------------------------------------------------------------

**Examples:**

``` r
# download treated Mapbiomas Cover data in English
data <- load_mapbiomas(
  dataset = "mapbiomas_cover",
  raw_data = FALSE,
  geo_level = "municipality",
  language = "eng"
)

# download treated data on mining on indigenous lands
data <- load_mapbiomas("mapbiomas_mining",
  raw_data = FALSE,
  geo_level = "indigenous_land"
)
```

## TerraClimate

Spatial data on several climate variables, extracted from Climatology
Lab’s [TerraClimate](https://www.climatologylab.org/terraclimate.html).
The table below shows all possible variables to be extracted, which are
chosen through the “dataset” parameter. Data ranges from 1958 to 2020.

Netcdf files are downloaded from the
[THREDDS](http://thredds.northwestknowledge.net:8080/thredds/terraclimate_catalog.html)
web server, as recommended for rectangular subsets of the global data.

<details>

<summary>

Click to see all dataset options
</summary>

| Dataset | Code | Description | Units |
|:---|:--:|:---|:--:|
| max_temperature | tmax | Maximum 2-m Temperature | degC |
| min_temperature | tmin | Minimum 2-m Temperature | degC |
| wind_speed | ws | Wind Speed at 10-m | m/s |
| vapor_pressure_deficit | vpd | Vapor Pressure Deficit | kPa |
| vapor_pressure | vap | 2-m Vapor Pressure | kPa |
| snow_water_equivalent | swe | Snow Water Equivalent at End of Month | mm |
| shortwave_radiation_flux | srad | Downward Shortwave Radiation Flux at the Surface | W/m^2 |
| soil_moisture | soil | Soil Moisture at End of Month | mm |
| runoff | q | Runoff | mm |
| precipitation | ppt | Accumulated Precipitation | mm |
| potential_evaporation | pet | Reference Evapotranspiration | mm |
| climatic_water_deficit | def | Climatic Water Deficit | mm |
| water_evaporation | aet | Actual Evapotranspiration | mm |
| palmer_drought_severity_index | PDSI | Palmer Drought Severity Index | unitless |

</details>

------------------------------------------------------------------------

**Options:**

1.  **dataset**: picks the variable to be read. Possible options are
    shown in the table above.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: picks the years for which the data will be
    downloaded

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

5.  **legal_amazon_only**: if set to `TRUE`, only downloads data from
    the Legal Amazon region

OBS: A good internet connection is needed, because the data is heavy.

------------------------------------------------------------------------

**Examples:**

``` r
# Downloading maximum temperature data from 2000 to 2001
max_temp <- load_climate(dataset = "max_temperature", time_period = 2000:2001)

# Downloading precipitation data only for the legal Amazon in 2010
amz_precipitation <- load_climate(
  dataset = "precipitation",
  time_period = 2010,
  legal_amazon_only = TRUE
)
```

## SEEG

Loads estimates of emission of greenhouse gases of Brazilian cities and
states from SEEG. SEEG is the System of Estimates of Emissions and
Removals of Greenhouse Gases (SEEG), an initiative of the Observatório
do Clima, a network of institutions focused on climate change research
in Brazil.

The data provided in SEEG’s Collection 9 is a series covering the period
from 2000 to 2018.

Using data collected from government entities, institutes, research
centers, NGOs and other institutions, the estimates are created using
the methodology of the Brazilian Inventory of Anthropic Emissions and
Removals of Greenhouse Gases, assembled by the Ministry of Science,
Technology and Innovation (MCTI), and the directives of
Intergovernmental Panel on Climate Change (IPCC)

Emissions are divided in five main sources: Agricultural and Cattle
Raising, Energy, Changes in Use of Land, Industrial Processes and
Residues. All greenhouse gases contained in the national inventory are
considered, encompassing CO2, CH4, N2O and the HFCs, with the conversion
to carbon equivalence (CO2e) also included, both in the metric of GWP
(Global Warming Potential) and GTP (Global Temperature Potential).

The data is downloaded from the SEEG website in the form of one single
file, so the option to select a certain range of years is not available.
Also, due to the size of the file, a stable internet connection is
necessary, and the function may take time to run.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are six choices:

    - `"seeg"`: provides all sectors in a same dataframe. Only works
      with `raw_data = TRUE`
    - `"seeg_farming"`
    - `"seeg_industry"`
    - `"seeg_energy"`
    - `"seeg_land"`
    - `"seeg_residuals"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"state"`, or `"municipality"`

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download raw data (raw_data = TRUE) of greenhouse gases (dataset = "seeg")
# by state (geo_level = "state")
data <- load_seeg(
  dataset = "seeg",
  raw_data = TRUE,
  geo_level = "state"
)

# Download treated data (raw_data = FALSE) of industry greenhouse gases (dataset = "seeg_industry")
data <- load_seeg(
  dataset = "seeg_industry",
  raw_data = FALSE,
  geo_level = "state"
)
```

🔴 This function uses the `googledrive` package to download data at the
municipality level. In case of authentication errors, see
[googledrive](#googledrive).

## CENSOAGRO

The census of agriculture collects information about agricultural
establishments and the agricultural activities carried out there,
covering characteristics of the producer and establishment, economy and
employment in rural areas, livestock, farming and agroindustry.

Data is collected by IBGE and is available at country, state and
municipality level.

------------------------------------------------------------------------

**Options:**

1.  **dataset**:there are 10 possible choices:

    - `"agricultural_land_area"`: area and number of agricultural
      properties
    - `"agricultural_area_use"`: area of agricultural properties by use
    - `"agricultural_employees_tractors"`: number of employees and
      tractors in agricultural properties
    - `"agricultural_producer_condition"`: condition of agricultural
      producer, whether they own the land
    - `"animal_production"`: number of animals farmed, by species
    - `"animal_products"`: amount of animal products, by product type
    - `"vegetable_production_area"`: area and amount produced, by
      vegetable product
    - `"vegetable_production_temporary"`: amount produced, by temporary
      crop
    - `"vegetable_production_permanent"`: amount produced, by permanent
      crop
    - `"livestock_production"`: amount of bovine cattle, and number of
      agricultural properties

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"` or `"state"`. For dataset
    `"livestock_production"`, it can also be `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded:

    - For datasets `"agricultural_land_area"`,
      `"agricultural_producer_condition"`, `"animal_products"`, and
      `"vegetable_production_area"`, it can be one of 1920, 1940, 1950,
      1960, 1970, 1975, 1980, 1985, 1995, or 2006.
    - For datasets `"vegetable_production_permanent"` and
      `"vegetable_production_permanent"`, it can only be from 1940
      onwards
    - For datasets `"agricultural_area_use"`,
      `"agricultural_employees_tractors"`, `"animal_production"`, it can
      only be from 1970 onwards
    - For dataset `"livestock_production"`, it can only be 2017

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download total land area data at the country level in year 2006
data <- load_censoagro(
  dataset = "agricultural_land_area",
  raw_data = TRUE,
  geo_level = "country",
  time_period = 2006
)

# Download temporary production crops data by state (geo_level = "state") in year 2006
# in portuguese (language = "pt")
data <- load_censoagro(
  dataset = "vegetable_production_temporary",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 1995,
  language = "pt"
)
```

# Social Data

## IPS

Loads information on the social and environmental performance of the
Legal Amazon.

Data from the Amazon Social Progress Index, an initiative from Imazon
with support from the Social Progress Imperative that measures the
social and environmental progress of its locations. Namely, the 772
municipalities in the Amazon region. Survey is done at the municipal
level.

------------------------------------------------------------------------

**Options:**

1.  **dataset**:

    - `"all"`, `"life_quality"`, `"sanit_habit"`, `"violence"`,
      `"educ"`, `"communic"`, `"mortality"`, or `"deforest"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: can be 2014, 2018, 2021, 2023 or a vector with some
    combination thereof

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download raw data from 2014
data <- load_ips(dataset = "all", raw_data = TRUE, time_period = 2014)

# Download treated deforest data from 2018 in portuguese
data <- load_ips(
  dataset = "deforest", raw_data = FALSE,
  time_period = 2018, language = "pt"
)
```

## IEMA

Data from the Institute of Environment and Water Resources (IEMA),
documenting the number of people without access to eletric energy
throughout the Amazon region in the year 2018.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"iema"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data
data <- load_iema(raw_data = FALSE)
```

🔴 This function uses the `googledrive` package to download data. In
case of authentication errors, see [googledrive](#googledrive).

## Population

Loads IBGE information on estimated population (2001-2006, 2008-2009,
2011-2021) or population (2007 and 2010) data. Data is available at
country, state and municipality level and from 2001 to 2021.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"population"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"state"`, or `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated population data at the state level for 2010 to 2012
data <- load_population(
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2010:2012
)
```

# Economic Data

## COMEX

The Comex dataset gathers data extracted from [Siscomex (Integrated
System of Foreign
Trade)](https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta/),
which is a database containing information from all products that are
imported to or exported from Brazil. Using data reported from the
companies which are responsible for the process of transporting the
products, the system adheres to internationally standardized
nomenclatures, such as the Harmonized System and the Mercosul Common
Nomenclature (which pertains to members of the Mercosul organization).

The data has a monthly frequency and is available starting from the year
1989. From 1989 to 1996, a different system of nomenclatures was
adopted, but all conversions are available on a dictionary in the Comex
website
(<https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta/>).
Systems of nomenclature vary in the degree of detail in terms of the
product involved, as well as other characteristics, such as unit and
granularity of location.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are four choices:

    - `"export_mun"`: selects exports data by municipality
    - `"import_mun"`: selects imports data by municipality
    - `"export_prod"`: selects exports data by producer
    - `"import_prod"`: selects imports data by producer

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: picks the years for which the data will be
    downloaded

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated (raw_data = FALSE) exports data by municipality (dataset = "export_mun")
# from 2020 to 2021 (time_period = 2020:2021)
data <- load_br_trade(
  dataset = "export_mun",
  raw_data = FALSE,
  time_period = 2020:2021
)
# download treated(raw_data = FALSE) imports data by municipality (dataset = "import_mun")
# from 2020 to 2021 (time_period = 2020:2021)
data <- load_br_trade(
  dataset = "import_mun",
  raw_data = FALSE,
  time_period = 2020:2021
)
```

## BACI

Loads disaggregated data on bilateral trade flows for more than 5000
products and 200 countries. The data is from the
[CEPII](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
and is built from data directly reported by each country to the United
Nations Statistical Division (Comtrade).

As all of the data is packed into one single .zip file in the website,
data on all years must be downloaded, even if not all of it is used.
Therefore, downloading the data can take a long time.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there is one choice:

    - `"HS92"` which follows the Harmonized System method

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: picks the years for which the data will be
    downloaded

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated data for 2016 (takes a long time to download)
clean_baci <- load_baci(
  raw_data = FALSE,
  time_period = 2016
)
```

## PIB-Munic

Loads IBGE information on gross domestic product at current prices,
taxes, net of subsidies, on products at current prices and gross value
added at current prices, total and by economic activity, and respective
shares. Data is available at country, state and municipality level and
from 2002 to 2018.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"pibmunic"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"state"`, or `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated municipal GDP data at the state level for 2010 to 2012
data <- load_pibmunic(
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2010:2012
)
```

## CEMPRE

Employment, salary and firm data from IBGE’s [Cadastro Central de
Empresas (CEMPRE)](https://sidra.ibge.gov.br/pesquisa/cempre/tabelas).
Loads information on companies and other organizations and their
respective formally constituted local units, registered with the CNPJ -
National Register of Legal Entities. Data is available between 2006 and
2019.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"cempre"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"state"` or `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

6.  **sectors**: defines if the data will be return separated by sectors
    (`sectors = TRUE`) or not (`sectors = FALSE`)

------------------------------------------------------------------------

**Examples:**

``` r
# Download raw data (raw_data = TRUE) at the country level
# from 2008 to 2010 (time_period = 2008:2010).
data <- load_cempre(
  raw_data = TRUE,
  geo_level = "country",
  time_period = 2008:2010
)
# Download treted data (raw_data = FALSE) by state (geo_level = "state")
# from 2008 to 2010 (time_period = 2008:2010) in portuguese (language = "pt").
# In this example, data is split by sector (sectors = TRUE)
data <- load_cempre(
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2008:2010,
  language = "pt",
  sectors = TRUE
)
```

## PAM

[Municipal Agricultural
Production](https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/16773-municipal-agricultural-production-temporary-and-permanent-crops.html?=&t=o-que-e)
(PAM, in Portuguese) is a nationwide annual survey conducted by IBGE
(Brazilian Institute of Geography and Statistics) which provides
information on agricultural products, such as quantity produced, area
planted and harvested, average quantity of output and monetary value of
such output. The products are divided in permanent and temporary farmed
land, as well as dedicated surveys to the four products that yield
multiple harvests a year (beans, potato, peanut and corn), which all sum
to a total survey of 64 agricultural products (31 of temporary tillage
and 33 of permanent tillage). Output, however, is only included in the
dataset if the planted area occupies over 1 acre or if output exceeds
one tonne.

Permanent farming is characterized by a cycle of long duration, whose
harvests may be done multiple times across the years without the need of
planting seeds again. Temporary farming, on the other hand, consists of
cycles of short and medium duration, which after harvesting require
planting seeds again.

The data also has multiple aggregation levels, such as nationwide, by
region, mesoregion and microregion, as well as state and municipality.

The data available has a yearly frequency and is available from 1974 to
the present, with the exception of the four multiple-harvest products,
which are only available from 2003. More information can be found on
[this
link](https://www.ibge.gov.br/estatisticas/economicas/agricultura-e-pecuaria/9117-producao-agricola-municipal-culturas-temporarias-e-permanentes.html#:~:text=A%20pesquisa%20Produ%C3%A7%C3%A3o%20Agr%C3%ADcola%20Municipal,s%C3%A3o%20da%20cesta%20b%C3%A1sica%20do)
(only in Portuguese).

------------------------------------------------------------------------

**Options:**

1.  **dataset**: See tables below

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"region"`, `"state"`, or
    `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

The datasets supported are shown in the tables below, made up of both
the original databases and their narrower subsets. Note that downloading
only specific crops is considerably faster.

<details>

<summary>

Full datasets provided by IBGE:
</summary>

| dataset         |
|:----------------|
| all_crops       |
| temporary_crops |
| permanent_crops |
| corn            |
| potato          |
| peanut          |
| beans           |

</details>

<details>

<summary>

Datasets generated from Temporary Crops:
</summary>

| dataset | Name (pt) | Name (eng) |
|:---|:--:|:--:|
| total_temporary | Total | Total |
| abacaxi | Abacaxi | Pineapple |
| alfafa | Alfafa Fenada | Alfafa Fenada |
| alho | Alho | Garlic |
| algodao_herbaceo | Algodao Herbaceo (em Caroco) | Herbaceous Cotton (in Caroco) |
| amendoim_temporary | Amendoim (em Casca) | Peanuts (in Shell) |
| arroz | Arroz (em Casca) | Rice (in husk) |
| aveia | Aveia (em Grao) | Oats (in grain) |
| batata_doce | Batata Doce | Sweet potato |
| batata_inglesa | Batata Inglesa | English potato |
| cana_de_acucar | Cana de Acucar | Sugar cane |
| cana_para_forragem | Cana para Forragem | Forage cane |
| castor_bean | Mamona (Baga) | Castor bean (Berry) |
| cebola | Cebola | Onion |
| cevada | Cevada (em Grao) | Barley (in Grain) |
| ervilha | Ervilha (em Grao) | Pea (in Grain) |
| fava | Fava (em Grao) | Broad Bean (in Grain) |
| feijao_temporary | Feijao (em Grao) | Beans (in Grain) |
| fumo | Fumo (em Folha) | Smoke (in Sheet) |
| girassol_sementes | Girassol (em Grao) | Sunflower (in Grain) |
| juta_fibra | Juta (Fibra) | Jute (Fiber) |
| linho_sementes | Linho (Semente) | Linen (Seed) |
| malva_fibra | Malva (Fibra) | Malva (Fiber) |
| mandioca | Mandioca | Cassava |
| melancia | Melancia | watermelon |
| melao | Melao | Melon |
| milho_temporary | Milho (em Grao) | corn (in grain) |
| rami_fibra | Rami (Fibra) | Ramie (Fiber) |
| rye | Centeio (em Grao) | Rye (in grain) |
| soja | Soja (em Grao) | Soybean (in grain) |
| sorgo | Sorgo (em Grao) | Sorghum (in Grain) |
| tomate | Tomate | Tomato |
| trigo | Trigo (em Grao) | Wheat in grain) |
| triticale | Triticale (em Grao) | Triticale (in grain) |

</details>

<details>

<summary>

Datasets generated from Permanent Crops:
</summary>

| dataset | Name (pt) | Name (eng) |
|:---|:--:|:--:|
| acai | Acai | Acai |
| annatto_seeds | Urucum (Semente) | Annatto (Seed) |
| apple | Maca | Apple |
| avocado | Abacate | Avocado |
| banana | Banana (Cacho) | Banana (Bunch) |
| black_pepper | Pimenta do Reino | Black pepper |
| cashew | Caju | Cashew |
| cashew_nut | Castanha de Caju | Cashew Nuts |
| cocoa_beans | Cacau (em Amendoa) | Cocoa (in Almonds) |
| coffee_arabica | Cafe (em Grao) Arabica | Cafe (in Grao) Arabica |
| coffee_canephora | Cafe (em Grao) Canephora | Cafe (in Grain) Canephora |
| coffee_total | Cafe (em Grao) Total | Coffee (in Grain) Total |
| coconut | Coco da Baia | Coconut |
| coconut_bunch | Dende (Cacho de Coco) | Coconut Bunch |
| cotton_arboreo | Algodao Arboreo (em Caroco) | Arboreo cotton (in Caroco) |
| fig | Figo | Fig |
| grape | Uva | Grape |
| guarana_seeds | Guarana (Semente) | Guarana (Seed) |
| guava | Goiaba | Guava |
| heart_of_palm | Palmito | Palm heart |
| india_tea | Cha da India (Folha Verde) | India Tea (Leaf) |
| khaki | Caqui | Khaki |
| lemon | Limao | Lemon |
| mango | Manga | Mango |
| papaya | Mamao | Papaya |
| passion_fruit | Maracuja | Passion fruit |
| peach | Pessego | Peach |
| pear | Pera | Pear |
| permanent_total | Total | Total |
| quince | Marmelo | Quince |
| rubber_coagulated_latex | Borracha (Latex Coagulado) | Rubber (Coagulated Latex) |
| rubber_liquid_latex | Borracha (Latex Liquido) | Rubber (Liquid Latex) |
| sisal_or_agave | Sisal ou Agave (Fibra) | Sisal or Agave (Fiber) |
| tangerine | Tangerina | Tangerine |
| tung | Tungue (Fruto Seco) | Tung (Dry Fruit) |
| walnut | Noz (Fruto Seco) | Walnut (Dry Fruit) |
| yerba_mate | Erva Mate (Folha Verde) | Mate Herb (Leaf) |

</details>

**Examples:**

``` r
# download treated data at the state level from 2010 to 2011 for all crops
data <- load_pam(
  dataset = "all_crops",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2010:2011,
  language = "eng"
)
```

## PEVS

Loads information on the amount and value of the production of the
exploitation of native plant resources and planted forest massifs, as
well as existing total and harvested areas of forest crops.

Data is from the Silviculture and Forestry Extraction Production (PEVS,
in Portuguese), a nationwide annual survey conducted by IBGE (Brazilian
Institute of Geography and Statistics). The data also has multiple
aggregation levels, such as nationwide, by region, mesoregion and
microregion, as well as state and municipality.

The data available has a yearly frequency and is available from 1986 to
the present, with the exception of the data on total area for
production, which are only available from 2013 onwards. More information
can be found in [this
link](https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/18374-forestry-activities.html?=&t=o-que-e).

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are three choices:

    - `"pevs_forest_crops"`: provides data related to both quantity and
      value of the forestry activities. The data goes from 1986 to 2019
      and it is divided by type of product.
    - `"pevs_silviculture"`: provides data related to both quantity and
      value of the silviculture. The data goes from 1986 to 2019 and it
      is divided by type of product.
    - `"pevs_silviculture_area"`: total existing area used for
      silviculture in 12/31.The data goes from 2013 to 2019 and it is
      divided by forestry species.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"region"`, `"state"`, or
    `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated (raw_data = FALSE) silviculture data (dataset = 'pevs_silviculture')
# by state (geo_level = 'state') from 2012 (time_period =  2012)
# in portuguese (language = "pt")
data <- load_pevs(
  dataset = "pevs_silviculture",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2012,
  language = "pt"
)

# Download raw (raw_data = TRUE) forest crops data by region
# from 2012 to 2013 in english
data <- load_pevs(
  dataset = "pevs_forest_crops",
  raw_data = TRUE,
  geo_level = "region",
  time_period = 2012:2013
)
```

## PPM

Data on livestock inventories (e.g:cattle, pigs and hogs) in Brazilian
Municipalities, as well as amount and value of animal products
(e.g:output of milk, hen eggs, quail eggs, honey).

The periodicity of the survey is annual. The geographic coverage is
national, with results released for Brazil, Major Regions, Federation
Units, Mesoregions, Microregions and Municipalities.

The data available has a yearly frequency and is available from 1974 to
the present. More information can be found in [this
link](https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/17353-municipal-livestock-production.html?=&t=o-que-e).

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are five possible choices:

    - `"ppm_livestock_inventory"`: livestock herds, with the number of
      animals by species.

    - `"ppm_sheep_farming"`: total sheep reared and specific shearing
      data.

    - `"ppm_animal_origin_production"`: animal-origin products like
      milk, eggs, and honey, as well as wool and other derivatives.

    - `"ppm_cow_farming"`: dairy cow farming, with the number of milked
      cows, their geographic distribution, and productivity rates.

    - `"ppm_aquaculture"`: aquaculture activities, including fish
      farming, shrimp farming, and mollusk farming.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **geo_level**: `"country"`, `"region"`, `"state"`, or
    `"municipality"`

4.  **time_period**: picks the years for which the data will be
    downloaded

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data (raw_data = FALSE) about aquaculture (dataset = "ppm_aquaculture")
# from 2013 to 2015 (time_period = 2013:2015) in english
# with the level of aggregation being the country (geo_level = "country").
data <- load_ppm(
  dataset = "ppm_aquaculture",
  raw_data = FALSE,
  geo_level = "country",
  time_period = 2013:2015
)

# Download raw data about sheep farming by state from 1980 to 1995 in portuguese (language = "pt")
data <- load_ppm(
  dataset = "ppm_sheep_farming",
  raw_data = TRUE,
  geo_level = "state",
  time_period = 1980:1995,
  language = "pt"
)
```

## SIGMINE

Loads information the mines being explored legally in Brazil, including
their location, status, product being mined and area in square meters
etc. Survey is done at municipal and state level. The National Mining
Agency (ANM) is responsible for this survey.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: `"sigmine_active"`

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

3.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated data (raw_data = FALSE) in portuguese (language = "pt").
data <- load_sigmine(
  dataset = "sigmine_active",
  raw_data = FALSE,
  language = "pt"
)
```

## ANEEL

Loads data from the National Electrical Energy Agency (ANEEL), a
Brazilian independent federal agency linked to the Ministry of Mines and
Energy (MME). ANEEL works to provide favorable conditions for the
Electrical Energy Market to develop with balance and for the benefit of
society.

As for now, there are three different datasets available for download:
the Energy Development Budget and the Energy Generation.

#### Energy Development Budget

The Energy Development Budget dataset showcases the Energy Development
Account’s (CDE) anual budget expenses. The CDE is designed to promote
the Brazilian energy development and is managed by the Electrical Energy
Commercialization Chamber (CCEE).

The dataset makes available the year of the observation – from 2013 to
2022 –, the type of expense, its value in R\$ (Reais) and its share over
the total amount of CDE budget expenses on the year\*.

\*Note that ‘share_of_total’ values sum to 1 for each year available.

#### Energy Generation

The Energy Generation dataset showcases information about ANEEL’s
Generation Informations System (SIGA). SIGA provides information about
the Brazilian electrical energy generation installed capacity.

The dataset provides information at the individual venture/entity level.
It contains information about the power, source, stage, type of
permission, origin and final fuel with which each venture/entity
operates, as well as other legal, technical and geographical
information.\* Operation start dates contained in the dataset go as far
back as 1924 up to 2022.

\* For more details on each variable, access [This
link](https://app.powerbi.com/view?r=eyJrIjoiNjc4OGYyYjQtYWM2ZC00YjllLWJlYmEtYzdkNTQ1MTc1NjM2IiwidCI6IjQwZDZmOWI4LWVjYTctNDZhMi05MmQ0LWVhNGU5YzAxNzBlMSIsImMiOjR9)
and select “Manual do Usuario”.

#### Energy Enterprises

The Energy Enterprises dataset showcases information about distributed
micro and mini generators, covered by the Regulatory Resolution nº
482/2012. The list of projects is classified by variables that make up
their identification, namely: connected distributor, project code,
numerical nucleus of the project code, owner name, production class,
subgroup, name of the owner, number of consumer units that receive
credits, connection date, type of generating unit, source, installed
power, municipality, and federative unit where it is located.

The data is expressed in quantities and installed power in kW
(kilowatt). The quantity corresponds to the number of distributed micro
or mini generators installed in the specified period. The installed
power is defined by the sum of the nominal active electric power of the
generating units.

\* For more details on each variable, access [This
link](https://dadosabertos.aneel.gov.br/dataset/relacao-de-empreendimentos-de-geracao-distribuida)
and select “Dicionário de dados”.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are three choices:
    - `"energy_development_budget"`: government spending towards energy
      sources
    - `"energy_generation"`: energy generation by entity/corporation
    - `"energy_enterprises_distributed"`: distributed micro and mini
      generators
2.  **raw_data**: there are two options:
    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.
3.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated data about energy generation
clean_aneel <- load_aneel(
  dataset = "energy generation",
  raw_data = FALSE
)
```

## EPE

Loads data from the Energy Research Company (EPE), a Brazilian public
company that works closely with the Brazilian Ministry of Mines and
Energy (MME) and other agencies to ensure the sustainable development of
Brazil’s energy infrastructure. EPE’s duty on that mission is to support
MME with quality research and studies in order to aid Brazil’s energy
infrastructure planning.

As for now, there are three different datasets available for download:
Consumer Energy Consumption, Industrial Energy Consumption, and the
National Energy Balance. All of them were obtained from the [EPE
website](https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/publicacoes/).

#### Consumer Energy Consumption

The Consumer Energy Consumption dataset provides monthly data from 2004
to 2025 about energy consumption and number of consumers. The data is
organized by State, Region, or Electric Subsystem, and is broken down by
class of service and type of consumer.

The available classes are: Residential, Commercial, Industrial, Rural,
and Others. For each observation, the dataset reports the type of
consumer (Captive or Free), total consumption in megawatt-hours (MWh),
and the number of consumers.

When using the Subsystem or Region level, consumer totals are provided
but are not disaggregated for all classes and consumer types.

#### Industrial Energy Consumption

The Industrial Energy Consumption dataset provides monthly data from
2004 to 2025 on energy consumption by industrial sector. Data is
available at the State or Subsystem level. Each observation identifies
the industrial sector responsible for the consumption and the amount
consumed in megawatt-hours (MWh).

#### National Energy Balance

The National Energy Balance is a thorough and extensive research
developed and published by EPE that contains useful data about energy
production, consumption, imports, exports, transformation, and final
use.

The processed dataset provides yearly data from 2003 to 2023. It covers
all Brazilian energy sources (such as petróleo, gás natural, carvão,
eletricidade, lenha, solar and others) and distinguishes between
different types of energy flow: production, transformation, final
consumption, losses, and adjustments.

Each energy source appears as a separate column in the original
spreadsheets. The cleaned data is returned in long format, with one row
per combination of year, energy source, and account type. The account
type is labeled to indicate whether it refers to production,
transformation (for example, “TRANSFORMAÇÃO – REFINARIAS DE PETRÓLEO”),
or consumption (for example, “CONSUMO – RESIDENCIAL”).

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are three choices:  
    `"consumer_energy_consumption"`: monthly energy consumption and
    consumers by State, Region or Electric Subsystem  
    `"industrial_energy_consumption"`: monthly industrial energy
    consumption by State or Subsystem  
    `"national_energy_balance"`: yearly energy flow by account and
    energy source

2.  **raw_data**: there are two options:  
    `TRUE`: if you want the data as it is originally.  
    `FALSE`: if you want the treated version of the data.

3.  **geo_level**: only applies to `"consumer_energy_consumption"` and
    `"industrial_energy_consumption"` datasets.  
    `"state"`  
    `"subsystem"`

4.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

------------------------------------------------------------------------

**Examples:**

``` r
# download treated data about consumer energy consumption at the state level
clean_epe <- load_epe(
  dataset = "consumer_energy_consumption",
  geo_level = "state",
  raw_data = FALSE
)

# download treated data from the National Energy Balance
balance <- load_epe(
  dataset = "national_energy_balance",
  raw_data = FALSE
)
```

# Other tools

## Legal Amazon Municipalities

Many of our functions use a dataset with Brazilian municipalities, their
municipality codes, whether they belong to the Legal Amazon, their
state, and some more variables. It was constructed from the [IBGE
spreadsheet](https://www.ibge.gov.br/geociencias/cartas-e-mapas/mapas-regionais/15819-amazonia-legal.html?=&t=acesso-ao-produto)
with Legal Amazon municipalities, along with a data frame from the
‘geobr’ package. For more information on the columns, run
`??datazoom.amazonia::municipalities`.

Regarding the statistics in this package reported at the **municipality
level**, for municipalities that are only partially included in the
Legal Amazon (according to IBGE’s latest release), the data refer to the
entire municipality, not just to the portion that lies within the Legal
Amazon.

``` r
# load Brazilian municipalities dataset
data <- datazoom.amazonia::municipalities
```

## <a name="googledrive"></a> The ‘googledrive’ package

For some of our functions, the original data is stored in Google Drive
and exceeds the file size limit for which direct downloads are possible.
As a result, the `googledrive` package is required to download the data
though the Google Drive API and run the function.

The first time the package is called, it requires you to link your
Google account and grant permissions to be able to download data through
the Google Drive API.

You **must** tick all boxes when the permissions page opens, or else the
following error will occur:

``` r
# Error in `gargle_abort_request_failed()`:
# ! Client error: (403) Forbidden
# Insufficient Permission: Request had insufficient authentication scopes.
# • domain: global
# • reason: insufficientPermissions
# • message: Insufficient Permission: Request had insufficient authentication
#  scopes.
# Run `rlang::last_error()` to see where the error occurred.
```

For further information, click
[here](https://googledrive.tidyverse.org/) to access the official
package page.

## Credits

DataZoom is developed by a team at Pontifícia Universidade Católica do
Rio de Janeiro (PUC-Rio), Department of Economics. Our official website
is at: <https://www.econ.puc-rio.br/datazoom/>.

To cite package `datazoom.amazonia` in publications use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata.  
> <https://www.econ.puc-rio.br/datazoom/english/index.html>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2023,
        author = {Data Zoom},
        title = {Data Zoom: Simplifying Access To Brazilian Microdata},
        url = {https://www.econ.puc-rio.br/datazoom/english/index.html},
        year = {2023},
    }
