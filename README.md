
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
| **[COMEX](#comex)**         | *Brazilian foreign trade*       |
| **[BACI](#baci)**           | *Global foreign trade*          |
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
| **[Legal Amazon Municipalities](#legal-amazon-municipalities)** | *Dataset with Brazilian cities and whether they belong to the Legal Amazon* |
| **[The ‘googledrive’ package](#googledrive)** | *Troubleshooting and information for downloads from Google Drive* |

</table>

# Environmental Data

## PRODES

## Overview

[PRODES](https://www.gov.br/inpe/pt-br) (Projeto de Monitoramento da
Floresta Amazônica Brasileira - Project for Monitoring the Brazilian
Amazon Forest) is Brazil’s primary satellite-based deforestation
monitoring system operated by [INPE](https://www.inpe.br/) (National
Institute for Space Research).

This dataset provides:

- **Amazon deforestation monitoring**: Official deforestation data using
  satellite imagery
- **Multiple landscape classes**: Deforestation, residual deforestation,
  native vegetation, hydrography, non-forest areas, clouds
- **Raster data**: High-resolution spatial data covering entire Legal
  Amazon
- **Municipality aggregates**: Affected areas per municipality for
  non-spatial analysis
- **Cumulative and incremental data**: Total deforestation since 1988
  and year-by-year changes
- **Long time series**: Available from 2007 onwards (cumulative) and
  incrementally by year
- **Official baseline**: Used by Brazilian government for forest policy
  and enforcement

PRODES is the authoritative source for Amazon deforestation statistics,
used for environmental monitoring, policy evaluation, and international
reporting on Brazilian forest protection.

### Data Source and Methodology

PRODES monitoring: - Uses satellite imagery from multiple sources
(Landsat, CBERS, others) - Automated and manual analysis to detect
clear-cut deforestation - Annual assessment of forest loss - Recently
(2020+) raster data published through
[TerraBrasilis](https://terrabrasilis.dpi.inpe.br/) - Data available as
both raster files and aggregated by municipality

For more information, visit [INPE PRODES
Project](https://www.gov.br/inpe/pt-br) and
[TerraBrasilis](https://terrabrasilis.dpi.inpe.br/).

------------------------------------------------------------------------

## Available Datasets

### **1. deforestation**

Clear-cut deforestation areas (complete forest loss).

- **Coverage**: Entire Legal Amazon region
- **Time period**:
  - 2007: Cumulative deforestation from 1988 to 2007
  - 2008-2023: Annual incremental deforestation (year-specific)
- **Data types**:
  - Raw: Raster/grid data (SpatRaster format)
  - Treated: Municipality-level area aggregates
- **Variables**: Deforested area (hectares) by municipality
- **Use cases**:
  - Track annual deforestation trends
  - Identify deforestation hotspots
  - Cumulative forest loss assessment
  - Municipality-level deforestation analysis

### **2. residual_deforestation**

Deforestation that was not captured in previous surveys (detected
through improved methodology).

- **Coverage**: Legal Amazon
- **Time period**: 2010-2023
- **Data types**: Raster or municipality aggregates
- **Variables**: Additional detected deforestation (hectares)
- **Use cases**:
  - More comprehensive deforestation assessment
  - Correct for historical detection gaps
  - Full accounting of forest loss

### **3. native_vegetation**

Remaining native forest and natural vegetation areas.

- **Coverage**: All vegetation in Legal Amazon
- **Time period**: 2023 (snapshot of current state)
- **Data types**: Raster only
- **Variables**: Forest and vegetation extent
- **Use cases**:
  - Assess remaining forest cover
  - Identify conservation priority areas
  - Vegetation extent mapping

### **4. hydrography**

Water bodies and hydrographic features.

- **Coverage**: All water features in Amazon
- **Time period**: 2023
- **Data types**: Raster only
- **Variables**: Water body extent
- **Use cases**:
  - Environmental impact assessment
  - Hydrology and water resource mapping

### **5. non_forest**

Non-forest areas including savanna, grasslands, and other vegetation
types.

- **Coverage**: Legal Amazon region
- **Time period**: 2023
- **Data types**: Raster only
- **Variables**: Non-forest vegetation extent
- **Use cases**:
  - Distinguish non-forest from forest areas
  - Natural vegetation assessment
  - Land-use classification

### **6. clouds**

Cloud cover in satellite imagery (data quality indicator).

- **Coverage**: Legal Amazon
- **Time period**: 2023
- **Data types**: Raster only
- **Variables**: Cloud-covered areas where analysis is uncertain
- **Use cases**:
  - Assess data quality and coverage
  - Identify areas needing re-analysis
  - Quantify monitoring gaps

------------------------------------------------------------------------

## Important Data Characteristics

### Raw vs. Treated Data

- **Raw data** (`raw_data = TRUE`): Returns SpatRaster objects (raster
  grids) from TerraBrasilis
- **Treated data** (`raw_data = FALSE`): Aggregated to municipality
  level showing total affected area
- **Raster resolution**: Typically ~30-meter pixels
- **Large size**: Raster data is high-resolution and may be large

### Cumulative vs. Incremental Data

| Year      | Type        | Definition                                   |
|-----------|-------------|----------------------------------------------|
| 2007      | Cumulative  | All deforestation from 1988-2007             |
| 2008-2023 | Incremental | Deforestation detected in that specific year |

When analyzing time trends, be aware that 2007 includes 19 years of
accumulated loss.

### Data Organization

- **Raster format**: High-resolution grid with classified pixels
  (deforestation, forest, etc.)
- **Municipality level**: Treated data aggregates raster to municipality
  boundaries
- **Non-spatial**: Municipality aggregates don’t retain spatial location
  (summarized only)

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which landscape classification to download.

``` r
dataset = "deforestation"             # Clear-cut deforestation (main product)
dataset = "residual_deforestation"    # Previously undetected deforestation
dataset = "native_vegetation"         # Remaining forests and vegetation (2023 only)
dataset = "hydrography"               # Water bodies (2023 only)
dataset = "non_forest"                # Savanna and other non-forest (2023 only)
dataset = "clouds"                    # Cloud cover (2023 only)
```

### 2. **raw_data**

Controls data format: raster or municipality aggregates.

- `TRUE`: Returns raw raster data (SpatRaster objects)
- `FALSE`: Returns municipality-level aggregated area data

``` r
raw_data = FALSE  # logical
```

**Important**: Raster data is large; ensure sufficient storage and
memory.

### 3. **time_period**

Specifies which year(s) to download.

**Available by dataset:**

| Dataset                  | Available Years                            |
|--------------------------|--------------------------------------------|
| `deforestation`          | 2007 (cumulative), 2008-2023 (incremental) |
| `residual_deforestation` | 2010-2023                                  |
| `native_vegetation`      | 2023 only                                  |
| `hydrography`            | 2023 only                                  |
| `non_forest`             | 2023 only                                  |
| `clouds`                 | 2023 only                                  |

``` r
time_period = 2020              # single year
time_period = c(2015, 2020)     # multiple years
time_period = 2015:2020         # range of years (for deforestation only)
```

### 4. **language**

Output language.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

### Example 1: Deforestation data for a single year

``` r
# download treated deforestation data for 2023
deforestation <- load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  time_period = 2023,
  language = "eng"
)
```

### Example 2: Deforestation data over multiple years

``` r
# download treated deforestation data for 2008 to 2023
deforestation_series <- load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  time_period = 2008:2023,
  language = "eng"
)
```

### Example 3: Cumulative forest loss since 1988

``` r
# download 2007 (cumulative 1988-2007) plus all years up to 2023
all_deforestation <- load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  time_period = c(2007, 2008:2023),
  language = "eng"
)
```

### Example 4: Residual deforestation

``` r
# download treated residual deforestation data for 2020
residual <- load_prodes(
  dataset = "residual_deforestation",
  raw_data = FALSE,
  time_period = 2020,
  language = "eng"
)
```

## Data Notes

### Raw Raster Data Characteristics

- **Format**: SpatRaster (from terra package)
- **Resolution**: ~30 meter pixels
- **Coverage**: Complete Legal Amazon region
- **Size**: Very large files; may require significant storage/memory
- **Classification**: Pixel values indicate land cover class
  (deforestation, forest, etc.)
- **CRS**: UTM zones or standard projection; verify after loading

### Municipality Aggregates

- **Simplified for analysis**: Loses spatial detail but much smaller
  files
- **Aggregation**: Total deforested area per municipality per year
- **Ideal for**: Time series, trend analysis, comparative studies

### Data Quality and Accuracy

1.  **Detection accuracy**: ~95% for clear-cut deforestation
2.  **Minimum mapping unit**: Small forest clearances may not be
    detected
3.  **Cloud contamination**: Clouds may prevent detection in some areas
    (see clouds dataset)
4.  **Seasonal effects**: Deforestation easier to detect in dry season

### Limitations

1.  **Clear-cut only**: Degradation and partial logging not captured
    (see DEGRAD for that)
2.  **Legal Amazon only**: Data limited to Legal Amazon definition;
    doesn’t cover all forest regions
3.  **Raster data very large**: May require specialized tools and
    significant computing resources
4.  **Historical changes**: INPE occasionally revises historical
    estimates as methodology improves
5.  **Recent data provisional**: 2023 data may be subject to revision as
    final processing completes

------------------------------------------------------------------------

## DETER

## Overview

[DETER](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter)
(Real-Time Detection System) is a satellite-based monitoring system
operated by [INPE](https://www.inpe.br/) (National Institute of Space
Research) that detects and reports changes in forest cover with near
real-time frequency. This system provides:

- **Forest cover change detection**: Identifies deforestation and forest
  degradation events
- **Near real-time monitoring**: Updates frequently (typically daily or
  weekly)
- **Spatial precision**: Geolocated detection of disturbances with
  geographic coordinates
- **Biome coverage**: Monitors Legal Amazon and Cerrado biome regions
- **Event-based data**: Each detection is a separate record with
  location and date
- **Historical records**: Accumulated database of past detection events

DETER is the primary early-warning system for deforestation in the
Amazon, used by Brazilian environmental agencies for enforcement,
research institutions for analysis, and internationally for monitoring
compliance with forest conservation goals.

### Data Source and Methodology

DETER monitoring: - Uses satellite imagery from multiple sources (MODIS,
Landsat, Sentinel) - Automated and manual analysis to detect recent
disturbances - Generates “alerts” - polyons representing areas of
detected change - Updates available regularly (frequency varies by
satellite availability) - Operated by INPE’s Remote Sensing Division

For technical details, visit [INPE DETER
System](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/deter/deter).

------------------------------------------------------------------------

## Available Datasets

### **1. deter_amz (DETER Amazon)**

DETER monitoring data for the Legal Amazon biome.

- **Geographic coverage**: Legal Amazon region (approximately 5 million
  km²)
- **Biome**: Tropical forest
- **Time period**: Historical data spanning multiple years with
  continuing updates
- **Spatial unit**: Polyons/spatial geometries with municipality
  identification
- **Variables**: Detection date, deforestation/degradation type, area,
  municipality, state
- **Update frequency**: Regular updates (typically daily or weekly)
- **Use cases**:
  - Monitor recent deforestation hotspots
  - Analyze deforestation trends in specific regions
  - Track enforcement impact on forest cover
  - Early-warning system for forest loss
  - Academic research on deforestation drivers

### **2. deter_cerrado (DETER Cerrado)**

DETER monitoring data for the Cerrado biome.

- **Geographic coverage**: Cerrado region (Brazilian tropical savanna)
- **Biome**: Tropical savanna/grassland
- **Time period**: Historical data with continuing updates
- **Spatial unit**: Polygons/spatial geometries with municipality
  identification
- **Variables**: Detection date, vegetation loss type, area,
  municipality, state
- **Update frequency**: Regular updates
- **Use cases**:
  - Monitor Cerrado vegetation loss (increasingly threatened biome)
  - Analyze agricultural expansion in Cerrado region
  - Compare forest and savanna degradation patterns
  - Track conservation effectiveness

------------------------------------------------------------------------

## Important Data Characteristics

### Raw Data Structure

The raw DETER data from INPE reports one alert/detection per row, with
each row typically associated with a single municipality in the raw
data. However, **many alerts actually overlap multiple municipalities**
(typically 2-4 municipalities) that are not all shown in the original
records. \### Data Processing

This package provides an important enhancement: **spatial intersection
with IBGE municipality geometries** (2019 version) to identify ALL
municipalities that each DETER alert overlaps with. This creates a more
complete and accurate geographic picture than the original raw data.

**Important note on CRS metadata**: The CRS (Coordinate Reference
System) information may need verification after loading, as coordinate
system metadata can sometimes be unclear in the original INPE data.

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which biome’s DETER data to download.

``` r
dataset = "deter_amz"      # Legal Amazon monitoring
dataset = "deter_cerrado"  # Cerrado biome monitoring
```

### 2. **raw_data**

Controls whether to download the original data or the processed/enhanced
version.

- `TRUE`: Returns raw INPE data with limited municipality information
- `FALSE`: Returns treated data with spatial intersection to identify
  all affected municipalities, standardized English variable names

``` r
raw_data = FALSE  # logical
```

**Recommendation**: Use `raw_data = FALSE` to get the enhanced
municipality identification from spatial intersection.

### 3. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

``` r
# download treated DETER Amazon data
deter_amz <- load_deter(
  dataset = "deter_amz",
  raw_data = FALSE,
  language = "eng"
)

# download treated DETER Cerrado data
deter_cerrado <- load_deter(
  dataset = "deter_cerrado",
  raw_data = FALSE,
  language = "eng"
)
```

## Data Notes

### Raw Data Limitations

- **Original INPE data**: Shows one alert per municipality, even when
  alerts overlap multiple municipalities
- **Enhanced version**: Spatial intersection with IBGE 2019 municipality
  boundaries identifies all affected municipalities
- **Advantage of treated data**: More complete geographic picture of
  deforestation extent

### Data Structure

Each alert/row typically contains: - **Spatial geometry**: Polygon
coordinates (SF object) - **Detection date**: When the alert was
issued - **Alert type**: Deforestation, forest degradation, or other
disturbance - **Area**: Size of detected change in hectares -
**Municipality**: Geographic unit identification - **State**: Brazilian
state - **Metadata**: Satellite source, confidence level (varies by
product)

### Important Considerations

1.  **Alert types vary**: “Deforestation” is permanent forest loss;
    “degradation” is forest damage but not clear-cut
2.  **Near real-time data**: Alerts are issued frequently; data is
    continuously updated
3.  **Minimum detection size**: Varies by sensor; typically 25 hectares
    for Amazon, larger for Cerrado
4.  **CRS metadata**: Verify coordinate system after loading; typically
    UTM zones for Brazil
5.  **Overlapping municipalities**: Enhanced version accounts for alerts
    crossing municipality boundaries
6.  **False positives possible**: Satellite detection can occasionally
    misclassify cloud shadows or other features as forest loss

------------------------------------------------------------------------

## DEGRAD

## Overview

The [DEGRAD
project](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad)
is a research initiative that uses satellite imagery to monitor forest
degradation in the Amazon. Unlike DETER’s near real-time alerts, DEGRAD
provides a more detailed annual analysis of forest degradation patterns.

This dataset captures:

- **Forest degradation monitoring**: Tracks areas where forests are
  being damaged without complete clearing
- **Annual editions**: Data released as yearly reports with accumulated
  observations
- **Spatial polygons**: Detailed geographic boundaries of degradation
  events
- **Municipality linkage**: Enhanced version links degradation areas to
  affected municipalities
- **Historical coverage**: Multiple years of degradation monitoring from
  2007 onwards

DEGRAD data is valuable for understanding forest degradation as a
distinct phenomenon from clear-cut deforestation, important for carbon
accounting, biodiversity protection, and understanding transition stages
toward complete forest loss.

### Data Source and Methodology

DEGRAD monitoring: - Conducted by INPE’s forest monitoring programs -
Uses satellite imagery interpretation to identify forest degradation
signs - Focuses on selective logging, small-scale agriculture, forest
fires, and other degrading activities - Released as annual editions with
comprehensive analysis - Limited documentation available (original INPE
documentation is sparse)

For information, visit [INPE Forest
Monitoring](http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad).

------------------------------------------------------------------------

## Important Data Characteristics

### Data Organization

**Important**: DEGRAD data is organized differently than real-time
systems. Key points:

1.  **Yearly editions**: Data is organized by publication year (e.g.,
    “DEGRAD 2016”), not event year
2.  **Mixed event years**: A DEGRAD edition may contain degradation
    events from different years
    - Example: DEGRAD 2016 edition may include events detected in 2015
      or even earlier
3.  **Documentation limited**: Original INPE documentation is minimal;
    users should be aware of potential inconsistencies

### Spatial Integration

This package enhances the raw DEGRAD data by: - Intersecting DEGRAD
spatial polygons with IBGE municipality boundaries (2019 version) -
Providing municipality identification for each degradation event -
Converting to Simple Features (SF) objects for spatial analysis

**Note on CRS**: Coordinate system metadata should be verified after
loading, as original INPE data sometimes has unclear CRS information.

------------------------------------------------------------------------

## Available Dataset

### **degrad (Forest Degradation)**

Detailed monitoring of forest degradation across the Legal Amazon.

- **Coverage**: Legal Amazon region with focus on degradation detection
- **Time period**: 2007 onwards (but events within editions may vary)
- **Spatial unit**: Polygons/spatial geometries with municipality
  identification
- **Variables**: Event date/year, degradation type/cause, area,
  municipality, state, edition
- **Data format**: Simple Features (SF) spatial objects with geographic
  boundaries
- **Use cases**:
  - Distinguish degradation from complete deforestation
  - Analyze forest degradation hotspots
  - Understand selective logging extent
  - Carbon stock assessment
  - Forest fire impacts
  - Long-term degradation trends

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Only one dataset is available:

``` r
dataset = "degrad"  # Forest degradation monitoring
```

### 2. **raw_data**

Controls whether to download the original data or the processed/enhanced
version.

- `TRUE`: Returns raw INPE data with minimal processing
- `FALSE`: Returns treated data with English variable names,
  municipality identification from spatial intersection, and
  standardized formatting

``` r
raw_data = FALSE  # logical
```

**Recommendation**: Use `raw_data = FALSE` for most applications to get
municipality-level information. \### 3. **time_period**

Specifies which year(s) of degradation events to download.

- **Available range**: Generally 2007-2016 (check current availability)
- **Format**: Single year, vector of years, or range

**Important**: When you request a year, you get events from that year
regardless of which DEGRAD edition they appear in.

``` r
time_period = 2015              # single year
time_period = c(2010, 2015)     # multiple specific years
time_period = 2010:2015         # range of years
```

### 4. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Data Structure

The returned data is a Simple Features (SF) spatial object with:

- **Spatial column**: Geometric polygons representing degradation areas
- **year**: Year when degradation was detected/event occurred
- **degradation_type**: Type of degradation (logging, fire, agriculture,
  etc.)
- **area_hectares**: Size of degradation event
- **municipality**: Name of affected municipality
- **state**: Brazilian state
- **edition**: Which DEGRAD edition this event appears in
- **Additional attributes**: Quality metrics, confidence levels (vary by
  edition)

------------------------------------------------------------------------

## Examples

``` r
# download treated forest degradation data from 2010 to 2012
data <- load_degrad(
  dataset = "degrad",
  raw_data = FALSE,
  time_period = 2010:2012,
  language = "eng"
)
```

## Data Notes

### Data Organization Complexity

The annual edition structure (e.g., “DEGRAD 2016”) mixed with variable
event years within those editions means: - When you request year 2015,
you get all detected 2015 events regardless of edition - Some 2015
events may appear in both DEGRAD 2015 and DEGRAD 2016 editions -
Duplication is handled in the data loading process

### Degradation Types

Common degradation types include: - **Selective logging**: Commercial
timber extraction - **Forest fires**: Fire damage to forest areas -
**Agricultural clearing**: Small-scale farming expansion - **Mining**:
Degradation from mining activities - **Other**: Mixed or unclassified
degradation causes

(Exact categories vary by edition; verify with your loaded data)

### Spatial Considerations

1.  **Polygons not points**: Each event is a geometric polygon, not a
    single location point
2.  **Municipality intersection**: Treated data identifies all
    municipalities polygon overlaps
3.  **CRS verification**: Check coordinate system after loading
4.  **Geometry validity**: Some polygons may have validity issues; use
    `st_is_valid()` to check

### Data Limitations

1.  **Limited documentation**: INPE’s original documentation for DEGRAD
    is sparse
2.  **Mixed time periods**: Events from different years appear in same
    edition
3.  **Possible inconsistencies**: Classification and methodology may
    vary across editions
4.  **Detection limits**: Minimum detectable degradation size varies by
    methodology/edition
5.  **Not real-time**: This is annual analysis, not near-real-time
    detection like DETER

------------------------------------------------------------------------

## Imazon

## Overview

[Imazon](https://imazon.org.br/) (Instituto do Homem e Meio Ambiente da
Amazônia - Institute of Man and Environment of the Amazon) is an
independent Brazilian research organization that produces its own
deforestation monitoring and analysis.

This dataset provides:

- **Municipality-level deforestation pressure classification**: Risk
  assessment of deforestation threat
- **Pressure categories**: Three-level classification system (0, 1,
  2, 3) representing deforestation pressure intensity
- **Geographic coverage**: Municipalities in the Legal Amazon region
- **Municipal boundaries**: Provided as spatial features (shapefile
  format)
- **Policy-relevant indicator**: Used in environmental monitoring and
  policy assessments

Imazon’s deforestation pressure index helps identify which
municipalities face high deforestation risk, valuable for targeted
conservation, enforcement, and development planning.

### Data Source

Imazon’s classification is based on: - Deforestation monitoring using
satellite imagery - Analysis of deforestation trends and drivers - Risk
assessment methodology to classify municipalities - Regular updates as
monitoring data accumulates

For more information, visit [Imazon Official
Website](https://imazon.org.br/).

------------------------------------------------------------------------

## Available Dataset

### **imazon_shp (Imazon Municipality Pressure Index)**

Municipality-level deforestation pressure classification with spatial
geometries.

- **Geographic unit**: Brazilian municipalities
- **Coverage**: Legal Amazon municipalities
- **Variables**: Municipality name, state, deforestation pressure
  category (0-3)
- **Data format**: Shapefile / Simple Features (SF) spatial objects
- **Update frequency**: Periodic updates as Imazon releases new
  assessments
- **Use cases**:
  - Identify high-risk deforestation municipalities
  - Target conservation resources to threatened areas
  - Analyze geographic patterns of deforestation pressure
  - Environmental risk assessment
  - Monitoring enforcement effectiveness
  - Correlate pressure with socioeconomic variables

------------------------------------------------------------------------

## Deforestation Pressure Categories

Imazon’s three-level classification system: \| Category \| Pressure
Level \| Description \| \|———-\|—\|—\| \| 0 \| No/Low Pressure \|
Minimal deforestation threat \| \| 1 \| Moderate Pressure \| Some
deforestation risk \| \| 2 \| High Pressure \| Significant deforestation
threat \| \| 3 \| Very High Pressure \| Severe/Critical deforestation
risk \|

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Only one dataset is available:

``` r
dataset = "imazon_shp"  # Imazon municipality pressure classification
```

### 2. **raw_data**

Controls whether to download the original data or the processed/cleaned
version.

- `TRUE`: Returns raw shapefile data exactly as provided by Imazon
- `FALSE`: Returns treated data with standardized English variable
  names, consistent formatting, and SF object structure

``` r
raw_data = FALSE  # logical
```

### 3. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

``` r
# download treated Imazon deforestation pressure data
data <- load_imazon(
  raw_data = FALSE,
  language = "eng"
)
```

## Data Notes

### Data Format

- **Shapefile format**: Returned as Simple Features (SF) spatial objects
- **Geometric representation**: Municipality polygons with pressure
  classification
- **Attributes**: Municipality name, state, pressure category

### Pressure Categories

The 0-3 scale represents Imazon’s assessment of deforestation risk: -
Based on historical deforestation trends - Current forest cover -
Accessibility and economic drivers - Human pressure factors

### Important Limitations

1.  **Static classification**: This is a classification/index, not
    time-series deforestation data
2.  **Methodology may change**: Imazon’s methodology for calculating
    pressure may be updated
3.  **Subjective risk assessment**: Classification involves judgment in
    addition to objective metrics
4.  **Not current alerts**: This is not real-time detection like DETER;
    it’s a classification product
5.  **Municipal-level only**: Finer spatial resolution not available

### Using with Other Datasets

This dataset works well in combination with: - **DETER**: Compare
pressure classification with actual recent deforestation - **DEGRAD**:
Correlate pressure with degradation patterns - **CEMPRE**: Analyze
relationship between employment and deforestation pressure - **COMEX**:
Examine trade patterns in high-pressure vs low-pressure areas

------------------------------------------------------------------------

🔴 This function uses the `googledrive` package to download data. In
case of authentication errors, see [googledrive](#googledrive).

## IBAMA

## Overview

The Brazilian Institute of Environment and Renewable Natural Resources
(Instituto Brasileiro do Meio Ambiente e dos Recursos Naturais
Renováveis - IBAMA) dataset documents environmental enforcement actions,
including embargoes of productive assets and fines issued for
environmental violations. This dataset provides individual-level records
of environmental infractions from 2005 onwards, representing the
enforcement activity of Brazil’s primary federal environmental agency.

The data covers environmental violations across multiple sectors
including deforestation, illegal mining, wildlife trafficking, illegal
fishing, and other environmental crimes.

## Data Coverage

The IBAMA dataset includes:

- **Time Period**: Individual enforcement records from 2005 to present
- **Geographic Coverage**: All Brazilian states and the Legal Amazon
  region
- **Infraction Types**: Environmental violations across forestry,
  biodiversity, and natural resource management
- **Actions**: Embargoed areas, distributed fines, and collected fines
- **Individual-Level Detail**: Each record represents a specific
  infraction event

## Dataset Description

### Available Datasets

The function provides access to three distinct datasets tracking
different stages of environmental enforcement:

1.  **Embargoed Areas** (`"embargoed_areas"`)
    - Areas of land subject to federal environmental embargo
    - Productively assets frozen due to illegal activities (typically
      deforestation)
    - Includes area size, location, and embargo date
    - Useful for measuring enforcement scope and geographic patterns
2.  **Distributed Fines** (`"distributed_fines"`)
    - Environmental fines issued by IBAMA for violations
    - Fines not yet paid by individuals or corporations
    - Include violation type, fine amount, and entity responsible
    - Track enforcement intensity and violation frequency
3.  **Collected Fines** (`"collected_fines"`)
    - Environmental fines that have been paid
    - Represents actual revenue recovery from environmental violations
    - Subset of distributed fines with payment confirmation
    - Indicates compliance and enforcement effectiveness

### Key Variables

1.  **Infraction Information**: Type of violation, legal basis, date of
    infraction
2.  **Location**: State, municipality, coordinates (when available)
3.  **Enforcement Action**: Type of action (embargo, fine), date of
    action
4.  **Financial Data**: Fine amount, payment status, payment date
5.  **Entity Information**: Individual or corporate identifier, sector
6.  **Embargo Details**: Embargoed area size, land type, geographic
    descriptors

------------------------------------------------------------------------

## Data Aggregation

The function returns either: - **Raw Data**: Individual infraction
records (original format) - **Aggregated Data**: Summary statistics for
each time-location period, including: - Total number of infractions -
Infractions sent to prosecution - Infractions with ongoing legal
proceedings - Embargoed area totals - Fine totals and collection rates

------------------------------------------------------------------------

## Function Parameters

**Options:**

1.  **dataset**: Three possible choices
    - `"embargoed_areas"`: Embargoed productive areas
    - `"distributed_fines"`: Issued fines (paid or unpaid)
    - `"collected_fines"`: Fines that have been paid
2.  **raw_data**:
    - `TRUE`: Individual-level records as originally recorded
    - `FALSE`: Treated/aggregated version of the data
3.  **states**:
    - Specifies which states to download (default: `"all"`)
    - Single state example: `"AC"` (Acre)
    - Multiple states example: `c("AC", "AM", "AP")` (Acre, Amazonas,
      Amapá)
    - Does not apply to embargoed_areas dataset
4.  **language**:
    - `"pt"`: Portuguese language labels and names
    - `"eng"`: English language

------------------------------------------------------------------------

------------------------------------------------------------------------

## Examples

``` r
# download treated embargoed areas data in english
data <- load_ibama(
  dataset = "embargoed_areas",
  raw_data = FALSE,
  language = "eng"
)

# download treated collected fines data from Bahia
data <- load_ibama(
  dataset = "collected_fines",
  raw_data = FALSE,
  states = "BA",
  language = "pt"
)
```

## MapBiomas

## Overview

The MapBiomas project provides comprehensive satellite-based data on
land cover and land use changes across Brazil. The project uses machine
learning algorithms applied to Landsat satellite imagery to classify and
track changes in biomes, land cover types, and economic activities over
time. This dataset is essential for understanding deforestation,
agricultural expansion, conservation outcomes, and landscape dynamics in
the Amazon and other biomes.

MapBiomas represents one of the most detailed and scientifically
rigorous sources of remote sensing data on Brazilian land cover, with
coverage spanning from 1985 to present and annual updates.

## Data Coverage

The MapBiomas dataset includes:

- **Time Period**: Annual data from 1985 to 2023 (with updates
  continuing)
- **Geographic Coverage**: All Brazilian biomes (Amazon, Cerrado,
  Atlantic Forest, Caatinga, Pampa, Pantanal)
- **Spatial Resolution**: 30 meters (Landsat resolution)
- **Temporal Frequency**: Annual maps with seasonal detail
- **Update Schedule**: New data typically released annually with recent
  years added progressively
- **Aggregation Levels**: Municipalities, indigenous lands, states,
  biomes, and protected areas

## Available Datasets

### 1. Land Cover (`"mapbiomas_cover"`)

Annual maps of land cover types including: - **Forest Types**: Native
forest, forest formation, forest plantation - **Non-Forest Vegetation**:
Grassland/pasture, shrub vegetation, herbaceous - **Agricultural Land**:
Temporary crops (annual/seasonal), perennial crops (sugarcane, coffee,
cocoa) - **Urban Areas**: Urban/built-up land and infrastructure -
**Water Bodies**: Rivers, lakes, reservoirs, aquaculture -
**Non-vegetated**: Mining, bare soil, rock outcrops - **Other
Categories**: Cloud coverage, nodata

**Key Applications**: Deforestation monitoring, agricultural expansion,
urbanization patterns, forest conservation

### 2. Land Cover Transitions (`"mapbiomas_transition"`)

Year-to-year changes in land cover including: - **Deforestation**:
Conversion from forest to other uses - **Reforestation**: Conversion to
forest types - **Agricultural Transitions**: Changes between crop types
or from other uses to agriculture - **Degradation**: Forest to
degraded/shrub vegetation - **Regeneration**: Abandoned areas returning
to vegetation

**Key Applications**: Deforestation tracking, regeneration assessment,
agricultural dynamics, conservation effectiveness

### 3. Deforestation and Regeneration (`"mapbiomas_deforestation_regeneration"`)

Specific focus on forest cover changes: - **Deforestation**: Permanent
loss of forest cover - **Forest Regeneration**: Secondary forest
regrowth and natural recovery - **Degradation Signals**: Forest areas
showing stress indicators - **Cumulative Deforestation**: Total forest
loss since baseline

**Key Applications**: Amazon monitoring, conservation evaluation,
regeneration potential, climate impact studies

### 4. Mining Activities (`"mapbiomas_mining"`)

Areas used for mining operations: - **Active Mining**: Currently
operational extraction sites - **Abandoned Mining**: Previous mining
areas, potentially available for rehabilitation - **Mining Extent**:
Total area impacted by mining activities - **Mining Types**: Surface
mining, artisanal mining, infrastructure

**Key Applications**: Environmental impact assessment, land degradation,
restoration planning, environmental compliance

### 5. Irrigation (`"mapbiomas_irrigation"`)

**Note**: Temporarily unavailable - new collection coming soon

Previously included: - **Irrigated Areas**: Extent of irrigation in
agricultural systems - **Irrigation Type**: Drip, center-pivot, flood
irrigation - **Crop Types Under Irrigation**: Which crops receive
irrigation

### 6. Water Bodies (`"mapbiomas_water"`)

**Note**: Temporarily unavailable - new collection coming soon

Previously included: - **Surface Water Extent**: Permanent and seasonal
water bodies - **Water Type**: Natural rivers/lakes vs. artificial
reservoirs - **Seasonal Variation**: Dry vs. wet season water extent

### 7. Wildfire Burn Scars (`"mapbiomas_fire"`)

Areas affected by wildfires: - **Burn Scars**: Areas burned in recent
fires - **Fire Extent**: Total area impacted per year - **Fire
Frequency**: Repeated burning in same areas (indicating high fire
risk) - **Fire Season**: Temporal pattern of fires

**Key Applications**: Fire monitoring, ecosystem vulnerability, climate
impacts, fire management

------------------------------------------------------------------------

## Function Parameters

**Options:**

1.  **dataset**: Seven possible choices (two temporarily unavailable)
    - `"mapbiomas_cover"`: Land cover types
    - `"mapbiomas_transition"`: Changes in land cover
    - `"mapbiomas_deforestation_regeneration"`: Forest cover changes
    - `"mapbiomas_mining"`: Mining areas
    - `"mapbiomas_irrigation"`: Irrigated areas (temporarily
      unavailable)
    - `"mapbiomas_water"`: Water bodies (temporarily unavailable)
    - `"mapbiomas_fire"`: Wildfire burn scars
2.  **raw_data**:
    - `TRUE`: Data in original format from MapBiomas
    - `FALSE`: Cleaned and standardized version (recommended for
      analysis)
3.  **geo_level**: Varies by dataset
    - For `"mapbiomas_cover"`: `"municipality"` or `"indigenous_land"`
    - For `"mapbiomas_transition"`: `"municipality"` or `"biome"`
      (faster)
    - For `"mapbiomas_deforestation_regeneration"`: `"municipality"`
      only
    - For `"mapbiomas_mining"`: `"indigenous_land"` or `"municipality"`
    - For `"mapbiomas_irrigation"`: `"state"` or `"biome"`
    - For `"mapbiomas_water"`: `"municipality"`, `"state"`, or `"biome"`
    - For `"mapbiomas_fire"`: `"state"` only
4.  **language**:
    - `"pt"`: Portuguese language labels
    - `"eng"`: English language

------------------------------------------------------------------------

## Measurement Units and Notes

- **All Areas**: Measured in hectares (ha)
- **1 hectare**: Approximately 2.47 acres
- **Validation**: MapBiomas uses field validation and visual
  interpretation to ensure accuracy
- **Updates**: Historical data may be revised with new collections and
  improved algorithms

------------------------------------------------------------------------

------------------------------------------------------------------------

## Examples

``` r
# download treated MapBiomas land cover data by municipality
data <- load_mapbiomas(
  dataset = "mapbiomas_cover",
  raw_data = FALSE,
  geo_level = "municipality",
  language = "eng"
)

# download treated data on mining on indigenous lands
data <- load_mapbiomas(
  dataset = "mapbiomas_mining",
  raw_data = FALSE,
  geo_level = "indigenous_land",
  language = "eng"
)

# download treated wildfire burn scar data by state
data <- load_mapbiomas(
  dataset = "mapbiomas_fire",
  raw_data = FALSE,
  geo_level = "state",
  language = "eng"
)
```

## TerraClimate

## Overview

TerraClimate is a global climate and climatic water balance dataset
developed by the [Climatology Lab](https://www.climatologylab.org/) at
University of California, Merced. This package provides access to
TerraClimate data for Brazil and the Amazon region.

This dataset provides:

- **High-resolution climate data**: Monthly climate variables at ~4km
  resolution globally
- **Comprehensive variables**: Temperature, precipitation, wind,
  radiation, soil moisture, drought indices
- **Water balance data**: Evapotranspiration, runoff, water deficit
  calculations
- **Temporal coverage**: Monthly data from 1958 to present (continuously
  updated)
- **Global coverage**: Available worldwide, subset here for Brazil
- **Satellite-derived data**: Combines satellite observations with
  ground station networks
- **Research quality**: Peer-reviewed, widely used in climate and
  ecological research

TerraClimate is essential for understanding climate variability, water
availability, drought risk, agricultural potential, and climate change
impacts across Brazil and the Amazon.

### Data Source and Methodology

TerraClimate data is compiled by: - University of California Climatology
Lab - Integration of satellite and ground-based observations - Validated
against station networks - Downscaled to ~4km global resolution -
Monthly temporal resolution with daily and subdaily estimates available

For more information, visit [TerraClimate
Project](https://www.climatologylab.org/terraclimate.html).

------------------------------------------------------------------------

## Available Climate Variables

TerraClimate provides 13 main climate and water balance variables:

| Dataset | Code | Description | Units |
|----|----|----|----|
| max_temperature | tmax | Maximum 2-m Temperature | °C |
| min_temperature | tmin | Minimum 2-m Temperature | °C |
| wind_speed | ws | Wind Speed at 10-m | m/s |
| vapor_pressure_deficit | vpd | Vapor Pressure Deficit | kPa |
| vapor_pressure | vap | 2-m Vapor Pressure | kPa |
| snow_water_equivalent | swe | Snow Water Equivalent at End of Month | mm |
| shortwave_radiation_flux | srad | Downward Shortwave Radiation Flux | W/m² |
| soil_moisture | soil | Soil Moisture at End of Month | mm |
| runoff | q | Runoff | mm |
| precipitation | ppt | Accumulated Precipitation | mm |
| potential_evaporation | pet | Reference Evapotranspiration | mm |
| climatic_water_deficit | def | Climatic Water Deficit | mm |
| water_evaporation | aet | Actual Evapotranspiration | mm |
| palmer_drought_severity_index | PDSI | Palmer Drought Severity Index | unitless |

------------------------------------------------------------------------

## Data Format and Coverage

### Spatial Resolution

- **Resolution**: Approximately 4 km (0.04° at equator)
- **Coverage**: Global; subset available for Brazil and Legal Amazon
- **Coordinates**: WGS84 latitude/longitude

### Temporal Resolution

- **Frequency**: Monthly
- **Time span**: 1958 to present (continuously updated)
- **Data lag**: Recent months added as they become available (typically
  2-3 months delay)

### Data Type

- **Format**: NetCDF files (raster/grid data)
- **Size**: Large for multi-year, multi-variable downloads
- **Access**: Downloaded from THREDDS server

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which climate variable to download.

``` r
# Temperature and radiation
dataset = "max_temperature"         # tmax
dataset = "min_temperature"         # tmin
dataset = "shortwave_radiation_flux" # srad

# Water and moisture
dataset = "precipitation"           # ppt
dataset = "potential_evaporation"   # pet
dataset = "water_evaporation"       # aet (actual evapotranspiration)
dataset = "soil_moisture"           # soil
dataset = "runoff"                  # q

# Atmospheric variables
dataset = "wind_speed"              # ws
dataset = "vapor_pressure"          # vap
dataset = "vapor_pressure_deficit"  # vpd

# Drought and composite indices
dataset = "climatic_water_deficit"  # def
dataset = "palmer_drought_severity_index" # PDSI
dataset = "snow_water_equivalent"   # swe
```

### 2. **raw_data**

Controls data format returned.

- `TRUE`: Returns raw raster data (NetCDF, SpatRaster format)
- `FALSE`: Returns aggregated data (specific format depends on
  configuration)

``` r
raw_data = FALSE  # logical
```

### 3. **time_period**

Specifies which year(s) to download.

**Available range**: 1958 to present (most recent months have 2-3 month
lag)

``` r
time_period = 2020              # single year
time_period = c(2010, 2020)     # specific years
time_period = 2010:2020         # range of years
```

### 4. **legal_amazon_only**

Restricts geographic coverage to Legal Amazon region.

- `TRUE`: Downloads only data for Legal Amazon region (much smaller
  files)
- `FALSE`: Downloads data for all Brazil (larger files)

``` r
legal_amazon_only = TRUE  # logical
```

**Recommendation**: Use `TRUE` to significantly reduce download size for
Amazon-focused research.

### 5. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Important Download Considerations

**Data Size**: TerraClimate raster data is substantial. Consider:

- **File size**: Multi-year downloads can be very large (hundreds of MB
  to GBs)
- **Internet**: High-speed connection recommended; THREDDS downloads can
  be slow
- **Storage**: Ensure sufficient disk space (multiple years of global
  data is large)
- **Time**: Downloads may take considerable time
- **Memory**: Raster processing requires sufficient RAM

**Recommendations**: - Use `legal_amazon_only = TRUE` to reduce size by
~95% - Download single or 2-3 year periods rather than decades at once -
Use high-speed internet connection - Have at least 10-50 GB free disk
space for multi-year downloads

------------------------------------------------------------------------

## Examples

``` r
# download precipitation data for the Legal Amazon (2020)
precip <- load_climate(
  dataset = "precipitation",
  time_period = 2020,
  legal_amazon_only = TRUE,
  language = "eng"
)
# download maximum temperature for multiple years, all of Brazil
max_temp <- load_climate(
  dataset = "max_temperature",
  time_period = 2010:2012,
  language = "eng"
)
```

------------------------------------------------------------------------

## Data Notes

### Variable Definitions

- **Tmax/Tmin**: Monthly average maximum and minimum 2-meter air
  temperatures
- **Precipitation**: Accumulated monthly precipitation
- **Evapotranspiration (AET)**: Actual water loss from soil + plants
  through evaporation/transpiration
- **Potential Evaporation (PET)**: Theoretical maximum
  evapotranspiration if unlimited water
- **Runoff**: Water flowing overland/through soil to streams
- **Soil Moisture**: Water stored in root zone at month end
- **Vapor Pressure Deficit (VPD)**: Difference between saturated and
  actual vapor pressure (indicator of atmospheric dryness)
- **Water Deficit (DEF)**: Accumulated water stress (PET - AET)
- **PDSI**: Standardized drought index (-4 to +4 scale)

### Data Quality

- **Validation**: Validated against independent station networks

- **Uncertainty**: Varies by region; higher in data-sparse areas

- **Interpolation**: Satellite and station data combined and downscaled
  to 4km

- **Reliability**: Generally excellent for temperature and
  precipitation; water balance variables have higher uncertainty

### Important Limitations

1.  **Raster data large**: Multi-year downloads can be hundreds of MB to
    GBs
2.  **4km resolution**: Suitable for regional analysis; may miss local
    variation
3.  **Monthly aggregation**: Daily and sub-daily variation not captured
4.  **Recent data lag**: Most recent 2-3 months not yet available
5.  **Interpolation uncertainty**: Some regions have lower data density
6.  **Snow/ice areas**: Less accurate in high mountains or glaciated
    regions (not major issue for Amazon)

### Recommended Processing

Due to large file size: - **Aggregate early**: Summarize by month/year
quickly to reduce memory use - **Legal Amazon only**: Massive size
reduction if working in Amazon region - **Subset years**: Download only
years of interest rather than decades - **Extract points**: If doing
point-based analysis, extract specific coordinates to simplify data -
**Use cloud computing**: Consider cloud platforms (Google Earth Engine)
for very large analyses

------------------------------------------------------------------------

## SEEG

## Overview

SEEG (Sistema de Estimativa de Emissões e Remoções de Gases de Efeito
Estufa - System of Estimates of Emissions and Removals of Greenhouse
Gases) is Brazil’s most comprehensive greenhouse gas emissions database
developed by [Observatório do Clima](https://oc.eco.br/) (Climate
Observatory).

This dataset provides:

- **Greenhouse gas emissions**: Complete estimates of all major
  climate-relevant gases
- **Multi-sector coverage**: Agriculture, energy, land use, industry,
  waste
- **Sub-sectoral detail**: Detailed breakdowns within each sector
- **Municipality and state levels**: Geographic disaggregation for
  regional analysis
- **Time series**: Historical data from 2000 onwards
- **Removal accounting**: Also includes carbon sequestration and
  removals
- **Comprehensive methodology**: Based on Brazilian national inventory
  standards
- **Transparent assumptions**: Well-documented methodology and data
  sources

SEEG is the primary tool for understanding Brazil’s greenhouse gas
emissions profile, tracking progress toward climate goals, identifying
emission hotspots, and supporting climate policy.

### Data Source and Methodology

SEEG emissions estimates are compiled using: - Government data from
multiple agencies (MAPA, IBGE, ANP, etc.) - Satellite monitoring of
deforestation and land use - International IPCC methodology standards -
Peer-reviewed scientific research - Regular updates as new government
data becomes available

For more information, visit [SEEG Project](https://www.seeg.org.br/) and
[Observatório do Clima](https://oc.eco.br/).

------------------------------------------------------------------------

## Available Datasets

### **1. seeg (All Sectors Combined)**

Complete greenhouse gas emissions across all sectors in one dataset.

- **Coverage**: All emission sources in Brazil
- **Sectors included**: All five (agriculture, energy, land use,
  industry, waste)
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Total emissions (CO₂e), by sector and sub-sector
- **Format**: Comprehensive view of Brazil’s total emissions profile
- **Note**: Only available with `raw_data = TRUE`
- **Use cases**:
  - Understand overall emissions landscape
  - Identify dominant emission sources
  - Track total emissions trends over time

### **2. seeg_farming (Agricultural and Livestock Emissions)**

Greenhouse gas emissions from agriculture and livestock activities.

- **Coverage**: All agricultural and livestock production
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Emissions from cattle, crop production, soil
  management, manure
- **Dominant source**: Usually the largest single emissions sector in
  Brazil
- **Components**:
  - Livestock (enteric fermentation, manure)
  - Crop production and soil management
  - Agricultural land preparation
- **Use cases**:
  - Assess agricultural emission contributions
  - Identify highest-emission municipalities
  - Evaluate livestock and farming intensity
  - Policy targets for agricultural emissions reduction

### **3. seeg_energy (Energy Sector Emissions)**

Emissions from energy production and consumption.

- **Coverage**: All energy-related emissions
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Emissions from electricity, transport, heating,
  fuel production
- **Components**:
  - Energy generation and distribution
  - Transportation fuels
  - Energy consumption
  - Industrial energy use
- **Use cases**:
  - Understand energy sector contribution to climate change
  - Track renewable vs. fossil fuel impacts
  - Identify regional energy emission patterns

### **4. seeg_land (Land Use Change Emissions)**

Emissions and removals from changes in forest cover and land use.

- **Coverage**: Deforestation, forest degradation, reforestation effects
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Net emissions/removals from land use change
- **Components**:
  - Deforestation and forest loss
  - Forest degradation
  - Reforestation and afforestation
  - Vegetation conversion
- **Importance**: Often largest single contributor to Brazil’s emissions
- **Use cases**:
  - Analyze deforestation climate impact
  - Identify reforestation opportunities
  - Assess forest conservation value
  - Link with PRODES and DETER deforestation data

### **5. seeg_industry (Industrial Process Emissions)**

Emissions from manufacturing and industrial processes.

- **Coverage**: All industrial sectors
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Emissions from cement, chemicals, metals, minerals,
  other manufacturing
- **Components**:
  - Chemical production (ammonia, soda ash, etc.)
  - Metal production (iron, aluminum, others)
  - Mineral processing (cement, lime, glass)
  - Other industrial processes
- **Use cases**:
  - Identify industrial emission hotspots
  - Regional manufacturing impacts
  - Process-specific emission reduction opportunities

### **6. seeg_residuals (Waste and Residuals Emissions)**

Emissions from waste management, landfills, and waste treatment.

- **Coverage**: All waste-related emissions
- **Time period**: 2000-2018
- **Geographic levels**: Country, State, Municipality
- **Key variables**: Emissions from solid waste, wastewater treatment,
  waste treatment
- **Components**:
  - Landfill methane emissions
  - Wastewater treatment
  - Waste disposal and treatment
  - Municipal solid waste management
- **Use cases**:
  - Assess waste sector contributions
  - Identify waste management improvement opportunities
  - Evaluate circular economy potential

------------------------------------------------------------------------

## Important Data Characteristics

### Collection 9 Data

The data provided is from SEEG’s Collection 9: - **Time period**:
2000-2018 - **Methodology**: Latest available when data was compiled -
**Quality**: Peer-reviewed and validated - **Revisions**: May be updated
in future SEEG collections as better data becomes available

### Emissions Units

- **Standard unit**: Gigatonnes CO₂ equivalent (Gt CO₂e)
- **CO₂e equivalence**: Uses global warming potentials (GWP) to convert
  CH₄ and N₂O to CO₂ equivalent
- **Consistency**: Allows comparison across different gases and sectors

### Download Considerations

**Important**: The complete SEEG dataset is quite large. When
downloading: - Entire datasets are downloaded as single files; year
selection is limited - A stable, high-speed internet connection is
recommended - Downloads may take time depending on connection speed -
Ensure sufficient disk space for storage

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which emission sector(s) to download.

``` r
dataset = "seeg"              # All sectors (raw_data = TRUE only)
dataset = "seeg_farming"      # Agriculture and livestock
dataset = "seeg_energy"       # Energy sector
dataset = "seeg_land"         # Land use changes
dataset = "seeg_industry"     # Industrial processes
dataset = "seeg_residuals"    # Waste and residuals
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw SEEG data format (more detailed)
- `FALSE`: Returns treated data with English variable names and
  standardized format

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies geographic aggregation level.

- `"country"`: National total
- `"state"`: State-level emissions (27 units)
- `"municipality"`: All 5,570+ municipalities

``` r
geo_level = "state"  # character string
```

### 4. **language**

Output language for variable names and labels.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

**Note on timing**: Downloads may take considerable time due to file
size.

------------------------------------------------------------------------

## Examples

### Example 1: All sectors combined (raw data) at the country level

``` r
# download raw SEEG data (all sectors) at the country level
# note: dataset = "seeg" only works with raw_data = TRUE
all_emissions <- load_seeg(
  dataset = "seeg",
  raw_data = TRUE,
  geo_level = "country",
  language = "eng"
)
```

### Example 2: Agricultural emissions by state

``` r
# download treated agricultural emissions at the state level
farming <- load_seeg(
  dataset = "seeg_farming",
  raw_data = FALSE,
  geo_level = "state",
  language = "eng"
)
```

### Example 3: Land use change emissions by state

``` r
# download treated land use change emissions at the state level
land_use <- load_seeg(
  dataset = "seeg_land",
  raw_data = FALSE,
  geo_level = "state",
  language = "eng"
)
```

### Example 4: Energy emissions by municipality

``` r
# download treated energy emissions at the municipality level
energy <- load_seeg(
  dataset = "seeg_energy",
  raw_data = FALSE,
  geo_level = "municipality",
  language = "eng"
)
```

### Example 5: Industrial process emissions by state

``` r
# download treated industrial process emissions at the state level
industry <- load_seeg(
  dataset = "seeg_industry",
  raw_data = FALSE,
  geo_level = "state",
  language = "eng"
)
```

### Example 6: Waste emissions by state

``` r
# download treated waste emissions at the state level
residuals <- load_seeg(
  dataset = "seeg_residuals",
  raw_data = FALSE,
  geo_level = "state",
  language = "eng"
)
```

## Data Notes

### Emission Sources Included

SEEG includes all major anthropogenic emission sources: - Agriculture
(livestock, crops, soil) - Energy (electricity, transport, heating) -
Land use change (deforestation, afforestation) - Industrial processes
(cement, chemicals, metals) - Waste (landfills, wastewater)

### Methodology

Estimates follow: - IPCC guidelines for national greenhouse gas
inventories - Brazilian national inventory standards - International
best practices - Transparent, documented assumptions

### Data Quality

- Peer-reviewed methodology
- Validated against government data
- Uncertainty ranges available in detailed products
- Regular methodology updates

### Limitations

1.  **Fixed time period**: Collection 9 covers 2000-2018 only
2.  **File size**: Large downloads; requires good internet
3.  **Year aggregation**: Cannot select individual years; entire dataset
    downloaded
4.  **Revisions**: Methodology may change in future SEEG releases
5.  **Sub-national uncertainty**: Municipal and state estimates have
    higher uncertainty than national

------------------------------------------------------------------------

🔴 This function uses the `googledrive` package to download data at the
municipality level. In case of authentication errors, see
[googledrive](#googledrive).

## CENSOAGRO

## Overview

The Census of Agriculture (Censo Agropecuário) is Brazil’s comprehensive
survey of agricultural establishments and activities, conducted by IBGE
(Instituto Brasileiro de Geografia e Estatística). This census collects
detailed information about:

- **Agricultural establishments**: characteristics, size, and management
- **Agricultural producers**: demographics, education, and land
  ownership conditions
- **Production activities**: crops, livestock, and agroindustry
  operations
- **Rural employment and labor**: workforce characteristics and wages
- **Agricultural inputs**: machinery, equipment, and technology adoption

The census provides critical data for agricultural policy, market
research, and understanding the structure of Brazilian agriculture
across regional and temporal dimensions.

### Data Coverage

Data is collected at multiple geographic levels: - **Country level**:
aggregate national statistics - **State level**: disaggregated by
Brazilian states - **Municipality level**: available for select datasets
(currently `"livestock_production"`)

Historical data spans from 1920 onwards, with different time series
available for different datasets based on IBGE’s survey methodology
evolution.

------------------------------------------------------------------------

## Available Datasets

### 1. **agricultural_land_area**

Provides comprehensive data on total agricultural land area and the
number of agricultural properties.

- **Key metrics**: Total land area (hectares), number of properties
- **Time period**: 1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995,
  2006, 2017
- **Geographic levels**: Country, State
- **Use case**: Track long-term trends in farm consolidation and total
  agricultural land expansion

### 2. **agricultural_area_use**

Details how agricultural properties use their land (crop farming,
pasture, forests, etc.).

- **Key metrics**: Area by use category (temporary crops, permanent
  crops, natural pastures, planted pastures, forest for forest
  production, protected natural vegetation, other areas)
- **Time period**: 1970 onwards (1970, 1975, 1980, 1985, 1995, 2006,
  2017)
- **Geographic levels**: Country, State
- **Use case**: Analyze land use transitions, deforestation patterns,
  and agricultural intensification

### 3. **agricultural_employees_tractors**

Captures information about the agricultural workforce and mechanization
levels.

- **Key metrics**: Number of employees, number of tractors, employed
  persons
- **Time period**: 1970 onwards (1970, 1975, 1980, 1985, 1995, 2006,
  2017)
- **Geographic levels**: Country, State
- **Use case**: Study agricultural mechanization trends and rural
  employment dynamics

### 4. **agricultural_producer_condition**

Describes the tenure status of agricultural land (ownership, rental,
partnership, etc.).

- **Key metrics**: Number of properties by producer condition (owner,
  tenant, partner, occupant)
- **Time period**: 1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995,
  2006, 2017
- **Geographic levels**: Country, State
- **Use case**: Understand land tenure structures and changes in
  property ownership patterns

### 5. **animal_production**

Details the number of livestock animals farmed by species and type.

- **Key metrics**: Number of animals by species (cattle, pigs, poultry,
  sheep, horses, goats, water buffalo, etc.), number of establishments
- **Time period**: 1970 onwards (1970, 1975, 1980, 1985, 1995, 2006,
  2017)
- **Geographic levels**: Country, State
- **Use case**: Monitor livestock herd sizes and sectoral changes in
  animal agriculture

### 6. **animal_products**

Quantifies production volumes of animal-based products.

- **Key metrics**: Production quantities (eggs, milk, honey, wool, hide,
  etc.)
- **Time period**: 1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995,
  2006, 2017
- **Geographic levels**: Country, State
- **Use case**: Track historical trends in dairy, poultry, and other
  animal product sectors

### 7. **vegetable_production_area**

Provides detailed crop production data including area planted and volume
produced.

- **Key metrics**: Area planted (hectares), quantity produced
  (kilograms), number of establishments by crop type
- **Time period**: 1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995,
  2006, 2017
- **Geographic levels**: Country, State
- **Use case**: Comprehensive analysis of crop production patterns and
  agricultural productivity

### 8. **vegetable_production_temporary**

Focuses specifically on temporary crops (annual crops that must be
replanted each season).

- **Key metrics**: Area planted, quantity produced for crops like
  soybeans, corn, beans, cassava
- **Time period**: 1970 onwards (1970, 1975, 1980, 1985, 1995, 2006,
  2017)
- **Geographic levels**: Country, State
- **Use case**: Study annual crop production cycles and seasonal
  variations

### 9. **vegetable_production_permanent**

Focuses on permanent crops (perennial crops that produce for multiple
years).

- **Key metrics**: Area planted, quantity produced for crops like
  coffee, sugarcane, cocoa, oranges
- **Time period**: 1940 onwards (1940, 1950, 1960, 1970, 1975, 1980,
  1985, 1995, 2006, 2017)
- **Geographic levels**: Country, State
- **Use case**: Analyze long-cycle crop production and regional
  specialization

### 10. **livestock_production**

Specialized dataset on bovine cattle production and related
establishments.

- **Key metrics**: Number of cattle establishments, herd size, number of
  properties
- **Time period**: 2017 (most recent census year)
- **Geographic levels**: Country, State, **Municipality** (unique to
  this dataset)
- **Use case**: Detailed regional analysis of cattle ranching, including
  municipality-level data

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which dataset to download. See dataset descriptions above.

``` r
dataset = "agricultural_land_area"  # character string
```

### 2. **raw_data**

Controls whether to download the original data or the processed/cleaned
version.

- `TRUE`: Returns raw data exactly as published by IBGE
- `FALSE`: Returns treated data with standardized formatting, variable
  names in English, and consistent units

**Default behavior**: Raw data typically requires more cleaning and
interpretation, while treated data is ready for immediate analysis.

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies the geographic aggregation level.

- `"country"`: National aggregate
- `"state"`: Disaggregated by Brazilian state
- `"municipality"`: Available only for `"livestock_production"` dataset

``` r
geo_level = "state"  # character string
```

### 4. **time_period**

Defines which year(s) to download. Availability varies by dataset:

| Dataset | Available Years |
|----|----|
| `agricultural_land_area` | `1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `agricultural_area_use` | `1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `agricultural_employees_tractors` | `1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `agricultural_producer_condition` | `1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `animal_production` | `1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `animal_products` | `1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `vegetable_production_area` | `1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `vegetable_production_temporary` | `1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `vegetable_production_permanent` | `1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006, 2017` |
| `livestock_production` | `2017` |

You can request a single year or a range of years:

``` r
time_period = 2006           # single year
time_period = c(1995, 2006)  # multiple specific years
time_period = 1995:2006      # will select years within this range that are available
```

### 5. **language**

Output language for variable names and labels.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

``` r
# download treated land area data at the country level in 2017
data <- load_censoagro(
  dataset = "agricultural_land_area",
  raw_data = FALSE,
  geo_level = "country",
  time_period = 2017,
  language = "eng"
)

# download treated temporary crop data by state in 1995 in portuguese
data <- load_censoagro(
  dataset = "vegetable_production_temporary",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 1995,
  language = "pt"
)

# download municipality-level cattle data (only available for livestock_production)
data <- load_censoagro(
  dataset = "livestock_production",
  raw_data = FALSE,
  geo_level = "municipality",
  time_period = 2017,
  language = "eng"
)
```

## Data Notes

### Raw vs. Treated Data

- **Raw data** (`raw_data = TRUE`): Exactly as published by IBGE, with
  original formatting and Portuguese variable names
- **Treated data** (`raw_data = FALSE`): Cleaned and standardized with
  English variable names, consistent units (hectares for area, kilograms
  for production quantities), and NA values properly handled

### Data Organization

When using treated data, the output is typically in long format with one
row per observation unit, containing: - Geographic identifiers (state,
municipality if applicable) - Year of the census - Product/category
names (crop type, animal species, etc.) - Quantitative measurements
(area, quantity, count) - Number of establishments/properties

### Important Considerations

1.  **Time gaps**: Census data is not collected every year. Years with
    no data simply won’t be available.
2.  **Geographic changes**: Brazil’s state boundaries have changed
    historically; use caution when comparing very old data
3.  **Definition changes**: IBGE’s classification of crops and
    agricultural activities has evolved. Variables may not be directly
    comparable across all decades.
4.  **Municipality data**: Currently only available for
    `livestock_production` in 2017
5.  **Download size**: Historical data requests with multiple years may
    be large; plan accordingly

### Citing the Data

When using this data in research or publications, cite:

> IBGE - Instituto Brasileiro de Geografia e Estatística. Censo
> Agropecuário. Available at:
> <https://sidra.ibge.gov.br/pesquisa/censo-agropecuario>

------------------------------------------------------------------------

# Social Data

## IPS

## Overview

The [Amazon Social Progress Index (IPS)](https://imazon.org.br/) is a
comprehensive indicator framework that measures social and environmental
progress in the Legal Amazon region. This collaborative initiative
combines:

- **Imazon** (Instituto do Homem e Meio Ambiente da Amazônia): Brazilian
  research organization
- **Social Progress Imperative**: International organization focused on
  measuring societal well-being

This dataset captures:

- **Multi-dimensional development indicators**: Spanning 8 domains of
  social and environmental progress
- **Municipality-level data**: All Legal Amazon municipalities assessed
- **Quality of life metrics**: Health, education, sanitation,
  infrastructure
- **Environmental indicators**: Forest cover, deforestation risk,
  sustainability
- **Violence and safety**: Public safety and security metrics
- **Temporal coverage**: Data from 2014, 2018, 2021, 2023
- **Geographic coverage**: 570+ municipalities across Legal Amazon

The IPS provides a holistic view of sustainable development, moving
beyond simple economic measures (GDP) to encompass environmental
sustainability and social well-being.

### Data Source and Methodology

The Social Progress Index: - Based on 50+ individual indicators across
12 domains - Uses data from government agencies, NGOs, and research
institutions - Aggregated into 3 main dimensions and 12 subdimensions -
Indexed to 0-100 scale for comparability - Methodologically rigorous
with transparent weighting

For detailed methodology, visit [Social Progress
Imperative](https://www.socialprogress.org/).

------------------------------------------------------------------------

## Available Dimensions

The IPS framework includes 8 main dataset options:

### **1. all**

Complete Social Progress Index with all dimensions and indicators.

- **Coverage**: Comprehensive assessment across all domains
- **Variables**: All indicators and index scores
- **Use cases**: Holistic development analysis, overall progress
  tracking, multi-dimensional comparisons

### **2. life_quality**

Indicators related to quality of life and well-being.

- **Variables**: Healthcare quality, life expectancy, nutrition, shelter
  quality
- **Use cases**: Health and wellness analysis, living standards
  assessment, healthcare quality evaluation

### **3. sanit_habit**

Sanitation and habitat indicators.

- **Variables**: Access to improved sanitation, water quality, housing
  conditions
- **Use cases**: Infrastructure assessment, water and sanitation access
  analysis, housing quality evaluation

### **4. violence**

Public safety and violence indicators.

- **Variables**: Crime rates, safety perceptions, homicide data
- **Use cases**: Public safety analysis, violence hotspot
  identification, security trends

### **5. educ**

Education and literacy indicators.

- **Variables**: School enrollment, literacy rates, educational
  attainment, quality of education
- **Use cases**: Education access analysis, literacy trends, human
  capital assessment

### **6. communic**

Communication and connectivity indicators.

- **Variables**: Internet access, mobile phone coverage, communication
  infrastructure
- **Use cases**: Digital divide analysis, connectivity assessment, tech
  adoption patterns

### **7. mortality**

Health and mortality indicators.

- **Variables**: Child mortality, maternal mortality, mortality rates by
  cause
- **Use cases**: Health outcomes analysis, maternal/child health
  assessment, disease burden evaluation

### **8. deforest**

Environmental and deforestation indicators.

- **Variables**: Forest cover, deforestation rates, environmental
  sustainability
- **Use cases**: Forest monitoring, environmental assessment,
  climate/conservation analysis

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which dimension(s) to download.

``` r
dataset = "all"         # All dimensions
dataset = "life_quality" # Quality of life metrics
dataset = "sanit_habit"  # Sanitation and habitat
dataset = "violence"     # Public safety and violence
dataset = "educ"         # Education indicators
dataset = "communic"     # Communication and connectivity
dataset = "mortality"    # Health and mortality
dataset = "deforest"     # Environmental and deforestation
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw data exactly as published
- `FALSE`: Returns treated data with standardized English variable names
  and formatting

``` r
raw_data = FALSE  # logical
```

### 3. **time_period**

Specifies which assessment year(s) to download.

**Available years**: 2014, 2018, 2021, 2023

``` r
time_period = 2023              # Most recent
time_period = c(2018, 2023)     # Specific years
time_period = c(2014, 2018, 2021, 2023)  # Multiple years
```

### 4. **language**

Output language for variable names and labels.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

``` r
# download raw IPS data from 2014
data <- load_ips(
  dataset = "all",
  raw_data = TRUE,
  time_period = 2014,
  language = "eng"
)

# download treated deforestation IPS data from 2018 in portuguese
data <- load_ips(
  dataset = "deforest",
  raw_data = FALSE,
  time_period = 2018,
  language = "pt"
)
```

## Data Notes

### Index Scales

- **0-100 scale**: All indices standardized to 0-100 for comparison
- **Higher is better**: Across all dimensions except deforestation
  (where higher forest index = better)
- **Comparable across dimensions**: Standardized scale allows
  cross-dimension comparison

### Dimensions and Indicators

Each dimension contains multiple indicators: - **Life quality**: 4-6
indicators - **Sanitation/habitat**: 3-5 indicators  
- **Violence**: 3-4 indicators - **Education**: 3-4 indicators -
**Communication**: 2-3 indicators - **Mortality**: 3-4 indicators -
**Deforestation**: 2-3 indicators

(Exact number varies by year and methodology)

### Temporal Comparisons

When comparing across years (2014, 2018, 2021, 2023): - Methodology may
have evolved between assessments - New indicators may have been added -
Some municipalities may not have data in all years - Use caution
comparing very old (2014) with recent (2023) data

### Missing Data

- Some municipalities may lack data for specific indicators
- Remote or less accessible areas may have less complete data
- Use `na.rm = TRUE` in aggregations to handle missing values

### Geographic Coverage

- Covers 570+ municipalities in the Legal Amazon
- Includes all states with Amazon territory
- Some frontier/protected areas may lack complete data

------------------------------------------------------------------------

## IEMA

## Overview

Data from the Institute of Environment and Water Resources (Instituto de
Energia e Meio Ambiente - IEMA), documenting electric energy access
across the Amazon region. This dataset provides comprehensive
information on populations without access to electric energy throughout
the Legal Amazon in 2018, offering critical insights into energy poverty
and infrastructure gaps in the region.

The IEMA dataset is particularly valuable for understanding energy
access disparities in the Amazon, including remote communities,
indigenous territories, and areas with limited infrastructure
development.

## Data Coverage

The IEMA dataset includes:

- **Time Period**: Cross-sectional data from 2018
- **Geographic Coverage**: Legal Amazon region (9 states and partial
  coverage of others)
- **Unit of Analysis**: Municipalities and/or census areas within the
  Amazon
- **Population Focus**: Population without access to electric energy
  (energy-poor populations)
- **Variables**: Energy access indicators, population counts, and
  geographic identifiers

## Dataset Description

### Key Variables

1.  **Population Without Energy Access**: Number of individuals lacking
    access to electric energy
2.  **Geographic Identifiers**: Municipality, state, and biome
    information
3.  **Settlement Type**: Rural vs. urban classification where available
4.  **Indigenous Territory Data**: Energy access in indigenous lands
    (where applicable)
5.  **Infrastructure Indicators**: Distance to grid, cost of connection,
    barriers to access

### Geographic Coverage

The Legal Amazon encompasses: - **9 Full States**: Amazonas, Roraima,
Acre, Amazonas, Rondônia, Mato Grosso, Amapá, Pará, Maranhão - **Partial
Coverage**: Parts of Maranhão and other states - **Total Area**:
Approximately 5.5 million square kilometers - **Population Focus**:
Amazon-dwelling populations with particular attention to vulnerable
groups

### Energy Access Dimensions

The dataset addresses multiple aspects of energy poverty: - **Access
Type**: Grid connection vs. off-grid solutions - **Reliability**:
Service quality and availability - **Affordability**: Connection costs
and monthly tariffs - **Geographic Barriers**: Remote location and
access challenges - **Population Characteristics**: Indigenous
communities, rural settlements, urban poor

------------------------------------------------------------------------

## Function Parameters

**Options:**

1.  **dataset**: `"iema"`

2.  **raw_data**:

    - `TRUE`: Data in original format from IEMA
    - `FALSE`: Cleaned and standardized version

3.  **language**:

    - `"pt"`: Portuguese language
    - `"eng"`: English language

------------------------------------------------------------------------

## Data Limitations

Since this dataset captures a single year (2018), it represents a
snapshot rather than a time series. The cross-sectional nature means: -
**No Trend Analysis**: Cannot track changes over time without merging
with other sources - **Temporal Stability**: Conditions may have changed
since 2018 - **Recent Updates**: Users may need to contact IEMA for more
recent data

------------------------------------------------------------------------

## Examples

``` r
# download treated IEMA energy access data
data <- load_iema(
  raw_data = FALSE,
  language = "eng"
)
```

🔴 This function uses the `googledrive` package to download data. In
case of authentication errors, see [googledrive](#googledrive).

## Population

## Overview

Population dataset provides Brazil’s official population statistics from
[IBGE](https://www.ibge.gov.br/) (Brazilian Institute of Geography and
Statistics). This dataset includes:

- **Population estimates**: Official projections and estimates for
  non-census years
- **Census data**: Actual population counts from census years (2007,
  2010)
- **Time coverage**: Data from 2001 to 2021 (with some gaps)
- **Geographic detail**: Available at country, state, and municipality
  levels
- **Regular updates**: Annual estimates published by IBGE
- **Demographic foundation**: Base data for per capita calculations and
  demographic analysis

Population data is fundamental to many analyses, providing denominators
for per capita metrics, understanding urbanization patterns, tracking
demographic trends, and supporting policy and development planning.

### Data Source and Methodology

Population data comes from: - **Census years (2007, 2010)**: Actual
population enumeration - **Estimation years (2001-2006, 2008-2009,
2011-2021)**: IBGE official population projections based on census data
and vital statistics - **Methodology**: Cohort-component methodology
considering births, deaths, migration - **Updates**: Revised annually as
new vital statistics data becomes available

For more information, visit [IBGE Population
Estimates](https://www.ibge.gov.br/en/statistics/social/population/).

------------------------------------------------------------------------

## Available Dataset

### **population**

Official Brazilian population statistics and estimates.

- **Coverage**: All Brazilian municipalities, states, and national level
- **Time period**: 2001 to 2021
- **Data types**: Census counts (2007, 2010) and official estimates
  (other years)
- **Geographic levels**: Country, State, Municipality
- **Use cases**:
  - Calculate per capita indicators for all metrics
  - Track demographic changes and urbanization
  - Assess population concentration
  - Population-weighted aggregations
  - Demographic planning and projections
  - Support for epidemiological and social statistics

------------------------------------------------------------------------

## Important Data Characteristics

### Census Years vs. Estimate Years

| Year Type | Years                           | Nature                    |
|-----------|---------------------------------|---------------------------|
| Census    | 2007, 2010                      | Actual population count   |
| Estimates | 2001-2006, 2008-2009, 2011-2021 | Official IBGE projections |

Census years provide actual counts; other years are official
projections.

### Population Definition

- **Resident population**: People living in a given area as of the
  reference date
- **Includes**: Brazilian citizens and legal residents
- **Reference date**: July 1 for most estimate years, September 1 for
  2010 census

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Only one dataset is available:

``` r
dataset = "population"  # Population statistics
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw IBGE data format
- `FALSE`: Returns treated data with English variable names and
  standardized format

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies geographic aggregation level.

- `"country"`: National total
- `"state"`: State-level population (27 units)
- `"municipality"`: All 5,570+ municipalities

``` r
geo_level = "municipality"  # character string
```

### 4. **time_period**

Specifies which year(s) to download.

**Available years**: 2001-2006, 2007 (census), 2008-2009, 2010 (census),
2011-2021

``` r
time_period = 2020              # single year
time_period = c(2010, 2020)     # specific years (includes 2010 census)
time_period = 2010:2020         # range of years
```

### 5. **language**

Output language for variable names.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

### Example 1: Population by state for a single year

``` r
# download treated population data at the state level for 2021
pop_2021 <- load_population(
  dataset = "population",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2021,
  language = "eng"
)
```

### Example 2: Population by state over time

``` r
# download treated population data at the state level for 2010 to 2021
pop_series <- load_population(
  dataset = "population",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2010:2021,
  language = "eng"
)
```

### Example 3: Population by municipality

``` r
# download treated population data at the municipality level for 2020
pop_munic <- load_population(
  dataset = "population",
  raw_data = FALSE,
  geo_level = "municipality",
  time_period = 2020,
  language = "eng"
)
```

## Data Notes

### Data Structure

Each record contains: - Geographic identifier (state or municipality
name/code) - Year - Population count

Simple and straightforward structure, useful as base for other
calculations.

### Census vs. Estimate Quality

- **Census years (2007, 2010)**: High quality, actual counts
- **Estimate years**: Official IBGE projections based on demographic
  models
- **Estimates are less certain**: Especially for small geographic areas

### Municipal Variations

- **Not all municipalities equally reliable**: Very small municipalities
  may have estimation uncertainty
- **Boundary changes**: Municipalities occasionally merged or divided;
  affects time comparisons
- **Code consistency**: Use IBGE municipal codes (rather than names) for
  accurate merging with other datasets

### Important Limitations

1.  **Estimate years**: Non-census years are projections, not actual
    counts
2.  **Lag in release**: May take time for latest estimates to be
    published
3.  **Revisions**: Estimates revised annually as better vital statistics
    data becomes available
4.  **Boundary changes**: Municipal boundaries changed; affects
    historical comparisons
5.  **Definition**: Includes registered residents; may not match actual
    lived population

------------------------------------------------------------------------

# Economic Data

## COMEX

## Overview

COMEX (Comércio Exterior - Foreign Trade) dataset provides Brazil’s
official international trade statistics extracted from
[Siscomex](https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/),
the Integrated System of Foreign Trade maintained by the Brazilian
government.

This dataset captures:

- **Export data**: Brazilian goods leaving the country, disaggregated by
  municipality and product
- **Import data**: Foreign goods entering Brazil, disaggregated by
  municipality and product  
- **Monthly frequency**: High-frequency trade data for detailed temporal
  analysis
- **Product classification**: Detailed product codes and descriptions
- **Geographic coverage**: Trade flows identified by Brazilian
  municipality
- **Long historical coverage**: Available from 1989 onwards

COMEX is the primary official source for Brazil’s international trade
statistics, widely used for trade policy analysis, business
intelligence, academic research, and economic monitoring.

### Data Source and Coverage

COMEX data comes from: - Official records from Siscomex (Brazil’s
foreign trade system) - Mandatory declarations by exporters and
importers - Updated monthly with current month data - Historical data
from 1989 onwards

**Important note on nomenclature**: From 1989 to 1996, Brazil used a
different system of product nomenclature (NBLC - Nomenclatura Brasileira
de Mercadorias). All conversions to the current nomenclature system are
available and the package handles this transparently.

For more information, visit the [Brazilian Ministry of Productivity,
Employment and Foreign
Trade](https://www.gov.br/produtividade-e-comercio-exterior/).

------------------------------------------------------------------------

## Available Datasets

### **1. export_mun (Exports by Municipality)**

Export data disaggregated at the municipality level.

- **Coverage**: All Brazilian municipalities engaged in international
  trade
- **Frequency**: Monthly
- **Time period**: 1989 onwards
- **Key variables**: Export value (USD), quantity, product code,
  municipality, date
- **Use cases**:
  - Identify which municipalities are export hubs
  - Analyze export diversification by region
  - Track geographic shifts in export capacity
  - Municipal-level trade policy impact assessment

### **2. import_mun (Imports by Municipality)**

Import data disaggregated at the municipality level.

- **Coverage**: All Brazilian municipalities receiving imports
- **Frequency**: Monthly
- **Time period**: 1989 onwards
- **Key variables**: Import value (USD), quantity, product code,
  municipality, date
- **Use cases**:
  - Understand which regions import specific products
  - Analyze import dependency patterns
  - Track geographic consumption patterns
  - Regional supply chain analysis

### **3. export_prod (Exports by Producer)**

Export data organized by producer/exporter and product.

- **Coverage**: All registered exporters in Brazil
- **Frequency**: Monthly
- **Time period**: 1989 onwards
- **Key variables**: Export value (USD), quantity, product code,
  exporter code, date
- **Use cases**:
  - Firm-level export analysis
  - Identify major exporters and their product mix
  - Export concentration analysis
  - Exporter persistence and dynamics

### **4. import_prod (Imports by Producer)**

Import data organized by importer/distributor and product.

- **Coverage**: All registered importers in Brazil
- **Frequency**: Monthly
- **Time period**: 1989 onwards
- **Key variables**: Import value (USD), quantity, product code,
  importer code, date
- **Use cases**:
  - Firm-level import behavior
  - Supply chain relationships
  - Importer concentration analysis
  - International sourcing patterns

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which trade dataset to download.

``` r
dataset = "export_mun"   # exports by municipality
dataset = "import_mun"   # imports by municipality
dataset = "export_prod"  # exports by producer/exporter
dataset = "import_prod"  # imports by producer/importer
```

### 2. **raw_data**

Controls whether to download the original data or the processed/cleaned
version.

- `TRUE`: Returns raw data exactly as published by Siscomex
- `FALSE`: Returns treated data with standardized formatting, English
  variable names, and cleaned values

``` r
raw_data = FALSE  # logical
```

### 3. **time_period**

Specifies which year(s) to download. Available from 1989 onwards.

``` r
time_period = 2020              # single year
time_period = c(2018, 2020)     # specific years
time_period = 2015:2020         # range of years
```

**Note**: Monthly data means each year can be quite large. Consider
downloading specific years or ranges to manage file size.

### 4. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

``` r
# download treated exports data by municipality from 2020 to 2021
data <- load_br_trade(
  dataset = "export_mun",
  raw_data = FALSE,
  time_period = 2020:2021,
  language = "eng"
)

# download treated imports data by municipality from 2020 to 2021
data <- load_br_trade(
  dataset = "import_mun",
  raw_data = FALSE,
  time_period = 2020:2021,
  language = "eng"
)
```

## Data Notes

### Raw vs. Treated Data

- **Raw data** (`raw_data = TRUE`): Original Siscomex format,
  potentially with inconsistencies and naming conventions from different
  time periods
- **Treated data** (`raw_data = FALSE`): Standardized with English
  variable names, consistent units (USD for values), and cleaned
  formatting

### Product Classification

- **1989-1996**: Uses NBLC (Nomenclatura Brasileira de Mercadorias) -
  conversions are handled transparently
- **1997 onwards**: Uses HS (Harmonized System) classification aligned
  with international standards
- Product codes enable comparison with international trade databases

### Data Characteristics

1.  **Monthly frequency**: Data is reported monthly; aggregation to
    annual or quarterly is straightforward
2.  **Producer vs. Municipality**:
    - Municipality data groups trade by geographic origin/destination
    - Producer data groups by firm/exporter-importer code
    - Use municipality for regional analysis, producer for firm analysis
3.  **Missing data**: Some small trade flows may not be reported
4.  **Currency**: All values in USD

### Nomenclature Conversion

When using data spanning 1989-1996 to 1997 onwards, be aware: - Product
categories may differ between nomenclature systems - Conversions are
available but not always 1:1 - Compare very old with recent data with
caution

## BACI

## Overview

BACI (Base pour l’Analyse du Commerce International) is a comprehensive
database of bilateral trade flows developed by
[CEPII](https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
(Centre d’Études Prospectives et d’Informations Internationales), a
leading French research center on international trade.

This dataset provides:

- **Bilateral trade data**: Export and import flows between country
  pairs
- **Product-level detail**: Over 5,000 distinct products classified by
  the Harmonized System (HS)
- **Global coverage**: More than 200 countries and territories
- **Historical depth**: Data spanning multiple decades of international
  commerce
- **Quality assurance**: Reconciliation of mirror flows (exports
  reported by one country matched with imports reported by trading
  partner)

The BACI dataset is widely used in academic research, policy analysis,
and international trade studies due to its comprehensive coverage and
quality control procedures.

### Data Source and Methodology

BACI is constructed from UN Comtrade data with significant processing: -
Reconciliation of discrepancies between reported imports and exports -
Imputation of missing values using econometric techniques -
Classification using the Harmonized System (HS) nomenclature - All
values converted to USD for comparability

For detailed methodological information, visit the [CEPII BACI
documentation](https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37).

------------------------------------------------------------------------

## Available Dataset

### **HS92 (Harmonized System 1992)**

The HS92 classification provides trade data organized under the 1992
version of the Harmonized System nomenclature.

- **Coverage**: Bilateral trade flows for 5,000+ products (HS 6-digit
  level)
- **Countries**: 200+ countries and territories
- **Time period**: 1995 onwards (varies by country; most comprehensive
  from 2000 onwards)
- **Variables**: Trade value (USD), quantity (where available),
  exporter, importer, product code
- **Use cases**:
  - Analyzing trade specialization and comparative advantage
  - Studying trade relationships between countries
  - Product-level trade analysis
  - Research on globalization and supply chains

------------------------------------------------------------------------

## Important Information About Downloads

### Download Size and Time

**Important**: The BACI dataset is very large. All data is packaged in a
single compressed file on the CEPII server. **Even if you only need data
for specific years, the entire dataset must be downloaded first, then
processed locally.**

- **Typical download size**: 500+ MB (compressed)
- **Uncompressed size**: Several GB
- **Download time**: Can take 30-60+ minutes depending on internet
  connection
- **Processing time**: Additional time required after download for
  extraction and filtering

**Recommendation**: Plan your download during off-peak hours or use a
stable, high-speed connection.

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Currently only one dataset is available:

``` r
dataset = "HS92"  # Harmonized System 1992 classification
```

### 2. **raw_data**

Controls whether to download the original data or the processed/cleaned
version.

- `TRUE`: Returns raw CEPII data with original formatting and column
  names
- `FALSE`: Returns treated data with standardized English variable names
  and cleaned formatting

``` r
raw_data = FALSE  # logical
```

### 3. **time_period**

Specifies which year(s) to download. You can request single or multiple
years.

- **Available**: Generally 1995 onwards, though coverage varies by
  country
- **Most complete**: 2000 onwards for the majority of countries
- **Note**: Specifying years helps with filtering but still requires
  downloading the full dataset

``` r
time_period = 2016              # single year
time_period = c(2010, 2015)     # multiple specific years
time_period = 2010:2020         # range (will select available years)
```

### 4. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

------------------------------------------------------------------------

## Examples

``` r
# download treated trade data for 2016 (HS92 classification)
# Warning: large download, may take a long time
trade_2016 <- load_baci(
  dataset = "HS92",
  raw_data = FALSE,
  time_period = 2016,
  language = "eng"
)
```

## Data Notes

### Raw vs. Treated Data

- **Raw data** (`raw_data = TRUE`): CEPII’s original format with
  variable codes and potentially non-standard naming
- **Treated data** (`raw_data = FALSE`): Standardized with English
  variable names, consistent column formatting, and ready for immediate
  analysis

### Data Structure

Each row typically represents a trade flow with: - **Exporter**: Country
code (ISO 3-letter code) - **Importer**: Country code (ISO 3-letter
code) - **Year**: Calendar year of the trade flow - **Product code**: HS
6-digit classification - **Product name**: Description of the product -
**Value**: Trade value in USD - **Quantity**: Physical quantity (where
available)

### Country and Product Coverage

- Most countries covered from 2000 onwards
- Coverage varies by country; some countries have earlier data available
- HS 6-digit codes ensure consistency with international standards
- Product classifications may change over time as the HS is updated

### Quality Notes

- BACI reconciles mirror flows to improve data quality
- Missing values may occur for certain country-product-year combinations
- Some countries report data more frequently/accurately than others
- Small trade flows may have been imputed or estimated

------------------------------------------------------------------------

## PIB-Munic

## Overview

PIBMUNIC (Produto Interno Bruto por Município - Gross Domestic Product
by Municipality) is Brazil’s official municipal-level GDP data produced
by [IBGE](https://www.ibge.gov.br/). This dataset provides:

- **Gross Domestic Product (GDP)**: Total economic output at current
  market prices
- **Value Added**: Gross value added by economic activity
- **Taxes and Subsidies**: Net taxes on products
- **Sectoral breakdown**: GDP disaggregated by economic sectors
  (agriculture, industry, services)
- **Municipal coverage**: All Brazilian municipalities
- **Long time series**: Historical data spanning multiple decades
- **Multi-level aggregation**: Available at country, state, and
  municipality levels

PIBMUNIC is essential for understanding Brazil’s regional economic
structure, identifying economic disparities, analyzing sectoral
specialization, and assessing economic development across
municipalities.

### Data Source and Methodology

PIBMUNIC data is compiled by IBGE using: - Data from CEMPRE (firm
registry) for employment and output - Production and consumption
surveys - Tax and financial records - Trade and services data - National
accounts framework aligned with international standards

For more information, visit [IBGE National
Accounts](https://www.ibge.gov.br/en/statistics/economic/national-accounts/).

------------------------------------------------------------------------

## Available Dataset

### **pibmunic**

Complete municipal GDP statistics with sectoral detail.

- **Coverage**: All 5,570+ Brazilian municipalities
- **Variables**: GDP at current prices, value added by sector, taxes,
  subsidies
- **Sectors included**: Agriculture, industry, services, public
  administration
- **Measurement**: Brazilian Real (R\$) at current prices
- **Time period**: Varies by year; typically 2002 onwards
- **Use cases**:
  - Identify richest and poorest municipalities
  - Analyze regional economic disparities
  - Assess sectoral specialization (agriculture vs. industry
    vs. services)
  - Economic growth analysis by municipality
  - Development planning and policy evaluation
  - Correlate with social/environmental indicators

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Only one dataset is available:

``` r
dataset = "pibmunic"  # Municipal GDP data
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw IBGE format
- `FALSE`: Returns treated data with English variable names and
  standardized formatting

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies geographic aggregation level.

- `"country"`: National aggregate
- `"state"`: State-level aggregation
- `"municipality"`: Most detailed level with all municipalities

``` r
geo_level = "municipality"  # character string
```

### 4. **time_period**

Specifies which year(s) to download.

``` r
time_period = 2020              # single year
time_period = c(2015, 2020)     # specific years
time_period = 2015:2020         # range of years
```

### 5. **language**

Output language for variable names.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

### Example 1: Municipal GDP for a single year

``` r
# download treated municipal GDP data for 2020
pib_munic <- load_pibmunic(
  dataset = "pibmunic",
  raw_data = FALSE,
  geo_level = "municipality",
  time_period = 2020,
  language = "eng"
)
```

### Example 2: State-level GDP over time

``` r
# download treated state-level GDP data for 2015 to 2020
pib_state <- load_pibmunic(
  dataset = "pibmunic",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2015:2020,
  language = "eng"
)
```

### Example 3: Country-level GDP in Portuguese

``` r
# download treated country-level GDP data for 2010 to 2020 in Portuguese
pib_br <- load_pibmunic(
  dataset = "pibmunic",
  raw_data = FALSE,
  geo_level = "country",
  time_period = 2010:2020,
  language = "pt"
)
```

## Data Notes

### Variables Structure

Typical variables include: - **gdp**: Gross Domestic Product at current
prices (R\$) - **value_added_agriculture**: Farming and forestry
sector - **value_added_industry**: Manufacturing and construction -
**value_added_services**: Commerce, finance, transport, etc. -
**value_added_public_admin**: Government services - **net_taxes**: Taxes
minus subsidies - Additional detail depends on specific IBGE release

### Current Prices

- All values are in “current prices” (nominal values, not deflated)
- For real (inflation-adjusted) comparisons, you need to apply price
  deflators
- Comparisons across years should account for inflation

### Data Coverage

- **All municipalities**: Includes all 5,570+ Brazilian municipalities
- **Time lag**: Data typically released 2+ years after reference year
- **Revisions**: IBGE periodically revises historical data

### Important Limitations

1.  **Current prices**: Values not adjusted for inflation
2.  **Time lag**: Recent years may not be available
3.  **Confidentiality**: Some small municipalities may have aggregated
    data
4.  **Methodological changes**: IBGE occasionally updates national
    accounts methodology
5.  **Municipal boundaries**: Changed in 2021; affects historical
    comparisons

------------------------------------------------------------------------

## CEMPRE

## Overview

Employment, salary and firm data from IBGE’s [Cadastro Central de
Empresas (CEMPRE)](https://sidra.ibge.gov.br/pesquisa/cempre/tabelas).
This comprehensive dataset provides information on companies and other
organizations registered with Brazil’s tax authority (Receita Federal),
including employment levels, wage information, and business
establishment data across Brazilian municipalities and sectors.

The CEMPRE dataset is one of the most detailed sources of firm-level
data in Brazil, covering virtually all formal enterprises and
organizations operating in the country.

## Data Coverage

The CEMPRE dataset includes:

- **Time Period**: Annual data from 1985 to the most recent available
  year
- **Geographic Coverage**: All Brazilian states, regions, and
  municipalities
- **Sectoral Detail**: Economic activities classified by CNAE
  (Classificação Nacional de Atividades Econômicas)
- **Variables**: Number of establishments, total employment, average
  salary, payroll totals, and other firm characteristics

## Dataset Description

### Key Variables

1.  **Establishment Count**: Number of registered establishments
2.  **Employment**: Total number of employees and average employees per
    establishment
3.  **Wages**: Average salary, total payroll, and wage bill information
4.  **Economic Classification**: CNAE codes for sector identification
5.  **Location**: State, region, and municipality identifiers

### Geographic Aggregation Levels

The data is available at three different aggregation levels: - **Country
Level**: Aggregate statistics for all of Brazil - **State Level**: Data
aggregated by state (27 units) - **Municipality Level**: Data
disaggregated to municipality level (5,570+ municipalities)

### Sectoral Detail

Data can be retrieved with sector disaggregation or aggregate form: -
**Sectoral Disaggregation**: Detailed breakdown by CNAE 2.0 (main
divisions and subdivisions) - **Aggregate**: Total across all sectors

------------------------------------------------------------------------

## Function Parameters

**Options:**

1.  **dataset**: `"cempre"`

2.  **raw_data**:

    - `TRUE`: Returns the data in its original format from IBGE
    - `FALSE`: Returns cleaned and standardized data

3.  **geo_level**:

    - `"country"`: National aggregate
    - `"state"`: Aggregated by state
    - `"municipality"`: Disaggregated to municipality level (detailed
      results)

4.  **time_period**: Specifies the years for which data will be
    downloaded (e.g., `2010:2020` for 2010 through 2020)

5.  **language**:

    - `"pt"`: Portuguese language (variable names and labels)
    - `"eng"`: English language

6.  **sectors**:

    - `TRUE`: Data is returned separated and disaggregated by economic
      sector (CNAE)
    - `FALSE`: Data is aggregated across all sectors

------------------------------------------------------------------------

## Examples

``` r
# download raw data at the country level from 2008 to 2010
data <- load_cempre(
  raw_data = TRUE,
  geo_level = "country",
  time_period = 2008:2010,
  language = "eng"
)

# download treated state-level data split by sector in portuguese
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

## Overview

PEVS (Produção da Silvicultura e da Extração Vegetal - Silviculture and
Forestry Extraction Production) is a comprehensive annual survey
conducted by [IBGE](https://www.ibge.gov.br/) that collects data on
forestry and related activities in Brazil.

This dataset provides:

- **Native forest extraction**: Data on harvesting of native plant
  resources from natural forests
- **Planted forest production**: Information on timber and other
  products from managed forest plantations
- **Silviculture activities**: Forest management, afforestation, and
  reforestation data
- **Production volumes and values**: Both physical quantities and
  economic values
- **Harvested areas**: Land area from which extraction or harvest
  occurred
- **Forest crop areas**: Total existing areas of forest crops
  (silviculture)
- **Multi-level geographic data**: Available at country, regional,
  state, and municipality levels
- **Long historical coverage**: Annual data from 1986 onwards

PEVS is Brazil’s primary source for forestry production statistics,
important for understanding the timber industry, forest management
practices, and sustainable use of forest resources.

### Data Source and Coverage

PEVS data comes from: - Direct surveys of forestry companies and
producers - Administrative records from forestry operations - Compiled
and validated by IBGE’s agriculture statistics division - Annual release
with data for previous year - Covers both industrial and
subsistence-level forestry activities

For more information, visit [IBGE Agriculture
Statistics](https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry/).

------------------------------------------------------------------------

## Available Datasets

### **1. pevs_forest_crops**

Production data from forest crop plantations (timber and non-timber
products).

- **Coverage**: All Brazilian forest crops and related products
- **Time period**: 1986 to 2019
- **Geographic levels**: Country, Region, State, Municipality
- **Key variables**: Quantity produced, value of production, by product
  type
- **Products included**: Eucalyptus, pine, other timber species,
  charcoal, resins, turpentine, cork
- **Use cases**:
  - Analyze timber production by region and species
  - Track forest product market values
  - Identify regional forestry specialization
  - Assess economic contribution of forest crops
  - Monitor charcoal and other non-timber forest products

### **2. pevs_silviculture**

Data on silviculture activities including afforestation, reforestation,
and forest management.

- **Coverage**: All silviculture operations across Brazil
- **Time period**: 1986 to 2019
- **Geographic levels**: Country, Region, State, Municipality
- **Key variables**: Quantity and value of silviculture products, area
  under management
- **Activities tracked**: Timber production, forest management,
  conservation activities
- **Use cases**:
  - Track silviculture expansion and development
  - Analyze productivity of managed forests
  - Regional specialization in forest management
  - Economic analysis of forest-based industries
  - Environmental and conservation-oriented forestry

### **3. pevs_silviculture_area**

Total existing area used for silviculture operations, disaggregated by
forest species.

- **Coverage**: All silviculture land areas in Brazil
- **Time period**: 2013 to 2019 (limited historical coverage)
- **Geographic levels**: Country, Region, State, Municipality
- **Key variables**: Total area by species (hectares), area by type of
  forest operation
- **Species tracked**: Eucalyptus, pine, acacia, other species
- **Measurement**: Area as of December 31st of each year
- **Use cases**:
  - Monitor forest plantation expansion
  - Track species-specific area trends
  - Land use change analysis
  - Assessment of silviculture infrastructure
  - Regional forest resource assessment
  - Support for long-term forest management planning

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which forestry dataset to download.

``` r
dataset = "pevs_forest_crops"       # Forest crop production (1986-2019)
dataset = "pevs_silviculture"       # Silviculture production (1986-2019)
dataset = "pevs_silviculture_area"  # Silviculture land area (2013-2019)
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw IBGE data with original Portuguese variable names
- `FALSE`: Returns treated data with English variable names,
  standardized units, and cleaned formatting

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies the geographic aggregation level.

- `"country"`: National aggregate
- `"region"`: Brazilian geographic regions (North, Northeast,
  Center-West, Southeast, South)
- `"state"`: State-level data (27 units)
- `"municipality"`: Most granular level with 5,570+ municipalities

``` r
geo_level = "state"  # character string
```

### 4. **time_period**

Defines which year(s) to download.

**Important: Dataset-specific availability:**

| Dataset                  | Available Years |
|--------------------------|-----------------|
| `pevs_forest_crops`      | 1986-2019       |
| `pevs_silviculture`      | 1986-2019       |
| `pevs_silviculture_area` | 2013-2019       |

``` r
time_period = 2019              # single year
time_period = c(2010, 2015)     # specific years
time_period = 2010:2019         # range of years
```

### 5. **language**

Output language for variable names.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

### Example 1: Forest crop production by state

``` r
# download treated forest crops data at the state level for 2019
forest_crops <- load_pevs(
  dataset = "pevs_forest_crops",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2019,
  language = "eng"
)
```

### Example 2: Silviculture area by state over time

``` r
# download treated silviculture area data at the state level for 2013 to 2019
silvi_area <- load_pevs(
  dataset = "pevs_silviculture_area",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2013:2019,
  language = "eng"
)
```

### Example 3: Silviculture production by region

``` r
# download treated silviculture production data at the region level for 2019
silvi_prod <- load_pevs(
  dataset = "pevs_silviculture",
  raw_data = FALSE,
  geo_level = "region",
  time_period = 2019,
  language = "eng"
)
```

## Data Notes

### Data Structure

Each row typically represents: - A geographic unit (country, region,
state, or municipality) - A specific year - A product type or forest
species - Quantity produced (in appropriate units: cubic meters, tons,
etc.) - Value of production (in currency units)

### Product Categories

Forest crops include: - **Timber species**: Eucalyptus, pine, other
timber - **Non-timber products**: Charcoal, resins, turpentine, cork,
bark - **Other forest products**: Tannin, rosin, pulpwood

Exact categories vary by dataset and year.

### Raw vs. Treated Data

- **Raw data** (`raw_data = TRUE`): IBGE original format, Portuguese
  variable names
- **Treated data** (`raw_data = FALSE`): English names, standardized
  units, cleaned formatting

### Important Limitations

1.  **Data collection changes**: Survey methodology may evolve over time
2.  **Area vs. production**: Area data only available from 2013; earlier
    years have production data only
3.  **Municipality data sparse**: Many small municipalities may have
    zero or no reported data
4.  **Seasonal nature**: Some products are seasonal; annual aggregates
    smooth out variation
5.  **Informal forestry**: May undercount small-scale or informal
    forestry operations

### Units of Measurement

- **Area**: Hectares (ha)
- **Quantity**: Varies by product (cubic meters for timber, tons for
  charcoal, etc.)
- **Value**: Brazilian Real (R\$) at current prices

------------------------------------------------------------------------

## PPM

## Overview

PPM (Pesquisa da Pecuária Municipal - Municipal Livestock Survey) is
Brazil’s comprehensive annual survey of livestock activities conducted
by [IBGE](https://www.ibge.gov.br/). This dataset provides:

- **Livestock inventories**: Number of animals by species (cattle, pigs,
  poultry, sheep, horses, etc.)
- **Animal products**: Production volumes and values of milk, eggs,
  honey, wool, and other animal-origin products
- **Dairy farming**: Specialized data on milked cows, geographic
  distribution, productivity
- **Aquaculture**: Fish farming, shrimp farming, and mollusk farming
  activities
- **Sheep specialization**: Detailed shearing and wool production data
- **Multi-level geographic detail**: Country, region, state, and
  municipality levels
- **Long historical series**: Available from 1974 onwards
- **Economic value**: Both production quantities and market values

PPM is the primary data source for understanding Brazil’s livestock
sector, which is economically significant and globally important for
beef, poultry, and dairy exports.

### Data Source and Methodology

PPM data is compiled from: - Direct surveys of livestock producers -
Agricultural censuses and administrative records - Municipal agriculture
secretariats - Processed and validated by IBGE - Annual release with
data for reference year

For more information, visit [IBGE Livestock
Statistics](https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry/).

------------------------------------------------------------------------

## Available Datasets

### **1. ppm_livestock_inventory**

Total livestock herds disaggregated by animal species.

- **Coverage**: All livestock species across all Brazilian
  municipalities
- **Time period**: 1974 onwards
- **Geographic levels**: Country, Region, State, Municipality
- **Animal species**: Cattle, pigs, chickens, sheep, horses, goats,
  buffalo, others
- **Variables**: Number of animals by species, number of establishments
- **Use cases**:
  - Identify regional livestock specialization
  - Track herd size trends
  - Analyze geographic concentration of livestock
  - Understand animal agriculture structure

### **2. ppm_sheep_farming**

Specialized data on sheep production and wool/fleece harvest.

- **Coverage**: Sheep farming across Brazil
- **Time period**: 1974 onwards
- **Geographic levels**: Country, Region, State, Municipality
- **Variables**: Total sheep, sheared sheep, fleece weight, wool
  production
- **Use cases**:
  - Analyze wool production and sheep farming specialization
  - Track shearing practices and yields
  - Regional wool industry assessment

### **3. ppm_animal_origin_production**

Production of animal-based products (milk, eggs, honey, wool, etc.).

- **Coverage**: All animal product production activities
- **Time period**: 1974 onwards
- **Geographic levels**: Country, Region, State, Municipality
- **Products included**: Cow milk, goat milk, chicken eggs, quail eggs,
  honey, wool, hides, wax
- **Variables**: Quantity produced and value of production
- **Use cases**:
  - Track dairy and egg production
  - Analyze honey and other bee products
  - Economic analysis of animal product sectors

### **4. ppm_cow_farming**

Detailed dairy cow farming data with milking and productivity metrics.

- **Coverage**: Dairy cow operations
- **Time period**: 1974 onwards
- **Geographic levels**: Country, Region, State, Municipality
- **Variables**: Milked cows, milk production volume, productivity
  (liters per cow)
- **Use cases**:
  - Dairy sector analysis
  - Productivity assessment
  - Geographic specialization in dairy
  - Production trend analysis

### **5. ppm_aquaculture**

Aquaculture activities including fish, shrimp, and mollusk farming.

- **Coverage**: All aquaculture operations
- **Time period**: 1974 onwards (though aquaculture data more recent)
- **Geographic levels**: Country, Region, State, Municipality
- **Activities**: Fish farming, shrimp farming, mollusk/oyster farming,
  other aquaculture
- **Variables**: Quantity and value of aquaculture production by type
- **Use cases**:
  - Aquaculture sector analysis
  - Regional aquaculture potential
  - Fish and seafood production trends
  - Emerging aquaculture development

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Selects which livestock/animal production dataset to download.

``` r
dataset = "ppm_livestock_inventory"     # Animal populations by species
dataset = "ppm_sheep_farming"           # Sheep and wool production
dataset = "ppm_animal_origin_production"  # Milk, eggs, honey, wool
dataset = "ppm_cow_farming"             # Dairy cow productivity
dataset = "ppm_aquaculture"             # Fish and aquaculture production
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw IBGE format
- `FALSE`: Returns treated data with English variable names and
  standardized units

``` r
raw_data = FALSE  # logical
```

### 3. **geo_level**

Specifies geographic aggregation level.

- `"country"`: National aggregate
- `"region"`: Brazilian geographic regions (5 regions)
- `"state"`: State-level data (27 units)
- `"municipality"`: All 5,570+ municipalities

``` r
geo_level = "state"  # character string
```

### 4. **time_period**

Specifies which year(s) to download.

``` r
time_period = 2020              # single year
time_period = c(2010, 2020)     # specific years
time_period = 2010:2020         # range of years
```

**Note**: All datasets available from 1974 onwards, though aquaculture
more complete from 2000s.

### 5. **language**

Output language for variable names.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Examples

### Example 1: Livestock inventory by state

``` r
# download treated livestock inventory data at the state level for 2020
livestock <- load_ppm(
  dataset = "ppm_livestock_inventory",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2020,
  language = "eng"
)
```

### Example 2: Dairy cow farming by state

``` r
# download treated dairy cow data at the state level for 2020
dairy <- load_ppm(
  dataset = "ppm_cow_farming",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2020,
  language = "eng"
)
```

### Example 3: Animal origin production at the country level

``` r
# download treated animal origin production data at the country level for 2020
animal_products <- load_ppm(
  dataset = "ppm_animal_origin_production",
  raw_data = FALSE,
  geo_level = "country",
  time_period = 2020,
  language = "eng"
)
```

### Example 4: Sheep farming by state

``` r
# download treated sheep farming data at the state level for 2020
sheep <- load_ppm(
  dataset = "ppm_sheep_farming",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2020,
  language = "eng"
)
```

### Example 5: Aquaculture by state over time

``` r
# download treated aquaculture data at the state level for 2015 to 2020
aquaculture <- load_ppm(
  dataset = "ppm_aquaculture",
  raw_data = FALSE,
  geo_level = "state",
  time_period = 2015:2020,
  language = "eng"
)
```

## Data Notes

### Data Structure

Each record typically contains: - Geographic identifier (state or
municipality) - Year - Animal species or product type - Quantity (number
of animals or production volume) - Value (if applicable) - Number of
establishments

### Units of Measurement

- **Livestock counts**: Number of animals
- **Milk**: Liters
- **Eggs**: Dozens or units (verify in data)
- **Honey**: Kilograms
- **Wool**: Kilograms
- **Aquaculture**: Kilograms or tons

### Raw vs. Treated Data

- **Raw data**: IBGE original format, Portuguese names
- **Treated data**: English variable names, standardized units

### Important Limitations

1.  **Survey-based data**: Subject to sampling and reporting error
2.  **Informal operations**: May undercount small or informal livestock
    operations
3.  **Data lag**: Published with delay; recent years may not be
    available
4.  **Aquaculture newer**: Aquaculture data less complete for very early
    years
5.  **Methodology changes**: Survey methods may evolve; can affect
    comparability

------------------------------------------------------------------------

## SIGMINE

## Overview

SIGMINE (Sistema de Informações Geográficas da Mineração - Mining
Geographic Information System) is Brazil’s official mining registry
maintained by the [National Mining Agency
(ANM)](https://www.gov.br/anm/pt-br), the regulatory body that succeeded
the former National Department of Mineral Production (DNPM).

This dataset provides:

- **Active mining operations**: All legally authorized mining projects
  across Brazil
- **Mine locations**: Geographic coordinates and spatial extent
- **Product information**: What minerals/resources are being mined
- **Operational status**: Whether mines are active, suspended, or
  recovering
- **Area data**: Size of mining concessions and operating areas
- **Company information**: Operator and ownership details
- **Environmental permits**: Status of environmental authorization
- **Geographic coverage**: National coverage including Legal Amazon
  region

SIGMINE data is critical for understanding Brazil’s mining sector impact
on the environment, economy, and land use, particularly in sensitive
regions like the Amazon.

### Data Source and Information

SIGMINE data comes from: - Brazilian mining authorization and regulatory
system - Geological Service of Brazil (SGB-CPRM) - Environmental
licensing agencies - Updated continuously as mining permits are
issued/revoked - Publicly accessible through government databases

For more information, visit the [National Mining Agency
(ANM)](https://www.gov.br/anm/pt-br) and mining databases.

------------------------------------------------------------------------

## Available Dataset

### **sigmine_active (Active Mining Operations)**

Comprehensive registry of legally operating mining projects in Brazil.

- **Coverage**: All active, legally authorized mining operations
- **Status**: Currently operating, suspended, or in recovery phases
- **Geographic levels**: National coverage with state and municipality
  identification
- **Key variables**: Mine name, operator, location, mineral type, area,
  authorization status
- **Mineral types**: Precious metals, industrial minerals, energy
  minerals (coal, uranium), others
- **Environmental data**: Environmental license status, compliance
  information
- **Spatial data**: Polygon boundaries of mining concessions/operations
- **Use cases**:
  - Assess mining impacts in regions of interest
  - Identify mining activity in conservation areas
  - Analyze relationships between mining and deforestation
  - Economic analysis of mining sector
  - Environmental impact assessment
  - Conflict identification (mining vs. indigenous lands, protected
    areas)

------------------------------------------------------------------------

## Function Parameters

### 1. **dataset**

Only one dataset is available:

``` r
dataset = "sigmine_active"  # Active mining operations
```

### 2. **raw_data**

Controls whether to download original or processed data.

- `TRUE`: Returns raw SIGMINE/DNPM data format with original coding
- `FALSE`: Returns treated data with English variable names and
  standardized formatting

``` r
raw_data = FALSE  # logical
```

### 3. **language**

Output language for variable names and documentation.

- `"pt"`: Portuguese
- `"eng"`: English

``` r
language = "eng"  # character string
```

------------------------------------------------------------------------

## Data Structure

SIGMINE data typically includes:

- **Mine identification**: Name, registration number, operator
- **Location**: State, municipality, geographic coordinates
- **Resource**: Mineral type being mined
- **Area**: Size of mining concession (square meters)
- **Status**: Active, suspended, recovery, or other operational status
- **Authorization**: Mining and environmental permits
- **Geometry**: Spatial polygon of mining area (when available)

------------------------------------------------------------------------

## Examples

### Example 1: Active mining data in portuguese

``` r
# download treated active mining data in portuguese
mining_active <- load_sigmine(
  dataset = "sigmine_active",
  raw_data = FALSE,
  language = "pt"
)
```

### Example 2: Active mining data in english

``` r
# download treated active mining data in english
mining_eng <- load_sigmine(
  dataset = "sigmine_active",
  raw_data = FALSE,
  language = "eng"
)
```

## Data Notes

### Mining Authorization Status

SIGMINE tracks mines at various stages: - **Active**: Currently
operating, producing minerals - **Suspended**: Temporarily halted
operations - **Recovery**: Reclamation and environmental restoration
phase - **Other**: Various other regulatory statuses

### Minerals Covered

SIGMINE includes: - **Precious metals**: Gold, silver, platinum -
**Industrial minerals**: Iron, copper, aluminum, tin, others - **Energy
minerals**: Coal, uranium - **Gemstones**: Diamonds, emeralds, others -
**Construction minerals**: Sand, gravel, granite

### Data Updates

- SIGMINE is continuously updated as new permits are issued/revoked
- The dataset you access reflects the most current government registry
- Historical changes available from government archives

### Geographic Information

- **Coordinates**: GPS coordinates of mine locations
- **Polygons**: Spatial boundaries of mining concessions (when
  available)
- **Accuracy**: Depends on government survey quality

### Important Limitations

1.  **Only legal operations**: Artisanal/informal mining not included
2.  **Status lags**: Official status may lag actual operational status
3.  **Environmental permits**: May not reflect real-time compliance
4.  **Spatial accuracy**: Boundaries may have uncertain precision
5.  **Definition changes**: Mining classification/categories may evolve

------------------------------------------------------------------------

## ANEEL

Loads data from the National Electrical Energy Agency (ANEEL), a
Brazilian independent federal agency linked to the Ministry of Mines and
Energy (MME). ANEEL works to provide favorable conditions for the
Electrical Energy Market to develop with balance and for the benefit of
society.

As for now, there are three different datasets available for download:
Energy Development Budget, Energy Generation, and Energy Enterprises
Distributed.

#### Energy Development Budget

The Energy Development Budget dataset showcases the Energy Development
Account’s (CDE) annual budget expenses. The CDE is designed to promote
the Brazilian energy development and is managed by the Electrical Energy
Commercialization Chamber (CCEE).

In the current implementation, data is available from 2017 to 2022 and
must be downloaded by year. The year argument can be a single year or a
vector of years. The dataset includes the type of expense, its value in
R\$ (Reais), and its share over the total amount of CDE budget expenses
in each year.\*.

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
4.  **year**: only used for “energy_development_budget”. It can be a
    single year or a vector of years from 2017:2022. This argument is
    required for “energy_development_budget” and can be omitted for the
    other datasets.

------------------------------------------------------------------------

**Examples:**

``` r
# download treated data about energy generation
clean_aneel <- load_aneel(
  dataset = "energy_generation",
  raw_data = FALSE
)


# download raw CDE data for one year
budget_aneel_2019 <- load_aneel(
  dataset = "energy_development_budget",
  raw_data = TRUE,
  year = 2019
)

# download raw CDE data for multiple years
budget_aneel_multi <- load_aneel(
  dataset = "energy_development_budget",
  raw_data = TRUE,
  year = c(2020, 2021)
)
```

## EPE

Loads data from the Energy Research Company (EPE), a Brazilian public
company that works closely with the Brazilian Ministry of Mines and
Energy (MME) and other agencies to ensure the sustainable development of
Brazil’s energy infrastructure. EPE’s duty on that mission is to support
MME with quality research and studies in order to aid Brazil’s energy
infrastructure planning.

As for now, there are four different datasets available for download:
Consumer Energy Consumption, Industrial Energy Consumption, the National
Energy Balance, and the State Energy Production Panel. All of them were
obtained from the [EPE
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

#### State Energy Production Panel

The State Energy Production Panel provides yearly data from 2011 to 2024
on electricity generation by energy source and Brazilian state. The data
is sourced from EPE’s BEN Chapter 8 (Dados Estaduais) and covers all 27
states, including the Federal District.

Each row corresponds to one state-year combination. The dataset includes
production by source (hydro, wind, solar, nuclear, thermal, sugar cane,
firewood, black liquor, other renewables, steam coal, natural gas, coke
oven gas, fuel oil, diesel, and other non-renewables), total production,
and an indicator of whether the state belongs to the Legal Amazon.

------------------------------------------------------------------------

**Options:**

1.  **dataset**: there are three choices:  
    `"consumer_energy_consumption"`: monthly energy consumption and
    consumers by State, Region or Electric Subsystem  
    `"industrial_energy_consumption"`: monthly industrial energy
    consumption by State or Subsystem  
    `"national_energy_balance"`: yearly energy flow by account and
    energy source  
    `"energy_state_panel"`: yearly energy production by source and state

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

# download treated data on state energy production panel
state_panel <- load_epe(
  dataset = "energy_state_panel",
  raw_data = FALSE,
  language = "eng"
)
```

# Other tools

## Legal Amazon Municipalities

## Overview

The `municipalities` dataset is a foundational reference dataset
included in the datazoom.amazonia package. It contains key information
about all Brazilian municipalities and identifies which are located in
(or overlapping with) the Legal Amazon region.

This dataset includes:

- **Municipality identification**: Official names, codes (IBGE), and
  geographic identifiers
- **Legal Amazon status**: Identifies whether each municipality is in
  the Legal Amazon
- **Geographic information**: State, region, and spatial boundaries
- **Supporting attributes**: Additional useful variables for geographic
  matching and analysis
- **Historical coverage**: Updated to reflect current municipal
  boundaries (2019 onwards)

The municipalities dataset is essential infrastructure for this
package—most package functions that return geographic data match results
to this municipalities reference to enable Legal Amazon filtering and
consistent geographic identification.

### Data Source

The municipalities dataset is compiled from: - **IBGE**: Brazilian
Institute of Geography and Statistics official municipal data - **Legal
Amazon definition**: Based on official Brazilian government definition -
**Spatial boundaries**: IBGE 2019 municipal boundary shapefile -
**Maintained by**: datazoom.amazonia package developers

------------------------------------------------------------------------

## Dataset Structure

The municipalities dataset contains the following information:

### Key Variables

- **code**: IBGE municipal code (unique identifier)
- **name**: Official municipality name
- **state**: Two-letter state abbreviation (e.g., “SP”, “AM”)
- **state_code**: IBGE state code
- **legal_amazon**: TRUE/FALSE indicating Legal Amazon membership
- **region**: Geographic region (North, Northeast, Center-West,
  Southeast, South)
- **geometry**: Spatial polygon (when using full SF object)

### Data Types

- **IBGE codes**: Numeric identifiers used throughout Brazilian
  statistics
- **State abbreviations**: Standard two-letter codes for all 27
  Brazilian states/federal district
- **Region names**: Official Brazilian geographic regions
- **Geographic boundaries**: Available as simple features (SF) objects
  for spatial analysis

------------------------------------------------------------------------

## Accessing the Dataset

### In R Code

``` r
# Load Brazilian municipalities dataset
data <- datazoom.amazonia::municipalities

# Or after loading the package
library(datazoom.amazonia)
data <- municipalities

# View structure
str(municipalities)
head(municipalities)

# Filter for Legal Amazon municipalities only
amazon_municipalities <- municipalities %>%
  filter(legal_amazon == TRUE)
```

### What It Contains

The dataset includes all 5,570+ Brazilian municipalities with: -
Official IBGE identification codes - State and region classification -
Legal Amazon flag for filtering - Spatial geometries for geographic
analysis

------------------------------------------------------------------------

## Common Use Cases

### Use Case 1: Filter Data to Legal Amazon

Many analyses focus specifically on the Legal Amazon region. Use the
municipalities dataset to identify relevant municipalities:

``` r
library(dplyr)
# Load any dataset with municipality information
data <- load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  geo_level = "municipality",
  language = "eng"
)

# Filter to Legal Amazon using municipalities reference
amazon_data <- data %>%
  inner_join(
    municipalities %>% 
      filter(legal_amazon == TRUE) %>%
      select(code, name),
    by = c("municipality_code" = "code")
  )
```

### Use Case 2: Partial Amazon Municipalities

**Important**: Some municipalities are only partially within the Legal
Amazon.

For statistics reported at municipality level in this package: -
**Partial Amazon municipalities**: Data is reported for only the
Amazon-included portion - **Identification**: The municipalities dataset
identifies these cases - **Interpretation**: When a municipality is
partially in Amazon, reported statistics reflect the Amazon portion only

``` r
# Identify fully vs. partially included municipalities
full_amazon <- municipalities %>%
  filter(legal_amazon == TRUE & fully_included == TRUE)

partial_amazon <- municipalities %>%
  filter(legal_amazon == TRUE & fully_included == FALSE)

print(paste("Fully in Amazon:", nrow(full_amazon)))
print(paste("Partially in Amazon:", nrow(partial_amazon)))
```

------------------------------------------------------------------------

## Key Characteristics

### Municipal Coverage

- **Total municipalities**: 5,570+ (exact number updated periodically)
- **Geographic coverage**: All Brazilian territory
- **Boundaries**: Based on IBGE 2019 definitions (latest update)
- **Changes**: Municipal boundaries periodically updated (most recent:
  2021)

### Legal Amazon Definition

The Legal Amazon includes: - **States**: All or parts of Acre, Amapá,
Amazonas, Distrito Federal, Goiás, Maranhão, Mato Grosso, Mato Grosso do
Sul, Pará, Rondônia, Roraima, Tocantins - **Municipalities**: 570+
municipalities fully or partially in Legal Amazon - **Definition**:
Based on official Brazilian legislation (Law 8,001/1990)

### Partial Inclusion

Important note on municipality-level data: - **Partial municipalities**:
Some municipalities extend beyond Legal Amazon boundaries - **Data
reporting**: When data is reported at municipality level for partial
municipalities, it reflects **only the Legal Amazon portion** -
**Identification**: This dataset identifies which municipalities are
partial

``` r
# Check for partial municipalities
partial_check <- municipalities %>%
  filter(legal_amazon == TRUE) %>%
  filter(!is.na(amazon_percentage)) %>%
  filter(amazon_percentage < 100)

if (nrow(partial_check) > 0) {
  print("Municipalities partially in Legal Amazon:")
  print(partial_check)
}
```

------------------------------------------------------------------------

## Important Notes

### Boundary Changes

Brazilian municipalities occasionally undergo changes: - **New
municipalities**: Created from existing ones (last major change 2021) -
**Boundary adjustments**: IBGE periodically refines municipal
boundaries - **Historical data**: When comparing very old data with
recent data, be aware municipalities may have been reorganized

### Code Consistency

Always use IBGE municipality codes (not names) when: - Merging multiple
datasets - Doing time-series analysis - Comparing across sources -
Municipality names may change or be ambiguous; codes are unique and
stable

### Spatial Data

The municipalities dataset includes spatial boundaries (when loaded as
SF object): - **Format**: Simple features (SF) polygons - **CRS**: WGS84
(EPSG:4326) - **Use**: Spatial operations, mapping, spatial joins with
other geographic data

------------------------------------------------------------------------

## Accessing Spatial Data

### For Mapping and Spatial Analysis

``` r
library(sf)
library(ggplot2)

# Load municipalities with geometry
municipalities_sf <- municipalities %>%
  st_as_sf()  # If not already SF format

# Map Legal Amazon
amazon_map <- municipalities_sf %>%
  filter(legal_amazon == TRUE)

ggplot(amazon_map) +
  geom_sf(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Legal Amazon Municipalities") +
  theme_minimal()

# Spatial operations example: count municipalities by state
munic_by_state <- municipalities_sf %>%
  group_by(state) %>%
  summarize(
    num_municipalities = n(),
    total_area_km2 = sum(st_area(.), na.rm = TRUE) / 1e6,
    .groups = 'drop'
  )
```

------------------------------------------------------------------------

## Troubleshooting

### Matching Issues

**Problem**: Municipality names don’t match between datasets
**Solution**: Use IBGE municipality codes instead of names for joining
data

**Problem**: Some municipalities missing after filtering **Solution**:
Check for name spelling variations; use code-based matching

### Geographic Analysis

**Problem**: Spatial operations are slow **Solution**: Simplify
geometries (`st_simplify()`) or work with state/region level first

**Problem**: Mapping appears incorrect **Solution**: Verify CRS (should
be WGS84); check for invalid geometries (`st_is_valid()`)

### Data Integration

**Problem**: Aggregating municipality data to regions **Solution**: Use
`left_join()` with municipalities dataset to add region information

**Problem**: Partial municipalities causing data discrepancies
**Solution**: Account for partial Amazon municipalities; some statistics
are reported for Amazon portion only

------------------------------------------------------------------------

## Related Resources

- **IBGE**: <https://www.ibge.gov.br/> (Brazilian statistics authority)
- **Legal Amazon**: Official definition <https://www.gov.br/pt-br>
- **Spatial data**: IBGE municipal boundaries available from various
  sources
- **Within this package**: All geographic functions integrate this
  municipalities reference

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
is at: <https://datazoom.com.br/pt/>.

To cite package `datazoom.amazonia` in publications use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata.  
> <https://datazoom.com.br/en/>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2023,
        author = {Data Zoom},
        title = {Data Zoom: Simplifying Access To Brazilian Microdata},
        url = {https://datazoom.com.br/en/},
        year = {2023},
    }
