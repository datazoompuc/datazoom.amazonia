# datazoom.amazonia 1.1.2.9000 (development version)

  * Updated `load_mapbiomas` to download the newest Mapbiomas Collections and to warn about the currently unavailable download URL of the datasets water and irrigation.

# datazoom.amazonia 1.1.1.9000

  * Updated `load_baci` to support the newest version of the data, fixing the previous broken download URL. (Thanks to @OlivazShai)

# datazoom.amazonia 1.1.0.9000 (development version)

  * Updated `load_prodes` data cleaning and download to allow more recent data

# datazoom.amazonia 1.1.0

  * Added a citation template for work using the package
  
  * Added the `"energy_enterprises_distributed"` dataset to `load_aneel`
  
  * Updated `load_mapbiomas` to the Collection 8 data, with both new and reformed datasets
  
  * Added the `load_population` function with Brazilian population data and estimates
  
  * Added the `load_censoagro` function with data from the Census of Agriculture
  
  * Updated `load_ips` to include 2023 data
  
  * Removed the `load_cipo` function
  
  * Many small bug fixes

# datazoom.amazonia 1.0.0

  * Adding new `load_epe` function for EPE data and adding new `load_aneel` function for ANEEL data
  
  * Changing the internal structure of `download.R`, which is behind all functions

# datazoom.amazonia 0.9.3.0

  * Making documentation consistent across functions

# datazoom.amazonia 0.9.2.9000

  * Fixing `load_climate` error when option `legal_amazon_only = TRUE`

# datazoom.amazonia 0.9.1.9000

  * Fixing `load_baci` timeout error upon download
  
# datazoom.amazonia 0.9.0.9000

  * Adding new `load_imazon` function for Imazon data

# datazoom.amazonia 0.8.5.9000

  * Updated collection 6 Mapbiomas data for "mapbiomas_transition" dataset

# datazoom.amazonia 0.8.4.9000

  * Exporting dataset with municipality codes and minor bug fixes

# datazoom.amazonia 0.8.3.9000

  * Code maintenance tweaks

# datazoom.amazonia 0.8.2.9000

  * Adding a sequential identification variable to `load_deter`

# datazoom.amazonia 0.8.1.9000

  * Changed code to initiate Deter download

# datazoom.amazonia 0.8.0.9000

  * Added new datasets to `load_ibama` for environmental distributed and collected fines

# datazoom.amazonia 0.7.2.9000

  * Fixing SSL verification error in download for `load_ibama`
  
# datazoom.amazonia 0.7.1.9000

  * Added municipalities code into `load_iema` function for energy data

# datazoom.amazonia 0.7.0.9000

  * Added new `load_iema` function for energy data

# datazoom.amazonia 0.6.0.9000

  * Added new `load_datasus` function for health data

# datazoom.amazonia 0.5.0.9000

  * Added new IPS datasets 

# datazoom.amazonia 0.4.0.9000

  * Added new Mapbiomas Mining dataset 

# datazoom.amazonia 0.3.0
  
## New functions
  
  * Plataforma CIPÓ data
  * TerraClimate data
  * BACI data for global trade
  
## Improvements
  
  * Documentation for `load_mapbiomas`
  * Bug fixes for PRODES, SEEG, and Ibama
  * Supports new IPS data format
  * Supports many more PAM datasets
  * No longer dependent on packages not published in CRAN

# datazoom.amazonia 0.2.0

* All functions supporting treated data download

# datazoom.amazonia 0.1.0

* All functions supporting raw data download

# datazoom.amazonia 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added functions for PRODES data

# datazoom.amazonia 0.3.0.9000
* Added functions for GDP data 

# datazoom.amazonia 0.4.0.9000
* Added functions for COMEX data 

# datazoom.amazonia 0.5.0.9000
* Added functions for MAPBIOMAS data 

# datazoom.amazonia 0.6.0.9000
* Added functions for DEGRAD data 

# datazoom.amazonia 0.7.0.9000
* Added functions for DETER data 

# datazoom.amazonia 0.8.0.9000
* Added functions for SIGMINE data 

# datazoom.amazonia 0.9.0.9000
* Added functions for SIGMINE data 

# datazoom.amazonia 0.10.0.9000
* Added functions for IBGE - CEMPRE data
* Added functions for IBGE - Census data

# datazoom.amazonia 0.11.0.9000
* Added functions for IBGE - PAM data

# datazoom.amazonia 0.12.0.9000
* Added functions for IPS data 

# datazoom.amazonia 0.13.0.9000
* Added functions for SEEG data
