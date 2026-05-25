#' @title TerraClimate - Climate monitoring
#'
#' @description Spatial data on climate variables, extracted from Climatology Lab's TerraClimate.
#'
#' @param dataset A dataset name, choosing which variable will be loaded. One of ("max_temperature", "min_temperature", "wind_speed", "vapor_pressure_deficit", "vapor_pressure", "snow_water_equivalent", "shortwave_radiation_flux", "soil_moisture", "runoff", "precipitation", "potential_evaporation", "climatic_water_deficit", "water_evaporation", "palmer_drought_severity_index"). For extra details, try \code{vignette("TERRACLIMATE")}.
#' @inheritParams load_baci
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE}). Defaults to \code{FALSE}
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' @examples
#' # Example 1: Precipitation Patterns
#' \dontrun{
#' library(raster)
#' library(dplyr)
#' precip <- load_climate(dataset = "precipitation", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' summary(precip)
#' plot(precip, main = "Amazon Precipitation (2020)")
#' annual_precip <- sum(precip, na.rm = TRUE)
#' plot(annual_precip, main = "Annual Precipitation in Amazon")
#' }
#'
#' # Example 2: Temperature Trends
#' \dontrun{
#' library(raster)
#' max_temp <- load_climate(dataset = "max_temperature", time_period = 2010:2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' avg_temp <- mean(max_temp, na.rm = TRUE)
#' plot(avg_temp, main = "Average Maximum Temperature (2010-2020)")
#' temp_2010 <- max_temp[[1:12]]
#' temp_2020 <- max_temp[[(nlayers(max_temp) - 11):nlayers(max_temp)]]
#' avg_2010 <- mean(temp_2010, na.rm = TRUE)
#' avg_2020 <- mean(temp_2020, na.rm = TRUE)
#' temp_change <- avg_2020 - avg_2010
#' plot(temp_change, main = "Temperature Change 2010-2020")
#' }
#'
#' # Example 3: Drought Assessment
#' \dontrun{
#' library(raster)
#' water_def <- load_climate(dataset = "climatic_water_deficit", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' avg_water_def <- mean(water_def, na.rm = TRUE)
#' plot(avg_water_def, main = "Average Climatic Water Deficit (2020)")
#' drought_areas <- avg_water_def > quantile(avg_water_def, 0.75, na.rm = TRUE)
#' plot(drought_areas, main = "High Drought Risk Areas")
#' pdsi <- load_climate(dataset = "palmer_drought_severity_index", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' plot(pdsi, main = "Palmer Drought Severity Index (2020)")
#' }
#'
#' # Example 4: Radiation and Growing Season
#' \dontrun{
#' library(raster)
#' radiation <- load_climate(dataset = "shortwave_radiation_flux", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' temp <- load_climate(dataset = "max_temperature", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' annual_radiation <- sum(radiation, na.rm = TRUE)
#' plot(annual_radiation, main = "Annual Solar Radiation")
#' avg_temp <- mean(temp, na.rm = TRUE)
#' plot(avg_temp, main = "Average Maximum Temperature")
#' radiation_temp <- stack(annual_radiation, avg_temp)
#' names(radiation_temp) <- c("Radiation", "Temperature")
#' }
#'
#' # Example 5: Soil Moisture Dynamics
#' \dontrun{
#' library(raster)
#' soil_moisture <- load_climate(dataset = "soil_moisture", time_period = 2020,
#'   legal_amazon_only = TRUE, language = "eng")
#' plot(soil_moisture, main = "Monthly Soil Moisture (2020)")
#' avg_soil <- mean(soil_moisture, na.rm = TRUE)
#' high_moisture <- soil_moisture > quantile(soil_moisture, 0.75, na.rm = TRUE)
#' low_moisture <- soil_moisture < quantile(soil_moisture, 0.25, na.rm = TRUE)
#' plot(high_moisture, main = "High Soil Moisture Areas")
#' plot(low_moisture, main = "Low Soil Moisture Areas")
#' }
#'
#' @return A \code{tibble}.
#' @export

load_climate <- function(dataset, raw_data = FALSE,
                         time_period,
                         language = "eng",
                         legal_amazon_only = FALSE) {
  # Checking for terra package (in Suggests)

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package \"terra\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  code_muni <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$source <- "terraclimate"
  param$dataset <- dataset
  param$time_period <- time_period
  param$raw_data <- raw_data
  param$language <- language
  param$initial_time <- min(time_period)
  param$final_time <- max(time_period)
  param$legal_amazon_only <- legal_amazon_only

  ## Coordinates of rectangle around the Legal Amazon

  # latitude bounding box -90.0 to 90.0 (North is positive, South is negative)
  LAT_MIN <- "-18.04177"
  LAT_MAX <- "5.272225"

  # longitude bounding box: -180.0 to 180.0 (East is positive, West is negative)
  LON_MIN <- "-73.99094"
  LON_MAX <- "-44"

  if (!legal_amazon_only) {
    LAT_MIN <- "-33.75208"
    LAT_MAX <- "5.271841"
    LON_MIN <- "-73.99045"
    LON_MAX <- "-28.83609"
  }

  # variable choices: tmax,tmin,ws,vpd,vap,swe,srad,soil,q,ppt,pet,def,aet,PDSI
  # VARIABLES=("tmax" "tmin" "ws" "vpd" "vap" "swe" "srad" "soil" "q" "ppt" "pet" "def" "aet" "PDSI")

  dataset_names <- c(
    "max_temperature" = "tmax",
    "min_temperature" = "tmin",
    "wind_speed" = "ws",
    "vapor_pressure_deficit" = "vpd",
    "vapor_pressure" = "vap",
    "snow_water_equivalent" = "swe",
    "shortwave_radiation_flux" = "srad",
    "soil_moisture" = "soil",
    "runoff" = "q",
    "precipitation" = "ppt",
    "potential_evaporation" = "pet",
    "climatic_water_deficit" = "def",
    "water_evaporation" = "aet",
    "palmer_drought_severity_index" = "PDSI"
  )

  param$dataset_code <- param$dataset %>%
    dplyr::recode(!!!dataset_names)

  # tmax - Maximum 2-m Temperature; air_temperature (degC)
  # tmin - Minimum 2-m Temperature; air_temperature (degC)
  # ws - Wind Speed at 10-m; wind_speed (m/s)
  # vpd - Vapor Pressure Deficit; vapor_pressure_deficit (kPa)
  # vap - 2-m Vapor Pressure; water_vapor_partial_pressure_in_air (kPa)
  # swe - Snow Water Equivalent at End of Month; liquid_water_content_of_surface_snow (mm)
  # srad - Downward Shortwave Radiation Flux at the Surface; downwelling_shortwave_flux_in_air (W/m^2)
  # soil - Soil Moisture at End of Month; soil_moisture_content (mm)
  # q - Runoff; runoff_amount (mm)
  # ppt - Accumulated Precipitation; precipitation_amount (mm)
  # pet - Reference Evapotranspiration; water_potential_evaporation_amount (mm)
  # def - Climatic Water Deficit; water_potential_evaporation_amount_minus_water_evaporation_amount (mm)
  # aet - Actual Evapotranspiration; water_evaporation_amount (mm)
  # PDSI - Palmer Drought Severity Index; palmer_drought_severity_index (unitless)

  # check if dataset and time_period are valid

  check_params(param)

  # time range choices - 1958-01 to 2017-01
  param$initial_time <- ifelse(
    nchar(param$initial_time) == 4,
    paste0(param$initial_time, "-01-01T00%3A00%3A00Z"),
    paste0(param$initial_time, "T00%3A00%3A00Z")
  )
  param$final_time <- ifelse(
    nchar(param$final_time) == 4,
    paste0(param$final_time, "-12-01T00%3A00%3A00Z"),
    paste0(param$final_time, "T00%3A00%3A00Z")
  )

  ######################
  ## Downloading Data ##
  ######################

  base::message("Downloading TerraClimate data")

  dat <- external_download(
    source = param$source,
    dataset = param$dataset,
    dataset_code = param$dataset_code,
    coords = list(
      "lat_min" = LAT_MIN,
      "lat_max" = LAT_MAX,
      "lon_min" = LON_MIN,
      "lon_max" = LON_MAX
    ),
    year = list(
      "initial_time" = param$initial_time,
      "final_time" = param$final_time
    )
  )

  time <- terra::time(dat) %>%
    as.Date()

  names(time) <- seq_along(time)

  points <- dat %>%
    terra::as.points() %>%
    sf::st_as_sf()

  ######################
  ## Data Engineering ##
  ######################

  ## Brazilian municipalities/states/country to merge

  base::message("Downloading Brazilian map data")

  map <- external_download(
    dataset = "geo_municipalities",
    source = "internal"
  )

  ## Filtering for Legal Amazon

  if (param$legal_amazon_only) {
    legal_amazon <- datazoom.amazonia::municipalities %>%
      dplyr::filter(legal_amazon == 1)

    map <- map %>%
      dplyr::filter(code_muni %in% legal_amazon$code_muni)
  }

  ## Performing merge

  points <- points %>%
    sf::st_transform(crs = sf::st_crs(map)) # merge requires matching
  # coordinate systems

  dat <- sf::st_join(map, points)

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ## Converting to Tibble

  # dat <- sf::st_drop_geometry(dat)

  ## Reshaping data

  dat <- dat %>%
    tidyr::pivot_longer(dplyr::starts_with(param$dataset_code),
      names_prefix = paste0(param$dataset_code, "_"),
      names_to = "date",
      values_to = param$dataset
    )

  ## Restoring correct dates

  dat <- dat %>%
    dplyr::mutate(
      date = dplyr::recode(date, !!!time)
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality_code" = "code_muni",
        "municipality" = "name_muni",
        "state" = "abbrev_state",
        "state_code" = "code_state",
        "geometry" = "geom"
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "cod_municipio" = "code_muni",
        "municipio" = "name_muni",
        "uf" = "abbrev_state",
        "cod_uf" = "code_state",
        "geometry" = "geom"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
