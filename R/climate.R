#' TerraClimate
#'
#' @param dataset A dataset name, choosing which variable will be loaded. One of ("max_temperature", "min_temperature", "wind_speed", "vapor_pressure_deficit", "vapor_pressure", "snow_water_equivalent", "shortwave_radiation_flux", "soil_moisture", "runoff", "precipitation", "potential_evaporation", "climatic_water_deficit", "water_evaporation", "palmer_drought_severity_index"). For extra details, try \code{vignette("TERRACLIMATE")}.
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param time_period A \code{numeric} indicating for what years the data will the be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE}). Defaults to \code{FALSE}
#'
#' @examples
#' \dontrun{
#' max_temp <- load_climate(dataset = "max_temperature", time_period = 2000:2020)
#' }
#'
#' @return A \code{tibble}
#' @export

load_climate <- function(dataset, raw_data = FALSE,
                         time_period,
                         language = "eng",
                         legal_amazon_only = FALSE){

  ##############################
  ## Binding Global Variables ##
  ##############################

  AMZ_LEGAL <- code_muni <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$dataset_name <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$initial_time <- min(time_period)
  param$final_time <- max(time_period)

  ## Coordinates of rectangle around the Legal Amazon

  #latitude bounding box -90.0 to 90.0 (North is positive, South is negative)
  LAT_MIN <- "-18.04177"
  LAT_MAX <- "5.272225"

  #longitude bounding box: -180.0 to 180.0 (East is positive, West is negative)
  LON_MIN <- "-73.99094"
  LON_MAX <- "-44"

  if (!legal_amazon_only){
    LAT_MIN <- "-33.75208"
    LAT_MAX <- "5.271841"
    LON_MIN <- "-73.99045"
    LON_MAX <- "-28.83609"
  }

  #variable choices: tmax,tmin,ws,vpd,vap,swe,srad,soil,q,ppt,pet,def,aet,PDSI
  #VARIABLES=("tmax" "tmin" "ws" "vpd" "vap" "swe" "srad" "soil" "q" "ppt" "pet" "def" "aet" "PDSI")

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

  param$dataset_code <- param$dataset_name %>%
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


  #time range choices - 1958-01 to 2017-01
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


  dat <- external_download(
    source = "terraclimate",
    dataset = param$dataset_name,
    dataset_code = param$dataset_code,
    coords = list("lat_min" = LAT_MIN,
                  "lat_max" = LAT_MAX,
                  "lon_min" = LON_MIN,
                  "lon_max" = LON_MAX),
    year = list("initial_time" = param$initial_time,
                "final_time" = param$final_time)
  )

  time <- terra::time(dat) %>%
    as.Date()

  names(time) <- 1:length(time)

  points <- dat %>%
    terra::as.points() %>%
    sf::st_as_sf()

  ######################
  ## Data Engineering ##
  ######################

  ## Brazilian municipalities/states/country to merge

  map <- geobr::read_municipality() %>%
    sf::st_make_valid()

  ## Filtering for Legal Amazon

  if (legal_amazon_only){
    legal_amazon <- legal_amazon %>%
      dplyr::filter(AMZ_LEGAL == 1)

    map <- map %>%
      dplyr::filter(code_muni %in% legal_amazon$CD_MUN)
  }

  ## Performing merge

  points <- points %>%
    sf::st_transform(crs = sf::st_crs(map)) # merge requires matching
                                            # coordinate systems

  dat <- sf::st_join(map, points)

  ## Return Raw Data

  if (param$raw_data == TRUE) {return(dat)}

  ## Converting to Tibble

  #dat <- sf::st_drop_geometry(dat)

  ## Reshaping data

  dat <- dat %>%
    tidyr::pivot_longer(dplyr::starts_with(param$dataset_code),
                 names_prefix = paste0(param$dataset_code, "_"),
                 names_to = "date",
                 values_to = param$dataset_name)

  ## Restoring correct dates

  dat <- dat %>%
    dplyr::mutate(
      date = dplyr::recode(date, !!!time)
    )

  ######################
  ## Aggregating Data ##
  ######################

  #dat <- dat %>%
    #dplyr::group_by(code_muni, name_muni, code_state, date) %>%
    #dplyr::summarise(across(param$dataset_name, ~ mean(., na.rm = TRUE)))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng"){
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality_code" = "code_muni",
        "municipality" = "name_muni",
        "state" = "abbrev_state",
        "state_code" = "code_state",
        "geometry" = "geom"
    )
  }
  if (param$language == "pt"){
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

