load_climate <- function(dataset, raw_data = FALSE,
                         geo_level = "municipality",
                         time_period,
                         language = "eng",
                         legal_amazon_only = TRUE){

  ##############################
  ## Binding Global Variables ##
  ##############################

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$dataset_name <- dataset
  param$raw_data <- raw_data
  param$geo_level <- geo_level
  param$language <- language
  param$initial_time <- time_period[1]
  param$final_time <- time_period[2]

  if (is.na(param$final_time)) {
    param$final_time <- param$initial_time
  }

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
    recode(!!!dataset_names)

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

  ncssPath="http://thredds.northwestknowledge.net:8080/thredds/ncss"

  filename <- paste0(
    "agg_terraclimate_",
    param$dataset_code,
    "_1958_CurrentYear_GLOBE.nc"
  )

  queryString <- paste0(
    ncssPath,
    "/",
    filename,
    "?",
    "&var=",
    param$dataset_code,

    "&south=",
    LAT_MIN,
    "&north=",
    LAT_MAX,
    "&west=",
    LON_MIN,
    "&east=",
    LON_MAX,
    "&horizStride=1",

    "&time_start=",
    param$initial_time,
    "&time_end=",
    param$final_time,
    "&timeStride=1",

    "&disableProjSubset=on&addLatLon=true&accept=netcdf"
  )

  dir = tempdir()
  temp = tempfile(fileext = ".nc", tmpdir = dir)

  utils::download.file(
    queryString,
    destfile = temp,
    method = "curl"
  )

  ##################
  ## Reading Data ##
  ##################

  r <- terra::rast(temp)

  time <- terra::time(r) %>%
    as.Date()

  names(time) <- 1:length(time)

  points <- r %>%
    terra::as.points() %>%
    sf::st_as_sf()

  ## Return Raw Data

  if(raw_data == TRUE) {return(dat)}

  ######################
  ## Data Engineering ##
  ######################

  ## Brazilian municipalities to merge

  mun <- geobr::read_municipality() %>%
    sf::st_make_valid()

  points <- points %>%
    sf::st_transform(crs = terra::crs(mun)) # merge requires matching
                                            # coordinate systems

  dat <- sf::st_join(mun, points)

  ## Converting to Tibble

  dat <- sf::st_drop_geometry(dat)

  ## Reshaping data

  dat <- dat %>%
    pivot_longer(starts_with(param$dataset_code),
                 names_prefix = paste0(param$dataset_code, "_"),
                 names_to = "date",
                 values_to = param$dataset_name)

  ## Restoring correct dates

  dat <- dat %>%
    mutate(
      date = recode(date, !!!time)
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng"){
    dat <- dat %>%
      rename(
        "municipality_code" = "code_muni",
        "municipality" = "name_muni",
        "state_code" = "code_state"
    )
  }
  if (param$language == "pt"){
    dat <- dat %>%
      rename(
        "cod_municipio" = "code_muni",
        "municipio" = "name_muni",
        "cod_uf" = "code_state"
      )
  }

  ######################
  ## Aggregating Data ##
  ######################

  dat

}

