#' @title TerraClimate - Climate monitoring
#'
#' @description Spatial data on climate variables, extracted from Climatology Lab's TerraClimate.
#'
#' @param dataset A dataset name, choosing which variable will be loaded. One of ("max_temperature", "min_temperature", "wind_speed", "vapor_pressure_deficit", "vapor_pressure", "snow_water_equivalent", "shortwave_radiation_flux", "soil_moisture", "runoff", "precipitation", "potential_evaporation", "climatic_water_deficit", "water_evaporation", "palmer_drought_severity_index"). For extra details, try \code{vignette("TERRACLIMATE")}.
#' @inheritParams load_baci
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE}). Defaults to \code{FALSE}
#'
#' @return An \code{sf} object with the selected climate data merged onto Brazilian municipalities.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download maximum temperature data from 2000 to 2001
#' max_temp <- load_climate(
#'   dataset = "max_temperature",
#'   time_period = 2000:2001,
#'   language = "eng"
#' )
#'
#' # download precipitation data only for the legal Amazon in 2010
#' amz_precip <- load_climate(
#'   dataset = "precipitation",
#'   time_period = 2010,
#'   legal_amazon_only = TRUE,
#'   language = "eng"
#' )
#'
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
  # FRAGILE: these rectangles are hard-coded strings, not derived from the
  # municipality map joined below. Measured 2026-10-19 against
  # data-raw/geo_municipalities.rds: the Brazil one (set a few lines down)
  # matches the 5,570 municipalities' extent, but this Legal Amazon one stops
  # at 44 degrees West while 21 Maranhao municipalities (Codo, Icatu, Timbiras,
  # Pastos Bons, ...) extend to 43.37 West -- they get only the grid cells west
  # of 44, silently, so a per-municipality summary of them covers part of the
  # municipality. A boundary update in geo_municipalities that moves an
  # extreme municipality outside a rectangle would do the same, or leave it
  # with NA, with no error.
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

  # time range choices: the calendar years in the manifest's available_time
  # (kept current by actions/scrapers/resolve_terraclimate.R)
  # FRAGILE: every request spans January 1 of the first year to December 1 of
  # the final year, whole calendar years only. Verified live 2026-10-19, the
  # server answers a range that merely STRADDLES the end of its data with
  # HTTP 200 and whatever months exist (time_period = 2025:2026 returns 2025
  # only, 12 months, no error; a final year only partly published would
  # return fewer than 12) and a range entirely outside the data with HTTP 400. The only
  # signal a user gets about the first case is check_params()'s warning about
  # a year outside available_time (1950-2025 on that date; the resolver lists
  # only years with all 12 months). In the second case the 400's text -- which
  # names the real range -- never becomes an R error: download.file(method =
  # "curl") has no --fail, so that text is saved as the "netCDF" file and the
  # user gets the same warning followed by terra's "[rast] cannot open this
  # file as a SpatRaster" (reproduced 2026-10-19 with time_period = 2026).
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

  # FRAGILE: relies on terra naming the NCSS file's layers "<code>_1", ...,
  # "<code>_<n>" in time order (verified live 2026-10-19: tmax_1..tmax_12,
  # q_1.., PDSI_1.., def_1..) -- that is what lets names(time) <- seq_along(time)
  # above and the recode() below turn a layer index back into a date. The
  # columns are picked with starts_with(<code>), which matches by PREFIX and
  # ignores case: a column of geo_municipalities that ever starts with the
  # code (a "q..." or "def..." one, say) would be silently pivoted as if it
  # were a month, and a different layer naming would leave every date NA.
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
