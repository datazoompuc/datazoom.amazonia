sigmine_municipality_intersection <- function(polygons, municipalities) {
  if (is.na(sf::st_crs(polygons))) {
    stop("The SIGMINE polygons do not have a defined CRS.", call. = FALSE)
  }
  if (is.na(sf::st_crs(municipalities))) {
    stop("The municipal layer does not have a defined CRS.", call. = FALSE)
  }

  polygons <- polygons %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_make_valid()
  municipalities <- sf::st_make_valid(municipalities)

  operation_crs <- sf::st_crs(
    "+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
  )
  polygons <- sf::st_transform(polygons, operation_crs)
  municipalities <- sf::st_transform(municipalities, operation_crs)

  # A process may have multiple source polygons. Union them before overlay so
  # a process-municipality pair is represented once and overlaps are not
  # counted twice in the geometric intersection area.
  process_id <- as.character(polygons$processo)
  group_id <- process_id
  missing_id <- is.na(group_id) | !nzchar(group_id)
  group_id[missing_id] <- paste0(".source_row_", which(missing_id))
  groups <- split(seq_len(nrow(polygons)), factor(group_id, levels = unique(group_id)))

  processes <- lapply(groups, function(ix) {
    process <- polygons[ix[1], ]
    process$area_m2 <- if (all(is.na(polygons$area_m2[ix]))) {
      NA_real_
    } else {
      sum(polygons$area_m2[ix], na.rm = TRUE)
    }
    process$source_polygon_count <- length(ix)
    sf::st_geometry(process) <- sf::st_union(sf::st_geometry(polygons[ix, ]))
    process
  })
  processes <- do.call(rbind, processes)
  processes$.process_key <- names(groups)

  municipality_columns <- c(
    "code_muni", "name_muni", "code_state", "abbrev_state",
    "name_state", "code_region", "name_region"
  )
  missing_municipality_columns <- setdiff(municipality_columns, names(municipalities))
  if (length(missing_municipality_columns)) {
    stop(
      "The municipal layer is missing required columns: ",
      paste(missing_municipality_columns, collapse = ", "),
      call. = FALSE
    )
  }

  intersections <- suppressWarnings(sf::st_intersection(
    processes,
    municipalities[, municipality_columns]
  ))
  intersections$intersection_area_m2 <- as.numeric(sf::st_area(intersections))
  intersections <- intersections[intersections$intersection_area_m2 > 0, ]

  # Keep processes with no positive-area match, as the previous output did,
  # but make the missing municipality explicit.
  matched <- unique(intersections$.process_key)
  unmatched <- processes[!processes$.process_key %in% matched, ]
  if (nrow(unmatched)) {
    for (column in municipality_columns) {
      unmatched[[column]] <- municipalities[[column]][NA_integer_]
    }
    unmatched$intersection_area_m2 <- NA_real_
    intersections <- rbind(intersections, unmatched)
  }

  result <- sf::st_drop_geometry(intersections)
  result$.process_key <- NULL
  if ("uf" %in% names(result)) result$uf <- NULL
  result
}

#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads the registry of mining processes maintained by Brazil's National Mining Agency (ANM). Each record is a polygon. \code{"sigmine_active"} covers active processes in any phase (research authorizations, mining concessions, licensing, among others), not only mines in operation; \code{"sigmine_inactive"} covers processes that are no longer active.
#'
#' @param dataset A dataset name ("sigmine_active" or "sigmine_inactive"). Note that "sigmine_inactive" has fewer columns available than "sigmine_active" (no mineral, company, phase, last event, or state information at the source) -- this reflects what ANM publishes for closed/inactive processes, not a limitation of this function.
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be "state" or "municipality". With "state" (the default) there is no aggregation and no spatial join: the function returns one row per polygon, with the state reported at the source. With "municipality" each process is associated with every municipality that has a positive-area intersection with its polygons, and the result has one row per process-municipality pair and no geometry. A process value is repeated in each municipality it reaches, so municipal values are not additive across municipalities. Ignored when \code{raw_data = TRUE}.
#'
#' @details With \code{raw_data = TRUE} the function returns the ANM shapefile as published (an \code{sf} object), with only the column names cleaned; \code{geo_level} and \code{language} are ignored. With \code{raw_data = FALSE} the columns are renamed according to \code{language}, the area is converted from hectares (\code{area_ha}) to square meters (\code{area_m2}) and, for "sigmine_active", fields registered as "DADO NAO CADASTRADO" at the source become \code{NA}. The process description column (\code{ds_processo} in the ANM file) is returned as \code{process_description} when \code{language = "eng"} and as \code{descricao_processo} when \code{language = "pt"}. A process can appear in more than one polygon of the ANM file: in the October 2026 data, 4,028 of the 261,025 active processes (1.5%) and 7,447 of the 652,652 inactive ones (1.1%) have more than one polygon. At the municipality level, \code{area_m2} is the sum of the source polygon area values for the process and is repeated for each municipality reached. \code{intersection_area_m2} is a separate geometric measure: the area of the union of that process's polygons inside that municipality. It is not used to apportion source attributes such as production. Municipal rows are not additive when summed across municipalities.
#'
#' @return An \code{sf} object (one row per polygon, with a \code{geometry} column) when \code{raw_data = TRUE} or \code{geo_level = "state"}; a \code{tibble} without geometry (one row per process-municipality pair) when \code{geo_level = "municipality"}. Processes without a positive-area intersection with a municipality are retained once with missing municipality fields.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated active mining data in portuguese, by state
#' mining_active <- load_sigmine(
#'   dataset = "sigmine_active",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "pt"
#' )
#'
#' # download treated active mining data in portuguese, by municipality
#' mining_active_munic <- load_sigmine(
#'   dataset = "sigmine_active",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "pt"
#' )
#'
#' # download treated inactive (closed) mining data, by municipality
#' mining_inactive_munic <- load_sigmine(
#'   dataset = "sigmine_inactive",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "pt"
#' )
#'
#' # download the raw ANM shapefile for the inactive processes
#' mining_inactive_raw <- load_sigmine(
#'   dataset = "sigmine_inactive",
#'   raw_data = TRUE
#' )
#'
#' @export

load_sigmine <- function(dataset = "sigmine_active",
                         raw_data = FALSE,
                         geo_level = "state",
                         language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- nome <- uf <- processo <- geometry <- NULL
  code_muni <- name_muni <- abbrev_state <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "sigmine"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and geo_level are supported

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  dat <- external_download(dataset = param$dataset, source = param$source) %>%
    janitor::clean_names()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################################################
  ## Data Engineering -- dataset == "sigmine_active"   ##
  ######################################################

  if (dataset == "sigmine_active") {
    a <- dat %>%
      dplyr::mutate(dplyr::across(
        nome:uf,
        ~ ifelse(stringr::str_detect(.x, "DADO N.O CADASTRADO"),
                 NA, .x
        )
      ))

    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "area_ha"] <- "area_m2"

    if (geo_level == "municipality") {
      munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")
      a <- sigmine_municipality_intersection(a, munic_shp)
    }

    if (language == "pt") {
      names(a)[names(a) == "ult_evento"] <- "ultimo_evento"
      names(a)[names(a) == "ds_processo"] <- "descricao_processo"
      names(a)[names(a) == "nome"]       <- "empresa"
      names(a)[names(a) == "subs"]       <- "mineral"
      names(a)[names(a) == "uso"]        <- "uso"

      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "cod_municipio"
        names(a)[names(a) == "name_muni"]     <- "municipio"
        names(a)[names(a) == "code_state"]    <- "cod_uf"
        names(a)[names(a) == "abbrev_state"]  <- "uf"
        names(a)[names(a) == "name_state"]    <- "nome_uf"
        names(a)[names(a) == "code_region"]   <- "cod_regiao"
        names(a)[names(a) == "name_region"]   <- "nome_regiao"
        names(a)[names(a) == "intersection_area_m2"] <- "area_intersecao_m2"
        names(a)[names(a) == "source_polygon_count"] <- "numero_poligonais_origem"
      }
    } else if (language == "eng") {
      names(a)[names(a) == "numero"]     <- "number"
      names(a)[names(a) == "ult_evento"] <- "last_event"
      names(a)[names(a) == "uf"]         <- "state"
      names(a)[names(a) == "ano"]        <- "year"
      names(a)[names(a) == "processo"]   <- "process"
      names(a)[names(a) == "ds_processo"] <- "process_description"
      names(a)[names(a) == "id"]         <- "id"
      names(a)[names(a) == "fase"]       <- "phase"
      names(a)[names(a) == "nome"]       <- "company"
      names(a)[names(a) == "subs"]       <- "mineral"
      names(a)[names(a) == "uso"]        <- "use"

      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "municipality_code"
        names(a)[names(a) == "name_muni"]     <- "municipality"
        names(a)[names(a) == "code_state"]    <- "state_code"
        names(a)[names(a) == "abbrev_state"]  <- "state"
        names(a)[names(a) == "name_state"]    <- "state_name"
        names(a)[names(a) == "code_region"]   <- "region_code"
        names(a)[names(a) == "name_region"]   <- "region_name"
      }
    }
  }

  ########################################################
  ## Data Engineering -- dataset == "sigmine_inactive"   ##
  ########################################################

  if (dataset == "sigmine_inactive") {
    a <- dat
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "area_ha"] <- "area_m2"

    if (geo_level == "municipality") {
      munic_shp <- external_download(source = "internal", dataset = "geo_municipalities")
      a <- sigmine_municipality_intersection(a, munic_shp)
    }

    if (language == "pt") {
      names(a)[names(a) == "ds_processo"] <- "descricao_processo"
      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]     <- "cod_municipio"
        names(a)[names(a) == "name_muni"]     <- "municipio"
        names(a)[names(a) == "code_state"]    <- "cod_uf"
        names(a)[names(a) == "abbrev_state"]  <- "uf"
        names(a)[names(a) == "name_state"]    <- "nome_uf"
        names(a)[names(a) == "code_region"]   <- "cod_regiao"
        names(a)[names(a) == "name_region"]   <- "nome_regiao"
        names(a)[names(a) == "intersection_area_m2"] <- "area_intersecao_m2"
        names(a)[names(a) == "source_polygon_count"] <- "numero_poligonais_origem"
      }
    } else if (language == "eng") {
      names(a)[names(a) == "numero"]        <- "number"
      names(a)[names(a) == "ano"]           <- "year"
      names(a)[names(a) == "processo"]      <- "process"
      names(a)[names(a) == "ds_processo"]   <- "process_description"
      if (geo_level == "municipality") {
        names(a)[names(a) == "code_muni"]    <- "municipality_code"
        names(a)[names(a) == "name_muni"]    <- "municipality"
        names(a)[names(a) == "code_state"]   <- "state_code"
        names(a)[names(a) == "abbrev_state"] <- "state"
        names(a)[names(a) == "name_state"]   <- "state_name"
        names(a)[names(a) == "code_region"]  <- "region_code"
        names(a)[names(a) == "name_region"]  <- "region_name"
      }
    }
  }

  return(a)
}
