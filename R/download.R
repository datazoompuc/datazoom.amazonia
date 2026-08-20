sidra_download <- function(sidra_code = NULL, year, geo_level = "municipality",
                           classific = "all", category = "all") {
  ## Bind Global Variables

  code_state <- NULL
  code_meso <- NULL
  code_micro <- NULL

  # Obs: Sometimes there are non-catched municipalieis - user should check on IBGE SIDRA
  # f = geo %>% filter(!(code_muni %in% unique(dat_uf$`Município (Código)`)))

  # ----------------------------------------------------

  ## Download from Sidra IBGE

  ## Include Progress Bar
  ## Omit Warnings
  ## We should include support for microregion/mesoregion

  # ------------------------------------------------------

  ##############################
  ## Setting Basic Parameters ##
  ##############################

  param <- list()

  param$sidra_code <- sidra_code
  param$year <- year
  param$classific <- classific
  param$category <- category

  if (geo_level == "country") {
    param$geo_reg <- "Brazil"
  }
  if (geo_level == "region") {
    param$geo_reg <- "Region"
  }
  if (geo_level == "state") {
    param$geo_reg <- "State"
  }
  if (geo_level == "municipality") {
    param$geo_reg <- "City"
  }

  ### SPECIAL CASES: POPULATION FOR YEARS 2007 AND 2010
  ### CONTAGEM DA POPULACAO AND CENSO DEMOGRAFICO
  if (param$sidra_code == 6579) {
    if (year == 2007) param$sidra_code <- 793 # https://sidra.ibge.gov.br/tabela/793
    if (year == 2010) {
      param$sidra_code <- 1378 # https://sidra.ibge.gov.br/tabela/1378

      param$classific <- "c1"
      param$category <- list(0)
    }
  }
  if (param$sidra_code == 6907) {
    param$classific <- c("c12443")
    param$category <- list(110056)
  }


  ##################################
  ## Get Geographical Information ##
  ##################################

  geo <- datazoom.amazonia::municipalities %>%
    tidyr::drop_na() # 5 municipalities have no micro code

  # uf = list('RO' = 11,'AC' = 12,'AM' = 13,'RR' = 14,'PA' = 15,'AP' = 16,'TO' = 17,
  #           'MA' = 21,'PI' = 22,'CE' = 23,'RN' = 24,'PB' = 25,'PE' = 26,'AL' = 27,'SE' = 28,'BA' = 29,
  #           'MG' = 31,'ES' = 32,'RJ' = 33,'SP' = 35,
  #           'PR' = 41,'SC' = 42,'RS' = 43,
  #           'MS' = 50,'MT' = 51,'GO' = 52,'DF' = 53)

  ###############
  ## Load Data ##
  ###############

  get_sidra_safe <- purrr::safely(sidrar::get_sidra)


  if (param$geo_reg %in% c("Brazil", "Region", "State")) {
    base::message("Downloading Data at the ", param$geo_reg, " level") ## Show Message

    ## Download

    dat <- suppressMessages(
      get_sidra_safe(
        x = param$sidra_code,
        geo = param$geo_reg,
        period = as.character(param$year),
        classific = param$classific,
        category = param$category
      )
    )

    dat <- dat$result

    dat <- dat %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    if (nrow(dat) == 0) {
      stop("Error in Download.")
    }

    base::message("Download Succesfully Completed!")

    return(dat)
  }

  if (param$geo_reg == "City") {
    ##############################
    ## Download at the UF Level ##
    ##############################

    uf_list <- geo %>%
      dplyr::select(code_state) %>%
      unlist() %>%
      unique() %>%
      as.list()

    names(uf_list) <- uf_list

    dat_raw_uf <- purrr::map(uf_list, function(uf) {
      base::message(which(uf == uf_list), " in ", length(uf_list), " states...\n")

      suppressMessages(
        get_sidra_safe(
          x = param$sidra_code,
          geo = param$geo_reg,
          period = as.character(param$year),
          geo.filter = list("State" = uf),
          classific = param$classific,
          category = param$category
        )
      )
    })

    dat_mod_uf <- base::lapply(dat_raw_uf, "[[", 1)

    dat_uf <- dat_mod_uf[unlist(lapply(dat_mod_uf, is.data.frame))] %>% ## Filter for only found dataframes
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    ###########################################
    ## Checking for Completeness of Download ##
    ###########################################

    missed_uf <- dat_mod_uf[!unlist(lapply(dat_mod_uf, is.data.frame))] %>% names()

    rm(dat_mod_uf)

    if (length(missed_uf) > 0) {
      base::message(
        "Download at the State Level Completed! ", length(missed_uf), " failed.\n",
        "Attempting to Download at the Mesoregion Level..."
      )
    } else if (length(missed_uf) == 0) {
      base::message("Download Succesfully Completed!")

      return(dat_uf)
    }

    #################
    ## Meso Region ##
    #################

    if (length(missed_uf) > 0) {
      geo_meso <- geo %>%
        dplyr::filter(code_state %in% missed_uf)

      meso_reg_list <- geo_meso %>%
        dplyr::select(code_meso) %>%
        unlist() %>%
        unique() %>%
        as.list()

      names(meso_reg_list) <- meso_reg_list

      dat_raw_meso <- purrr::map(meso_reg_list, function(meso_reg) {
        base::message(which(meso_reg == meso_reg_list), " in ", length(meso_reg_list), " mesoregions...\n")

        base::suppressMessages(
          get_sidra_safe(
            x = param$sidra_code,
            geo = param$geo_reg,
            period = as.character(param$year),
            geo.filter = list("MesoRegion" = meso_reg),
            classific = param$classific,
            category = param$category
          )
        )
      })

      dat_mod_meso <- base::lapply(dat_raw_meso, "[[", 1)

      dat_meso <- dat_mod_meso[unlist(lapply(dat_mod_meso, is.data.frame))] %>% ## Filter for only found dataframes
        dplyr::bind_rows() %>%
        tibble::as_tibble() %>%
        janitor::clean_names()

      dat_uf <- dat_uf %>%
        dplyr::bind_rows(dat_meso)

      ###########################################
      ## Checking for Completeness of Download ##
      ###########################################

      missed_meso <- dat_mod_meso[!unlist(lapply(dat_mod_meso, is.data.frame))] %>% names()

      rm(dat_mod_meso)

      if (length(missed_meso) > 0) {
        base::message(
          "Download at the Mesoregion Level Completed! ", length(missed_meso), " failed.\n",
          "Attempting to Download at the Microregion Level...\n"
        )
      } else if (length(missed_meso) == 0) {
        base::message("Download Succesfully Completed!")

        return(dat_uf)
      }

      ##################
      ## Micro Region ##
      ##################

      if (length(missed_meso) > 0) {
        geo_micro <- geo %>%
          dplyr::filter(code_meso %in% missed_meso)

        micro_reg_list <- geo_micro %>%
          dplyr::select(code_micro) %>%
          unlist() %>%
          unique() %>%
          as.list()

        names(micro_reg_list) <- micro_reg_list

        dat_raw_micro <- purrr::map(micro_reg_list, function(micro_reg) {
          base::message(which(micro_reg == micro_reg_list), " in ", length(micro_reg_list), " microregions...\n")

          base::suppressMessages(
            get_sidra_safe(
              x = param$sidra_code,
              geo = param$geo_reg,
              period = as.character(param$year),
              geo.filter = list("MicroRegion" = micro_reg),
              classific = param$classific,
              category = param$category
            )
          )
        })

        dat_mod_micro <- base::lapply(dat_raw_micro, "[[", 1)

        dat_micro <- dat_mod_micro[unlist(lapply(dat_mod_micro, is.data.frame))] %>% ## Filter for only found dataframes
          dplyr::bind_rows() %>%
          tibble::as_tibble() %>%
          janitor::clean_names()

        dat_uf <- dat_uf %>%
          dplyr::bind_rows(dat_micro)

        ###########################################
        ## Checking for Completeness of Download ##
        ###########################################

        missed_micro <- dat_mod_micro[!unlist(lapply(dat_mod_micro, is.data.frame))] %>% names()

        rm(dat_mod_micro)

        if (length(missed_micro) > 0) {
          base::message(
            length(missed_micro),
            " missed API requests at the Microregion level.
                              Please report this problem to package developers..."
          )
        }
        if (length(missed_micro) == 0) {
          base::message("Download Succesfully Completed!")

          return(dat_uf)
        }
      } # End of if Meso
    } # End of if Uf
  } # End of If - Download at the Municipality Level
}

external_download <- function(dataset = NULL, source = NULL, year = NULL,
                              geo_level = NULL, coords = NULL, dataset_code = NULL,
                              sheet = NULL, skip_rows = NULL, file_name = NULL,
                              state = NULL) {
  ## Bind Global Variables

  link <- NULL

  ## Restore global options on exit (CRAN compliance)

  old_options <- list(
    timeout = getOption("timeout"),
    download.file.method = getOption("download.file.method"),
    download.file.extra = getOption("download.file.extra")
  )
  on.exit(options(old_options), add = TRUE)

  ## Define Basic Parameters

  param <- list()
  param$dataset <- dataset
  param$source <- source

  # Optional parameters for functions that need them:

  param$year <- year # if download is perform separately by year
  param$geo_level <- geo_level # if some geo_levels have a different download link
  param$coords <- coords
  param$dataset_code <- dataset_code
  param$skip_rows <- skip_rows # number of rows to skip atop a spreadsheet
  param$file_name <- file_name
  param$sheet <- sheet # which sheet of a .xlsx to read
  param$state <- state

  if (is.null(param$skip_rows)) param$skip_rows <- 0 # makes it more error-proof

  #####################
  ## Construct Links ##
  #####################

  ## Pull URL from the manifest
  #
  # dataset_url() resolves the most specific matching row for
  # (source, dataset, geo_level, year), so geo_level/year-dependent URLs
  # (MapBiomas overrides, ANEEL CDE per-year links, ...) no longer need a
  # dedicated branch here -- they are just more specific rows in the
  # manifest (inst/extdata/manifest/v1/datasets_link.csv).

  param$url <- dataset_url(
    source = param$source,
    dataset = param$dataset,
    geo_level = param$geo_level,
    year = param$year
  )

  if (is.na(param$url)) {
    stop(
      "No download URL found for source '", param$source, "', dataset '", param$dataset, "'",
      if (!is.null(param$year)) paste0(", year ", paste(param$year, collapse = ", ")) else "",
      if (!is.null(param$geo_level)) paste0(", geo_level '", param$geo_level, "'") else "",
      "."
    )
  }

  # For most sources, the URL in the manifest is already the URL needed for the download

  path <- param$url

  ## Filling in URLs

  # Some URLs are in the form www.data/$year$_$dataset$.csv, where expression
  # surrounded by $ are placeholders. The code below subs them in for actual parameters

  if (stringr::str_detect(path, "\\$year\\$|\\$state\\$|\\$file_name\\$")) {
    if (!is.null(param$year)) {
      path <- path %>%
        stringr::str_replace("\\$year\\$", as.character(param$year))
    }
    if (!is.null(param$state)) {
      path <- sub("$state$", param$state, path, fixed = TRUE)
    }
    if (!is.null(param$file_name)) {
      path <- path %>%
        stringr::str_replace("\\$file_name\\$", param$file_name)
    }
  }

  # Below are the exceptions, for which manipulation is needed

  ##### Exceptions only #####

  # If the manifest URL is the download path you need,
  # do not change this section for a new function

  ## TerraClimate

  # Download path depends on geographical parameters

  if (source == "terraclimate") {
    filename <- paste0(
      "agg_terraclimate_",
      param$dataset_code,
      "_1958_CurrentYear_GLOBE.nc"
    )

    path <- paste0(
      param$url,
      "/",
      filename,
      "?",
      "&var=",
      param$dataset_code,
      "&south=",
      param$coords$lat_min,
      "&north=",
      param$coords$lat_max,
      "&west=",
      param$coords$lon_min,
      "&east=",
      param$coords$lon_max,
      "&horizStride=1",
      "&time_start=",
      param$year$initial_time,
      "&time_end=",
      param$year$final_time,
      "&timeStride=1",
      "&disableProjSubset=on&addLatLon=true&accept=netcdf"
    )
  }

  #######################
  ## Initiate Download ##
  #######################

  ## Specify file extension to be passed to tempfile()

  # For most functions, the file extension is automatically detected

  file_extension <- sub(".*\\.", ".", path) %>%
    tolower()

  ##### Exceptions only #####

  # Only manually input the file_extension if the download_path does
  # not end in ".ext", where .ext is any file extension

  # googledrive links do not contain the file extension, for example

  if (source %in% c("seeg", "iema", "ips")) {
    file_extension <- ".xlsx"
  }
  if (source == "mapbiomas") {
    # Dataverse-hosted urls (inst/extdata/manifest/v1/datasets_link.csv,
    # see actions/scrapers/resolve_mapbiomas.R) end in
    # ".../api/access/datafile/{id}?format=original" -- no ".xlsx" at the
    # end for the extension-sniffing above to find, same shape of problem
    # as the googledrive-hosted sources above. Every mapbiomas source
    # (Dataverse or the couple of rows still pointing at a legacy .xlsx
    # url) is genuinely an xlsx file.
    file_extension <- ".xlsx"
  }
  if (source == "terraclimate") {
    file_extension <- ".nc"
  }
  if (source == "deter") {
    file_extension <- ".zip"
  }
  if (source == "ibama") {
    if (dataset == "embargoed_areas") {
      file_extension <- ".zip"
    } else {
      file_extension <- ".csv"
    }
  }
  if (source == "imazon") {
    file_extension <- ".rds"
  }
  if (source == "aneel") {
    if (dataset == "energy_development_budget") {
      file_extension <- ".csv"
    }
    if (dataset == "energy_generation") {
      file_extension <- ".xlsx"
    }
    if (dataset == "energy_enterprises_distributed") {
      file_extension <- ".csv"
    }
  }

  ## Define Empty Directory and Files For Download

  dir <- tempdir()
  temp <- tempfile(fileext = file_extension, tmpdir = dir)

  ## Picking the way to download the file

  download_method <- "standard" # works for most functions

  if (source %in% c("iema", "imazon")) {
    download_method <- "googledrive"
  }
  if (source == "aneel") {
    if (dataset == "energy_enterprises_distributed") {
      message("This may take a while.\n")
      options(timeout = 1000) # increase timeout limit
    }
  }
  if (source == "prodes") {
    message("This may take a while.\n")
    options(timeout = max(1000, getOption("timeout")))
  }
  if (source %in% c("deter", "terraclimate", "baci", "mapbiomas")) {
    download_method <- "curl"
    quiet <- FALSE
  }
  if (source == "sigmine") {
    options(timeout = max(1000, getOption("timeout")))
  }
  if (source == "ibama") {
    download_method <- "curl"
    options(download.file.method = "curl", download.file.extra = "-L") # https://stackoverflow.com/questions/69716835/turning-ssl-verification-off-inside-download-file
    quiet <- TRUE
  }
  if (source == "seeg") {
    download_method <- "googledrive"
  }

  ## Downloading file by the selected method

  if (download_method == "standard") {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }
  if (download_method == "curl") {
    if (source == "deter") {
      options(download.file.method = "curl", download.file.extra = "-L")
    }
    if (source == "baci") {
      options(download.file.extra = "--ssl-no-revoke")
    }
    utils::download.file(url = path, destfile = temp, method = "curl", quiet = quiet)
  }
  if (download_method == "googledrive") {
    message("Please follow the steps from `googledrive` package to download the data. This may take a while.\nIn case of authentication errors, run vignette(\"GOOGLEDRIVE\").")
    if (source == "seeg") {
      googledrive::drive_deauth()
    }
    googledrive::drive_download(path, path = temp, overwrite = TRUE)
  }

  ## Unzipping if the file is zipped

  if (file_extension == ".zip") {
    utils::unzip(temp, exdir = dir)
  }

  ###############
  ## Load Data ##
  ###############


  ##### Exceptions only #####

  if (file_extension == ".zip") {
    if (param$dataset == "degrad") {
      dat <- sf::read_sf(file.path(dir, param$file_name))
      dat$year <- param$year
    }
    if (param$source == "deter") {
      shp_name <- dataset_field(param$source, param$dataset, "archive_file")
      dat <- sf::read_sf(file.path(dir, shp_name))
    }
    if (param$source == "sigmine") {
      shp <- list.files(dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      if (length(shp) == 0) {
        stop("No shapefile found in the downloaded SIGMINE archive.")
      }
      dat <- purrr::quietly(sf::read_sf)(shp[1])$result
    }
    if (param$source == "ibama") {
      shp <- list.files(dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      dat_sf <- sf::read_sf(shp[1], quiet = TRUE)
      dat <- tibble::as_tibble(dat_sf)
    }
    if (param$source == "baci") {
      # archive_file carries the same version stamp as the URL (e.g.
      # "*$year$_V202601.csv"), so the two never drift apart -- as year can
      # be a vector, str_replace() recycles it into one expression per year
      archive_file <- dataset_field(param$source, param$dataset, "archive_file")
      file_expression <- stringr::str_replace(archive_file, "\\$year\\$", as.character(param$year))
      # now turning into *XXXX_V202601.csv|YYYY_V202601.csv|ZZZZ_V202601.csv" to match as regex
      file_expression <- paste0(file_expression, collapse = "|")

      file <- list.files(dir, pattern = file_expression, full.names = TRUE) %>%
        as.list()

      # now reads each file

      dat <- lapply(file, data.table::fread, header = TRUE, sep = ",")

      # each data frame in the list is named after the corresponding year
      names(dat) <- param$year
    }
    if (param$source == "prodes") {
      # clearing rasters to avoid overlap

      terra::tmpFiles(remove = TRUE)
      file <- list.files(dir, pattern = "*.tif", full.names = TRUE)
      dat <- terra::rast(file)
    }

  } else if (param$source == "aneel") {
    if (param$dataset %in% c("energy_enterprises_distributed", "energy_development_budget")) {
      dat <- data.table::fread(temp, encoding = "Latin-1")
    } else if (param$dataset == "energy_generation") {
      dat <- readxl::read_xlsx(
        temp,
        sheet = param$sheet,
        skip = param$skip_rows,
        na = c("-", "")
      )
    }

  } else if (param$source == "ips") {
    dat <- param$sheet %>%
      purrr::map(
        ~ readxl::read_xlsx(temp, sheet = .)
      )

  } else if (param$source == "epe") {
    dat <- param$sheet %>%
      purrr::map(
        ~ base::suppressMessages(readxl::read_xlsx(temp, sheet = .))
      )

    ## Now the rest of the functions

    # This Depends on Data Type (.csv, .shp, ...) and on the data source

  } else {
    if (file_extension == ".csv") {
      dat <- data.table::fread(temp)
    }
    if (file_extension == ".txt") {
      dat <- readr::read_csv(temp)
    }
    if (file_extension == ".nc") {
      dat <- terra::rast(temp)
    }
    if (file_extension == ".rds") {
      dat <- readr::read_rds(temp)
    }
    if (file_extension == ".xlsx") {
      dat <- readxl::read_xlsx(temp, sheet = param$sheet, skip = param$skip_rows)
    }
  }




  ##############################
  ## Excluding Temporary File ##
  ##############################

  # Folder is kept

  if (!file_extension %in% c(".nc")) {
    unlink(temp)
  }

  #################
  ## Return Data ##
  #################

  return(dat)
}

datasets_link <- function(source = NULL, dataset = NULL, url = FALSE) {
  # The URL table used to be a hardcoded tibble::tribble() here (~280 lines).
  # It now lives in inst/extdata/manifest/v1/datasets_link.csv (see
  # R/manifest.R), refreshed by a scheduled GitHub Action and fetched at
  # runtime with a silent fallback to the packaged snapshot. This function
  # keeps its original signature and return shape -- effective_table() drops
  # every geo_level/year override row, so exactly one row per (survey,
  # dataset) -- its own self-sufficient base row -- is ever visible here, in
  # the original 6 columns and order (with "link" now named "url" -- no call
  # site ever depended on that literal name; see R/manifest.R). Every
  # existing call site is unaffected.

  survey <- geo_level <- year <- NULL
  sidra_code <- available_time <- available_geo <- NULL

  link <- effective_table() %>%
    dplyr::select(
      survey, dataset, sidra_code, available_time, available_geo, url
    )

  if (!is.null(source)) {
    link <- link %>%
      dplyr::filter(survey == source)
  }

  if (!is.null(dataset)) {
    link <- link %>%
      dplyr::filter(dataset == !!dataset)
  }

  if (url) {
    link <- link %>%
      purrr::pluck("url")
  }

  return(link)
}
