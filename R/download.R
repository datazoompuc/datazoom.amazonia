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
    base::message(base::cat("Downloading Data at the", param$geo_reg, "level")) ## Show Message

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

    base::message(base::cat("Download Succesfully Completed!"))

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
      base::message(base::cat(which(uf == uf_list), "in", length(uf_list), "states...\n"))

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
      base::message(base::cat(
        "Download at the State Level Completed!", length(missed_uf), "failed.\n",
        "Attempting to Download at the Mesoregion Level..."
      ))
    } else if (length(missed_uf) == 0) {
      base::message(base::cat("Download Succesfully Completed!"))

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
        base::message(base::cat(which(meso_reg == meso_reg_list), "in", length(meso_reg_list), "mesoregions...\n"))

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
        base::message(base::cat(
          "Download at the Mesoregion Level Completed!", length(missed_meso), "failed.\n",
          "Attempting to Download at the Microregion Level...\n"
        ))
      } else if (length(missed_meso) == 0) {
        base::message(base::cat("Download Succesfully Completed!"))

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
          base::message(base::cat(which(micro_reg == micro_reg_list), "in", length(micro_reg_list), "microregions...\n"))

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
          base::message(base::cat(
            length(missed_micro),
            "missed API requests at the Microregion level.
                              Please report this problem to package developers..."
          ))
        }
        if (length(missed_micro) == 0) {
          base::message(base::cat("Download Succesfully Completed!"))

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

  ## Pull URL from datasets_link

  param$url <- datasets_link(
    source = param$source,
    dataset = param$dataset,
    url = TRUE
  )

  # For most sources, the URL in datasets_link is already the URL needed for the download

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
      path <- path %>%
        stringr::str_replace("\\$state\\$", param$state)
    }
    if (!is.null(param$file_name)) {
      path <- path %>%
        stringr::str_replace("\\$file_name\\$", param$file_name)
    }
  }

  # Below are the exceptions, for which manipulation is needed

  ##### Exceptions only #####

  # If the datasets_link URL is the download path you need,
  # do not change this section for a new function

  ## MapBiomas

  # Download path depends on dataset and geo_level

  if (source == "mapbiomas") {
    if (dataset == "mapbiomas_transition") {
      if (param$geo_level == "biome") {
        path <- "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-BIOMES.xlsx"
      }
      if (param$geo_level == "municipality") {
        path <- "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_state_municipality.xlsx"
      }
    }
  }

  ## SEEG

  # Download path depends on geo_level

  if (source == "seeg") {
    if (geo_level == "municipality") {
      path <- "https://drive.google.com/u/0/uc?confirm=bhfS&id=1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd"
    }
  }

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
  if (source == "prodes") {
    file_extension <- ".txt"
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
  if (source == "epe") {
    if (param$dataset == "national_energy_balance") {
      file_extension <- ".csv"
    }
  }
  if (source == "aneel") {
    if (dataset == "energy_development_budget") {
      file_extension <- ".rds"
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
    if (dataset == "energy_development_budget") {
      download_method <- "googledrive"
    }
    if (dataset == "energy_enterprises_distributed") {
      message("This may take a while.\n")
      options(timeout = 1000) # increase timeout limit
    }
  }
  if (source == "epe") {
    if (dataset == "national_energy_balance") {
      download_method <- "googledrive"
    }
  }
  if (source %in% c("deter", "terraclimate", "baci", "sigmine", "mapbiomas")) {
    download_method <- "curl"
    quiet <- FALSE
  }
  if (source == "datasus") {
    download_method <- "curl"
    quiet <- TRUE
  }
  if (source == "ibama") {
    download_method <- "curl"
    options(download.file.method = "curl", download.file.extra = "-k -L") # https://stackoverflow.com/questions/69716835/turning-ssl-verification-off-inside-download-file
    quiet <- TRUE
  }
  if (source == "seeg") {
    if (geo_level == "municipality") {
      download_method <- "googledrive"
    }
  }

  ## Downloading file by the selected method

  if (download_method == "standard") {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }
  if (download_method == "curl") {
    utils::download.file(url = path, destfile = temp, method = "curl", quiet = quiet)
  }
  if (download_method == "googledrive") {
    message("Please follow the steps from `googledrive` package to download the data. This may take a while.\nIn case of authentication errors, run vignette(\"GOOGLEDRIVE\").")

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
      if (param$dataset == "deter_amz") {
        dat <- sf::read_sf(file.path(dir, "deter-amz-deter-public.shp"))
      }
      if (param$dataset == "deter_cerrado") {
        dat <- sf::read_sf(file.path(dir, "deter_public.shp"))
      }
    }
    if (param$source == "sigmine") {
      dat <- sf::read_sf(file.path(dir, "BRASIL.shp"))
    }

    if (param$source == "ibama") {
      # get latest downloaded file (the name changes daily)
      file <- file.info(list.files(dir, pattern = "rel_areas_embargadas_.*.xls"))
      file <- file[with(file, order(as.POSIXct(mtime))), ]
      file <- rownames(file)

      doc <- XML::htmlParse(file.path(dir, file), encoding = "UTF-8")

      tableNode <- XML::getNodeSet(doc, "//table")

      dataset <- XML::readHTMLTable(tableNode[[1]])


      colnames(dataset) <- dataset[5, ]

      dat <- dataset[-c(1:5), ]
    }

    if (param$source == "baci") {
      # as year can be a vector, sets up expressions of the form "*YYYY_V202401b.csv" for each year to match file names
      file_expression <- paste0("*", param$year, "_V202401b.csv")

      # now turning into *XXXX_V202401b.csv|YYYY_V202401b.csv|ZZZZ_V202401b.csv" to match as regex
      file_expression <- paste0(file_expression, collapse = "|")

      file <- list.files(dir, pattern = file_expression, full.names = TRUE) %>%
        as.list()

      # now reads each file
      dat <- lapply(file, data.table::fread, header = TRUE, sep = ",")

      # each data frame in the list is named after the corresponding year
      names(dat) <- param$year
    }
  }

  if (param$source == "epe" & param$dataset == "energy_consumption_per_class") {
    # param$sheet contains the selected sheets

    # Making a list with all the sheets
    dat <- purrr::imap(
      param$sheet,
      function(sheets, number) {
        base::message(
          paste0("Reading sheet ", number, " out of ", length(param$sheet), " (", sheets, ")")
        )
        base::suppressMessages(
          readxl::read_xls(temp, sheet = sheets)
        )
      }
    )

    names(dat) <- param$sheet
  }
  if (param$source == "aneel") {
    if (param$dataset == "energy_enterprises_distributed") {
      dat <- data.table::fread(temp, encoding = "Latin-1")
    } else if (dataset == "energy_generation") {
      dat <- readxl::read_xlsx(temp, sheet = param$sheet, skip = param$skip_rows, na = c("-", ""))
    }
  }

  if (param$source == "ips") {
    dat <- param$sheet %>%
      purrr::map(
        ~ readxl::read_xlsx(temp, sheet = .)
      )
  }

  ## Now the rest of the functions

  # This Depends on Data Type (.csv, .shp, ...) and on the data source

  else {
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
    if (file_extension == ".dbc") {
      dat <- read.dbc(temp)
    }
  }

  ##############################
  ## Excluding Temporary File ##
  ##############################

  # Folder is kept

  if (file_extension != ".nc") {
    unlink(temp)
  }

  #################
  ## Return Data ##
  #################

  return(dat)
}

datasets_link <- function(source = NULL, dataset = NULL, url = FALSE) {
  survey <- NULL

  link <- tibble::tribble(
    ~survey, ~dataset, ~sidra_code, ~available_time, ~available_geo, ~link,

    ########################
    ## Environmental data ##
    ########################

    ## PRODES

    "prodes", "deforestation", NA, "2000-2023", "Municipality", "http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=2023&estado=&ordem=MUNICIPIO&type=tabela&output=txt&",

    ## DETER

    "deter", "deter_amz", NA, "2016-2022", "Municipality", "http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-amz/shape",
    "deter", "deter_cerrado", NA, "2018-2022", "Municipality", "http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-cerrado/shape",

    ## DEGRAD

    "degrad", "degrad", NA, "2007-2016", NA, "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad/arquivos/degrad$year$_final_shp.zip",

    ## Imazon

    "imazon", "imazon_shp", NA, "2020", "Municipality", "https://docs.google.com/uc?export=download&id=1JHc2J_U8VXHVuWVsi8wVBnNzZ37y1ehv",

    ## IBAMA

    "ibama", "embargoed_areas", NA, NA, "Municipality", "https://servicos.ibama.gov.br/ctf/publico/areasembargadas/downloadListaAreasEmbargadas.php",
    "ibama", "distributed_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/$state$/Quantidade/multasDistribuidasBensTutelados.csv",
    "ibama", "collected_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/$state$/Arrecadacao/arrecadacaobenstutelados.csv",

    ## MapBiomas

    "mapbiomas", "mapbiomas_cover", NA, "1985-2023", "Municipality" , "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/statistics/mapbiomas_brazil_col_coverage_biome_state_municipality.xlsx",
    "mapbiomas", "mapbiomas_transition", NA, "1985-2023", "Municipality, Biome", "https://brasil.mapbiomas.org/estatisticas/",
    "mapbiomas", "mapbiomas_deforestation_regeneration", NA, "1985-2023", "Municipality", "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_deforestation_and_secondary_vegetation_state_municipality.xlsx",
    "mapbiomas", "mapbiomas_irrigation", NA, "2000-2019", "State, Biome", "https://mapbiomas-br-site.s3.amazonaws.com/downloads/Estatisticas%20/Colecao_7_Irrigacao_Biomes_UF.xlsx",
    "mapbiomas", "mapbiomas_mining", NA, "1985-2022", "Municipality, Indigenous_Land", "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/09/TABELA-MINERACAO-MAPBIOMAS-COL8.0.xlsx",
    "mapbiomas", "mapbiomas_fire", NA, "1985-2023", "State", "https://storage.googleapis.com/mapbiomas-public/brasil/fire/collection_3_stats/MB-Fogo-3-Biome-State.xlsx",
    "mapbiomas", "mapbiomas_water", NA, "1985-2022", "State, Municipality, Biome", "https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Estatisticas_Superficie%C3%81gua_Col2_SITE.xlsx",

    ## TerraClimate

    "terraclimate", "max_temperature", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "min_temperature", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "wind_speed", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "vapor_pressure_deficit", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "vapor_pressure", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "snow_water_equivalent", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "shortwave_radiation_flux", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "soil_moisture", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "runoff", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "precipitation", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "potential_evaporation", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "climatic_water_deficit", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "water_evaporation", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "terraclimate", "palmer_drought_severity_index", NA, "1958-2022", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",

    ## SEEG

    "seeg", "seeg", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",
    "seeg", "seeg_farming", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",
    "seeg", "seeg_industry", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",
    "seeg", "seeg_energy", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",
    "seeg", "seeg_land", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",
    "seeg", "seeg_residuals", NA, "1970-2021", "Country, State, Municipality", "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx",

    ## Censo Agro

    "censoagro", "agricultural_land_area", "263", "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "agricultural_area_use", "264", "1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "agricultural_employees_tractors", "265", "1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "agricultural_producer_condition", "280", "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "animal_production", "281", "1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "animal_products", "282", "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "vegetable_production_area", "283", "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "vegetable_production_permanent", "1730", "1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "vegetable_production_temporary", "1731", "1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006", "Country, State", "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais",
    "censoagro", "livestock_production", "6907", "2017", "Municipality", "https://sidra.ibge.gov.br/tabela/6907",

    #################
    ## Social data ##
    #################

    ## IPS

    "ips", "all", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "life_quality", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "sanit_habit", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "violence", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "educ", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "communic", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "mortality", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",
    "ips", "deforest", NA, "2014, 2018, 2021, 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",

    ## DATASUS

    "datasus", "datasus_sim_do", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/$file_name$",
    "datasus", "datasus_sim_dofet", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_doext", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_doinf", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_domat", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_cnes_lt", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/$file_name$",
    "datasus", "datasus_cnes_st", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/$file_name$",
    "datasus", "datasus_cnes_dc", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/$file_name$",
    "datasus", "datasus_cnes_eq", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/$file_name$",
    "datasus", "datasus_cnes_sr", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/$file_name$",
    "datasus", "datasus_cnes_hb", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/$file_name$",
    "datasus", "datasus_cnes_pf", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/$file_name$",
    "datasus", "datasus_cnes_ep", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/$file_name$",
    "datasus", "datasus_cnes_rc", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/$file_name$",
    "datasus", "datasus_cnes_in", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/$file_name$",
    "datasus", "datasus_cnes_ee", NA, "2007-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/$file_name$",
    "datasus", "datasus_cnes_ef", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/$file_name$",
    "datasus", "datasus_cnes_gm", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/$file_name$",
    "datasus", "datasus_sih", NA, "2008-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/$file_name$",

    ## IEMA

    "iema", "iema", NA, "2018", "Municipality", "https://drive.google.com/uc?export=download&id=10JMRtzu3k95vl8cQmHkVMQ9nJovvIeNl",

    ## Population

    "population", "population", "6579", "2001-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/estimapop/tabelas",

    ###################
    ## Economic data ##
    ###################

    ## Comex

    "comex", "comex_export_prod", NA, "1997-2023", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_$year$.csv",
    "comex", "comex_import_prod", NA, "1997-2023", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_$year$.csv",
    "comex", "comex_export_mun", NA, "1997-2023", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_$year$_MUN.csv",
    "comex", "comex_import_mun", NA, "1997-2023", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_$year$_MUN.csv",

    ## BACI

    "baci", "HS92", NA, "1995-2022", "Country", "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/baci_HS92_V202401b.zip",

    ## PIB-Munic

    "pibmunic", "pibmunic", "5938", "2002-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas",

    ## CEMPRE

    "cempre", "cempre", "6449", "2006-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/cempre/tabelas",

    ## PAM

    "pam", "all_crops", "5457/all/all", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "permanent_crops", "1613/all/all", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "temporary_crops", "1612/all/all", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "corn", "839/all/all", "2003-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "potato", "1001/all/all", "2003-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "peanut", "1000/all/all", "2003-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "beans", "1002/all/all", "2003-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    # Categories within temporary crops

    "pam", "temporary_total", "1612/c81/0", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "pineapple", "1612/c81/2688", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "alfafa", "1612/c81/40471", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cotton_herbaceous", "1612/c81/2689", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "garlic", "1612/c81/2690", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "peanut_temporary", "1612/c81/2691", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "rice", "1612/c81/2692", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "oats", "1612/c81/2693", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "sweet_potato", "1612/c81/2694", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "potato_temporary", "1612/c81/2695", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "sugar_cane", "1612/c81/2696", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "forage_cane", "1612/c81/40470", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "onion", "1612/c81/2697", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "rye", "1612/c81/2698", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "barley", "1612/c81/2699", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "pea", "1612/c81/2700", "1988-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "broad_bean", "1612/c81/2701", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "beans_temporary", "1612/c81/2702", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "tobacco", "1612/c81/2703", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "sunflower_seeds", "1612/c81/109179", "2005-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "jute_fiber", "1612/c81/2704", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "linen_seeds", "1612/c81/2705", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "malva_fiber", "1612/c81/2706", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "castor_bean", "1612/c81/2707", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cassava", "1612/c81/2708", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "watermelon", "1612/c81/2709", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "melon", "1612/c81/2710", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "corn_temporary", "1612/c81/2711", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "ramie_fiber", "1612/c81/2712", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "soybean", "1612/c81/2713", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "sorghum", "1612/c81/2714", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "tomato", "1612/c81/2715", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "wheat", "1612/c81/2716", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "triticale", "1612/c81/109180", "2005-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    # Categories within permanent crops

    "pam", "permanent_total", "1613/c82/0", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "avocado", "1613/c82/2717", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cotton_arboreo", "1613/c82/2718", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "acai", "1613/c82/45981", "2015-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "olive", "1613/c82/2719", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "banana", "1613/c82/2720", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "rubber_coagulated_latex", "1613/c82/2721", "1981-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "rubber_liquid_latex", "1613/c82/40472", "1981-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cocoa_beans", "1613/c82/2722", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "coffee_total", "1613/c82/2723", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "coffee_arabica", "1613/c82/31619", "2012-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "coffee_canephora", "1613/c82/31620", "2012-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cashew", "1613/c82/40473", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "khaki", "1613/c82/2724", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "cashew_nut", "1613/c82/2725", "1988-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "india_tea", "1613/c82/2726", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "coconut", "1613/c82/2727", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "coconut_bunch", "1613/c82/2728", "1988-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "yerba_mate", "1613/c82/2729", "1981-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "fig", "1613/c82/2730", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "guava", "1613/c82/2731", "1988-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "guarana_seeds", "1613/c82/2732", "1981-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "orange", "1613/c82/2733", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "lemon", "1613/c82/2734", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "apple", "1613/c82/2735", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "papaya", "1613/c82/2736", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "mango", "1613/c82/2737", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "passion_fruit", "1613/c82/2738", "1988-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "quince", "1613/c82/2739", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "walnut", "1613/c82/2740", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "heart_of_palm", "1613/c82/90001", "1981-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "pear", "1613/c82/2741", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "peach", "1613/c82/2742", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "black_pepper", "1613/c82/2743", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "sisal_or_agave", "1613/c82/2744", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "tangerine", "1613/c82/2745", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "tung", "1613/c82/2746", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "annatto_seeds", "1613/c82/2747", "1981-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "pam", "grape", "1613/c82/2748", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    ## PEVS

    "pevs", "pevs_forest_crops", "289", "1986-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",
    "pevs", "pevs_silviculture", "291", "1986-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",
    "pevs", "pevs_silviculture_area", "5930", "2013-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",

    ## PPM

    "ppm", "ppm_livestock_inventory", "3939", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "ppm", "ppm_sheep_farming", "95", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "ppm", "ppm_animal_origin_production", "74", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "ppm", "ppm_cow_farming", "94", "1974-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "ppm", "ppm_aquaculture", "3940", "2013-2023", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",

    ## SIGMINE

    "sigmine", "sigmine_active", NA, NA, NA, "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip",

    ## ANEEL

    "aneel", "energy_development_budget", NA, "2013-2022", NA, "https://drive.google.com/file/d/1h7mu-9qbKfISk1-k4JSrBhXKBMQHTOH9/view?usp=share_link",
    "aneel", "energy_generation", NA, "1908-2021", "Municipality", "https://git.aneel.gov.br/publico/centralconteudo/-/raw/main/relatorioseindicadores/geracao/BD_SIGA.xlsx?inline=false",
    "aneel", "energy_enterprises_distributed", NA, NA, NA, "https://dadosabertos.aneel.gov.br/dataset/5e0fafd2-21b9-4d5b-b622-40438d40aba2/resource/b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/empreendimento-geracao-distribuida.csv",

    ## EPE

    "epe", "energy_consumption_per_class", NA, "2004-2021", "Region, Subsystem, State", "https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/publicacoes/Documents/CONSUMO%20MENSAL%20DE%20ENERGIA%20EL%c3%89TRICA%20POR%20CLASSE.xls",
    "epe", "national_energy_balance", NA, "2011-2022", NA, "https://drive.google.com/file/d/1_JTYyAPdbQayR-nrURts6OmbKcm2cLix/view?usp=share_link",

    ## Shapefile from github repository


    "internal", "geo_municipalities", NA, "2020", "Municipality", "https://raw.github.com/datazoompuc/datazoom.amazonia/master/data-raw/geo_municipalities.rds",
  )

  # returns only the desired rows

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
      purrr::pluck("link")
  }

  return(link)
}
