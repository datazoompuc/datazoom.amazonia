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

    if (year == 2007) param$sidra_code <- 793  # https://sidra.ibge.gov.br/tabela/793
    if (year == 2010){
      param$sidra_code <- 1378 # https://sidra.ibge.gov.br/tabela/1378

      param$classific <- "c1"
      param$category <- list(0)
    }

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

  ## Create Basic Url

  dat_url <- datasets_link()

  param$url <- dat_url %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    base::unlist() %>%
    as.character()

  #####################
  ## Construct Links ##
  #####################

  # For most sources, the URL in datasets_link is already the URL needed for the download

  path <- param$url

  # Below are the exceptions, for which manipulation is needed

  ##### Exceptions only #####

  # If the datasets_link URL is the download path you need,
  # do not change this section for a new function

  ## Comex

  # Download path depends on the dataset and year

  if (source == "comex") {
    if (dataset == "comex_export_mun") {
      path <- paste(param$url, "/EXP_", param$year, "_MUN.csv", sep = "")
    }
    if (dataset == "comex_import_mun") {
      path <- paste(param$url, "/IMP_", param$year, "_MUN.csv", sep = "")
    }
    if (dataset == "comex_export_prod") {
      path <- paste(param$url, "/EXP_", param$year, ".csv", sep = "")
    }
    if (dataset == "comex_import_prod") {
      path <- paste(param$url, "/IMP_", param$year, ".csv", sep = "")
    }
  }

  ## Prodes

  # Download path depends on the year

  if (source == "prodes") {
    path <- paste(param$url, "/tabelatxt.php?ano=", param$year, "&estado=&ordem=MUNICIPIO&type=tabela&output=txt&", sep = "")
  }


  ## Degrad

  # Download path depends on the year

  if (source == "degrad") {
    if (dataset == "degrad") {
      path <- paste(param$url, "/arquivos/degrad", param$year, "_final_shp.zip", sep = "")
    }
  }

  ## MapBiomas

  # Download path depends on dataset and geo_level

  if (source == "mapbiomas") {
    if (dataset %in% c("mapbiomas_cover", "mapbiomas_transition")) {
      if (param$geo_level == "state") {
        path <- paste(param$url, "Estat%C3%ADsticas/1_-_TABELA_GERAL_COL7_MAPBIOMAS_BIOMAS_UF_FINAL.xlsx", sep = "")
      }
      if (param$geo_level == "municipality") {
        path <- "https://storage.googleapis.com/mapbiomas-public/brasil/downloads/1-tabela-geral-col7-mapbiomas-biomas-municipio-final.xlsx"
      }
    }
    if (dataset == "mapbiomas_deforestation_regeneration") {
      path <- paste(param$url, "Estat%C3%ADsticas/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_irrigation") {
      path <- paste(param$url, "downloads/Estatisticas%20/Colecao_7_Irrigacao_Biomes_UF.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_grazing_quality") {
      path <- paste(param$url, "Estat%C3%ADsticas/MapBIomas_COL5_QUALIDADE_PASTAGEM-biomas-estados-SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_mining") {
      path <- "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/09/TABELA-MINERACAO-MAPBIOMAS-COL8.0.xlsx"
    }
    if (dataset == "mapbiomas_water") {
      path <- paste(param$url, "Estat%C3%ADsticas/Estatisticas_Superficie%C3%81gua_Col2_SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_fire") {
      path <- paste(param$url, "Estat%C3%ADsticas/mapbiomas-FIRE-ANUAL_Biome_UF_city-SITE.xlsx", sep = "")
    }
  }

  ## SEEG

  # Download path depends on geo_level

  if (source == "seeg") {
    if (geo_level == "municipality") {
      path <- "https://drive.google.com/u/0/uc?confirm=bhfS&id=1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd"
    }
    if (geo_level == "state" | geo_level == "country") {
      path <- "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG10/1-SEEG10_GERAL-BR_UF_2022.10.27-FINAL-SITE.xlsx"
    }
  }

  ## IBAMA

  # Download path depends on state

  if (source == "ibama") {
    if (dataset == "embargoed_areas") {
      path <- param$url
    } else if (dataset == "distributed_fines") {
      path <- paste0(param$url, param$state, "/Quantidade/multasDistribuidasBensTutelados.csv")
    } else if (dataset == "collected_fines") {
      path <- paste0(param$url, param$state, "/Arrecadacao/arrecadacaobenstutelados.csv")
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

  ## Datasus

  # Download path depends on the specific file wanted

  if (source == "datasus") {
    path <- paste0(param$url, param$file_name)
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

  if (source == "prodes") {
    file_extension <- ".txt"
  }
  if (source %in% c("seeg", "iema", "ips")) {
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
  if (source == "imazon_shp") {
    file_extension <- ".rds"
  }
  if (source == "EPE") {
    if (param$dataset == "national_energy_balance") {
      file_extension <- ".csv"
    }
  }
  if (source == "ANEEL") {
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

  if (source %in% c("iema", "imazon_shp")) {
    download_method <- "googledrive"
  }
  if (source == "ANEEL") {
    if (dataset == "energy_development_budget") {
      download_method <- "googledrive"
    }
    if (dataset == "energy_enterprises_distributed") {
      message("This may take a while.\n")
      options(timeout = 1000) # increase timeout limit
    }
  }
  if (source == "EPE") {
    if (dataset == "national_energy_balance") {
      download_method <- "googledrive"
    }
  }
  if (source %in% c("deter", "terraclimate", "baci")) {
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
      if (param$dataset == "deter_amz"){
        dat <- sf::read_sf(file.path(dir, "deter-amz-deter-public.shp"))
      }
      if (param$dataset == "deter_cerrado"){
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

      # as year can be a vector, sets up expressions of the form "*YYYY_V202201.csv" for each year to match file names
      file_expression <- paste0("*", param$year, "_V202201.csv")

      # now turning into *XXXX_V202201.csv|YYYY_V202201.csv|ZZZZ_V202201.csv" to match as regex
      file_expression <- paste0(file_expression, collapse = "|")

      file <- list.files(dir, pattern = file_expression, full.names = TRUE) %>%
        as.list()

      # now reads each file
      dat <- lapply(file, data.table::fread, header = TRUE, sep = ",")

      # each data frame in the list is named after the corresponding year
      names(dat) <- param$year
    }

  }

  if (param$source == "EPE" & param$dataset == "energy_consumption_per_class") {

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
  if (param$source == "ANEEL" & param$dataset == "energy_enterprises_distributed") {
    dat <- data.table::fread(temp, encoding = "Latin-1")
  }
  if (param$source == "ips") {
    dat <- param$sheet %>%
      purrr::map(
        ~ readxl::read_xlsx(temp, sheet = .),
      download_method <- "googledrive")
  }

  ## Now the rest of the functions

  # This Depends on Data Type (.csv, .shp, ...) and on the data source

  else {
    if (file_extension == ".csv") {
      dat <- data.table::fread(temp)
    }
    if (file_extension == ".txt") {
      dat <- readr::read_csv(temp, locale = readr::locale(encoding = "latin1"))
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

datasets_link <- function() {

  ## Add file type at the end in order to set the Curl Process

  link <- tibble::tribble(
    ~survey, ~dataset, ~sidra_code, ~available_time, ~available_geo, ~link,
    "PAM-IBGE", "all_crops", "5457/all/all", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "permanent_crops", "1613/all/all", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "temporary_crops", "1612/all/all", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "corn", "839/all/all", "2003-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "potato", "1001/all/all", "2003-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peanut", "1000/all/all", "2003-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "beans", "1002/all/all", "2003-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    ### Categories within temporary crops

    "PAM-IBGE", "temporary_total", "1612/c81/0", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pineapple", "1612/c81/2688", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "alfafa", "1612/c81/40471", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cotton_herbaceous", "1612/c81/2689", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "garlic", "1612/c81/2690", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peanut_temporary", "1612/c81/2691", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rice", "1612/c81/2692", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "oats", "1612/c81/2693", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sweet_potato", "1612/c81/2694", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "potato_temporary", "1612/c81/2695", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sugar_cane", "1612/c81/2696", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "forage_cane", "1612/c81/40470", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "onion", "1612/c81/2697", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rye", "1612/c81/2698", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "barley", "1612/c81/2699", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pea", "1612/c81/2700", "1988-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "broad_bean", "1612/c81/2701", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "beans_temporary", "1612/c81/2702", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tobacco", "1612/c81/2703", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sunflower_seeds", "1612/c81/109179", "2005-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "jute_fiber", "1612/c81/2704", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "linen_seeds", "1612/c81/2705", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "malva_fiber", "1612/c81/2706", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "castor_bean", "1612/c81/2707", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cassava", "1612/c81/2708", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "watermelon", "1612/c81/2709", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "melon", "1612/c81/2710", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "corn_temporary", "1612/c81/2711", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "ramie_fiber", "1612/c81/2712", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "soybean", "1612/c81/2713", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sorghum", "1612/c81/2714", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tomato", "1612/c81/2715", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "wheat", "1612/c81/2716", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "triticale", "1612/c81/109180", "2005-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    ### Categories within permanent crops

    "PAM-IBGE", "permanent_total", "1613/c82/0", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "avocado", "1613/c82/2717", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cotton_arboreo", "1613/c82/2718", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "acai", "1613/c82/45981", "2015-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "olive", "1613/c82/2719", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "banana", "1613/c82/2720", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rubber_coagulated_latex", "1613/c82/2721", "1981-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rubber_liquid_latex", "1613/c82/40472", "1981-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cocoa_beans", "1613/c82/2722", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_total", "1613/c82/2723", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_arabica", "1613/c82/31619", "2012-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_canephora", "1613/c82/31620", "2012-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cashew", "1613/c82/40473", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "khaki", "1613/c82/2724", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cashew_nut", "1613/c82/2725", "1988-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "india_tea", "1613/c82/2726", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coconut", "1613/c82/2727", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coconut_bunch", "1613/c82/2728", "1988-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "yerba_mate", "1613/c82/2729", "1981-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "fig", "1613/c82/2730", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "guava", "1613/c82/2731", "1988-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "guarana_seeds", "1613/c82/2732", "1981-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "orange", "1613/c82/2733", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "lemon", "1613/c82/2734", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "apple", "1613/c82/2735", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "papaya", "1613/c82/2736", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "mango", "1613/c82/2737", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "passion_fruit", "1613/c82/2738", "1988-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "quince", "1613/c82/2739", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "walnut", "1613/c82/2740", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "heart_of_palm", "1613/c82/90001", "1981-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pear", "1613/c82/2741", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peach", "1613/c82/2742", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "black_pepper", "1613/c82/2743", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sisal_or_agave", "1613/c82/2744", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tangerine", "1613/c82/2745", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tung", "1613/c82/2746", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "annatto_seeds", "1613/c82/2747", "1981-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "grape", "1613/c82/2748", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    #########
    ## PPM ##
    #########

    # Livestock

    "PPM-IBGE", "ppm_livestock_inventory", "3939", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "PPM-IBGE", "ppm_sheep_farming", "95", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "PPM-IBGE", "ppm_animal_origin_production", "74", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "PPM-IBGE", "ppm_cow_farming", "94", "1974-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",
    "PPM-IBGE", "ppm_aquaculture", "3940", "2013-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021",

    ##########
    ## PEVS ##
    ##########

    ## Vegetal Extraction

    "PEVS-IBGE", "pevs_forest_crops", "289", "1986-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",
    "PEVS-IBGE", "pevs_silviculture", "291", "1986-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",
    "PEVS-IBGE", "pevs_silviculture_area", "5930", "2013-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019",

    ###########
    ## COMEX ##
    ###########

    "COMEX-EXP-PROD_NCM", "comex_export_prod", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm",
    "COMEX-IMP-PROD_NCM", "comex_import_prod", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm",
    "COMEX-EXP-MUNIC_FIRM", "comex_export_mun", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun",
    "COMEX-IMP-MUNIC_FIRM", "comex_import_mun", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun",

    ##########
    ## INPE ##
    ##########

    "PRODES-INPE", "prodes", NA, "2000-2020", NA, "http://www.dpi.inpe.br/prodesdigital",
    "DETER-INPE", "deter_amz", NA, NA, NA, "http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-amz/shape",
    "DETER-INPE", "deter_cerrado", NA, NA, NA, "http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-cerrado/shape",
    "DEGRAD-INPE", "degrad", NA, "2007-2016", NA, "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad",

    ###############
    ## MapBiomas ##
    ###############

    "MAPBIOMAS", "mapbiomas_cover", NA, "1985-2019", "Municipality, State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_transition", NA, "1985-2019", "Municipality, State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_deforestation_regeneration", NA, "1988-2017", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_irrigation", NA, "2000-2019", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_grazing_quality", NA, "2010 & 2018", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_mining", NA, "1985-2020", "Country, State, Municipality, Biome, Indigenous", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_fire", NA, "1985-2020", "State, Municipality", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_water", NA, "1985-2022", "State, Municipality, Biome", "https://mapbiomas-br-site.s3.amazonaws.com/",

    #############
    ## SIGMINE ##
    #############

    "ANM-SIGMINE", "sigmine_active", NA, NA, NA, "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip",

    ##########
    ## SEEG ##
    ##########

    "SEEG", "seeg", NA, NA, "Country, State, Municipality", "http://seeg.eco.br/download",

    #########
    ## IPS ##
    #########

    "IPS", "ips", NA, "2014 and/or 2018 and/or 2021 and/or 2023", NA, "https://docs.google.com/uc?export=download&id=1ABcLZFraSd6kELHW-pZgpy7ITzs1JagN&format=xlsx",

    ###########
    ## IBAMA ##
    ###########

    "IBAMA", "embargoed_areas", NA, NA, "Municipality", "https://servicos.ibama.gov.br/ctf/publico/areasembargadas/downloadListaAreasEmbargadas.php",
    "IBAMA", "distributed_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/",
    "IBAMA", "collected_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/",

    #################################################################
    ## Other Economics Datasets IBGE - GDP Munic, Employment, Wage ##
    #################################################################

    ## Municipal GDP ##

    "PIB_MUNIC-IBGE", "pibmunic", "5938", "2002-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas",

    ## Labor Market Info ##

    "CEMPRE-IBGE", "cempre", "6449", "2006-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/cempre/tabelas",

    ## Population ##

    "POPULATION-IBGE", "population", "6579", "2001-2021", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/estimapop/tabelas",

    ######################################
    ## Censo Agropecuario - Time Series ##
    ######################################

    "CENSO_AGRO-IBGE" , "land_area_total" , "263" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "area_use" , "264" , "1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "employess_tractors" , "265" , "1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "land_area_producer_condition" , "280" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "animal_specie_production" , "281" , "1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "animal_production_type" , "282" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO_AGRO-IBGE" , "vegetable_production_area_type" , "283" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "land_area_total_mean" , "1030" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "use_type" , "1031" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "employess_total_mean" , "1032" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais"  ,
    "CENSO-AGRO-IBGE" , "tractors_total_mean" , "1033" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "bovine_area_mean" , "1034" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "animal_herd_type" , "1035" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "income_mean_vegetable_type" , "1250" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "vegetable_area_income_coffee_orange" , "1251" , "1920, 1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "production_permanent_crops" , "1730" , "1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,
    "CENSO-AGRO-IBGE" , "production_temporary_crops" , "1731" , "1940, 1950, 1960, 1970, 1975, 1980, 1985, 1995, 2006" , "Country, State, Municipality" , "https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/series-temporais" ,

    ##################
    ## TerraClimate ##
    ##################

    "TerraClimate", "max_temperature", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "min_temperature", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "wind_speed", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "vapor_pressure_deficit", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "vapor_pressure", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "snow_water_equivalent", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "shortwave_radiation_flux", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "soil_moisture", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "runoff", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "precipitation", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "potential_evaporation", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "climatic_water_deficit", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "water_evaporation", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",
    "TerraClimate", "palmer_drought_severity_index", NA, "1958-2020", "Municipality", "http://thredds.northwestknowledge.net:8080/thredds/ncss",

    #####################
    ## Health Datasets ##
    #####################

    "DATASUS", "datasus_sim_do", NA, "1996-2020", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
    "DATASUS", "datasus_sim_dofet", NA, "1996-2020", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
    "DATASUS", "datasus_sim_doext", NA, "1996-2020", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
    "DATASUS", "datasus_sim_doinf", NA, "1996-2020", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
    "DATASUS", "datasus_sim_domat", NA, "1996-2020", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
    "DATASUS", "datasus_cnes_lt", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/",
    "DATASUS", "datasus_cnes_st", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/",
    "DATASUS", "datasus_cnes_dc", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/",
    "DATASUS", "datasus_cnes_eq", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/",
    "DATASUS", "datasus_cnes_sr", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/",
    "DATASUS", "datasus_cnes_hb", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/",
    "DATASUS", "datasus_cnes_pf", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/",
    "DATASUS", "datasus_cnes_ep", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/",
    "DATASUS", "datasus_cnes_rc", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/",
    "DATASUS", "datasus_cnes_in", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/",
    "DATASUS", "datasus_cnes_ee", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE",
    "DATASUS", "datasus_cnes_ef", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/",
    "DATASUS", "datasus_cnes_gm", NA, "2005-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/",
    "DATASUS", "datasus_sih", NA, "2008-2022", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/",

    ##########
    ## IEMA ##
    ##########

    "IEMA", "iema", NA, "2018", "Municipality", "https://drive.google.com/uc?export=download&id=10JMRtzu3k95vl8cQmHkVMQ9nJovvIeNl",

    ##########
    ## SEEG ##
    ##########

    "SEEG", "seeg_farming", NA, "1970-2021", "Country, State, Municipality", "http://seeg.eco.br/download",
    "SEEG", "seeg_industry", NA, "1970-2021", "Country, State, Municipality", "http://seeg.eco.br/download",
    "SEEG", "seeg_energy", NA, "1970-2021", "Country, State, Municipality", "http://seeg.eco.br/download",
    "SEEG", "seeg_land", NA, "1970-2021", "Country, State, Municipality", "http://seeg.eco.br/download",
    "SEEG", "seeg_residuals", NA, "1970-2021", "Country, State, Municipality", "http://seeg.eco.br/download",

    ##########
    ## BACI ##
    ##########

    "BACI", "HS92", NA, "1995-2020", "Country", "http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202201.zip",

    ############
    ## IMAZON ##
    ############

    "Imazon", "imazon_shp", NA, "2020", "Municipality", "https://docs.google.com/uc?export=download&id=1JHc2J_U8VXHVuWVsi8wVBnNzZ37y1ehv",

    #########
    ## EPE ##
    #########

    "EPE", "energy_consumption_per_class", NA, "2004-2021", "Region, Subsystem, State", "https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/publicacoes/Documents/CONSUMO%20MENSAL%20DE%20ENERGIA%20EL%c3%89TRICA%20POR%20CLASSE.xls",
    "EPE", "national_energy_balance", NA, "2011-2022", "Region, Municipality", "https://drive.google.com/file/d/1_JTYyAPdbQayR-nrURts6OmbKcm2cLix/view?usp=share_link",

    ###########
    ## ANEEL ##
    ###########

    "ANEEL", "energy_development_budget", NA, "2013-2022", NA, "https://drive.google.com/file/d/1h7mu-9qbKfISk1-k4JSrBhXKBMQHTOH9/view?usp=share_link",
    "ANEEL", "energy_generation", NA, "1908-2021", "Municipality", "https://git.aneel.gov.br/publico/centralconteudo/-/raw/main/relatorioseindicadores/geracao/BD_SIGA.xlsx?inline=false",
    "ANEEL", "energy_enterprises_distributed", NA, NA, NA, "https://dadosabertos.aneel.gov.br/dataset/5e0fafd2-21b9-4d5b-b622-40438d40aba2/resource/b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/empreendimento-geracao-distribuida.csv",

    ## Shapefile from github repository


    "Internal", "geo_municipalities", NA, "2020", "Municipality", "https://raw.github.com/datazoompuc/datazoom.amazonia/master/data-raw/geo_municipalities.rds",
  )

  return(link)
}
