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

  ##################################
  ## Get Geographical Information ##
  ##################################

  geo <- municipalities %>%
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

  # Need to Check Mapbiomas, Prodes and Comex

  # ----------------------
  # To Do:
  # - Insert Error Message when user gives a wrong input
  # ----------------------

  ## Define Basic Parameters

  param <- list()
  param$dataset <- dataset
  param$source <- source
  param$year <- year # This may not make sense if data is downloaded for all time periods together
  param$geo_level <- geo_level # This could also not make sense
  param$coords <- coords
  param$dataset_code <- dataset_code
  param$skip_rows <- skip_rows
  param$file_name <- file_name
  param$sheet <- sheet
  param$state <- state

  # if (param$geo_level == 'legal_amazon' & param$source == 'prodes'){param$geo_level = 'legal-amz-prodes'}
  # if (param$geo_level == 'amazon_biome' & param$source == 'prodes'){param$geo_level = 'amz-prodes'}

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

  ###########
  ## Comex ##
  ###########

  # 2014 Examples
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2014.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncmv2/IMP_2014_V2.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2014_MUN.csv
  # https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2014_MUN.csv


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

  ############
  ## Prodes ##
  ############

  # http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=2020&estado=&ordem=MUNICIPIO&type=tabela&output=txt&

  if (source == "prodes") {
    path <- paste(param$url, "/tabelatxt.php?ano=", param$year, "&estado=&ordem=MUNICIPIO&type=tabela&output=txt&", sep = "")
  }

  # Create a previous if for termination of file (vector .shp)

  # if (source == 'prodes'){
  #   if (dataset == 'prodes_accum_deforestation'){path = paste(param$url,param$geo_level,'vector/accumulated_deforestation_1988_2007.zip',sep='/')}
  #   if (dataset == 'prodes_annual_increase_deforestation'){path = paste(param$url,param$geo_level,'vector/yearly_deforestation.zip',sep='/')}
  #   if (dataset == 'prodes_cloud'){path = paste(param$url,param$geo_level,'vector/cloud.zip',sep='/')}
  #   if (dataset == 'prodes_forest'){path = paste(param$url,param$geo_level,'vector/forest.zip',sep='/')}
  #   if (dataset == 'prodes_hydrography'){path = paste(param$url,param$geo_level,'vector/hydrography.zip',sep='/')}
  #   if (dataset == 'prodes_not_forest'){path = paste(param$url,param$geo_level,'vector/no_forest.zip',sep='/')}
  # }

  ###########
  ## Deter ##
  ###########

  # # Amazonia Legal = legal-amz-prodes
  # Bioma Amazonia = amz-prodes

  if (source == "deter") {
    path <- paste(param$url, stringr::str_replace(param$dataset, "_", "-"), "/shape", sep = "")
  }

  ############
  ## Degrad ##
  ############

  if (source == "degrad") {
    if (dataset == "degrad") {
      path <- paste(param$url, "/arquivos/degrad", param$year, "_final_shp.zip", sep = "")
    }
  }

  ###############
  ## MapBiomas ##
  ###############

  # https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx
  # https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx
  # https://storage.googleapis.com/mapbiomas-public/COLECAO/5/DOWNLOADS/ESTATISTICAS/Dados_Transicao_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx
  # https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/BD-DESM_e_REG_COL5_V8h__SITE.xlsx
  # https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/MapBIomas_COL5_IRRIGACAO-biomas-estados-SITE.xlsx
  # https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/MapBIomas_COL5_QUALIDADE_PASTAGEM-biomas-estados-SITE.xlsx


  if (source == "mapbiomas") {
    if (dataset == "mapbiomas_cover") {
      path <- paste(param$url, "Estat%C3%ADsticas/Cole%C3%A7%C3%A3o%206/1-ESTATISTICAS_MapBiomas_COL6.0_UF-BIOMAS_v12_SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_transition") {
      path <- "https://storage.googleapis.com/mapbiomas-public/COLECAO/5/DOWNLOADS/ESTATISTICAS/Dados_Transicao_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx"
    }
    if (dataset == "mapbiomas_deforestation_regeneration") {
      path <- paste(param$url, "Estat%C3%ADsticas/BD-DESM_e_REG_COL5_V8h__SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_irrigation") {
      path <- paste(param$url, "Estat%C3%ADsticas/MapBIomas_COL5_IRRIGACAO-biomas-estados-SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_grazing_quality") {
      path <- paste(param$url, "Estat%C3%ADsticas/MapBIomas_COL5_QUALIDADE_PASTAGEM-biomas-estados-SITE.xlsx", sep = "")
    }
    if (dataset == "mapbiomas_mining") {
      path <- paste(param$url, "Estat%C3%ADsticas/Cole%C3%A7%C3%A3o%206/Colecao_6_Mining_BR_UF_Biome_Mun_TI_SITE.xlsx", sep = "")
    }
  }

  #############
  ## SIGMINE ##
  #############

  if (source == "sigmine") {
    if (dataset == "sigmine_active") {
      path <- paste(param$url, "SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip", sep = "")
    }
  }

  ##########
  ## SEEG ##
  ##########

  if (source == "seeg") {
    if (geo_level == "municipality") {
      path <- "https://drive.google.com/u/0/uc?confirm=bhfS&id=1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd"
    }
    if (geo_level == "state" | geo_level == "country") {
      path <- "https://seeg-br.s3.amazonaws.com/Estat%C3%ADsticas/SEEG9/1-SEEG9_GERAL-BR_UF_2021.10.26_-_SITE.xlsx"
    }
  }

  #########
  ## IPS ##
  #########

  if (source == "ips") {
    path <- paste(param$url, "/assets/IPS_Tabela_Completa-1df30fcea79209e3c7e8634a586f95e6.xlsx", sep = "")
  }

  ###########
  ## IBAMA ##
  ###########

  if (source == "ibama") {
    if (dataset == "areas_embargadas") {
      path <- param$url
    } else if (dataset == "distributed_fines") {
      path <- paste0(param$url, param$state, "/Quantidade/multasDistribuidasBensTutelados.csv")
    } else if (dataset == "collected_fines") {
      path <- paste0(param$url, param$state, "/Arrecadacao/arrecadacaobenstutelados.csv")
    }
  }

  ##################
  ## TerraClimate ##
  ##################

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

  #################
  ## Health Data ##
  #################

  if (source == "health") {
    if (dataset == "ibge_mortality_table") {
      path <- param$url
    }
    if (stringr::str_detect(dataset, "datasus")) {
      path <- paste0(param$url, param$file_name)
    }
  }

  #####################
  ## IEMA            ##
  #####################

  if (source == "iema") {
    if (dataset == "iema") {
      path <- param$url
    }
  }

  #####################
  ## GeoBR Shapefile ##
  #####################

  if (source == "internal") {
    if (dataset == "geo_municipalities") {
      path <- param$url
    }
  }

  #######################
  ## Initiate Download ##
  #######################

  ## We should be careful when the downloaded files is terminated in .xlsx

  file_extension <- stringr::str_sub(path, -4)
  if (source == "mapbiomas") {
    file_extension <- ".xlsx"
  }
  if (source == "ips") {
    file_extension <- ".xlsx"
  }
  if (source == "prodes") {
    file_extension <- ".txt"
  }
  if (source == "deter") {
    file_extension <- ".zip"
  }
  if (source == "seeg") {
    file_extension <- ".xlsx"
  }
  if (source == "ibama") {
    if (dataset == "areas_embargadas") {
      file_extension <- ".zip"
    } else {
      file_extension <- ".csv"
    }
  }
  if (source == "terraclimate") {
    file_extension <- ".nc"
  }
  if (source == "internal") {
    file_extension <- ".rds"
  }
  if (source == "health") {
    if (stringr::str_detect(dataset, "datasus")) {
      file_extension <- ".dbc"
    }
  }
  if (source == "iema") {
    file_extension <- ".xlsx"
  }

  # !!!  We should Change This to a Curl Process

  ## Define Empty Directory and Files For Download

  dir <- tempdir()
  temp <- tempfile(fileext = file_extension, tmpdir = dir)

  ## Extraction through Curl Requests
  ## Investigate a bit more on how Curl Requests work

  if (source %in% c("comex", "degrad", "internal", "ips", "mapbiomas", "prodes", "sigmine")) {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }
  if (source == "iema") {
    googledrive::drive_download(path, path = temp, overwrite = TRUE)
  }
  if (source == "deter") {
    proc <- RCurl::CFILE(temp, mode = "wb")
    RCurl::curlPerform(url = path, writedata = proc@ref, noprogress = FALSE)
    RCurl::close(proc)
  }
  if (source == "seeg") {
    if (geo_level == "state" | geo_level == "country") {
      utils::download.file(url = path, destfile = temp, mode = "wb")
    }
    if (geo_level == "municipality") {
      googledrive::drive_download(path, path = temp, overwrite = TRUE)
    }
  }
  if (source %in% c("terraclimate", "ibama")) {
    utils::download.file(url = path, destfile = temp, method = "curl", quiet = TRUE)
  }
  if (source == "health") {
    if (stringr::str_detect(dataset, "datasus")) {
      utils::download.file(url = path, destfile = temp, method = "curl", quiet = TRUE)
    } else {
      utils::download.file(url = path, destfile = temp, mode = "wb")
    }
  }

  ## This Data Opening Part Should Include the .Shp, not DBF

  if (file_extension == ".zip") {
    utils::unzip(temp, exdir = dir)
  }

  ###############
  ## Load Data ##
  ###############

  # This Depends on Data Type (.csv, .shp, ...) and on datasource

  # df = sf::read_sf(paste(dir, "deter_public.shp", sep = "/"))

  if (file_extension == ".csv") {
    dat <- data.table::fread(temp) %>% tibble::as_tibble()
  }
  if (file_extension == ".txt") {
    dat <- readr::read_csv(temp, locale = readr::locale(encoding = "latin1")) %>%
      janitor::clean_names() %>%
      tibble::as_tibble()
  }
  if (file_extension == ".nc") {
    dat <- terra::rast(temp)
  }
  if (file_extension == ".rds") {
    dat <- readr::read_rds(temp)
  }
  if (file_extension == ".xlsx") {
    dat <- readxl::read_xlsx(temp, sheet = param$sheet)

    if (param$source == "seeg") {
      if (geo_level == "country") {
        dat <- readxl::read_excel(temp, sheet = "GEE Brasil")
      }
      if (geo_level == "state") {
        dat <- readxl::read_excel(temp, sheet = "GEE Estados")
      }
      if (geo_level == "municipality") {
        dat <- readxl::read_excel(temp, sheet = "BD GEE Municipios GWP-AR5")
      }
    }

    if (param$dataset == "ips") {
      if (param$year == 2014) {
        dat <- readxl::read_excel(temp, sheet = "IPS 2014")
      }
      if (param$year == 2018) {
        dat <- readxl::read_excel(temp, sheet = "IPS 2018 ")
      }
      if (param$year == 2021) {
        dat <- readxl::read_excel(temp, sheet = "IPS 2021")
      }
    }



    dat <- dat %>%
      janitor::clean_names() %>%
      tibble::as_tibble()
  }

  if (file_extension == ".zip") {
    if (param$dataset == "degrad") {
      if (param$year %in% 2007) {
        dat <- sf::read_sf(file.path(dir, "Degrad2007_Final_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2008) {
        dat <- sf::read_sf(file.path(dir, "Degrad2008_Final_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2009) {
        dat <- sf::read_sf(file.path(dir, "Degrad2009_Final_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2010) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2010_UF_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2011) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2011_INPE_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2012) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2012_INPE_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2013) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2013_INPE_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2014) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2014_pol.shp"))
        dat$year <- param$year
      }
      if (param$year == 2015) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2015.shp"))
        dat$year <- param$year
      }
      if (param$year == 2016) {
        dat <- sf::read_sf(file.path(dir, "DEGRAD_2016_pol.shp"))
        dat$year <- param$year
      }
    }

    if (param$source == "deter") {
      dat <- sf::read_sf(file.path(dir, "deter_public.shp")) %>%
        janitor::clean_names() %>%
        tibble::as_tibble()
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

      dat <- dataset[-c(1:5), ] %>%
        janitor::clean_names() %>%
        tibble::as_tibble()
    }
  }
  if (file_extension == ".dbc") {
    dat <- read.dbc(temp)
  }

  # if (source == 'prodes'){
  #   file = list.files(dir)[stringr::str_detect(list.files(dir),'.shp')]
  #   dat = sf::read_sf(paste(dir,file,sep='\\'))
  # }

  ##############################
  ## Excluding Temporary File ##
  ##############################

  # Folder is kept

  if (file_extension != ".nc") {
    unlink(temp)
  }

  ####################
  ## Pre-Processing ##
  ####################

  # Prodes and Txt need to have year extracted + encoding ajusted


  #################
  ## Return Data ##
  #################

  return(dat)
}

datasets_link <- function() {

  ## Add file type at the end in order to set the Curl Process

  link <- tibble::tribble(
    ~survey, ~dataset, ~sidra_code, ~available_time, ~available_geo, ~link,
    "PAM-IBGE", "all_crops", "5457/all/all", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "permanent_crops", "1613/all/all", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "temporary_crops", "1612/all/all", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "corn", "839/all/all", "2003-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "potato", "1001/all/all", "2003-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peanut", "1000/all/all", "2003-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "beans", "1002/all/all", "2003-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    ### Categories within temporary crops

    "PAM-IBGE", "temporary_total", "1612/c81/0", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pineapple", "1612/c81/2688", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "alfafa", "1612/c81/40471", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cotton_herbaceous", "1612/c81/2689", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "garlic", "1612/c81/2690", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peanut_temporary", "1612/c81/2691", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rice", "1612/c81/2692", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "oats", "1612/c81/2693", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sweet_potato", "1612/c81/2694", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "potato_temporary", "1612/c81/2695", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sugar_cane", "1612/c81/2696", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "forage_cane", "1612/c81/40470", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "onion", "1612/c81/2697", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rye", "1612/c81/2698", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "barley", "1612/c81/2699", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pea", "1612/c81/2700", "1988-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "broad_bean", "1612/c81/2701", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "beans_temporary", "1612/c81/2702", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tobacco", "1612/c81/2703", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sunflower_seeds", "1612/c81/109179", "2005-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "jute_fiber", "1612/c81/2704", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "linen_seeds", "1612/c81/2705", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "malva_fiber", "1612/c81/2706", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "castor_bean", "1612/c81/2707", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cassava", "1612/c81/2708", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "watermelon", "1612/c81/2709", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "melon", "1612/c81/2710", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "corn_temporary", "1612/c81/2711", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "ramie_fiber", "1612/c81/2712", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "soybean", "1612/c81/2713", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sorghum", "1612/c81/2714", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tomato", "1612/c81/2715", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "wheat", "1612/c81/2716", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "triticale", "1612/c81/109180", "2005-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    ### Categories within permanent crops

    "PAM-IBGE", "permanent_total", "1613/c82/0", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "avocado", "1613/c82/2717", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cotton_arboreo", "1613/c82/2718", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "acai", "1613/c82/45981", "2015-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "olive", "1613/c82/2719", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "banana", "1613/c82/2720", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rubber_coagulated_latex", "1613/c82/2721", "1981-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "rubber_liquid_latex", "1613/c82/40472", "1981-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cocoa_beans", "1613/c82/2722", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_total", "1613/c82/2723", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_arabica", "1613/c82/31619", "2012-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coffee_canephora", "1613/c82/31620", "2012-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cashew", "1613/c82/40473", "1974-1987", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "khaki", "1613/c82/2724", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "cashew_nut", "1613/c82/2725", "1988-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "india_tea", "1613/c82/2726", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coconut", "1613/c82/2727", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "coconut_bunch", "1613/c82/2728", "1988-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "yerba_mate", "1613/c82/2729", "1981-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "fig", "1613/c82/2730", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "guava", "1613/c82/2731", "1988-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "guarana_seeds", "1613/c82/2732", "1981-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "orange", "1613/c82/2733", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "lemon", "1613/c82/2734", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "apple", "1613/c82/2735", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "papaya", "1613/c82/2736", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "mango", "1613/c82/2737", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "passion_fruit", "1613/c82/2738", "1988-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "quince", "1613/c82/2739", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "walnut", "1613/c82/2740", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "heart_of_palm", "1613/c82/90001", "1981-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "pear", "1613/c82/2741", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "peach", "1613/c82/2742", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "black_pepper", "1613/c82/2743", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "sisal_or_agave", "1613/c82/2744", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tangerine", "1613/c82/2745", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "tung", "1613/c82/2746", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "annatto_seeds", "1613/c82/2747", "1981-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",
    "PAM-IBGE", "grape", "1613/c82/2748", "1974-2020", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pam/tabelas",

    #########
    ## PPM ##
    #########

    # Livestock

    "PPM-IBGE", "ppm_livestock_inventory", "3939", "1974-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019",
    "PPM-IBGE", "ppm_sheep_farming", "95", "1974-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019",
    "PPM-IBGE", "ppm_animal_origin_production", "74", "1974-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019",
    "PPM-IBGE", "ppm_cow_farming", "94", "1974-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019",
    "PPM-IBGE", "ppm_aquaculture", "3940", "2013-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019",

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

    # https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

    "COMEX-EXP-PROD_NCM", "comex_export_prod", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm",
    "COMEX-IMP-PROD_NCM", "comex_import_prod", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm",
    "COMEX-EXP-MUNIC_FIRM", "comex_export_mun", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun",
    "COMEX-IMP-MUNIC_FIRM", "comex_import_mun", NA, "1997-2021", NA, "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun",

    ##########
    ## INPE ##
    ##########

    # Todos os Biomas

    # We can include CAR as well

    # PRODES

    # http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/prodes
    # http://www.dpi.inpe.br/prodesdigital/prodesmunicipal.php
    # http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=2020&estado=&ordem=MUNICIPIO&type=tabela&output=txt&

    # Desmatamento Acumulado -http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/vector/accumulated_deforestation_1988_2007.zip
    # Floresta Anual - http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/vector/forest.zip
    # Hidrografia - http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/vector/hydrography.zip
    # Incremento Anual do Desmatamento - http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/vector/yearly_deforestation.zip
    # PRODES Completo - http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/raster/PDigital2000_2020_AMZ_raster_v20210521.zip
    # Nao Floresta - http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-prodes/vector/cloud.zip

    "PRODES-INPE", "prodes", NA, "2000-2020", NA, "http://www.dpi.inpe.br/prodesdigital",
    # 'PRODES-INPE','prodes_accum_deforestation',NA,'1988-2007',NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',
    # 'PRODES-INPE','prodes_forest',NA,'2016-2019',NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',
    # 'PRODES-INPE','prodes_hydrography',NA,NA,NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',
    # 'PRODES-INPE','prodes_annual_increase_deforestation',NA,'2008-2020',NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',
    # 'PRODES-INPE','prodes_cloud',NA,'2016-2020',NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',
    # 'PRODES-INPE','prodes_not_forest',NA,NA,NA,'http://terrabrasilis.dpi.inpe.br/download/dataset',


    ## Auxiliares

    # Estados - http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/states_amazon_biome.zip
    # Limite - http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/amazon_border.zip
    # Municipio Bioma Amazonia - http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/municipalities_amazon_biome.zip
    # Unidade Conservacao - http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/conservation_units_amazon_biome.zip
    # Area Indigena - http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/indigeneous_area_amazon_biome.zip

    # DETER (Somente Amazônia Legal e Cerrado)

    # DEGRAD is included here http://www.inpe.br/cra/projetos_pesquisas/deter.php

    # javascript: download('http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-amz/shape','file-download-1');
    # javascript: download('http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-cerrado/shape','file-download-2');

    "DETER-INPE", "deter_amz", NA, NA, NA, "http://terrabrasilis.dpi.inpe.br/file-delivery/download/",
    "DETER-INPE", "deter_cerrado", NA, NA, NA, "http://terrabrasilis.dpi.inpe.br/file-delivery/download/",

    # DEGRAD

    # "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad/arquivos/degrad",year,"_final_shp.zip"

    "DEGRAD-INPE", "degrad", NA, "2007-2016", NA, "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/degrad",

    ###############
    ## MapBiomas ##
    ###############

    "MAPBIOMAS", "mapbiomas_cover", NA, "1985-2019", "Municipality, State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_transition", NA, "1985-2019", "Municipality, State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_deforestation_regeneration", NA, "1988-2017", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_irrigation", NA, "2000-2019", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_grazing_quality", NA, "2010 & 2018", "State", "https://mapbiomas-br-site.s3.amazonaws.com/",
    "MAPBIOMAS", "mapbiomas_mining", NA, "1985-2020", "country, state, municipality, biome, indigenous", "https://mapbiomas-br-site.s3.amazonaws.com/",

    #############
    ## SIGMINE ##
    #############

    # Agencia Nacional de Mineracao (ANM)

    "ANM-SIGMINE", "sigmine_active", NA, NA, NA, "https://app.anm.gov.br/dadosabertos/",

    # https://dados.gov.br/dataset/sistema-de-informacoes-geograficas-da-mineracao-sigmine

    # Processos minerários ativos - Brasil
    # https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip
    # Processos minerários inativos - Brasil
    # https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL_INATIVOS.zip
    # Arrendamentos
    # https://app.anm.gov.br/dadosabertos/SIGMINE/ARRENDAMENTO.zip
    # Áreas de proteção de fonte
    # https://app.anm.gov.br/dadosabertos/SIGMINE/PROTECAO_FONTE.zip
    # Áreas de bloqueio
    # https://app.anm.gov.br/dadosabertos/SIGMINE/BLOQUEIO.zip
    # Reservas garimpeiras
    # https://app.anm.gov.br/dadosabertos/SIGMINE/RESERVAS_GARIMPEIRAS.zip


    ##########
    ## SEEG ##
    ##########

    "SEEG", "seeg", NA, NA, "Country, State, Municipality", "http://seeg.eco.br/download",

    # http://seeg.eco.br/download

    # UF - https://seeg-br.s3.amazonaws.com/2019-v7.0/download/1-SEEG8_GERAL-BR_UF_2020.11.05_-_SITE.xlsx
    # Municipios - https://drive.google.com/drive/folders/1S789njrMQCSJdnEjiOisk6VWy7eFwBfi?usp=sharing

    #########
    ## IPS ##
    #########

    #  http://www.ipsamazonia.org.br/assets/IPS_Tabela_Completa-8bb3b841e46c8fb17b0331d8ea92bef3.xlsx

    "IPS", "ips", NA, "2014 and/or 2018 and/or 2021", NA, "http://www.ipsamazonia.org.br",

    ###########
    ## IBAMA ##
    ###########

    # There is a lot to map, seem an incredible data source

    "IBAMA", "areas_embargadas", NA, NA, "Municipality", "https://servicos.ibama.gov.br/ctf/publico/areasembargadas/downloadListaAreasEmbargadas.php",
    "IBAMA", "distributed_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/",
    "IBAMA", "collected_fines", NA, NA, "Municipality", "https://dadosabertos.ibama.gov.br/dados/SICAFI/",
    # http://dadosabertos.ibama.gov.br/organization/instituto-brasileiro-do-meio-ambiente-e-dos-recursos-naturais-renovaveis


    #################################################################
    ## Other Economics Datasets IBGE - GDP Munic, Employment, Wage ##
    #################################################################

    ## Municipal GDP ##

    "PIB_MUNIC-IBGE", "pibmunic", "5938", "2002-2018", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas",

    ## Estimated Population ##

    # https://sidra.ibge.gov.br/pesquisa/estimapop/tabelas

    ## Labor Market Info ##

    "CEMPRE-IBGE", "cempre", "6449", "2006-2019", "Country, State, Municipality", "https://sidra.ibge.gov.br/pesquisa/cempre/tabelas",

    ## Demographic Info ##

    # https://sidra.ibge.gov.br/pesquisa/censo-demografico/series-temporais/series-temporais/

    ##########
    ## CIPÓ ##
    ##########

    "CIPO", "brazilian_actors", NA, NA, NA, "https://plataformacipo.org/mapa-crimes-ambientais/",
    "CIPO", "international_cooperation", NA, NA, NA, "https://plataformacipo.org/mapeamento-cooperacao-internacional/",
    "CIPO", "forest_governance", NA, NA, NA, "https://plataformacipo.org/arranjos-globais-de-governanca-florestal/",

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

    "SEEG", "seeg_farming", NA, "1970-2019","Country, State, Municipality","http://seeg.eco.br/download",
    "SEEG", "seeg_industry", NA,"1970-2019","Country, State, Municipality","http://seeg.eco.br/download",
    "SEEG", "seeg_energy", NA,"1970-2019","Country, State, Municipality","http://seeg.eco.br/download",
    "SEEG", "seeg_land", NA,"1970-2019","Country, State, Municipality","http://seeg.eco.br/download",
    "SEEG", "seeg_residuals", NA, "1970-2019","Country, State, Municipality","http://seeg.eco.br/download",

    ##########
    ## BACI ##
    ##########

    "BACI", "HS92", NA, "1995-2020", "Country", "http://www.cepii.fr/DATA_DOWNLOAD/baci/data",

    ## Shapefile from github repository

    "Internal", "geo_municipalities", NA, "2020", "Municipality", "https://raw.github.com/datazoompuc/datazoom.amazonia/master/data-raw/geo_municipalities.rds",

  )

  return(link)
}

