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

# ---------------------------------------------------------------------------
# Session-scoped download + parse cache
#
# external_download() is the single download helper for every load_*()
# except load_epe()'s two datasets (R/epe.R, a deliberate bypass because
# readxl chokes on that workbook's sharedStrings.xml). Several sources serve
# more than one dataset off one identical file -- SEEG (6 datasets, one
# 250MB+ xlsx), IPS (8 datasets, one xlsx), PRODES (6 datasets, one zip) --
# so loading more than one dataset from the same source used to pay for the
# download, and for xlsx sources the readxl::read_xlsx() parse, once per
# dataset. This cache fixes that for the lifetime of one R session; it never
# persists across sessions and needs no user consent (CRAN's restriction is
# on writing into the user's home/library, not into tempdir() subdirectories).
#
# Mirrors the .dz_manifest_cache precedent (R/manifest.R): a namespace-local
# environment, an options() escape hatch, and an internal clear_*() for tests.
# ---------------------------------------------------------------------------

# .dz_download_cache$files      : an environment, key string -> list(dir, temp)
# .dz_download_cache$parsed_key : the key of the one cached parsed object, or NULL
# .dz_download_cache$parsed_val : the cached parsed object itself
.dz_download_cache <- new.env(parent = emptyenv())

download_cache_enabled <- function() {
  !isFALSE(getOption("datazoom.amazonia.cache", TRUE))
}

#' Drop every cached download and parsed object. Internal only -- used by
#' tests to isolate cache state between test_that() blocks.
#' @noRd
clear_download_cache <- function() {
  .dz_download_cache$files <- NULL
  .dz_download_cache$parsed_key <- NULL
  .dz_download_cache$parsed_val <- NULL
  invisible(NULL)
}

download_cache_files_env <- function() {
  if (is.null(.dz_download_cache$files)) {
    .dz_download_cache$files <- new.env(parent = emptyenv())
  }
  .dz_download_cache$files
}

file_cache_key <- function(path, file_extension) {
  paste(file_extension, path, sep = "||")
}

# NULL on a miss, or when a previously cached dir/file has since been
# removed from under us (e.g. by a stale-tempdir cleanup) -- treated the
# same as a miss rather than erroring.
file_cache_get <- function(key) {
  env <- download_cache_files_env()
  if (!exists(key, envir = env, inherits = FALSE)) {
    return(NULL)
  }
  entry <- get(key, envir = env, inherits = FALSE)
  if (!dir.exists(entry$dir) || !file.exists(entry$temp)) {
    return(NULL)
  }
  entry
}

file_cache_set <- function(key, dir, temp) {
  assign(key, list(dir = dir, temp = temp), envir = download_cache_files_env())
}

# Which reads are cheap to keep a single parsed copy of: the .xlsx branches
# only (ips, epe, and the generic branch covering seeg/mapbiomas/iema --
# see the dispatch below). Deliberately excludes terra::rast() results (a
# SpatRaster wraps a C++ pointer tied to a file on disk, and the prodes branch
# calls terra::tmpFiles(remove = TRUE), which can invalidate a cached one)
# and data.table::fread() results (aneel/baci hand these straight to the user
# under raw_data = TRUE, and a user's dt[, x := 1] would mutate a shared
# cache entry in place). aneel is 100% CSV across all datasets and explicitly
# excluded.
parsed_cache_eligible <- function(source, file_extension) {
  file_extension == ".xlsx" && source %in% c("seeg", "ips", "epe", "mapbiomas", "iema")
}

#' Perform the actual file transfer for external_download(). Extracted as a
#' seam so tests can intercept it with testthat::local_mocked_bindings() --
#' utils::download.file()/googledrive::drive_download() are namespaced calls
#' in other packages and cannot otherwise be mocked or counted.
#' @noRd
perform_download <- function(path, temp, download_method, source, quiet = TRUE) {
  if (download_method == "standard") {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }
  if (download_method == "curl") {
    # This function's own caller (external_download()) already restores
    # every option it touches on exit, but perform_download() shouldn't
    # depend on that to stay behavior-preserving on its own -- guard locally
    # too, in case this is ever called directly (as the tests' mocking seam
    # does) without that caller's on.exit() wrapping it.
    old_options <- options()[c("download.file.method", "download.file.extra")]
    on.exit(options(old_options), add = TRUE)
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
    googledrive::drive_download(path, path = temp, overwrite = TRUE)
  }
  invisible(NULL)
}

# Must match the UA string actions/scrapers/resolve_seeg.R uses to verify
# this same seeg.eco.br URL live -- see the "seeg" branch above.
SEEG_USER_AGENT <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
)

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
    download.file.extra = getOption("download.file.extra"),
    HTTPUserAgent = getOption("HTTPUserAgent")
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
      "_1950_CurrentYear_GLOBE.nc"
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

  # googledrive links do not contain the file extension, for example. seeg no
  # longer needs this override (its URL, resolved live by resolve_seeg.R,
  # already ends in ".xlsx" -- auto-detected correctly above) but is kept
  # here too, harmlessly, so a future manifest URL shape change can't
  # silently break extension detection for it.

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
    if (dataset == "energy_enterprises_distributed") {
      # empreendimento-geracao-distribuida.zip is a real zip archive -- unlike
      # this dataset's siblings, which are bare CSVs. Without ".zip" here,
      # the unzip step below (keyed on file_extension == ".zip") never runs
      # and fread() is handed the raw zip bytes directly, which hard-errors
      # ("string with embedded nul: 'PK\003\004...'" -- the zip magic number).
      # Verified live 2026-09-21: this crashed on every real download before
      # this fix, unrelated to the encoding/numeric-parsing bugs fixed
      # alongside it in R/aneel.R.
      file_extension <- ".zip"
    } else {
      file_extension <- ".csv"
    }
  }

  # Define Directory and File For Download (session cache aware)
  #
  # A second external_download() call resolving to the same (path,
  # file_extension) -- e.g. any of the 6 SEEG datasets, or 2 back-to-back
  # PRODES datasets -- reuses what an earlier call already downloaded (and,
  # for a zip, already extracted) instead of doing it again. See the cache
  # helpers defined above external_download(). aneel used to be excluded
  # here too (a 2026-09-18 merge commit's "per maintainer decision"), but
  # that reasoning belongs to the SEPARATE parsed-object cache below
  # (parsed_cache_eligible() -- fread()'s data.table result is mutable by
  # reference, so sharing one across calls under raw_data = TRUE is a real
  # risk) and was mistakenly applied to this file-level cache too, which
  # only remembers a download's disk location -- no shared object, no
  # mutation risk. aneel's cache key (path = its resolved manifest URL,
  # already unique per dataset/year) has no collision risk either. Verified
  # live 2026-09-21: re-enabling this stopped a second load_aneel() call
  # for the same dataset in one session from re-downloading the ~106MB
  # energy_enterprises_distributed zip (or any other aneel file) a second
  # time.

  use_cache <- download_cache_enabled()
  cache_key <- file_cache_key(path, file_extension)
  cached_file <- if (use_cache) file_cache_get(cache_key) else NULL
  file_cache_hit <- !is.null(cached_file)

  if (file_cache_hit) {
    dir <- cached_file$dir
    temp <- cached_file$temp
  } else if (use_cache) {
    dir <- file.path(tempdir(check = TRUE), "datazoom-cache", rlang::hash(cache_key))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    temp <- tempfile(fileext = file_extension, tmpdir = dir)
  } else {
    dir <- tempdir()
    temp <- tempfile(fileext = file_extension, tmpdir = dir)
  }

  ## Downloading file by the selected method (skipped entirely on a cache hit)

  if (!file_cache_hit) {
    # A half-downloaded/half-extracted per-key dir left behind on failure
    # would be picked up by a later list.files(dir, ...) call just like a
    # real cache hit -- clean it up unless the download+unzip below both
    # complete and get registered.
    if (use_cache) {
      registered <- FALSE
      on.exit(if (!registered) unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
    }

    download_method <- "standard" # works for most functions
    quiet <- TRUE

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
      # 2026-09-15: the resolver used to point at a Google Drive share link
      # (hence "googledrive" here); it now emits the real seeg.eco.br URL
      # directly (see resolve_seeg.R), a plain HTTPS download -- EXCEPT
      # seeg.eco.br 403s any request without a browser-like User-Agent
      # (confirmed live), hence HTTPUserAgent here rather than the plain
      # "standard" download_method every other https source uses.
      # SEEG_USER_AGENT must match the UA string actions/scrapers/
      # resolve_seeg.R uses to verify this same URL live.
      options(HTTPUserAgent = SEEG_USER_AGENT)
      # The file is 250MB+ -- R's default 60s download.file() timeout
      # (confirmed live: hit it on a real run) is nowhere near enough on an
      # ordinary connection. Same fix as prodes/sigmine above.
      message("This may take a while.\n")
      options(timeout = max(1000, getOption("timeout")))
    }

    perform_download(path = path, temp = temp, download_method = download_method, source = source, quiet = quiet)

    ## Unzipping if the file is zipped

    if (file_extension == ".zip") {
      utils::unzip(temp, exdir = dir)
    }

    if (use_cache) {
      file_cache_set(cache_key, dir = dir, temp = temp)
      registered <- TRUE
    }
  }

  ###############
  ## Load Data ##
  ###############

  # A single 1-slot cache for the expensive-to-reparse xlsx reads -- see
  # parsed_cache_eligible() above. Keyed on exactly the inputs those read
  # branches consult (path/file_extension/source/sheet/skip_rows), NOT the
  # full param list: dataset/geo_level must NOT be part of this key, or
  # every one of SEEG's 6 datasets would miss and the cache would do
  # nothing for the case it exists for.
  parsed_key <- list(
    path = path, file_extension = file_extension, source = source,
    sheet = param$sheet, skip_rows = param$skip_rows
  )
  parsed_eligible <- use_cache && parsed_cache_eligible(source, file_extension)
  parsed_hit <- parsed_eligible &&
    !is.null(.dz_download_cache$parsed_key) &&
    identical(.dz_download_cache$parsed_key, parsed_key)

  if (parsed_hit) {
    dat <- .dz_download_cache$parsed_val
  } else {

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
    if (param$source == "aneel") {
      # energy_enterprises_distributed is the one aneel dataset that's a real
      # zip (empreendimento-geracao-distribuida.zip, ~1.5GB uncompressed) --
      # its siblings are bare CSVs handled in the non-zip aneel branch below.
      # temp is the zip itself here, not the extracted CSV, so find the real
      # file by pattern rather than assuming a fixed name. encoding = "UTF-8"
      # verified live 2026-09-21 by inspecting the extracted file's raw bytes
      # (e.g. "Condomínio" is the real 2-byte UTF-8 sequence 0xC3 0xAD for
      # "í", not a single Latin-1 byte) -- fread(encoding = "Latin-1") had
      # been misreading this file's real UTF-8 bytes as single-byte Latin-1
      # characters since this dataset was added (2023), producing mojibake
      # like "Ã­" that no amount of post-hoc iconv() can cleanly reverse.
      csv <- list.files(dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
      if (length(csv) == 0) {
        stop("No CSV found in the downloaded energy_enterprises_distributed archive.")
      }
      dat <- data.table::fread(csv[1], encoding = "UTF-8")
    }

  } else if (param$source == "aneel") {
    if (param$dataset == "energy_generation") {
      dat <- data.table::fread(temp, encoding = "UTF-8")
    } else if (param$dataset == "energy_development_budget") {
      # encoding = "UTF-8": same root-cause fix and same live verification
      # (raw bytes for "Rede Básica" are 0xC3 0xA1, real UTF-8 for "á") as
      # energy_enterprises_distributed above -- see that branch's comment.
      dat <- data.table::fread(temp, encoding = "UTF-8")
    }

  } else if (param$source == "ips") {
    # param$sheet carries the requested YEARS (see load_ips()), not raw tab
    # names -- resolved against the workbook's own tabs here so a whitespace
    # quirk (IPS Amazônia's "2018 " tab, verified live) can't break an exact
    # readxl::read_xlsx(sheet = ...) match. See ips_match_sheets() (R/ips.R).
    sheets <- ips_match_sheets(readxl::excel_sheets(temp), param$sheet)
    dat <- sheets %>%
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

  if (parsed_eligible) {
    .dz_download_cache$parsed_key <- parsed_key
    .dz_download_cache$parsed_val <- dat
  }

  } # end parsed_hit / else

  ##############################
  ## Excluding Temporary File ##
  ##############################

  # Folder is kept. The temp file itself is also kept when caching is on --
  # it is what a later cache hit re-reads/re-extracts from.

  if (!file_extension %in% c(".nc") && !use_cache) {
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
