load_health <- function(dataset,
                        time_period,
                        states = "all",
                        raw_data = FALSE,
                        language = "eng"){

  if (!requireNamespace("read.dbc", quietly = TRUE)) {
    stop(
      "Package \"read.dbc\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ## TODO
  # i) Tábuas de mortalidade do IBGE, por UF e Ano
  # ii) Microdados do SIM/Datasus
  # iii) SIOPS e CNES/Datasus
  # iv) Dados da ANS, cobertura de planos privados
  # v) Dados do e-Gestor, cobertura da atenção básica e SESAI
  # vi) Microdados da PNS e Vigitel

  ##############################
  ## Binding Global Variables ##
  ##############################

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language

  param$time_period <- paste0(time_period, collapse = "|")
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "", toupper(states))

  # Auxiliary parameters to be passed to external_download

  param$skip_rows <- NULL
  param$filenames <- NULL

  ######################
  ## Downloading Data ##
  ######################

  # For each dataset, a filenames object is generated with the list of file names to be pasted at the end of each URL

  if (param$dataset == "ibge_mortality_table"){
    param$skip_rows <- 5
    filenames <- ""
  }

  if (stringr::str_detect(param$dataset, "datasus")){

    # Get dataset source URL

    dat_url <- datasets_link()

    url <- dat_url %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(link) %>%
      base::unlist() %>%
      as.character()

    # Use RCurl to extract the names of all files stored in the server

    filenames <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
      stringr::str_split("\r*\n") %>%
      unlist()
  }

  # Filtering for chosen years
  if (param$dataset == "datasus_sim_do"){

    # time_period is turned into a character such as "2012|2013|2014" to be used in str_detect
    time_filter <- param$time_period %>%
      paste0(collapse = "|")

    filenames <- filenames[stringr::str_detect(filenames, time_filter)]
  }
  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
    time_filter <- param$time_period_yy %>%
      paste0(collapse = "|")

    filenames <- filenames[stringr::str_detect(filenames, time_filter)]
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes")){
    filenames <- filenames[substr(filenames, 5, 6) %in% param$time_period_yy]
  }

  # Filtering for chosen states when possible
  if (param$dataset %in% c("datasus_sim_do") | stringr::str_detect(param$dataset, "datasus_cnes")){
    uf_filter <- param$states %>%
      paste0(collapse = "|")

    filenames <- filenames[stringr::str_detect(filenames, uf_filter)]
  }

  # Filtering for the chosen dataset
  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")){
    suffix <- stringr::str_remove(param$dataset, "datasus_sim_") %>%
      toupper()

    filenames <- filenames[stringr::str_detect(filenames, suffix)]
  }

  param$filenames <- filenames

  # Downloading each file in filenames

  dat <- param$filenames %>%
    imap(
      function(file_name, iteration){
        base::message(paste0("Downloading file ", file_name, " (", iteration, " out of ", length(filenames), ")"))

        external_download(
          source = "health",
          dataset = param$dataset,
          skip_rows = param$skip_rows,
          file_name = file_name
        )
      }
    )

  names(dat) <- filenames

  ## Return Raw Data

  if (param$raw_data == TRUE) {return(dat)}


  ######################
  ## Data Engineering ##
  ######################

  if (dataset == "ibge_mortality_table"){

    # Names as they are in the original sheet
    names(dat) <- c("Idades Exatas (X)",
                    "Probabilidade de Morte entre Duas Idades Exatas Q(X,N) (Por Mil)",
                    "Óbitos D (X, N)",
                    "I(X)",
                    "L(X, N)",
                    "T(X)",
                    "Expectativa de Vida à Idade X E(X)")

    dat <- dat %>%
      tidyr::drop_na() %>%
      dplyr::slice(-41)
  }

  if (stringr::str_detect(param$dataset, "datasus")){
    dat <- dat %>%
      imap(~ dplyr::mutate(.x, file_name = .y)) %>%
      bind_rows()
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat_mod <- dat

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)



}
