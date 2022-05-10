load_datasus <- function(dataset,
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
  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sih")){
    filenames <- filenames[substr(filenames, 5, 6) %in% param$time_period_yy]
  }

  # Filtering for chosen states when possible
  if (param$dataset %in% c("datasus_sim_do") | stringr::str_detect(param$dataset, "datasus_cnes")){
    uf_filter <- param$states %>%
      paste0(collapse = "|")

    filenames <- filenames[stringr::str_detect(filenames, uf_filter)]
  }
  if (param$dataset == "datasus_sih"){
    filenames <- filenames[substr(filenames, 3, 4) %in% states]
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
    purrr::imap(
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


  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()


  if (stringr::str_detect(param$dataset, "datasus_sim")){
    dat <- dat %>%
      dplyr::mutate(
        data = as.Date(data, format = "%d%m%Y"),

        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) == "0" ~ NA_character_,
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3))
        )
      )

    dat <- load_dictionary("datasus") %>%
      purrr::transpose() %>%

      purrr::map_dfc(
        function(dic_row){
          dat %>%
            dplyr::mutate(value = dplyr::case_when(
                causabas %in% expand_cid_code(dic_row$cid_code) ~ 1
              )
            ) %>%
            dplyr::select(value) %>%
            dplyr::rename_with(~ dic_row$cid_code)
        }
      ) %>%
      bind_cols(dat, .)

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


expand_cid_code <- function(cid){
  # Turns a character "A050-B010" into an expanded vector c("A050", "A051", ..., "B010")
  # Also turns "A001, B001-B002" into c("A001", "B001", "B002")

  if (!(stringr::str_detect(cid, ",|-"))) return(cid)

  # Splitting sections separated by commas into a list
  cid <- cid %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.list()

  # Each element of the list becomes a vector with the initial and final value, separated by "-"
  cid <- cid %>%
    purrr::map(
      function(string){
        string %>%
          stringr::str_split("-") %>%
          unlist() %>%
          stringr::str_remove_all(" ")
      }
    )

  letter_to_number <- seq_along(letters)
  names(letter_to_number) <- toupper(letters)

  # Converting letters to numbers
  cid <- cid %>%
    purrr::map(
      function(code){
        letter <- stringr::str_extract(code, "[A-Z]") %>%
          dplyr::recode(!!!letter_to_number)

        code %>%
          stringr::str_remove("[A-Z]") %>%
          paste0(letter, .) %>%
          as.numeric()
      }
    )

  # Filling in sequence ex: A001-A005
  cid <- cid %>%
    purrr::map(
      ~ if (length(.x) == 2) .x[1]:.x[2]
    )

  # Converting back to letters

  number_to_letter <- names(letter_to_number)
  names(number_to_letter) <- seq_along(letters)

  cid <- cid %>%
    purrr::map(
      function(code){
        letter <- as.integer(code/1000)
        number <- code - 1000*letter

        letter <- letter %>%
          dplyr::recode(!!!number_to_letter)

        number <- paste0("00", number) %>%
          stringr::str_sub(start = -3)

        paste0(letter, number)
      }
    )

  cid %>%
    unlist()

}
