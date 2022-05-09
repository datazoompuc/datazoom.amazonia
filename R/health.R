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
      purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
      dplyr::bind_rows() %>%
      janitor::clean_names()
  }

  if (stringr::str_detect(param$dataset, "datasus_sim")){
    dat <- dat %>%
      dplyr::select(
        data = "dtobito",
        hora = "horaobito",
        nome_uf = "file_name",
        cod_mun = "codmunocor",
        cod_bairro = "codbaiocor",

        nascimento = "dtnasc",
        idade,
        sexo,
        raca_cor = "racacor",
        escolaridade = "esc",

        idade_mae = "idademae",
        peso_ao_nascer = "peso",

        obito_fetal = "tipobito",
        causa_basica = "causabas",
        tipo_obito = "circobito",
        acidente_trabalho = "acidtrab"
      ) %>%
      dplyr::mutate(
        data = as.Date(data, format = "%d%m%Y"),
        nome_uf = substr(nome_uf, 3, 4),
        nascimento = as.Date(nascimento, format = "%d%m%Y"),

        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) == "0" ~ NA_character_,
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3))
        ),

        letra_cid = substr(causa_basica, 1, 1),
        numero_cid = sub(".", "", causa_basica) %>% as.numeric(),

        m_total = 1,
        m_diabetes = case_when(
          letra_cid == "E" & numero_cid >= 10 & numero_cid <= 14 ~ 1
          ),
        m_neoplasias = case_when(
          letra_cid == "C" ~ 1,
          letra_cid == "D" & numero_cid <= 48 ~ 1
          ),
        m_causas_externas = case_when(
          letra_cid == "V" & numero_cid >= 1 ~ 1,
          letra_cid %in% c("W", "X") ~ 1,
          letra_cid == "Y" & numero_cid <= 98 ~ 1
        ),
        m_acidentes_transporte = case_when(
          letra_cid == "V" & numero_cid >= 1 & numero_cid <= 99 ~ 1
        ),
        m_agressoes = case_when(
          letra_cid == "X" & numero_cid >= 85 ~ 1,
          letra_cid == "Y" & numero_cid <= 9 ~ 1
        ),
        m_malaria = case_when(
          letra_cid == "B" & numero_cid >= 50 & numero_cid <= 54 ~ 1
        )

      ) %>%
      group_by(cod_mun) %>%
      dplyr::summarise(across(starts_with("m_"), ~ sum(., na.rm = TRUE)))
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
