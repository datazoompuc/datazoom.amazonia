load_health <- function(dataset,
                        raw_data = FALSE,
                        language = "eng"){

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

  ######################
  ## Downloading Data ##
  ######################

  if (dataset == "mortality_table"){
    path <- url <- "https://ftp.ibge.gov.br/Tabuas_Completas_de_Mortalidade/Tabuas_Completas_de_Mortalidade_2020/xls/ambos_os_sexos.xls"

    dir <- tempdir()
    temp <- tempfile(fileext = ".xls", tmpdir = dir)

    utils::download.file(url = path, destfile = temp, mode = "wb")

    dat <- readxl::read_xls(temp, skip = 5)

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

  if (dataset == "mortality"){

    path <- url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"

    h = new_handle(dirlistonly=TRUE)
    con = curl(url, "r", h)
    tbl = read.table(con, stringsAsFactors=TRUE, fill=TRUE) %>%
      unlist() %>%
      as.character() %>%
      paste0(url, .)

    proc <- RCurl::CFILE(temp, mode = "wb")
    RCurl::curlPerform(url = path, writedata = proc@ref, noprogress = FALSE)
    RCurl::close(proc)

    tbl <- RCurl::getURL(path,  dirlistonly = TRUE) %>%
      unlist(strsplit(., '\r'))

    tbl %>%
      map_dfr(
        function(filename){

          dir <- tempdir()
          temp <- tempfile(fileext = ".dbc", tmpdir = dir)

          utils::download.file(url = filename, destfile = temp, method = "libcurl")

          dat <- read.dbc::read.dbc(temp)

          dat

        }
      )

    dir <- tempdir()
    temp <- tempfile(fileext = ".dbc", tmpdir = dir)

    utils::download.file(url = path, destfile = temp, method = "libcurl")

    dat <- read.dbc::read.dbc(temp)

  }

  ## Return Raw Data

  if (param$raw_data == TRUE) {return(dat)}


  ######################
  ## Data Engineering ##
  ######################

  ################################
  ## Harmonizing Variable Names ##
  ################################

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)



}
