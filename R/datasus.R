#' @title DATASUS - Mortality, hospitalizations and hospital beds
#'
#' @description Loads DATASUS data on health establishments, mortality, access to health services and several health indicators.
#'
#' @param dataset A dataset name, can be one of ("datasus_sim_do", "datasus_sih", "datasus_cnes_lt", "datasus_sinasc","datasus_siasus"), or more. For more details, try \code{vignette("DATASUS")}.
#' @inheritParams load_baci
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM").
#' @param keep_all A \code{boolean} choosing whether to aggregate the data by municipality, in turn losing individual-level variables (\code{FALSE}) or to keep all the original variables. Only applies when raw_data is \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # download raw data for the year 2010 in the state of AM.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = "AM",
#'   raw_data = TRUE
#' )
#'
#' # download treated data with the number of deaths by cause in AM and PA.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = c("AM", "PA"),
#'   raw_data = FALSE
#' )
#'
#' # download treated data with the number of deaths by cause in AM and PA
#' # keeping all individual variables.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = c("AM", "PA"),
#'   raw_data = FALSE,
#'   keep_all = TRUE
#' )
#' }
#'
#' @return A \code{tibble}.
#' @export

load_datasus <- function(dataset,
                         time_period,
                         states = "all",
                         raw_data = FALSE,
                         keep_all = FALSE,
                         language = "eng") {
  # Checking for foreign package (in Suggests)

  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop(
      "Package \"foreign\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Checking for RCurl package (in Suggests)

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop(
      "Package \"RCurl\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  . <- abbrev_state <- code_muni <- code_muni_6 <- code_state <- codmunocor <- NULL
  codufmun <- dtobito <- file_name <- is_cid_code <- label_eng <- label_pt <- NULL
  legal_amazon <- link <- month <- name_muni <- qt_exist <- qt_nsus <- value <- NULL
  var_code <- year <- qt_sus <- causabas <- available_time <- NULL

  origem <- locnasc <- estcivmae <- escmae <- semagestac <- gravidez <- parto <- NULL
  consprenat <- sexo <- racacor <- idanomal <- escmae2010 <- dtnascmae <- NULL
  racacormae <- dtultmenst <- tpmetestim <- tpapresent <- sttrabpart <- NULL
  stcesparto <- tpnascassi <- codmunnasc <- dataset_prefix_map <- munic_res <- NULL
  mun_res <- sp_m_hosp <- dt_diag <- dt_trat <- dt_nasc <- mun_diag <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$source <- "datasus"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$keep_all <- keep_all

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

  # Auxiliary parameters to be passed to external_download

  param$skip_rows <- NULL
  param$filenames <- NULL

  # check if dataset and time_period are valid

  check_params(param)

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

  ### Filtering by year

  file_years <- NULL
  file_years_yy <- NULL

  if (param$dataset %in% c("datasus_sim_do", "datasus_sinasc", "datasus_po")) {
    file_years <- filenames %>%
      substr(5, 8)
  }
  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
    file_years_yy <- filenames %>%
      stringr::str_extract("\\d+")
    # In this case, the position varies
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sih")) {
    file_years_yy <- filenames %>%
      substr(5, 6)
  }

  if (stringr::str_detect(param$dataset, "datasus_siasus")) {

    siasus_two_digits <- c(
      "datasus_siasus_ab",
      "datasus_siasus_ad",
      "datasus_siasus_am",
      "datasus_siasus_an",
      "datasus_siasus_aq",
      "datasus_siasus_ar",
      "datasus_siasus_pa",
      "datasus_siasus_ps"
    )

    siasus_two_digits_alt <- c(
      "datasus_siasus_abo",
      "datasus_siasus_acf",
      "datasus_siasus_atd",
      "datasus_siasus_sad"
    )

    if (param$dataset %in% siasus_two_digits) {
      file_years_yy <- substr(filenames, 5, 6)
    } else if (param$dataset %in% siasus_two_digits_alt) {
      file_years_yy <- substr(filenames, 6, 7)
    }
  }


  # Only files whose name's year matches a chosen one are kept
  if (!is.null(file_years)) {
    filenames <- filenames[file_years %in% param$time_period]
  }
  if (!is.null(file_years_yy)) {
    filenames <- filenames[file_years_yy %in% param$time_period_yy]
  }

  ### Filtering for chosen states when possible

  file_state <- NULL

  if (param$dataset %in% c("datasus_sim_do", "datasus_sinasc") | stringr::str_detect(param$dataset, "datasus_cnes|datasus_sih|datasus_siasus")) {
    file_state <- filenames %>%
      substr(3, 4)
  } else if (paste0(param$states, collapse = "") != "all") {
    base::message("Filtering by state not supported for all datasets. Data for other states will be included.")
  }

  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sih|datasus_siasus")) {
    if (param$dataset %in% siasus_two_digits) {
      file_state <- filenames %>%
        substr(3, 4)
    } else if (param$dataset %in% siasus_two_digits_alt) {
      file_state <- filenames %>%
        substr(4, 5)
    }
  }

  if (!is.null(file_state) & paste0(param$states, collapse = "") != "all") {
    filenames <- filenames[file_state %in% param$states]
  }

  ### Filtering for the chosen dataset (they come clumped together in the same folder)

  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
    suffix <- stringr::str_remove(param$dataset, "datasus_sim_") %>%
      toupper()

    filenames <- filenames[stringr::str_detect(filenames, suffix)]
  }

  if(stringr::str_detect(param$dataset, "datasus_sih|datasus_siasus_")) {
    suffix <- stringr::str_remove(param$dataset, "datasus_sih_|datasus_siasus_") %>%
      toupper()

    filenames <- filenames[stringr::str_starts(filenames, suffix)]
  }

  param$filenames <- filenames

  ### Downloading each file in filenames

  dat <- param$filenames %>%
    purrr::imap(
      function(file_name, iteration) {
        base::message(paste0("Downloading file ", file_name, " (", iteration, " out of ", length(filenames), ")"))

        external_download(
          source = param$source,
          dataset = param$dataset,
          skip_rows = param$skip_rows,
          file_name = file_name
        )
      }
    )

  names(dat) <- filenames


  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()


  if (stringr::str_detect(param$dataset, "datasus_sim")) {
    dat <- dat %>%
      dplyr::mutate(
        codmunocor = as.numeric(as.character(codmunocor)),
        causabas = as.character(causabas),
        dtobito = as.Date(dtobito, format = "%d%m%Y"),
        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) == "0" ~ NA_character_,
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3))
        )
      )

    # The cid codes have no leading 0 when the numbers are only two digits

    dat <- dat %>%
      dplyr::mutate(
        causabas = dplyr::case_when(
          nchar(causabas) == 4 ~ causabas,
          nchar(causabas) == 3 ~ paste0(
            stringr::str_extract(causabas, "[a-zA-Z]"),
            0,
            stringr::str_extract(causabas, "\\d+")
          )
        )
      )

    dic_cid_codes <- load_dictionary(param$dataset) %>%
      dplyr::filter(is_cid_code)

    dat <- dic_cid_codes %>%
      purrr::transpose() %>%
      purrr::map_dfc(
        function(dic_row) {
          dat %>%
            dplyr::mutate(value = dplyr::case_when(
              causabas %in% expand_cid_code(dic_row$var_code) ~ 1,
              TRUE ~ 0
            )) %>%
            dplyr::select(value) %>%
            dplyr::rename_with(~ dic_row$var_code)
        }
      ) %>%
      dplyr::bind_cols(dat)

    dat <- dat %>%
      dplyr::rename("code_muni_6" = "codmunocor")
  }

  if (stringr::str_detect(param$dataset, "datasus_cnes")) {
    dat <- dat %>%
      dplyr::mutate(codufmun = as.numeric(as.character(codufmun))) %>%
      dplyr::rename("code_muni_6" = "codufmun")

    # Creating year and month variables

    dat <- dat %>%
      dplyr::mutate(
        year = as.numeric(paste0("20", substr(file_name, 5, 6))),
        month = as.numeric(substr(file_name, 7, 8))
      )
  }

  if(param$dataset == "datasus_sinasc") {

    geo <- datazoom.amazonia::municipalities %>%
      dplyr::select(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      dplyr::mutate(code_muni_6 = as.character(as.integer(code_muni / 10))) %>%
      dplyr::distinct(code_muni_6, .keep_all = TRUE) # Only keeps municipalities uniquely identified by the 6 digits

    labels <- tibble::tribble(
      ~ var_code, ~ value, ~ label_pt, ~ label_eng,
      "origem", 1, "oracle", "oracle",
      "origem", 2, "ftp", "ftp",
      "origem", 3, "sead", "sead",
      "locnasc", 1, "hospital", "hospital",
      "locnasc", 2, "outros estabelecimentos de saude", "other health establishments",
      "locnasc", 3, "domicilio", "home",
      "locnasc", 4, "outros", "other",
      "locnasc", 5, "aldeia indigena", "indigenous village",
      "locnasc", 9, "ignorado", "unknown",
      "estcivmae", 1, "solteira", "single",
      "estcivmae", 2, "casada", "married",
      "estcivmae", 3, "viuva", "widowed",
      "estcivmae", 4, "divorciada", "divorced",
      "estcivmae", 5, "uniao estavel", "civil union",
      "estcivmae", 9, "ignorado", "unknown",
      "escmae", 1, "nenhuma", "none",
      "escmae", 2, "1 a 2 anos", "1 to 2 years",
      "escmae", 3, "4 a 7 anos", "4 to 7 years",
      "escmae", 4, "8 a 11 anos", "8 to 11 years",
      "escmae", 5, "12 e mais", "12 or more years",
      "escmae", 9, "ignorado", "unknown",
      "semagestac", 1, "menos de 22 semanas", "less than 22 weeks",
      "semagestac", 2, "22 a 27 semanas", "22 to 27 weeks",
      "semagestac", 3, "28 a 31 semanas", "28 to 31 weeks",
      "semagestac", 4, "32 a 36 semanas", "32 to 36 weeks",
      "semagestac", 5, "37 a 41 semanas", "37 to 41 weeks",
      "semagestac", 6, "42 semanas e mais", "42 weeks or more",
      "semagestac", 9, "ignorado", "unknown",
      "gravidez", 1, "unica", "single",
      "gravidez", 2, "dupla", "twin",
      "gravidez", 3, "tripla ou mais", "triplet or more",
      "gravidez", 9, "ignorado", "unknown",
      "parto", 1, "vaginal", "vaginal",
      "parto", 2, "cesario", "cesarean",
      "parto", 9, "ignorado", "unknown",
      "consprenat", 1, "nenhuma", "none",
      "consprenat", 2, "de 1 a 3", "1 to 3",
      "consprenat", 3, "de 4 a 6", "4 to 6",
      "consprenat", 4, "7 e mais", "7 or more",
      "consprenat", 9, "ignorado", "unknown",
      "sexo", 0, "ignorado", "unknown",
      "sexo", 1, "masculino", "male",
      "sexo", 2, "feminino", "female",
      "racacor", 1, "branca", "white",
      "racacor", 2, "preta", "black",
      "racacor", 3, "amarela", "yellow",
      "racacor", 4, "parda", "brown",
      "racacor", 5, "indigena", "indigenous",
      "idanomal", 1, "ignorado", "unknown",
      "idanomal", 2, "sim", "yes",
      "idanomal", 9, "nao", "no",
      "escmae2010", 0, "sem escolaridade", "no education",
      "escmae2010", 1, "fundamental 1", "elementary 1",
      "escmae2010", 2, "fundamental 2", "elementary 2",
      "escmae2010", 3, "medio", "high school",
      "escmae2010", 4, "superior incompleto", "incomplete higher education",
      "escmae2010", 5, "superior completo", "complete higher education",
      "escmae2010", 9, "ignorado", "unknown",
      "racacormae", 1, "branca", "white",
      "racacormae", 2, "preta", "black",
      "racacormae", 3, "amarela", "yellow",
      "racacormae", 4, "parda", "brown",
      "racacormae", 5, "indigena", "indigenous",
      "tpmetestim", 1, "exame fisico", "physical exam",
      "tpmetestim", 2, "outro metodo", "other method",
      "tpmetestim", 9, "ignorado", "unknown",
      "tpapresent", 1, "cefalica", "cephalic",
      "tpapresent", 2, "pelvica ou podalica", "breech or footling",
      "tpapresent", 3, "transversa", "transverse",
      "tpapresent", 9, "ignorado", "unknown",
      "sttrabpart", 1, "sim", "yes",
      "sttrabpart", 2, "nao", "no",
      "sttrabpart", 9, "ignorado", "unknown",
      "stcesparto", 1, "sim", "yes",
      "stcesparto", 2, "nao", "no",
      "stcesparto", 3, "nao se aplica", "not applicable",
      "stcesparto", 9, "ignorado", "unknown",
      "tpnascassi", 1, "medico", "doctor",
      "tpnascassi", 2, "enfermeira obstetriz", "obstetric nurse",
      "tpnascassi", 3, "parteira", "midwife",
      "tpnascassi", 4, "outros", "other",
      "tpnascassi", 9, "ignorado", "unknown"
    )

    # adicionando factor labels

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(unique(labels$var_code)),
          function(x) {
            # linhas do dict correspondentes a cada variavel
            dic <- labels %>%
              dplyr::filter(var_code == dplyr::cur_column())

            # vetor de levels
            lev <- dic$value

            # vetor de labels
            if (param$language == "pt") {
              lab <- dic$label_pt
            }
            else {
              lab <- dic$label_eng
            }

            # transforma em factor

            factor(x, levels = lev, labels = lab)
          }
        )
      )

    # formatando dados

    dat <- dat %>%
      dplyr::mutate(
        dtnascmae = lubridate::dmy(as.character(dtnascmae)),
        dtultmenst = lubridate::dmy(as.character(dtultmenst)),
        codmunnasc = as.numeric(as.character(codmunnasc))
      ) %>%
      dplyr::rename("code_muni_6" = "codmunnasc")
  }

  if (stringr::str_detect(param$dataset, "datasus_sih")) {
    # Adding municipality data
    geo <- datazoom.amazonia::municipalities %>%
      dplyr::select(
        code_muni,
        name_muni,
        code_state,
        abbrev_state,
        legal_amazon
      )
    # Original data only has 6 IBGE digits instead of 7
    geo <- geo %>%
      dplyr::mutate(code_muni_6 = as.character(as.integer(code_muni / 10))) %>%
      dplyr::distinct(code_muni_6, .keep_all = TRUE) # Only keeps municipalities uniquely identified by the 6 digits

    if(param$dataset %in% c("datasus_sih_rd", "datasus_sih_rj")){
      dat <- dat %>%
        dplyr::mutate(munic_res = as.character(munic_res)) %>%
        dplyr::left_join(geo, by = c("munic_res" = "code_muni_6"))

    } else if(param$dataset == "datasus_sih_er"){
      dat <- dat %>%
        dplyr::mutate(mun_res = as.character(mun_res)) %>%
        dplyr::left_join(geo, by = c("mun_res" = "code_muni_6"))

    } else if(param$dataset == "datasus_sih_sp") {
      dat <- dat %>%
        dplyr::mutate(sp_m_hosp = as.character(sp_m_hosp)) %>%
        dplyr::left_join(geo, by = c("sp_m_hosp" = "code_muni_6"))

    }
  }

  if (param$dataset == "datasus_po") {

    labels <- tibble::tribble(
      ~ var_code, ~ value, ~ label_pt, ~ label_eng,
      "tratamento", "1", "cirurgia", "surgery",
      "tratamento", "2", "quimioterapia", "chemotherapy",
      "tratamento", "3", "radioterapia", "radiotherapy",
      "tratamento", "4", "quimioterapia + radioterapia", "chemotherapy + radiotherapy",
      "tratamento", "5", "sem informacao de tratamento", "no treatment information",
      "diagnostic", "1", "neoplasias malignas (lei no 12.732/12)", "malignant neoplasms (law no. 12.732/12)",
      "diagnostic", "2", "neoplasias in situ", "neoplasms in situ",
      "diagnostic", "3", "neoplasias de comportamento incerto ou desconhecido", "neoplasms of uncertain or unknown behavior",
      "diagnostic", "4", "C44 e C73", "C44 e C73",
      "sexo", "F", "feminino", "female",
      "sexo", "M", "masculino", "masculine",
      "estadiam", "0", "0", "0",
      "estadiam", "1", "I", "I",
      "estadiam", "2", "II", "II",
      "estadiam", "3", "III", "III",
      "estadiam", "4", "IV", "IV",
      "estadiam", "5", "nao se aplica", "not applicable",
      "estadiam", "9", "ignorado", "ignored",
    )

    # adicionando factor labels

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(unique(labels$var_code)),
          function(x) {
            # linhas do dict correspondentes a cada variavel
            dic <- labels %>%
              dplyr::filter(var_code == dplyr::cur_column())

            # vetor de levels
            lev <- dic$value

            # vetor de labels
            if (param$language == "pt") {
              lab <- dic$label_pt
            }
            else {
              lab <- dic$label_eng
            }

            # transforma em factor

            factor(x, levels = lev, labels = lab)
          }
        )
      )

    # formatando dados

    geo <- datazoom.amazonia::municipalities %>%
      dplyr::select(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      dplyr::mutate(code_muni_6 = as.integer(code_muni / 10)) %>%
      dplyr::distinct(code_muni_6, .keep_all = TRUE) # Only keeps municipalities uniquely identified by the 6 digits

    dat <- dat %>%
      dplyr::mutate(
        dt_diag = lubridate::dmy((dt_diag)),
        dt_trat = lubridate::dmy((dt_trat)),
        dt_nasc = lubridate::dmy((dt_nasc)),
        mun_diag = as.integer(as.character(mun_diag))) %>%
      dplyr::left_join(geo, by = c("mun_diag" = "code_muni_6"))

  }

  if(stringr::str_detect(param$dataset,"datasus_siasus")){

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.factor), as.character)
      )

    tem_zero_a_esquerda <- function(x) {
      # Força o encoding como latin1 → UTF-8 para evitar warnings
      x <- enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))
      any(grepl("^0", x))
    }

    coluna_numerica_valida <- function(x) {
      x <- enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))
      all(grepl("^\\d+$", x))
    }

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.character),
          ~ {
            if (!tem_zero_a_esquerda(.x) && coluna_numerica_valida(.x)) {
              suppressWarnings(as.numeric(.x))
            } else {
              .x
            }
          }
        )
      )

    geo <- datazoom.amazonia::municipalities %>%
      dplyr::select(
        code_muni,
        name_muni,
        code_state,
        abbrev_state,
        legal_amazon
      )

    geo <- geo %>%
      dplyr::mutate(code_muni_6 = as.integer(code_muni / 10))

    suffix <- if (param$dataset == "datasus_siasus_pa") {
      "pa_ufmun"
    } else if(param$dataset %in% c("datasus_siasus_ps","datasus_siasus_sad")) {
      "ufmun"
    } else {
      "ap_ufmun"
    }

    dat <- dat %>%
      dplyr::left_join(geo, by = stats::setNames("code_muni_6", suffix))
    }


  #################
  ## Aggregating ##
  #################

  if (stringr::str_detect(param$dataset, "datasus_sim") & !param$keep_all) {
    # Obtaining the mortality variables

    cid_vars <- load_dictionary(param$dataset) %>%
      dplyr::filter(is_cid_code) %>%
      dplyr::select(var_code) %>%
      unlist()

    names(cid_vars) <- NULL # To stop summarise from renaming the variables

    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, legal_amazon, dtobito) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of(cid_vars), sum))
  }

  if (param$dataset == "datasus_cnes_lt" & !param$keep_all) {
    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, legal_amazon, year, month) %>%
      dplyr::summarise(dplyr::across(c(qt_exist, qt_sus, qt_nsus)))
  }

  ###############
  ## Labelling ##
  ###############

  dic <- load_dictionary(param$dataset)

  row_numbers <- match(names(dat), dic$var_code)

  if (param$language == "pt") {
    dic <- dic %>%
      dplyr::select(label_pt)
  }
  if (param$language == "eng") {
    dic <- dic %>%
      dplyr::select(label_eng)
  }

  labels <- dic %>%
    dplyr::slice(row_numbers) %>%
    unlist()

  # Making sure 'labels' is the same length as the number of columns

  labels_full <- character(length = ncol(dat))

  labels_full[which(!is.na(row_numbers))] <- labels

  Hmisc::label(dat) <- as.list(labels_full)

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (stringr::str_detect(param$dataset, "datasus_siasus")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }

  if (stringr::str_detect(param$dataset, "datasus_sim")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon, dtobito) %>%
      tibble::as_tibble()
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sinasc|datasus_po")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }
  if (stringr::str_detect(param$dataset, "datasus_sih")) {

    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      dplyr::select(tidyselect::where(~ !(all(is.na(.)) || all(. == 0, na.rm = TRUE)))) %>% # Remove colunas que so possuem 0 e NA
      tibble::as_tibble()
  }

  dic <- load_dictionary(param$dataset)

  if (param$language == "pt") {
    var_names <- dic$name_pt
  }
  if (param$language == "eng") {
    var_names <- dic$name_eng
  }

  names(var_names) <- dic$var_code

  dat_mod <- dat_mod %>%
    dplyr::rename_with(
      ~ dplyr::recode(., !!!var_names)
    )


  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}


expand_cid_code <- function(cid) {
  # Turns a character "A050-B010" into an expanded vector c("A050", "A051", ..., "B010")
  # Also turns "A001, B001-B002" into c("A001", "B001", "B002")

  . <- NULL

  if (!(stringr::str_detect(cid, ",|-"))) {
    return(cid)
  }

  # Splitting sections separated by commas into a list
  cid <- cid %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.list()

  # Each element of the list becomes a vector with the initial and final value, separated by "-"
  cid <- cid %>%
    purrr::map(
      function(string) {
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
      function(code) {
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
      function(code) {
        letter <- as.integer(code / 1000)
        number <- code - 1000 * letter

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
