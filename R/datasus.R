#' @title DATASUS - Mortality, hospitalizations and hospital beds
#'
#' @description Loads DATASUS data on health establishments, mortality, access to health services and several health indicators.
#'
#' @param dataset A dataset name, can be one of ("datasus_sim_do", "datasus_sih", "datasus_cnes_lt", "datasus_sinasc), or more. For more details, try \code{vignette("DATASUS")}.
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
  stcesparto <- tpnascassi <- codmunnasc <- dataset_prefix_map <- NULL


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

  prefix <- dataset_prefix_map[[param$dataset]]

  if (!is.null(prefix)) {
    filenames <- filenames[grepl(paste0("^", prefix), filenames)]
  }

  ### Filtering by year

  file_years <- NULL
  file_years_yy <- NULL
  file_month_mm <- NULL

  if (param$dataset %in% c("datasus_sim_do", "datasus_sinasc")) {
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

  if (stringr::str_detect(param$dataset, "datasus_sih_rd|datasus_sih_rj|datasus_sih_sp|datasus_sih_er")) {
    file_years_yy <- filenames %>%
      substr(5, 6)
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

  if (param$dataset %in% c("datasus_sim_do", "datasus_sih", "datasus_sinasc", "datasus_sih_rd", "datasus_sih_rj", "datasus_sih_sp", "datasus_sih_er") | stringr::str_detect(param$dataset, "datasus_cnes")) {
    file_state <- filenames %>%
      substr(3, 4)
  } else if (paste0(param$states, collapse = "") != "all") {
    base::message("Filtering by state not supported for all datasets. Data for other states will be included.")
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

  if (param$dataset %in% c("datasus_sih_rd", "datasus_sih_er", "datasus_sih_rj", "datasus_sih_sp")) {
    suffix <- stringr::str_remove(param$dataset, "datasus_sih_") %>%
      toupper()

    filenames <- filenames[stringr::str_detect(filenames, suffix)]
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

  if (param$dataset == "datazoom_sih") {

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

  if(param$dataset == "datasus_sinasc" ) {

      if (param$lang == "pt") {
        dat <- dat %>%

          dplyr::mutate(
            origem = dplyr::case_when(origem == '1' ~ "oracle", origem == '2' ~ "ftp", origem == '3' ~ "sead", TRUE ~ origem),
            locnasc = dplyr::case_when(locnasc == '1' ~ "hospital", locnasc == '2' ~ "outros estabelecimentos de saude", locnasc == '3' ~ "domicilio", locnasc == '4' ~ "outros", locnasc == '5' ~ "aldeia indigena", locnasc == '9' ~ "ignorado", TRUE ~ locnasc),
            estcivmae = dplyr::case_when(estcivmae == '1' ~ "solteira", estcivmae == '2' ~ "casada", estcivmae == '3' ~ "viuva", estcivmae == '4' ~ "divorciada", estcivmae == '5' ~ "uniao estavel", estcivmae == '9' ~ "ignorada", TRUE ~ estcivmae),
            escmae = dplyr::case_when(escmae == '1' ~ "nenhuma", escmae == '2' ~ "1 a 2 anos", escmae == '3' ~ "4 a 7 anos", escmae == '4' ~ "8 a 11 anos", escmae == '5' ~ "12 e mais", escmae == '9' ~ "ignorado", TRUE ~ escmae),
            semagestac = dplyr::case_when(semagestac == '1' ~ "menos de 22 semanas", semagestac == '2' ~ "22 a 27 semanas", semagestac == '3' ~ "28 a 31 semanas", semagestac == '4' ~ "32 a 36 semanas", semagestac == '5' ~ "37 a 41 semanas", semagestac == '6' ~ "42 semanas e mais", semagestac == '9' ~ "ignorado", TRUE ~ semagestac),
            gravidez = dplyr::case_when(gravidez == '1' ~ "unica", gravidez == '2' ~ "dupla", gravidez == '3' ~ "tripla ou mais", gravidez == '9' ~ "ignorado", TRUE ~ gravidez),
            parto = dplyr::case_when(parto == '1' ~ "vaginal", parto == '2' ~ "cesario", parto == '9' ~ "ignorado", TRUE ~ parto),
            consprenat = dplyr::case_when(consprenat == '1' ~ "nenhuma", consprenat == '2' ~ "de 1 a 3", consprenat == '3' ~ "de 4 a 6", consprenat == '4' ~ "7 e mais", consprenat == '9' ~ "ignorado", TRUE ~ consprenat),
            sexo = dplyr::case_when(sexo == '0' ~ "ignorado", sexo == '1' ~ "masculino", sexo == '2' ~ "feminino", TRUE ~ sexo),
            racacor = dplyr::case_when(racacor == '1' ~ "branca", racacor == '2' ~ "preta", racacor == '3' ~ "amarela", racacor == '4' ~ "parda", racacor == '5' ~ "indigena", TRUE ~ racacor),
            idanomal = dplyr::case_when(idanomal == '9' ~ "ignorado", idanomal == '1' ~ "sim", idanomal == '2' ~ "nao", TRUE ~ idanomal),
            escmae2010 = dplyr::case_when(escmae2010 == '0' ~ "sem escolaridade", escmae2010 == '1' ~ "fundamental 1", escmae2010 == '2' ~ "fundamental 2", escmae2010 == '3' ~ "medio", escmae2010 == '4' ~ "superior incompleto", escmae2010 == '5' ~ "superior completo", escmae2010 == '9' ~ "ignorado", TRUE ~ escmae2010),
            dtnascmae = lubridate::dmy(as.character(dtnascmae)),
            racacormae = dplyr::case_when(racacormae == '1' ~ "branca", racacormae == '2' ~ "preta", racacormae == '3' ~ "amarela", racacormae == '4' ~ "parda", racacormae == '5' ~ "indigena", TRUE ~ racacormae),
            dtultmenst = lubridate::dmy(as.character(dtultmenst)),
            tpmetestim = dplyr::case_when(tpmetestim == '1' ~ "exame fisico", tpmetestim == '2' ~ "outro metodo", tpmetestim == '9' ~ "ignorado", TRUE ~ tpmetestim),
            tpapresent = dplyr::case_when(tpapresent == '1' ~ "cefalica", tpapresent == '2' ~ "pelvica ou podalica", tpapresent == '3' ~ "transversa", tpapresent == '9' ~ "ignorado", TRUE ~ tpapresent),
            sttrabpart = dplyr::case_when(sttrabpart == '1' ~ "sim", sttrabpart == '2' ~ "nao", sttrabpart == '9' ~ "ignorado", TRUE ~ sttrabpart),
            stcesparto = dplyr::case_when(stcesparto == '1' ~ "sim", stcesparto == '2' ~ "nao", stcesparto == '3' ~ "nao se aplica", stcesparto == '9' ~ "ignorado", TRUE ~ stcesparto),
            tpnascassi = dplyr::case_when(tpnascassi == '1' ~ "medico", tpnascassi == '2' ~ "enfermeira obstetriz", tpnascassi == '3' ~ "parteira", tpnascassi == '4' ~ "outros", tpnascassi == '9' ~ "ignorado", TRUE ~ tpnascassi)
          )
      }

    if (param$lang == "eng") {
        dat <- dat %>%

          dplyr::mutate(
            origem = dplyr::case_when(origem == '1' ~ "oracle", origem == '2' ~ "ftp", origem == '3' ~ "sead", TRUE ~ origem),
            locnasc = dplyr::case_when(locnasc == '1' ~ "hospital", locnasc == '2' ~ "other health facilities", locnasc == '3' ~ "domicile", locnasc == '4' ~ "others", locnasc == '5' ~ "indigenous village", locnasc == '9' ~ "ignored", TRUE ~ locnasc),
            estcivmae = dplyr::case_when(estcivmae == '1' ~ "single", estcivmae == '2' ~ "married", estcivmae == '3' ~ "widowed", estcivmae == '4' ~ "divorced", estcivmae == '5' ~ "stable union", estcivmae == '9' ~ "ignored", TRUE ~ estcivmae),
            escmae = dplyr::case_when(escmae == '1' ~ "none", escmae == '2' ~ "1 to 3 years", escmae == '3' ~ "4 to 7 years", escmae == '4' ~ "8 to 11 years", escmae == '5' ~ "12 or more", escmae == '9' ~ "ignored", TRUE ~ escmae),
            semagestac = dplyr::case_when(semagestac == '1' ~ "less than 22 weeks", semagestac == '2' ~ "22 to 27 weeks", semagestac == '3' ~ "28 to 31 weeks", semagestac == '4' ~ "32 to 36 weeks", semagestac == '5' ~ "37 to 41 weeks", semagestac == '6' ~ "42 weeks or more", semagestac == '9' ~ "ignored", TRUE ~ semagestac),
            gravidez = dplyr::case_when(gravidez == '1' ~ "single", gravidez == '2' ~ "twin", gravidez == '3' ~ "triplet or more", gravidez == '9' ~ "ignored", TRUE ~ gravidez),
            parto = dplyr::case_when(parto == '1' ~ "vaginal", parto == '2' ~ "cesarean", parto == '9' ~ "ignored", TRUE ~ parto),
            consprenat = dplyr::case_when(consprenat == '1' ~ "none", consprenat == '2' ~ "1 to 3", consprenat == '3' ~ "4 to 6", consprenat == '4' ~ "7 or more", consprenat == '9' ~ "ignored", TRUE ~ consprenat),
            sexo = dplyr::case_when(sexo == '0' ~ "ignored", sexo == '1' ~ "male", sexo == '2' ~ "female", TRUE ~ sexo),
            racacor = dplyr::case_when(racacor == '1' ~ "white", racacor == '2' ~ "black", racacor == '3' ~ "yellow", racacor == '4' ~ "brown", racacor == '5' ~ "indigenous", TRUE ~ racacor),
            idanomal = dplyr::case_when(idanomal == '1' ~ "yes", idanomal == '2' ~ "no", idanomal == '9' ~ "ignored", TRUE ~ idanomal),
            escmae2010 = dplyr::case_when(escmae2010 == '0' ~ "no schooling", escmae2010 == '1' ~ "primary 1", escmae2010 == '2' ~ "primary 2", escmae2010 == '3' ~ "secondary", escmae2010 == '4' ~ "incomplete higher", escmae2010 == '5' ~ "complete higher", escmae2010 == '9' ~ "ignored", TRUE ~ escmae2010),
            dtnascmae = lubridate::dmy(as.character(dtnascmae)),
            racacormae = dplyr::case_when(racacormae == '1' ~ "white", racacormae == '2' ~ "black", racacormae == '3' ~ "yellow", racacormae == '4' ~ "brown", racacormae == '5' ~ "indigenous", TRUE ~ racacormae),
            dtultmenst = lubridate::dmy(as.character(dtultmenst)),
            tpmetestim = dplyr::case_when(tpmetestim == '1' ~ "physical exam", tpmetestim == '2' ~ "other method", tpmetestim == '9' ~ "ignored", TRUE ~ tpmetestim),
            tpapresent = dplyr::case_when(tpapresent == '1' ~ "cephalic", tpapresent == '2' ~ "pelvic or podalic", tpapresent == '3' ~ "transverse", tpapresent == '9' ~ "ignored", TRUE ~ tpapresent),
            sttrabpart = dplyr::case_when(sttrabpart == '1' ~ "yes", sttrabpart == '2' ~ "no", sttrabpart == '9' ~ "ignored", TRUE ~ sttrabpart),
            stcesparto = dplyr::case_when(stcesparto == '1' ~ "yes", stcesparto == '2' ~ "no", stcesparto == '3' ~ "not applicable", stcesparto == '9' ~ "ignored", TRUE ~ stcesparto),
            tpnascassi = dplyr::case_when(tpnascassi == '1' ~ "doctor", tpnascassi == '2' ~ "obstetric nurse", tpnascassi == '3' ~ "midwife", tpnascassi == '4' ~ "others", tpnascassi == '9' ~ "ignored", TRUE ~ tpnascassi)
          )
      }

    dat <- dat %>%
      dplyr::mutate(codmunnasc = as.numeric(as.character(codmunnasc)),
                    dtrecebim = lubridate::dmy(as.character(dtrecebim)),
                    dtcadastro = lubridate::dmy(as.character(dtcadastro))) %>%
      dplyr::rename(code_muni_6 = codmunnasc)
  }

  if (param$dataset %in% c("datasus_sih_rd", "datasus_sih_rj", "datasus_sih_sp", "datasus_sih_er")) {

    if(param$dataset %in% c("datasus_sih_er")) {
      dat <- dat %>%
        dplyr::mutate(mun_res = as.integer(as.character(mun_res))) %>%
        dplyr::rename(code_muni_6 = mun_res)
    }

    if(param$dataset %in% c("datasus_sih_sp")) {
      dat <- dat
    }

    if(param$dataset %in% c("datasus_sih_rd", "datasus_sih_rj")) {
      dat <- dat %>%
        dplyr::mutate(munic_res = as.integer(as.character(munic_res))) %>%
        dplyr::rename(code_muni_6 = munic_res)
    }

  }

  if (!(param$dataset %in% c("datasus_sih"))) {
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

    if(param$dataset != "datasus_sih_sp") {

      geo <- geo %>%
        dplyr::mutate(code_muni_6 = as.integer(code_muni / 10)) %>%
        dplyr::distinct(code_muni_6, .keep_all = TRUE) # Only keeps municipalities uniquely identified by the 6 digits

      dat <- dat %>%
        dplyr::left_join(geo, by = "code_muni_6")
    }
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

  if (stringr::str_detect(param$dataset, "datasus_sim")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon, dtobito) %>%
      tibble::as_tibble()
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sinasc")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }

  if (stringr::str_detect(param$dataset, "datasus_sih_rd|datasus_sih_er|datasus_sih_rj")) {
    dat_mod <- dat %>%
      dplyr::select(where(~ !is.numeric(.) || !all(. == 0 | is.na(.)))) %>%   # remove colunas com todos os valores iguais a 0
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }

  if (stringr::str_detect(param$dataset, "datasus_sih_sp")) {
    dat_mod <- dat %>%
      dplyr::select(where(~ !is.numeric(.) || !all(. == 0 | is.na(.)))) %>%   # remove colunas com todos os valores iguais a 0
      tibble::as_tibble()
  }

  if (param$dataset == "datasus_sih") {
    dat_mod <- dat %>%
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
