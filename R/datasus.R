#' DATASUS
#'
#' @param dataset A dataset name, can be one of ("datasus_sim", "satasus_sih", "datasus_cnes"), or subsets thereof. For more details, try \code{vignette("DATASUS")}.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY. Can be any vector of numbers, such as 2010:2012.
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param keep_all A \code{boolean} choosing whether to aggregate the data by municipality, in turn losing individual-level variables (\code{FALSE}) or to keep all the original variables. Only applies when raw_data is \code{TRUE}.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @examples\dontrun{
#' mortality_rj <- load_datasus(dataset = "datasus_sim", time_period = 2010, states = "RJ")
#' }
#'
#' @return A \code{tibble}
#' @export
load_datasus <- function(dataset,
                        time_period,
                        states = "all",
                        raw_data = FALSE,
                        keep_all = FALSE,
                        language = "eng"){

  #######################
  ## Combined Datasets ##
  #######################

  if (dataset == "datasus_sim") {
    dat <- list(
      "datasus_sim_do",
      "datasus_sim_dofet",
      "datasus_sim_doext",
      "datasus_sim_doinf",
      "datasus_sim_domat"
    ) %>%
      purrr::map(
        function(dataset){
          base::message(paste0("    ", dataset))
          load_datasus(
            dataset = dataset,
            time_period = time_period,
            states = states,
            raw_data = raw_data,
            keep_all = keep_all,
            language = language
          )
        }
      ) %>%
      dplyr::bind_rows()

    if (level == "municipality") {
      dat <- dat %>%
        dplyr::group_by(codmunocor, code_muni, name_muni, code_state, abbrev_state, dtobito) %>%
        dplyr::summarise(dplyr::across(dplyr::starts_with("t_"), ~ sum(., na.rm = TRUE)))
    }
  }
    if (dataset == "datasus_cnes") {
      dat <- list(
        "datasus_cnes_lt",
        "datasus_cnes_st",
        "datasus_cnes_dc",
        "datasus_cnes_eq",
        "datasus_cnes_sr",
        "datasus_cnes_hb",
        "datasus_cnes_pf",
        "datasus_cnes_ep",
        "datasus_cnes_rc",
        "datasus_cnes_in",
        "datasus_cnes_ee",
        "datasus_cnes_ef",
        "datasus_cnes_gm"
      ) %>%
        purrr::map(
          function(dataset){
            base::message(paste0("    ", dataset))
            load_datasus(
              dataset = dataset,
              time_period = time_period,
              states = states,
              raw_data = raw_data,
              keep_all = keep_all,
              language = language
            )
          }
        ) %>%
        dplyr::bind_rows()

    return(dat)
  }

  ###################
  ## Function Body ##
  ###################

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
  param$keep_all <- keep_all

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

  ### Filtering by year

  file_years <- NULL
  file_years_yy <- NULL

  if (param$dataset == "datasus_sim_do"){
    file_years <- filenames %>%
      substr(5, 8)
  }
  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
     file_years_yy <- filenames %>%
       stringr::str_extract("\\d+")
     # In this case, the position varies
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_sih")){
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

  if (param$dataset %in% c("datasus_sim_do", "datasus_sih") | stringr::str_detect(param$dataset, "datasus_cnes")){
    file_state <- filenames %>%
      substr(3, 4)
  }
  else if (param$states != "all") {
    base::message("Filtering by state not supported for all datasets. Data for other states will be included.")
  }

  if (!is.null(file_state)) {
    filenames <- filenames[file_state %in% param$states]
  }

  ### Filtering for the chosen dataset (they come clumped together in the same folder)

  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")){
    suffix <- stringr::str_remove(param$dataset, "datasus_sim_") %>%
      toupper()

    filenames <- filenames[stringr::str_detect(filenames, suffix)]
  }

  param$filenames <- filenames

  ### Downloading each file in filenames

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
        codmunocor = as.numeric(as.character(codmunocor)),

        dtobito = as.Date(dtobito, format = "%d%m%Y"),

        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) == "0" ~ NA_character_,
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3))
        )
      )

    dic_cid_codes <- load_dictionary("datasus") %>%
      dplyr::filter(is_cid_code)

    dat <- dic_cid_codes %>%
      purrr::transpose() %>%

      purrr::map_dfc(
        function(dic_row){
          dat %>%
            dplyr::mutate(value = dplyr::case_when(
                causabas %in% expand_cid_code(dic_row$var_code) ~ 1
              )
            ) %>%
            dplyr::select(value) %>%
            dplyr::rename_with(~ dic_row$var_code)
        }
      ) %>%
      dplyr::bind_cols(dat)

      dat <- dat %>%
        dplyr::rename("code_muni_6" = "codmunocor")
  }

  if (param$dataset == "datazoom_sih"){

  }

  if (stringr::str_detect(param$dataset, "datasus_cnes")){
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

  # Adding municipality data

  geo <- municipalities %>%
    dplyr::select(code_muni,
                  name_muni,
                  code_state,
                  abbrev_state,
                  legal_amazon)

  # Original data only has 6 IBGE digits instead of 7

  geo <- geo %>%
    dplyr::mutate(code_muni_6 = as.integer(code_muni/10)) %>%
    dplyr::distinct(code_muni_6, .keep_all = TRUE) # Only keeps municipalities uniquely identified by the 6 digits

  dat <- dat %>%
    dplyr::left_join(geo, by = "code_muni_6")

  #################
  ## Aggregating ##
  #################

  if (stringr::str_detect(param$dataset, "datasus_sim") & !param$keep_all){

    # Obtaining the mortality variables

    cid_vars <- load_dictionary("datasus") %>%
      dplyr::filter(is_cid_code) %>%
      dplyr::select(var_code) %>%
      unlist()

    names(cid_vars) <- NULL # To stop summarise from renaming the variables

    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, dtobito) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of(cid_vars), ~ sum(., na.rm = TRUE)))

  }

  if (param$dataset == "datasus_cnes_lt" & !param$keep_all) {
    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, year, month) %>%
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
  if (param$language == "eng"){
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

  if (stringr::str_detect(param$dataset, "datasus_sim")){
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, dtobito) %>%
      tibble::as_tibble()
  }
  if (stringr::str_detect(param$dataset, "datasus_cnes")){
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state) %>%
      tibble::as_tibble()
  }

  dic <- load_dictionary(param$dataset)

  if (param$language == "pt") {
    var_names <- dic$name_pt
  }
  if (param$language == "eng"){
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
