
## The Downloaded Data is from 760 Municipalities
  ## Are these the ones in the Legal Amazon


load_prodes <- function(time_period, geo_level = "municipality", language = "eng") {

  #################
  ## Import Data ##
  #################

  ## Function to Construct URLs

  urls_prodes <- function(year) {
    # Generates urls to get prodes data from corresponding year
    paste0(
      "http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=",
      year,
      "&estado=&ordem=MUNICIPIO&type=tabela&output=txt&"
    )
  }

  ## For each year, we are going to construct the urls for download
    ## Previous version included support for direct download and data import
    ## We need to run more checks to see if this procedure is robust

  urls <- purrr::map(time_period, urls_prodes)

  # if (is.numeric(source)) {
  #   urls <- purrr::map(source, urls_prodes)
  # }

  # If source is a directory, we expand and filter the list of files
    # else if (is.character(source) && length(source) == 1 && dir.exists(source)) {
    #   source <-
    #     list.files(path = source, full.names = TRUE) %>%
    #     grep(pattern = "DesmatamentoMunicipios\\d{4}.txt", value = TRUE)
    # }
  # Otherwise, we assume that source is something that can already be interpreted by read.csv()

  # Reads the csvs containing the data
  dat_raw = lapply(urls, readr::read_csv,
                   col_types = readr::cols("d", "d", "d", "c", "c", "c", "c", "c",
                                           "d", "d", "d", "d", "d", "d", "d", "d", "d"),
                   locale = readr::locale(encoding = "latin1"))

  ######################
  ## Data Engineering ##
  ######################

  # Treating data according to parameters selected
  dat <- lapply(dat_raw, treat_prodes_data,
                    geo_level = geo_level,
                    language = language) %>%
    dplyr::bind_rows()

  return(dat)
}


treat_prodes_data <- function(df, geo_level, language) {

  ####################
  ## Data Carpentry ##
  ####################

  geo_level <- tolower(geo_level)

  df = df %>%
    janitor::clean_names() %>%
    # Adds column with UF codes, eg. MA = 21
    dplyr::mutate(cod_uf = as.factor(base::substr(cod_ibge,start=1,stop=2))) %>%
    dplyr::rename(cod_munic_ibge = cod_ibge) %>%
    # Removes unnecessary columns
    dplyr::select(-c('lat','long','latgms','longms','municipio','nr','soma'))

  if (geo_level == "state") {

    df = df %>%
      dplyr::group_by(estado,cod_uf) %>%
      # Collapses data by state
      dplyr::summarize_if(is.numeric, sum, na.rm = TRUE)
  }

  if (geo_level == "municipality") {
    df = df %>%
      janitor::clean_names() %>%
      dplyr::mutate(cod_munic_ibge = as.factor(cod_munic_ibge)) %>%
      dplyr::relocate(cod_uf,.after=estado)
  }

  if (!(geo_level %in% c('municipality','state'))){

    stop("Aggregation level not supported. Please see documentation.")

  }

  ###############################
  ## Extract Year From Columns ##
  ###############################

  df$ano =
    colnames(df) %>%
    purrr::detect(function(x) startsWith(x, "des")) %>%
    gsub(pattern = ".*(\\d{4}).*", replacement = "\\1") %>%
    as.numeric()


  # Removes year from column name
  colnames(df) <- gsub("(.*)\\d{4}?", "\\1", colnames(df))

  df = df %>%
    dplyr::relocate(ano,.after=cod_munic_ibge)

  #################
  ## Translation ##
  #################

  language <- tolower(language)

  if (language == 'eng'){
    df = df %>%
      dplyr::rename(
        #munic_code_ibge = cod_munic_ibge,
        state = estado,
        deforestation = desmatado,
        increment = incremento,
        forest = floresta,
        cloud = nuvem,
        not_observed = nao_observado,
        not_forest = nao_floresta,
        hydrography = hidrografia,
        year = ano)
} else if (language != "pt") {
    warning("Selected language is not supported. Proceeding with Portuguese.")
  }


  return(df)
}


