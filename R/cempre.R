load_cempre <- function(dataset = "cempre", raw_data = FALSE,
                        geo_level = "municipality",
                        time_period = 2017:2018,
                        language = "eng", sectors = FALSE,
                        legal_amazon_only = FALSE) {


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$raw_data = raw_data
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language

  if (!is.numeric(param$dataset)){
    param$code = datasets_link() %>%
      dplyr::filter(dataset == param$dataset) %>%
      dplyr::select(sidra_code) %>%
      unlist() %>%
      as.numeric()
  } else {param$code = param$dataset}

  ## Check if year is acceptable

  year_check = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(available_time) %>%
    unlist() %>% as.character() %>%
    stringr::str_split(pattern = '-') %>%
    unlist() %>% as.numeric()

  if (min(time_period) < year_check[1]){stop('Provided time period less than supported. Check documentation for time availability.')}
  if (max(time_period) > year_check[2]){stop('Provided time period greater than supported. Check documentation for time availability.')}

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}
  if (legal_amazon_only & geo_level != "municipality"){stop('legal_amazon_only = TRUE is only available for geo_level = "municipality".')}


  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  if (sectors & geo_level == "municipality"){warning("This may take too long")}

  ## Download separate by sectors
  if (sectors) {
    cnaes <- list("117897", "116830", "116880", "116910", "117296",
                  "117307", "117329", "117363", "117484", "117543",
                  "117555", "117608", "117666", "117673", "117714",
                  "117774", "117788", "117810", "117838", "117861",
                  "117888", "117892")

    year_cnaes <- purrr::cross2(as.character(param$time_period), cnaes)

    dat = year_cnaes %>%
      purrr::map(function(year_cnae) {
        #suppressMessages(
        sidra_download(sidra_code = param$code,
                       year = year_cnae[[1]],
                       geo_level = param$geo_level,
                       classific = c("C12762"),
                       category = list(year_cnae[[2]]))
        #)
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

  ## Download only the total
  } else {
    cnaes <- list("117897")

    dat = as.list(as.character(param$time_period)) %>%
      purrr::map(function(year_num){
        #suppressMessages(
        sidra_download(sidra_code = param$code,
                       year = year_num,
                       geo_level = param$geo_level,
                       classific = c("C12762"),
                       category = cnaes)
        #)
      }) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  }


  ## Filter for Legal Amazon
  if (legal_amazon_only) {
    legal_amazon_filtered <- legal_amazon %>% dplyr::filter(AMZ_LEGAL == 1)

    dat <- dat %>%
      dplyr::filter(municipio_codigo %in% unique(legal_amazon_filtered$CD_MUN))
  }


  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}


}
