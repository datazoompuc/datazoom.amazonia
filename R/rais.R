library(tidyverse)



download_data <- function(download_dir, y = 2005:2019){

  dir.create(file.path(download_dir, 'RAIS_microdata'))

  links <- gen_links_RAIS(dates = y)

  links %>%
    map(
      ~ .x %>%
        utils::download.file(
          url = .,
          destfile = file.path(download_dir, 'RAIS_microdata',
                               stringr::str_match(., '[^/]+$')),
          mode = "wb"
        )
    )

  files <- list.files(file.path(download_dir, 'RAIS_microdata'),
                      full.names = TRUE)


  names.files <- files %>%
    stringr::str_match(., '[^/]+$') %>%
    gsub('.7z', "", .)


  datasets <- files[1:20] %>%
    purrr::map(
      ~ read.table(
        archive_read(.),
        sep = ';',
        nrows = 10,
        header = TRUE,
        stringsAsFactors = TRUE)
    )

  names(datasets) <- names.files[1:20]

  datasets <- datasets %>%
    purrr::map(
      ~.x %>%
        janitor::clean_names() %>%
        dplyr::select(
          ano,
        ))

  dplyr::bind_rows(datasets, .id = 'id') %>%
    dplyr::mutate(
      id = stringr::str_sub(id, start = -4)
    ) %>%
    dplyr::rename(ano = id)

}




gen_links_RAIS <- function(dates){

  links_preamble <- paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/")
  states <- c('AC', 'AM', 'AP', 'MA', 'PA', 'RO', 'RR', 'TO', 'MT')
  regions <- c('NORTE', 'NORDESTE', 'CENTRO_OESTE')

  dates_2018or2019 <- dates[dates %in% 2018:2019]
  dates_pre2018 <- dates[!dates %in% 2018:2019]

  if (!is_empty(dates_2018or2019)){

    links_2018or2019 <- purrr::map(dates_2018or2019,
                                   ~ paste0(links_preamble, . , '/RAIS_VINC_PUB_', regions, '.7z')) %>%
      purrr::flatten()
  } else{
    links_2018or2019 <- NA
  }


  if (!is_empty(dates_pre2018)){

    links_pre2018 <- purrr::map(dates_pre2018,
                                ~ paste0(links_preamble, . , '/', states, .,  '.7z')) %>%
      purrr::flatten()
  } else{
    links_pre2018 <- NA
  }

  links <- c(links_2018or2019, links_pre2018)
  links <- links[!is.na(links)]

  return(links)
}



agg_RAIS <- function(df,
                     g = c('municipality', 'state'),
                     by_sex = FALSE,
                     by_race = FALSE,
                     by_age_bracket = FALSE,
                     by_occupation = FALSE,
                     by_cnae_class = FALSE,
                     by_wage_bracket = FALSE,
                     by_firm_size = FALSE,
                     by_schooling = FALSE){


  df <- df %>%
    dplyr::filter(vinculo_ativo_31_12 == 1) %>%
    dplyr::mutate(
      vl_remun_media_nom = gsub(',', '.', vl_remun_media_nom),
      uf = substr(municipio, 1, 2))

  sex = ifelse(by_sex == TRUE, "sexo_trabalhador", NA)
  race = ifelse(by_race == TRUE, "raca_cor", NA)
  age_bracket = ifelse(by_age_bracket == TRUE, "faixa_etaria", NA)
  occupation = ifelse(by_occupation == TRUE, "cbo_ocupacao_2002", NA)
  cnae = ifelse(by_cnae_class == TRUE, "cnae_2_0_classe", NA)
  wage_bracket = ifelse(by_wage_bracket == TRUE, "faixa_remun_dezem_sm", NA)
  firm_size = ifelse(by_firm_size == TRUE, "tamanho_estabelecimento", NA)
  schooling = ifelse(by_schooling == TRUE, "escolaridade_apos_2005", NA)
  municipality = ifelse('municipality' %in% g, 'municipio', NA)
  state = ifelse('state' %in% g, 'uf', NA)

  grouping = c(
    sex, race, age_bracket, occupation, cnae, wage_bracket, firm_size, schooling,
    municipality, state
  )
  grouping = grouping[!is.na(grouping)]

  df %>%
    dplyr::group_by(across(grouping)) %>%
    dplyr::summarise(
      vinculos = n(),
      massa_salarial = sum(as.numeric(vl_remun_media_nom), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      remuneracao_media = massa_salarial/vinculos
    )
}
