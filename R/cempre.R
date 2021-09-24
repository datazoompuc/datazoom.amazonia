#' @title CEMPRE - Central Register of Companies
#'
#'
#' @description Loads information on companies and other organizations and their respective formally constituted local units, registered with the CNPJ - National Register of Legal Entities. Data is available from 2006 to 2019. See \url{https://sidra.ibge.gov.br/pesquisa/cempre/tabelas}
#'
#' @encoding UTF-8
#'
#' @param dataset A dataset name ("cempre").
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality". See documentation of \code{sidrar}.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#' @param sectors A \code{boolean} that defines if the data will be return separated by sectors (\code{TRUE}) or not (\code{FALSE}). Defaults to \code{FALSE}
#' @param legal_amazon_only A \code{boolean} setting the return of Legal Amazon Data (\code{TRUE}) or Country's Data (\code{FALSE}). Defaults to \code{FALSE}
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download raw data from 2006 to 2019
#' raw_cempre_all <- load_cempre(dataset = "cempre",
#'                               raw_data = TRUE,
#'                               geo_level = "municipality",
#'                               time_period = 2006:2019)
#' }
#'
#' @importFrom magrittr %>%
#' @export

load_cempre <- function(dataset = "cempre", raw_data,
                        geo_level, time_period,
                        language = "eng", sectors = FALSE,
                        legal_amazon_only = FALSE) {


  sidra_code <- available_time <- AMZ_LEGAL <- municipio_codigo <- ano <- ano_codigo <- classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo <- geo_id <- id_code <- nivel_territorial <- nivel_territorial_codigo <- valor <- variavel <- unidade_de_medida <- unidade_de_medida_codigo <- NULL

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


dat = dat %>%
  janitor::clean_names() %>%
  dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})


dat = dat %>%
  dplyr::select(-c(nivel_territorial_codigo,nivel_territorial,ano_codigo)) %>%
  dplyr::mutate(valor=as.numeric(valor))

## Only Keep Valid Observations

dat = dat %>%
  dplyr::filter(!is.na(valor))


if(geo_level == 'country'){
  dat$geo_id = dat$brasil
  dat = dplyr::select(dat,-'brasil_codigo',-'brasil')
}

if (geo_level == 'state'){
  dat$geo_id = dat$unidade_da_federacao_codigo
  dat = dplyr::select(dat,-'unidade_da_federacao_codigo',-'unidade_da_federacao')
}

if (geo_level == 'municipality'){
  dat$geo_id = dat$municipio_codigo
  dat = dplyr::select(dat,-'municipio',-'municipio_codigo')
}


################################
## Harmonizing Variable Names ##
################################

dat = dat %>%
  dplyr::select(-unidade_de_medida,-unidade_de_medida_codigo)

if(sectors == TRUE){dat = dat %>%
  dplyr::mutate(id_code = dplyr::case_when(variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117897 ~ "117897_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116830 ~ "116830_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116880 ~ "116880_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 116910 ~ "116910_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117296 ~ "117296_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117307 ~ "117307_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117329 ~ "117329_4",

                            variavel_codigo == 2585 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_1",
                             variavel == "Pessoal ocupado total" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_2",
                             variavel == "Pessoal ocupado assalariado" &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_3",
                             variavel_codigo == 662 &
                               classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117363 ~ "117363_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117484 ~ "117484_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117543 ~ "117543_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117555 ~ "117555_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117608 ~ "117608_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117666 ~ "117666_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117673 ~ "117673_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117714 ~ "117714_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117774 ~ "117774_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117788 ~ "117788_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117810 ~ "117810_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117838 ~ "117838_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117861 ~ "117861_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117888 ~ "117888_4",

                            variavel_codigo == 2585
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_1",
                             variavel == "Pessoal ocupado total"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_2",
                             variavel == "Pessoal ocupado assalariado"
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_3",
                             variavel_codigo == 662
                             & classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo == 117892 ~ "117892_4"
  ))}



dat = dat %>%
  dplyr::select(-'classificacao_nacional_de_atividades_economicas_cnae_2_0') %>%
  dplyr::arrange(classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo,variavel) %>%
  tidyr::pivot_wider(id_cols = c(ano, geo_id),
                     names_from = c(variavel, id_code),
                     values_from = valor,
                     names_sep = '_V',
                     values_fn = sum,
                     values_fill = NA) %>%
  janitor::clean_names()



labelled <- function(x, label) {
  Hmisc::label(x) <- label
  x
}

label_data_eng = function(df,cols,dic){

  label_value = as.character(dic[dic$id_code == cols,'var_eng'])

  df = df %>%
    dplyr::mutate_at(dplyr::vars(tidyr::matches(cols)),
                     ~ labelled(.,as.character(dic[dic$id_code == cols,'var_eng']))
    )

  return(df)

}


label_data_pt = function(df,cols,dic){

  label_value = as.character(dic[dic$id_code == cols,'var_pt'])

  df = df %>%
    dplyr::mutate_at(dplyr::vars(tidyr::matches(cols)),
                     ~ labelled(.,as.character(dic[dic$id_code == cols,'var_pt']))
    )

  return(df)

}



## Load Dictionary

dic = load_dictionary(param$dataset)

types = as.character(dic$id_code)
types = types[types != "0"] ## Remove 0


if (language == 'eng'){
  for (i in 1:length(types)){

    dat = label_data_eng(dat,cols=types[i],dic=dic)

  }

}

if (language == 'pt'){

  for (i in 1:length(types)){

    dat = label_data_pt(dat,cols=types[i],dic=dic)

  }
}

remove_numbers = function(string){

  stringr::str_remove(string = string, pattern = "_\\d")}

##########################
## Returning Data Frame ##
##########################

 return(dat)
}
