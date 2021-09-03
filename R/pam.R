#' @title PAM - Municipal Agricultural Production
#'
#' @description Loads information on the  temporary and permanent crops of the Country that are characterized not only by their great economic importance in the export agenda, but also by their social relevance, since its components are on the Brazilian table (IBGE). Survey is done at the municipal level and data is available from 1974 to 2019 for most datasets. See \url{https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/16773-municipal-agricultural-production-temporary-and-permanent-crops.html?=&t=o-que-e}
#'
#' @param dataset A dataset name ("pam_all_crops", "pam_permanent_crops", "pam_temporary_crops" or "pam_xx", in which xx needs to be corn, potato, peanut or beans). You can also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/pam/tabelas})
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality". See documentation of \code{sidrar}.
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported. Defaults to "eng".
#'
#' @return A \code{tibble} consisting of geographic units that present positive values for any of the variables in the dataset.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples \dontrun{
#' # download state raw data from 2012 for all crops
#' pam_all_crops <- load_pam(dataset = 'pam_all_crops',
#'                           raw_data = TRUE,
#'                           geo_level = 'state',
#'                           time_period = 2012)
#' }

load_pam = function(dataset = NULL, raw_data,
                    geo_level, time_period,
                    language = "eng") {

  ## Translation is only made through collapsing at the end
  # - What if we wanted to deliver raw data?

  ## To-Dos:
  ## Include Progress Bar
  ## Include Labels
  ## Support for Raw Downloads in English
  ## Write Vignettes

  ##############################
  ## Binding Global Variables ##
  ##############################

  sidra_code <- nivel_territorial_codigo <- nivel_territorial <- nivel_territorial_codigo <- NULL
  unidade_de_medida_codigo <- variavel_codigo <- ano_codigo <- valor <- NULL
  produto_das_lavouras_temporarias_e_permanentes_codigo  <- NULL
  produto_das_lavouras_temporarias_e_permanentes <- NULL
  produto_das_lavouras_permanentes_codigo <- NULL
  produto_das_lavouras_permanentes <- NULL
  produto_das_lavouras_temporarias_codigo <- NULL
  produto_das_lavouras_temporarias <- NULL
  geo_id <- ano <- variavel <- produto_das_lavouras <- NULL
  unidade_de_medida <- available_time <- vars <- NULL
  produto_das_lavouras_codigo <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
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

  if (is.null(dataset)){stop('Missing Dataset!')}

  if (param$code == 1612){ ## This is a subset of all crops
    param$data_name = 'Temporary Crops (Lavouras Temporarias)'
  }

  if (param$code == 1613){ ## This is a subset of all crops
    param$data_name = 'Permanent Crops (Lavouras Permanente)'
  }

  if (param$code == 5457){
    param$data_name = 'All Crops (Lavouras)'
  }


  if (param$code %in% c(839,1001,1000,1002)){
    param$data_name = 'Crop with more than one harvest (Corn, Potato, Peanut or Beans)'
  }

  ##############
  ## Download ##
  ##############

  # We need to show year that is being downloaded as well
  # Heavy Datasets may take several minutes

  dat = as.list(as.character(param$time_period)) %>%
    purrr::map(function(year_num){
      #suppressMessages(
        sidra_download(sidra_code = param$code,year = year_num,geo_level = param$geo_level)
        #)
      }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  ## Return Raw Data

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Enginnering ##
  ######################

  dat = dat %>%
          janitor::clean_names() %>%
          dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})# %>%
          # dplyr::mutate_all(clean_custom)

  dat = dat %>%
    dplyr::select(-c(nivel_territorial_codigo,nivel_territorial,ano_codigo)) %>%
    dplyr::mutate(valor=as.numeric(valor))

  ## Only Keep Valid Observations

  dat = dat %>%
    dplyr::filter(!is.na(valor))

  #########################################
  ## Create Geographical Unit Identifier ##
  #########################################

  if(geo_level == 'country'){
    dat$geo_id = dat$brasil
    dat = dplyr::select(dat,-'brasil_codigo',-'brasil')
  }
  if (geo_level == 'region'){
    dat$geo_id = dat$grande_regiao
    dat = dplyr::select(dat,-'grande_regiao_codigo',-'grande_regiao')
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

  ## Change Variable Names for Common Across Datasets

  if (param$code == 5457){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_e_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_temporarias_e_permanentes)
  }

  if (param$code == 1613){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_permanentes_codigo,
                    produto_das_lavouras = produto_das_lavouras_permanentes)
  }


  if (param$code %in% c(839,1000,1001,1002,1612)){
    dat = dat %>%
      dplyr::rename(produto_das_lavouras_codigo = produto_das_lavouras_temporarias_codigo,
                  produto_das_lavouras = produto_das_lavouras_temporarias)
  }

  ## Translation

  if (language == 'pt'){

    dat = dat %>%
      dplyr::mutate(variavel = dplyr::case_when(
        (variavel_codigo == '112') ~ 'rend_medio', # Rendimento medio da producao
        (variavel_codigo == '214') ~ 'quant', # Quantidade produzida
        (variavel_codigo == '215') ~ 'valor', # Valor da producao
        (variavel_codigo == '216') ~ 'area_colhida', # Area colhida
        (variavel_codigo == '8331') ~ 'area_plantada_dest_colheita', # Area plantada ou destinada a colheita,
        (variavel_codigo == '109') ~ 'area_plantada' # Area plantada,
        )
      )
  }

  if (language == 'eng'){

    dat = dat %>%
      dplyr::mutate(variavel = dplyr::case_when(
        (variavel_codigo == '112') ~ 'avg_yield', # Rendimento medio da producao
        (variavel_codigo == '214') ~ 'quant', # Quantidade produzida
        (variavel_codigo == '215') ~ 'value', # Valor da producao
        (variavel_codigo == '216') ~ 'harvested_area', # Area colhida
        (variavel_codigo == '8331') ~ 'planted_to_harvest_area', # Area plantada ou destinada a colheita
        (variavel_codigo == '109') ~ 'planted_area' # Area plantada,
      )
      )

  }


  #############################
  ## Create Long Format Data ##
  #############################

  ## The Output is a tibble with unit and year identifiers + production and/or value of each item

  dat = dat %>%
    dplyr::select(-'produto_das_lavouras') %>%
    dplyr::arrange(produto_das_lavouras_codigo,variavel) %>%
    tidyr::pivot_wider(id_cols = c(geo_id,ano),
                       names_from = variavel:produto_das_lavouras_codigo,
                       values_from=valor,
                       names_sep = '_V',
                       values_fn = sum,
                       values_fill = NA) %>%
    janitor::clean_names()

  ########################
  ## Changing Year Name ##
  ########################


  if (language == 'eng'){

    dat = dat %>%
      dplyr::rename(year = ano)
  }

  ###############
  ## Labelling ##
  ###############

  labelled <- function(x, label) {
    Hmisc::label(x) <- label
    x
  }

  label_data_eng = function(df,cols,dic){

    label_value = as.character(dic[dic$var_code == cols,'var_eng'])

    df = df %>%
      dplyr::mutate_at(dplyr::vars(tidyr::matches(cols)),
                       ~ labelled(.,as.character(dic[dic$var_code == cols,'var_eng']))
      )

    return(df)

  }

  label_data_pt = function(df,cols,dic){

    label_value = as.character(dic[dic$var_code == cols,'var_pt'])

    df = df %>%
      dplyr::mutate_at(dplyr::vars(tidyr::matches(cols)),
                       ~ labelled(.,as.character(dic[dic$var_code == cols,'var_pt']))
      )

    return(df)

  }

  ## Load Dictionary

  dic = load_dictionary(param$dataset)

  types = as.character(dic$var_code)
  types = types[types != "0"] ## Remove 0


  if (language == 'eng'){

    # f = dat %>%
    #   dplyr::mutate_at(vars(tidyr::matches(as.character(types[1]))),
    #                    ~ labelled::set_variable_labels(. = as.character(dic[dic$var_code == types[1],'var_eng']))
    #   )

    for (i in 1:length(types)){

      dat = label_data_eng(dat,cols=types[i],dic=dic)

    }

  }

  if (language == 'pt'){

    for (i in 1:length(types)){

      dat = label_data_pt(dat,cols=types[i],dic=dic)

    }
  }

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)

}
