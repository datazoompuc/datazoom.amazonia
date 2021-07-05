#' @title PEVS Plant extraction
#'
#' Loads quantity produced and yield value of plant extraction, by type of product, from 1986 to 2019
#'
#' @param dataset A code or name indicating the dataset to be downloaded (ADD MORE INFO)
#'
#' @param geo_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param time_period A \code{vector} indicating what years will the data be loaded
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @return A \code{data frame} or a \code{list} of data frames if \code{long} is set to \code{TRUE}.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_pevs
#'
#' @examples \dontrun{datazoom.amazonia::load_pevs_vegextr(2013, aggregation_level = "country")}

load_pevs <- function(dataset = NULL, geo_level = "municipality", time_period = 2018:2019, language = "pt"){

  sidra_code  <- NULL
  nivel_territorial_codigo <- NULL
  nivel_territorial <- NULL
  unidade_de_medida_codigo <- NULL
  variavel_codigo <- NULL
  ano_codigo <- NULL
  valor <- NULL
  tipo_de_produto_extrativo_codigo <- NULL
  tipo_de_produto_da_silvicultura_codigo <- NULL
  tipo_de_produto_da_silvicultura <- NULL
  especie_florestal_codigo <- NULL
  especie_florestal <- NULL
  geo_id <- NULL
  ano <- NULL
  variavel <- NULL
  tipo_de_produto <- NULL
  tipo_de_produto_extrativo <- NULL

  # Measure Conversion Before Translation
  # Adjust Tipo de Produto (There are several names in the front of the number and I need to check on how to deal with them)
  # Check municipality names (D'Oeste)
  # Check if any observation geo-time at the final data have multiple NA entries -- this would mean the data is "wrong"


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

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}

  if (param$code == 289){
    param$data_name = 'Vegetal extraction quantity and value (Quantidade e valor da extração vegetal)'
  }

  if (param$code == 291){
    param$data_name = 'Forestry quantity and value (Quantidade e valor da silvicultura)'
  }

  if (param$code == 5930){
    param$data_name = 'Forestry area (Area da silvicultura)'
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

  #######################
  ## Cleaning Function ##
  #######################

  # We need to do check if this really works

  clean_custom = function(var){
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='ª',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_to_lower(string=var)
    #var = stringr::str_replace(string=var,pattern="__",replacement = "_")
    #var = stringr::str_replace(string=var,pattern=".",replacement = "_")
    return(var)
  }


  ######################
  ## Data Enginnering ##
  ######################

  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    dplyr::mutate_all(clean_custom)

  # We need to check if this works for all data

  dat = dat %>%
    dplyr::select(-c(nivel_territorial_codigo,nivel_territorial,
                    unidade_de_medida_codigo,variavel_codigo,
                    ano_codigo)) %>%
    dplyr::mutate(valor=as.numeric(valor))

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

  #############################################
  ## Adding Measure Information to Variables ##
  #############################################

  ## Needs to be adjusted according to different units of measurement (suggestion: ton, ha and brl)

  # dat = dat %>%
  #   dplyr::mutate(variavel = dplyr::case_when(
  #     (variavel == 'area_total_existente_em_31/12_dos_efetivos_da_silvicultura') ~ 'area_total_ha',
  #     (variavel == 'quantidade_produzida_na_extracao_vegetal') ~ 'quant_produzida_extracao_vegetal_ton',
  #     (variavel == 'quantidade_produzida_na_silvicultura') ~ 'quant_produzida_silvicultura_ton',
  #     (variavel == 'valor_da_producao_na_extracao_vegetal') ~ 'valor_da_prod_extracao_vegetal_brl',
  #     (variavel == 'valor_da_producao_na_silvicultura') ~ 'valor_da_prod_silvicultura_brl'
  #   )
  #   )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  # dat$valor[dat$tipo_de_produto == 'pinheiro_sla'] = dat$valor*(formular de convervsao)
  # BUscar info no google e deixar link comentado aqui
  # table(dat$tipo_de_produto, dat$unidade_de_medida)

  if (param$code == 289){
    dat = dat %>%
      dplyr::rename(tipo_de_produto_codigo = tipo_de_produto_extrativo_codigo,
                    tipo_de_produto = tipo_de_produto_extrativo)
  }

  if (param$code == 291){
    dat = dat %>%
      dplyr::rename(tipo_de_produto_codigo = tipo_de_produto_da_silvicultura_codigo,
                    tipo_de_produto = tipo_de_produto_da_silvicultura)
  }

  if (param$code == 5930){
    dat = dat %>%
      dplyr::rename(tipo_de_produto_codigo = especie_florestal_codigo,
                    tipo_de_produto = especie_florestal)
  }

  #############################
  ## Create Long Format Data ##
  #############################

  ## The Output is a tibble with unit and year identifiers + production and/or value of each item

  dat = dat %>%
    dplyr::select(-'unidade_de_medida',-'tipo_de_produto_codigo') %>%
    tidyr::pivot_wider(id_cols = c(geo_id,ano),
                      names_from = variavel:tipo_de_produto,
                      values_from=valor,
                      names_sep = '_',
                      values_fn = sum,
                      values_fill = 0) %>%
    janitor::clean_names()

  ########################
  ## Changing Year Name ##
  ########################

  if (language == 'eng'){names(dat)[which(names(dat) == 'ano')] = 'year'}

  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)
}
