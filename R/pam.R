#' @title PAM - Municipal Agricultural Production
#'
#' Loads information on the  temporary and permanent crops of the Country that are characterized not only by their great economic importance in the export agenda, but also by their social relevance, since its components are on the Brazilian table (IBGE). Survey is done at the municipal level and data is available from 1974 to 2019 for most datasets. See \url{https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/18374-forestry-activities.html?=&t=o-que-e}
#'
#' @param dataset A dataset name (\code{pam_all_crops}, \code{pam_permanent_crops}, \code{pam_temporary_crops} or \code{pam_xx}, in which xx needs to be corn, potato, peanut or beans. You cal also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/pam/tabelas})
#'
#' @param geo_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" or "city". See documentation of \code{sidrar}.
#'
#' @param time_period A \code{numeric} indicating what years will the data be loaded in the format YYYY. Can be a sequence of numbers such as 2010:2012.
#'
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese and English are supported.
#'
#' @return A \code{tibble} with a panel of N x T observations, consisting of geographic units that present positive values for any of the variables in the dataset.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro.
#'
#' @encoding UTF-8
#'
#' @export load_pevs
#'
#' @examples datazoom.amazonia::load_pam(dataset = 'pam_all_crops', 'state', 2012, language = "pt")
#'
load_pam = function(dataset=NULL,geo_level = "municipality", time_period = 2017:2018, language = "eng") {

  ## Translation is only made through collapsing at the end
  # - What if we wanted to deliver raw data?

  ## To-Dos:
  ## Include Progress Bar
  ## Include Labels
  ## Support for Raw Downloads
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
        (variavel_codigo == '8331') ~ 'area_plantada_dest_colheita' # Area plantada ou destinada a colheita
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
        (variavel_codigo == '8331') ~ 'planted_to_harvest_area' # Area plantada ou destinada a colheita
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
                       values_fill = 0) %>%
    janitor::clean_names()

  ########################
  ## Changing Year Name ##
  ########################


  if (language == 'eng'){

    dat = dat %>%
      dplyr::rename(year = ano)
  }


  ##########################
  ## Returning Data Frame ##
  ##########################

  return(dat)

}
