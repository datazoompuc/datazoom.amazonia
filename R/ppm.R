#' @title PPM - Municipal Livestock Production
#'
#' Loads information on animal farming inventories and livestock products (IBGE). Survey is done at the municipal level and data is available from 1974 to 2019 for most datasets. See \url{https://www.ibge.gov.br/en/statistics/economic/agriculture-forestry-and-fishing/17353-municipal-livestock-production.html?=&t=o-que-e}
#'
#' @param dataset A dataset name (\code{ppm_livetock_inventory}, \code{ppm_sheep_farming}, \code{ppm_animal_orig_production}, \code{ppm_cow_farming} or \code{ppm_aquaculture}. You can also use SIDRA codes (see \url{https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2019})
#'
#' @param geo_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "state" or "municipality". See documentation of \code{sidrar}.
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
#' @export load_ppm
#'
#' @examples \dontrun{datazoom.amazonia::load_ppm(dataset = 'ppm_animal_origin_production', 'state', 2012, language = "pt")}

load_ppm = function(dataset=NULL,raw_data = FALSE,geo_level = "municipality",time_period=2019,language = 'pt'){

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

  sidra_code <- NULL
  nivel_territorial_codigo<- NULL
  nivel_territorial<- NULL
  unidade_de_medida_codigo<- NULL
  variavel_codigo<- NULL
  ano_codigo<- NULL
  valor<- NULL
  geo_id<- NULL
  ano<- NULL
  tipo_de_rebanho<- NULL
  variavel<- NULL
  unidade_de_medida<- NULL
  tipo_de_produto_de_origem_animal<- NULL
  tipo_de_produto_da_aquicultura<- NULL
  tipo_de_produto_de_origem_animal_codigo <- NULL

  # Adjust Code Based on State Level Information
  # Check if any observation geo-time at the final data have multiple NA entries -- this would mean the data is "wrong"
  # Double Check Translation

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

  if (param$code == 94){
    param$data_name = 'Cow Farming (Vacas ordenhadas - Cabecas)'
  }
  if (param$code == 95){
    param$data_name = 'Sheep Farming (Ovinos Tosquiados - Producao de La - Cabecas)'
  }
  if (param$code == 3939){
    param$data_name = 'Cattle Number (Efetivo dos rebanhos)'
  }
  if (param$code == 74){
    param$data_name = 'Animal Origin Production (Producao de origem animal, por tipo de produto)'
  }
  if (param$code == 3940){
    param$data_name = 'Water Origin Production - Fish and others (Producao da aquicultura - peixes e outros)'
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

  if (raw_data = TRUE){return(dat)}

  ######################
  ## Data Enginnering ##
  ######################

  dat = dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

  ## The Code Below Depends on the type of data

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


  if (param$code == 3939){

    ## Selecting Variables

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_rebanho_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

    ## Translating Names

    # if(language == 'pt'){
    #   dat$variavel = base::ifelse(dat$variavel == 'efetivo_dos_rebanhos','gado_cabeca','')
    # }

  #   if (language == 'eng'){
  #     dat$variavel = base::ifelse(dat$variavel == 'efetivo_dos_rebanhos','cattle_number','')
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'galinaceos_galinhas'] = 'gallinaceous_chicken'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'galinaceos_total'] = 'gallinaceous_total'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'suino_matrizes_de_suinos'] = 'swine_sows'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'suino_total'] = 'swine_total'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'bovino'] = 'bovine'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'bubalino'] = 'buffalo'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'caprino'] = 'goat'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'codornas'] = 'quail'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'equino'] = 'equine'
  #     dat$tipo_de_rebanho[dat$tipo_de_rebanho == 'ovino'] = 'ovine'
  #   }
  # }

  ## Sheep Farm

    ## Include number as "cabecas" measure

  # if (param$code == 95){
  #   # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
  #   #                             -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
  #   #   dplyr::mutate(valor = as.numeric(valor))
  #
  #   if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','sheep_farmed','')}
  #   if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'ovinos_tosquiados_nos_estabelecimentos_agropecuarios','ovinos_tosquiados','')

  #  }
  #
  }

  ## Animal Origin Production

  if (param$code == 74){

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_de_origem_animal_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

    if (language == 'pt'){

      dat = dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == '106') ~ 'quant', # Producao de origem animal
            (variavel_codigo == '215') ~ 'valor', #Valor da producao
          )
        )

    }

    if (language == 'eng'){

      dat = dat %>%
        dplyr::mutate(
          variavel = dplyr::case_when(
            (variavel_codigo == '106') ~ 'quant', # Producao de origem animal
            (variavel_codigo == '215') ~ 'value', #Valor da producao
          )
        )

    }
          # tipo_de_produto_de_origem_animal = dplyr::case_when(
          #   tipo_de_produto_de_origem_animal == 'la' ~ 'wool',
          #   tipo_de_produto_de_origem_animal == 'casulos_do_bichodaseda' ~ 'silk_worm_cocoons',
          #   tipo_de_produto_de_origem_animal == 'leite' ~ 'milk',
          #   tipo_de_produto_de_origem_animal == 'mel_de_abelha' ~ 'bee_honey',
          #   tipo_de_produto_de_origem_animal == 'ovos_de_codorna' ~ 'quail_eggs',
          #   tipo_de_produto_de_origem_animal == 'ovos_de_galinha' ~ 'chicken_eggs')
          #
          #

  }

  ## Milked Cows

  if (param$code == 94){

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))


    # if (language == 'eng'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','number_milked_cows','')}
    # if (language == 'pt'){dat$variavel = base::ifelse(dat$variavel == 'vacas_ordenhadas','vacas_ordenhadas_num','')}

  }

  ## Water Origin Production

    ## We need to finish the names translation!

  if (param$code == 3940){

    ## Selecting variables

    # dat = dat %>% dplyr::select(-tidyselect::matches('nivel_territorial'),
    #                             -'ano_codigo',-'variavel_codigo',-'tipo_de_produto_da_aquicultura_codigo',-'unidade_de_medida_codigo') %>%
    #   dplyr::mutate(valor = as.numeric(valor))

    ## Translation

    # if (language == 'pt'){
    #
    #   dat = dat %>%
    #     dplyr::mutate(
    #       variavel = dplyr::case_when(
    #         variavel == 'producao_da_aquicultura' ~  'producao_kg',
    #         variavel == 'valor_da_producao' ~ 'valor_brl',
    #         TRUE ~ variavel
    #       )
    #     )
    # }
    #
    # if (language == 'eng'){
    #
    #   dat = dat %>%
    #     dplyr::mutate(
    #       variavel = dplyr::case_when(
    #         variavel == 'producao_da_aquicultura' ~  'production_kg',
    #         variavel == 'valor_da_producao' ~ 'value_brl',
    #         TRUE ~ variavel
    #     ),
    #     tipo_de_produto_da_aquicultura = dplyr::case_when(
    #       tipo_de_produto_da_aquicultura == 'outros_peixes' ~ 'fish_others',
    #       tipo_de_produto_da_aquicultura == 'camarao' ~ 'shrimp',
    #       tipo_de_produto_da_aquicultura == 'larvas_e_poslarvas_de_camarao' ~ 'shrimp_larvae',
    #       tipo_de_produto_da_aquicultura == 'ostras_vieiras_e_mexilhoes' ~ 'oyester_mussels',
    #       tipo_de_produto_da_aquicultura == 'outros_produtos_ra_jacare_siri_caranguejo_lagosta_etc' ~ 'others',
    #       tipo_de_produto_da_aquicultura == 'sementes_de_moluscos' ~ 'shellfish_seeds',
    #       TRUE ~ tipo_de_produto_da_aquicultura
    #     )
    #
    #   )
    #
    # }
  }


  #############################
  ## Create Long Format Data ##
  #############################

  # ==> The Output is a tibble with unit and year identifiers + production and/or value of each item

  # Warning: A double check on this aggregation (despite the unique identifiers established) would be welcomed!

  ## Cattle Number

  if (param$code == 3939){
    dat = dat %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = c(tipo_de_rebanho,variavel),
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()
  }

  ## Sheep Farm

  if (param$code == 95){
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()
  }

  ## Animal Origin Production

  if (param$code == 74){ ## Animal Origin Production

    dat = dat %>%
      dplyr::arrange(tipo_de_produto_de_origem_animal_codigo,variavel) %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel:tipo_de_produto_de_origem_animal_codigo,
                         values_from=valor,
                         names_sep = '_V',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()

  }

  ## Milked Cows

  if (param$code == 94){ ## Milked Cows
    dat = dat %>% dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = variavel,
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()
  }

  ## Water Origin Production
    ## We need to create a dictionary for this!

  if (param$code == 3940){
    dat = dat %>%
      dplyr::filter(unidade_de_medida != '') %>%
      dplyr::select(-'unidade_de_medida') %>%
      tidyr::pivot_wider(id_cols = c(geo_id,ano),
                         names_from = c(tipo_de_produto_da_aquicultura,variavel),
                         values_from=valor,
                         names_sep = '_',
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()
  }


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
