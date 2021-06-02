#' @title PEVS Plant extraction
#'
#' Loads quantity produced and yield value of plant extraction, by type of product, from 1986 to 2019
#'
#' @param time_period A \code{vector} indicating what years will the data be loaded
#'
#' @param geo_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
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

load_pevs <- function(type = NULL, geo_level = "municipality", time_period = 2018:2019, language = "pt"){

  #############################
  ## Define Basic Parameters ##
  #############################

  param = list()
  param$uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  param$time_period = time_period
  
  ## Dataset
  
  if (is.null(type)){stop('Missing Dataset!')}
  
  if (as.numeric(type) == 289){
    param$type = 289
    param$data_name = 'Vegetal extraction quantity and value (Quantidade e valor da extração vegetal)'
  }
  
  if (as.numeric(type) == 291){
    param$type = 291
    param$data_name = 'Forestry quantity and value (Quantidade e valor da silvicultura)'
  }
  
  #leave 'Forestry area (Area da silvicultura)' for last
  
  if (as.numeric(type) == 5930){
    param$type = 5930
    param$data_name = 'Forestry area (Area da silvicultura)'
  }
  
  ## Aggregation Level
  
  if (geo_level == 'country'){param$geo_reg = 'Brazil'}
  if (geo_level == 'region'){param$geo_reg = 'Region'}
  if (geo_level == 'state'){param$geo_reg = 'State'}
  if (geo_level == 'municipality'){param$geo_reg = 'City'}
  
  
  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################
  
  input_df = expand.grid(
    x=param$uf,
    y=param$time_period
  )
  
  input_munic = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))
  input_other = list(x = as.list(as.character(param$time_period)))
  
  ###############
  ## Load Data ##
  ###############
  
  get_sidra_safe = purrr::safely(sidrar::get_sidra)
  
  ## We will need to check for the ones not considered in the loop afterwards
  
  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid
  
  if (geo_level %in% c('country','region','state')){
    
    base::message(base::cat('Downloading Data:',length(param$time_period),'API requests')) ## Show Message
    
    ## Download
    
    dat = input_other %>%
      purrr::pmap(function(x) get_sidra_safe(param$type,geo=param$geo_reg,period = x))
    
    ## Completion!
    
    base::message(base::cat('Download Completed! Starting Data Processing...'))
    
  }
  
  if (geo_level == 'municipality'){
    dat = input %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y,geo.filter = list("State" = x)))
  }
  
  ## Capture Errors
  
  ## Daniel Comment: The main point here is that the Sidra API has a limit for requests. Thus, for states with a large number
  ## of municipalities the request may break. We used the purrr::safely to not break the loop (or map), but we will need to
  ## deal with these problems in this step here. Now its under construction =)
  
  dat = lapply(dat,"[[", 1)
  
  boolean_downloaded = unlist(lapply(dat,is.data.frame))
  
  prob_data = input_df[!boolean_downloaded,]
  
  ## We need to check the municipalities in these UFs
  
  #######################
  ## Cleaning Function ##
  #######################
  
  clean_custom = function(var){
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='ª',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_replace_all(string=var,pattern='_',replacement='')
    var = stringr::str_replace_all(string=var,pattern='.',replacement='_')
    var = stringr::str_replace_all(string=var,pattern=',',replacement='_')
    var = stringr::str_to_lower(string=var)
    return(var)
  }
  
## Binding Rows
  
dat  = dat[boolean_downloaded] %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble()
  
######################
## Data Enginnering ##
######################

dat = dat %>%
  janitor::clean_names() %>%
  dplyr::mutate_all(function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
  dplyr::mutate_all(clean_custom)  

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

## Needs to be adjusted according to different units of measurement

