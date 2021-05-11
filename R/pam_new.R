

load_pam = function(type=NULL,space_aggregation = 'municipality', years = 2017:2018, language = "pt") {

  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  param$years = years

  ## Dataset

  if (is.null(type)){stop('Missing Dataset!')}

  if (as.numeric(type) == 1612){
    param$type = 1612
    param$data_name = 'Temporary Crops (Lavouras Temporárias)'
  }

  if (as.numeric(type) == 1613){
    param$type = 1613
    param$data_name = 'Permanent Crops (Lavouras Permanente)'
  }

  if (as.numeric(type) == 5457){
    param$type = 5457
    param$data_name = 'All Crops (Lavouras)'
  }

  ## Aggregation Level

  if (space_aggregation == 'country'){param$geo_reg = 'Brazil'}
  if (space_aggregation == 'region'){param$geo_reg = 'Region'}
  if (space_aggregation == 'state'){param$geo_reg = 'State'}
  if (space_aggregation == 'municipality'){param$geo_reg = 'City'}

  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################

  input_df = expand.grid(
    x=param$uf,
    y=param$years
  )

  input = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))

  ###############
  ## Load Data ##
  ###############

  get_sidra_safe = purrr::safely(sidrar::get_sidra)

  ## We will need to check for the ones not considered in the loop afterwards

  ## We use the purrr package (tidyverse equivalent of base apply functions) to run over the above grid

  if (space_aggregation %in% c('country','region','state')){
    dat = input %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y))
  }

  if (space_aggregation == 'municipality'){
    dat = input %>%
      purrr::pmap(function(x,y) get_sidra_safe(param$type,geo=param$geo_reg,period = y,geo.filter = list("State" = x)))
  }

  ## Capture Errors

  dat = lapply(dat,"[[", 1)

  boolean_downloaded = unlist(lapply(dat,is.data.frame))

  prob_data = input_df[!boolean_downloaded,]

  ## We need to check the municipalities in these UFs

  ## Binding Rows

  dat = dat[boolean_downloaded] %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(function(x){iconv(x,to='ASCII')})

  ######################
  ## Data Enginnering ##
  ######################

  #table(dat$unidade_de_medida,dat$variavel)

  ## Insert Code Similar to PPM

}
