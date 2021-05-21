


#' Title
#'
#' @param data
#' @param source
#' @param geo_id
#' @param time_id
#'
#' @return A tibble
#' @export download
#'
#' @examples
download = function(data=NULL,source='ibge',geo_id='municipality',time_id=2017:2018){

  ## Initialize Parameters

  param = list()
  param$dataset = data
  param$source = source

  ##

  if (param$source == 'ibge'){

    ## Download from Sidra IBGE

  }

  ##

  if (param$source != 'ibge'){

    ## Download from Site

    ## We need to have a database with the official links of the sites in which the data is stored



  }

  return('Hey everyone!')


}
