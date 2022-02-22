#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_grazing_quality")
#' @param raw_data A \code{boolean} setting the return of raw or processed data
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be only "municipality".
#' @param time_period A \code{numeric} indicating what years will the data be loaded. Can be only "all".
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#' @param time_id A \code {string} that indicates the time criteria for the data loaded. Can be "year" or "month". Defaults to year.
#' @param cover_level A \code {numeric} that indicates the cover aggregation level. Can be "0", "1", "2", "3" or "4".
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # download treated data from mapbiomas_grazing_quality
#' treated_mapbiomas_grazing <- load_mapbiomas(dataset = "mapbiomas_grazing_quality",
#'                             raw_data = FALSE, geo_level = "municipality",
#'                             time_period = "all", language = "eng")
#' }
#'
#' @importFrom magrittr %>%
#' @export


load_mapbiomas = function(dataset = NULL,raw_data=NULL,geo_level = 'municipality',
               time_period='all',language = 'eng',time_id = 'year',cover_level = 1){

  ## To-Do:
    ## Add Support to Transition Info Download at the State and Biome Level
    ## Add Support to Covering Info Download at the State and Biome Level
    ## Include Support for raw raster download


  ###########################
  ## Bind Global Variables ##
  ###########################
  survey <- link <- x1985 <- x2019 <- NULL
  territory_id <- municipality <- state <- year <- value <- NULL
  x1985_to_1986 <- x2018_to_2019 <- x1988 <- x2017 <- x2000 <- x2010 <- x2018 <- biome <- level_1 <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$geo_level = geo_level
  param$time_period = time_period
  param$language = language
  param$time_id = time_id
  param$raw_data = raw_data

  param$survey_name = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(survey) %>%
    unlist()

  param$url = datasets_link() %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    unlist()

  ## Dataset

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}

  #################
  ## Downloading ##
  #################

  dat = external_download(dataset = param$dataset,source = 'mapbiomas')

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == 'mapbiomas_cover'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1985:x2019,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    ## Aggregating by Cover Level

    if (cover_level == 0){dat$cover_level = dat$level_0}
    if (cover_level == 1){dat$cover_level = dat$level_1}
    if (cover_level == 2){dat$cover_level = dat$level_2}
    if (cover_level == 3){dat$cover_level = dat$level_3}
    if (cover_level == 4){dat$cover_level = dat$level_4}

    dat = dat %>%
      tidyr::pivot_wider(id_cols = c(territory_id,municipality,state,year),
                         names_from = cover_level,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()

    ## Adjusting Geo Level Names

    ## Translating Names

    ## Add Labels

    ## Return Data

    return(dat)

  }

  if (param$dataset == 'mapbiomas_transition'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1985_to_1986:x2018_to_2019,
        names_to = 'transition_year',
        names_prefix = 'x',
        values_to = 'value'
      )

    ## Aggregating by Cover Level

    # if (cover_level == 0){dat$cover_level = dat$level_0}
    # if (cover_level == 1){dat$cover_level = dat$level_1}
    # if (cover_level == 2){dat$cover_level = dat$level_2}
    # if (cover_level == 3){dat$cover_level = dat$level_3}
    # if (cover_level == 4){dat$cover_level = dat$level_4}
    #
    # dat = dat %>%
    #   tidyr::pivot_wider(id_cols = c(territory_id,municipality,state,year),
    #                      names_from = cover_level,
    #                      values_from = value,
    #                      values_fn = sum,
    #                      values_fill = NA) %>%
    #   janitor::clean_names()

    ## Adjusting Geo Level Names

    ## Translating Names

    ## Add Labels

    ## Return Data

    return(dat)




  }

  if (param$dataset == 'mapbiomas_deforestation_regeneration'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x1988:x2017,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    ## Return Data

    return(dat)

  }

  if (param$dataset == 'mapbiomas_irrigation'){

    ## Create Longer Data - Years as a Variable

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x2000:x2019,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    ## Return Data

    return(dat)



  }

  if (param$dataset == 'mapbiomas_grazing_quality'){

    dat = dat %>%
      tidyr::pivot_longer(
        cols = x2010:x2018,
        names_to = 'year',
        names_prefix = 'x',
        values_to = 'value'
      )

    dat = dat %>%
      tidyr::pivot_wider(id_cols = c(biome,state,year),
                         names_from = level_1,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA) %>%
      janitor::clean_names()

    ## Return Data

    return(dat)




  }

  #################################
  ## Data Engineering - Covering ##
  #################################

  ## Adjust State Names

  ## State Level

  # if (space_aggregation == "state" | space_aggregation == "estado") {
  #   a <- read_excel(path = p1f, sheet = 3)
  #   a <- a[!(substring(a$biome, 1, 4) != "Amaz"), ]
  #   a$state[a$state == "Acre"] <- "AC"
  #   a$state[substring(a$state, 1, 4) == "Amap"] <- "AP"
  #   a$state[a$state == "Amazonas"] <- "AM"
  #   a$state[a$state == "Tocantins"] <- "TO"
  #   a$state[a$state == "Mato Grosso"] <- "MT"
  #   a$state[substring(a$state, 1, 6) == "Maranh"] <- "MA"
  #   a$state[substring(a$state, 1, 4) == "Rond"] <- "RO"
  #   a$state[a$state == "Roraima"] <- "RR"
  #   a$state[substring(a$state, 1, 3) == "Par"] <- "PA"
  #   retorno <- data.frame()
  #   tab <- a
  #   if (!is.null(covering)) {
  #     tab <- tab[!(substring(tab$level_1, 1, 1) != covering), ]
  #   }
  #   for (i in years) {
  #     ret <- c()
  #     ret <- tab[, 1:7]
  #     ano <- as.character(i)
  #     ret <- cbind(ret, tab[, ano])
  #     colnames(ret)[which(colnames(ret) == ano)] <- "Area"
  #     num <- nrow(tab[, ano])
  #     data <- rep(c(i), num)
  #     ret <- cbind(ret, data)
  #     retorno <- rbind(retorno, ret)
  #   }
  # } else if (space_aggregation == "municipality" | space_aggregation == "municipio") {
  #
  #   ## Municipality Level
  #
  #   b <- read_excel(path = p1f, sheet = 3)
  #   tipos <- c("RR", "RO", "AC", "AM", "MA", "TO", "PA", "AP", "MT")
  #   `%notin%` <- Negate(`%in%`)
  #   b <- b[!(b$state %notin% as.vector(tipos)), ]
  #   retorno <- data.frame()
  #   tab <- b
  #   if (!is.null(covering)) {
  #     tab <- tab[!(substring(tab$level_1, 1, 1) != covering), ]
  #   }
  #   for (i in years) {
  #     ret <- c()
  #     ret <- tab[, 1:8]
  #     ano <- as.character(i)
  #     ret <- cbind(ret, tab[, ano])
  #     colnames(ret)[which(colnames(ret) == ano)] <- "Area"
  #     num <- nrow(tab[, ano])
  #     data <- rep(c(i), num)
  #     ret <- cbind(ret, data)
  #     retorno <- rbind(retorno, ret)
  #   }
  # }
  # return(retorno)

  #################################################
  ## Data Engineering - Transition - State Level ##
  #################################################

  # a<-a[!(substring(a$biome,1,4)!="Amaz"),]
  # a$state[a$state == "Acre"] <- "AC"
  # a$state[substring(a$state,1,4) == "Amap"] <- "AP"
  # a$state[a$state == "Amazonas"] <- "AM"
  # a$state[a$state == "Tocantins"] <- "TO"
  # a$state[a$state == "Mato Grosso"] <- "MT"
  # a$state[substring(a$state,1,6) == "Maranh"] <- "MA"
  # a$state[substring(a$state,1,4) == "Rond"] <- "RO"
  # a$state[a$state == "Roraima"] <- "RR"
  # a$state[substring(a$state,1,3) == "Par"] <- "PA"
  # retorno<-data.frame()
  # tab<-a
  # ncolu<-ncol(a)
  # if(is.null(transition_interval)){
  #   for(i in 15:ncolu){
  #     ret<-c()
  #     ret<-tab[,1:14]
  #     nomecol<-colnames(a)[i]
  #     anoi<-substring(nomecol,1,4)
  #     anof<-substring(nomecol,8,12)
  #     anoi<-as.integer(anoi)
  #     anof<-as.integer(anof)
  #     if(anof-anoi!=0){
  #       ret<-cbind(ret,tab[,i])
  #       colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
  #       num<-nrow(tab[,i])
  #       Period<-rep(c(nomecol),num)
  #       ret<-cbind(ret,Period)
  #       retorno<-rbind(retorno,ret)
  #     }
  #   }
  # }else if(!is.null(transition_interval)){
  #   for(i in 15:ncolu){
  #     ret<-c()
  #     ret<-tab[,1:14]
  #     nomecol<-colnames(a)[i]
  #     anoi<-substring(nomecol,1,4)
  #     anof<-substring(nomecol,8,12)
  #     anoi<-as.integer(anoi)
  #     anof<-as.integer(anof)
  #     if(anof-anoi==transition_interval){
  #       ret<-cbind(ret,tab[,i])
  #       colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
  #       num<-nrow(tab[,i])
  #       data<-rep(c(nomecol),num)
  #       ret<-cbind(ret,data)
  #       retorno<-rbind(retorno,ret)
  #     }
  #   }
  # }
  #

  ########################################################
  ## Data Engineering - Transition - Municipality Level ##
  ########################################################

#   retorno<-data.frame()
#   tab<-a
#   ncolu<-ncol(a)
#   ret<-c()
#   ret<-tab[,1:15]
#   if(is.null(transition_interval)){
#     for(i in 16:ncolu){
#       ret<-c()
#       ret<-tab[,1:15]
#       nomecol<-colnames(a)[i]
#       anoi<-substring(nomecol,1,4)
#       anof<-substring(nomecol,8,12)
#       anoi<-as.integer(anoi)
#       anof<-as.integer(anof)
#       if(anof-anoi!=0){
#         ret<-cbind(ret,tab[,i])
#         colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
#         num<-nrow(tab[,i])
#         data<-rep(c(nomecol),num)
#         ret<-cbind(ret,data)
#         retorno<-rbind(retorno,ret)
#       }
#     }
#   }else if(!is.null(transition_interval)){
#     for(i in 16:ncolu){
#       ret<-c()
#       ret<-tab[,1:15]
#       nomecol<-colnames(a)[i]
#       anoi<-substring(nomecol,1,4)
#       anof<-substring(nomecol,8,12)
#       anoi<-as.integer(anoi)
#       anof<-as.integer(anof)
#       if(anof-anoi==transition_interval){
#         ret<-cbind(ret,tab[,i])
#         colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
#         num<-nrow(tab[,i])
#         data<-rep(c(nomecol),num)
#         ret<-cbind(ret,data)
#         retorno<-rbind(retorno,ret)
#       }
#     }
#   }
#   retorno<-ret
# }
# return(retorno)

  return(dat)


}
