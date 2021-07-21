load_mapbiomas = function(dataset = NULL,raw_data=NULL,geo_level = 'municipality',
               time_period='all',language = 'eng',time_id = 'year'){

  ## To-Do:
    ## Include Support for raw raster download


  ###########################
  ## Bind Global Variables ##
  ###########################

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

  ## Covering

  # if (is.null(path)) {
  #   if (space_aggregation == "state" | space_aggregation == "estado") {
  #     url1 <- "https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx"
  #   } else if (space_aggregation == "municipality" | space_aggregation == "municipio") {
  #     url1 <- "https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx"
  #   }
  #   p1f <- tempfile()
  #   download.file(url1, p1f, mode = "wb")
  # } else {
  #   if (space_aggregation == "state" | space_aggregation == "estado") {
  #     p1f <- paste0(path, "/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx")
  #   } else if (space_aggregation == "municipality" | space_aggregation == "municipio") {
  #     p1f <- paste0(path, "/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE.xlsx")
  #   }
  # }

  ## Transition - State Level

  # if(is.null(path)){
  #   url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
  #   p1f <- tempfile()
  #   download.file(url1, p1f, mode="wb")
  # }else{
  #   p1f<-paste0(path,'/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
  # }
  #
  # a<-read_excel(path = p1f, sheet = 3)

  ## Transition - Municipality Level

  # if(is.null(path)){
  #   url1<-'https://storage.googleapis.com/mapbiomas-public/COLECAO/5/DOWNLOADS/ESTATISTICAS/Dados_Transicao_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx'
  #   p1f <- tempfile()
  #   download.file(url1, p1f, mode="wb")
  # }else{
  #   p1f<-paste0(path,'/Dados_Transicao_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx')
  # }
  # a<-read_excel(path = p1f, sheet = 3)

  dat = external_download(dataset = param$dataset,source = 'mapbiomas')

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")})

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
  #     ret <- tab[, 1:8]
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
