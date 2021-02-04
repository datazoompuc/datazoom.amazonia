#' @title load_mapbiomas_covering
#'
#' @description Download and filter data on type of soil covering by year
<<<<<<< HEAD
#'
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by municipality or state
=======
#' 
#' @param space_aggregation A string that indicates the level of aggregation of the data. It can be by municipality or state
>>>>>>> origin/master
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param covering Output contains only data over the selected covering
#' Input has to be the code of the desired covering
#' ATT.: This code ranges from 1 to 5 and is different from the covering code for the other functions in this package
#' Ex.: 1-Forest; 2-Non Forest Natural Formation; 3-Farming; 4-Non Vegetated Area; 5-Water
#' @param years A vector of numbers containing the years desired in the data base (years available are 1985 to 2020)
#' Ex.: years = c(1985:2020)
#'
#' @return A data base with data containing the area of each selected type of soil covering in each selected year
#'
#' @importFrom readxl read_excel
#' @importFrom utils download.file
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
<<<<<<< HEAD
#' \dontrun{load_mapbiomas_covering(aggregation_level = 'municipality', path = NULL,
#'                              covering = 3, years = c(2000:2010))}
=======
#' load_mapbiomas_covering(space_aggregation = 'municipality', path = NULL,
#'                              covering = 3, years = c(2000:2010))
>>>>>>> origin/master



load_mapbiomas_covering<-function(space_aggregation = c('municipality', 'state','municipio','estado'), path = NULL,
                                       covering = NULL, years = c(1985:2020)){
  if(is.null(path)){
    if(space_aggregation=='state' | space_aggregation=='estado'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
    }else if(space_aggregation=='municipality' | space_aggregation=='municipio'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx'
    }
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    if(space_aggregation=='state' | space_aggregation=='estado'){
      p1f<-paste0(path,'/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
    }else if(space_aggregation=='municipality' | space_aggregation=='municipio'){
      p1f<-paste0(path,'/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE.xlsx')
    }
  }
  if(space_aggregation=='state' | space_aggregation=='estado'){
    a<-read_excel(path = p1f, sheet = 3)
    a<-a[!(substring(a$biome,1,4)!="Amaz"),]
    a$state[a$state == "Acre"] <- "AC"
    a$state[substring(a$state,1,4) == "Amap"] <- "AP"
    a$state[a$state == "Amazonas"] <- "AM"
    a$state[a$state == "Tocantins"] <- "TO"
    a$state[a$state == "Mato Grosso"] <- "MT"
    a$state[substring(a$state,1,6) == "Maranh"] <- "MA"
    a$state[substring(a$state,1,4) == "Rond"] <- "RO"
    a$state[a$state == "Roraima"] <- "RR"
    a$state[substring(a$state,1,3) == "Par"] <- "PA"
    retorno<-data.frame()
    tab<-a
    if(!is.null(covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=covering),]
    }
      for(i in years){
        ret<-c()
        ret<-tab[,1:8]
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
        colnames(ret)[which(colnames(ret) == ano)] <- 'Area'
        num<-nrow(tab[,ano])
        data<-rep(c(i),num)
        ret<-cbind(ret,data)
        retorno<-rbind(retorno,ret)
      }
  }else if(space_aggregation=='municipality' | space_aggregation=='municipio'){
    b<-read_excel(path = p1f, sheet = 3)
    tipos<-c('RR','RO','AC','AM','MA','TO','PA','AP','MT')
    `%notin%` <- Negate(`%in%`)
    b<-b[!(b$state %notin% as.vector(tipos)),]
    retorno<-data.frame()
    tab<-b
    if(!is.null(covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=covering),]
    }
      for(i in years){
        ret<-c()
        ret<-tab[,1:8]
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
        colnames(ret)[which(colnames(ret) == ano)] <- 'Area'
        num<-nrow(tab[,ano])
        data<-rep(c(i),num)
        ret<-cbind(ret,data)
        retorno<-rbind(retorno,ret)
      }
  }
  return(retorno)
}

#' @title load_mapbiomas_transition
#'
#' @description Download and filter data on transition of types of soil covering by year
#'
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param transition_interval A numeric object containing the desired interval in years to observe transitions desired in the data base (1, 2, 5 or 10), or have all included (NULL)
#'
#' @return A data base with data containing the area of each selected type of soil covering transition in each selected year
#'
#' @importFrom readxl read_excel
#' @importFrom utils download.file
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{load_mapbiomas_transition(path = NULL, transition_interval = 5)}



load_mapbiomas_transition<-function(path = NULL, transition_interval = NULL){
  if(is.null(path)){
    url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    p1f<-paste0(path,'/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
  }
  a<-read_excel(path = p1f, sheet = 3)
  a<-a[!(substring(a$biome,1,4)!="Amaz"),]
  a$state[a$state == "Acre"] <- "AC"
  a$state[substring(a$state,1,4) == "Amap"] <- "AP"
  a$state[a$state == "Amazonas"] <- "AM"
  a$state[a$state == "Tocantins"] <- "TO"
  a$state[a$state == "Mato Grosso"] <- "MT"
  a$state[substring(a$state,1,6) == "Maranh"] <- "MA"
  a$state[substring(a$state,1,4) == "Rond"] <- "RO"
  a$state[a$state == "Roraima"] <- "RR"
  a$state[substring(a$state,1,3) == "Par"] <- "PA"
  retorno<-data.frame()
  tab<-a
  ncolu<-ncol(a)
  if(is.null(transition_interval)){
      for(i in 15:ncolu){
        ret<-c()
        ret<-tab[,1:14]
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi!=0){
          ret<-cbind(ret,tab[,i])
          colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
          num<-nrow(tab[,i])
          Period<-rep(c(nomecol),num)
          ret<-cbind(ret,Period)
          retorno<-rbind(retorno,ret)
        }
      }
  }else if(!is.null(transition_interval)){
      for(i in 15:ncolu){
        ret<-c()
        ret<-tab[,1:14]
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi==transition_interval){
          ret<-cbind(ret,tab[,i])
          colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
          num<-nrow(tab[,i])
          data<-rep(c(nomecol),num)
          ret<-cbind(ret,data)
          retorno<-rbind(retorno,ret)
        }
      }
  }
  return(retorno)
}
