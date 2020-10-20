#' @title datazoom_cobertura_mapbiomas
#' 
#' @description Download and filter data on type of soil covering by year
#' 
#' @param Aggregation_level A string that indicates the level of aggregation of the data. It can be by Municipality or State
#' @param Path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param Code_State Output contains only data from the state selected
#' Input has to be IBGE's coding for the state desired
#' @param Code_Mun Output contains only data from the municipality selected (not available for Aggregation_level=='State')
#' Input has to be IBGE's coding for the municipality desired
#' @param Covering Output contains only data over the selected covering
#' Input has to be the code of the desired covering
#' ATT.: This code ranges from 1 to 5 and is different from the covering code for the others functions in this package
#' @param Type Decide if the output should have a column for each year (Normal) or a single column for the areas of all years (Stacked | Empilhado)
#' @param Year_Begin A numeric object containing the first year desired in the data base
#' @param Year_End A numeric object containing the last year desired in the data base
#' 
#' @return A data base with data containing the area of each selected type of soil covering in each selected year
#'
#' @importFrom writexl
#' @importFrom readxl
#' 
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_cobertura_mapbiomas(Aggregation_level = 'Municipality', Path = '/Desktop',
#'                              Code_State = "PA", Code_Mun = NULL, Covering = 3,
#'                              Type = 'Stacked', Year_Begin = 2000, Year_End = 2010)



datazoom_cobertura_mapbiomas<-function(Aggregation_level = c('Municipality', 'State','Municipio','Estado'),Path = NULL,
                                       Code_State = NULL, Code_Mun = NULL, Covering = NULL,
                                       Type = c('Stacked','Normal','Empilhado'), Year_Begin = NULL, Year_End = NULL){
  if(is.null(Path)){
    if(Aggregation_level=='State' | Aggregation_level=='Estado'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
    }else if(Aggregation_level=='Municipality' | Aggregation_level=='Municipio'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE.xlsx'
    }
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    if(Aggregation_level=='State' | Aggregation_level=='Estado'){
      p1f<-paste0(Path,'/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
    }else if(Aggregation_level=='Municipality' | Aggregation_level=='Municipio'){
      p1f<-paste0(Path,'/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE.xlsx')
    }
  }
  if(is.null(Year_Begin)){
    Year_Begin<-1985
  }
  if(is.null(Year_End)){
    Year_End<-2018
  }
  if(Aggregation_level=='State' | Aggregation_level=='Estado'){
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
    if(!is.null(Code_State)){
      tab<-tab[!(tab$state!=Code_State),]
    }
    if(!is.null(Covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=Covering),]
    }
    if(Type=='Stacked' | Type=='Empilhado'){
      for(i in Year_Begin:Year_End){
        ret<-c()
        ret<-tab[,1:7]
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
        colnames(ret)[which(colnames(ret) == ano)] <- 'Area'
        num<-nrow(tab[,ano])
        data<-rep(c(i),num)
        ret<-cbind(ret,data)
        retorno<-rbind(retorno,ret)
      }
    }
    if(Type=='Normal'){
      ret<-tab[,1:7]
      for(i in Year_Begin:Year_End){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }else if(Aggregation_level=='Municipality' | Aggregation_level=='Municipio'){
    b<-read_excel(path = p1f, sheet = 3)
    tipos<-c('RR','RO','AC','AM','MA','TO','PA','AP','MT')
    `%notin%` <- Negate(`%in%`)
    b<-b[!(b$state %notin% as.vector(tipos)),]
    retorno<-data.frame()
    tab<-b
    if(!is.null(Code_State)){
      tab<-tab[!(tab$state!=Code_State),]
    }
    if(!is.null(Code_Mun)){
      tab<-tab[!(tab$municipality!=Code_Mun),]
    }
    if(!is.null(Covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=Covering),]
    }
    if(Type=='Stacked' | Type=='Empilhado'){
      for(i in Year_Begin:Year_End){
        ret<-c()
        ret<-tab[,1:7]
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
        colnames(ret)[which(colnames(ret) == ano)] <- 'Area'
        num<-nrow(tab[,ano])
        data<-rep(c(i),num)
        ret<-cbind(ret,data)
        retorno<-rbind(retorno,ret)
      }
    }
    if(Type=='Normal'){
      ret<-tab[,1:7]
      for(i in Year_Begin:Year_End){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }
  return(retorno)
}