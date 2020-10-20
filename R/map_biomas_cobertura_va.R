#' @title datazoom_cobertura_mapbiomas
#' 
#' @description Download and filter data on type of soil covering by year
#' 
#' @param Aggregation_level A string that indicates the level of aggregation of the data. It can be by Municipality or State
#' @param Path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param Code_State Output contains only data from the state selected
#' Input has to be IBGE's numeric coding for the state desired
#' @param Code_Mun Output contains only data from the municipality selected (not available for Aggregation_level=='State')
#' Input has to be IBGE's numeric coding for the municipality desired
#' @param Covering Output contains only data over the selected covering
#' Input has to be the code of the desired covering
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
#'                              Code_State = 11, Code_Mun = NULL, Covering = 3,
#'                              Type = 'Stacked', Year_Begin = 2000, Year_End = 2010)



datazoom_cobertura_mapbiomas_va<-function(Aggregation_level = c('Municipality', 'State','Municipio','Estado'),Path = NULL,
                                       Code_State = NULL, Code_Mun = NULL, Covering = NULL,
                                       Type = c('Stacked','Normal','Empilhado'), Year_Begin = NULL, Year_End = NULL){
  if(is.null(Path)){
    url1<-'https://mapbiomas-br-site.s3.amazonaws.com/downloads/Dados_Cobertura_MapBiomas_4.1_BIOMAS-UF-MUN_SITE.xlsx'
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    p1f<-paste0(Path,'/Dados_Cobertura_MapBiomas_4.1_BIOMAS-UF-MUN_SITE.xlsx')
  }
  if(is.null(Year_Begin)){
    Year_Begin<-1985
  }
  if(is.null(Year_End)){
    Year_End<-2018
  }
  if(Aggregation_level=='State' | Aggregation_level=='Estado'){
    a<-read_excel(path = p1f, sheet = 2)
    a<-a[!(a$bioma!="AMAZONIA"),]
    a[,1]<-(a[,1]-100)
    retorno<-data.frame()
    tab<-a
    if(!is.null(Code_State)){
      tab<-tab[!(tab$codigobiomasestados!=Code_State),]
    }
    if(!is.null(Covering)){
      tab<-tab[!(tab$cod.classe!=Covering),]
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
    tipos<-c(11,12,13,14,15,16,17,21,51)
    `%notin%` <- Negate(`%in%`)
    b<-b[!(b$COD_ESTADO %notin% as.vector(tipos)),]
    retorno<-data.frame()
    tab<-b
    if(!is.null(Code_State)){
      tab<-tab[!(tab$COD_ESTADO!=Code_State),]
    }
    if(!is.null(Code_Mun)){
      tab<-tab[!(tab$CODIBGE!=Code_Mun),]
    }
    if(!is.null(Covering)){
      tab<-tab[!(tab$cod.classe!=Covering),]
    }
    if(Type=='Stacked' | Type=='Empilhado'){
      for(i in Year_Begin:Year_End){
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
    if(Type=='Normal'){
      ret<-tab[,1:8]
      for(i in Year_Begin:Year_End){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }
  return(retorno)
}