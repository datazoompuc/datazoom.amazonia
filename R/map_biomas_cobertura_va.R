#' @title datazoom_cobertura_mapbiomas
#' 
#' @description Download and filter data on type of soil covering by year
#' 
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by municipality or state
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param code_state Output contains only data from the state selected
#' Input has to be IBGE's numeric coding for the state desired
#' @param code_mun Output contains only data from the municipality selected (not available for aggregation_level=='state')
#' Input has to be IBGE's numeric coding for the municipality desired
#' @param covering Output contains only data over the selected covering
#' Input has to be the code of the desired covering
#' @param type Decide if the output should have a column for each year (normal) or a single column for the areas of all years (stacked | empilhado)
#' @param year_begin A numeric object containing the first year desired in the data base
#' @param year_end A numeric object containing the last year desired in the data base
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
#' datazoom_cobertura_mapbiomas_va(aggregation_level = 'municipality', path = '/Desktop',
#'                              code_state = 11, code_mun = NULL, covering = 3,
#'                              type = 'stacked', year_begin = 2000, year_end = 2010)



datazoom_cobertura_mapbiomas_va<-function(aggregation_level = c('municipality', 'state','municipio','estado'), path = NULL,
                                       code_state = NULL, code_mun = NULL, covering = NULL,
                                       type = c('stacked','normal','empilhado'), year_begin = NULL, year_end = NULL){
  if(is.null(path)){
    url1<-'https://mapbiomas-br-site.s3.amazonaws.com/downloads/Dados_Cobertura_MapBiomas_4.1_BIOMAS-UF-MUN_SITE.xlsx'
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    p1f<-paste0(path,'/Dados_Cobertura_MapBiomas_4.1_BIOMAS-UF-MUN_SITE.xlsx')
  }
  if(is.null(year_begin)){
    Year_Begin<-1985
  }
  if(is.null(year_end)){
    Year_End<-2018
  }
  if(aggregation_level=='state' | aggregation_level=='estado'){
    a<-read_excel(path = p1f, sheet = 2)
    a<-a[!(a$bioma!="AMAZONIA"),]
    a[,1]<-(a[,1]-100)
    retorno<-data.frame()
    tab<-a
    if(!is.null(code_state)){
      tab<-tab[!(tab$codigobiomasestados!=code_state),]
    }
    if(!is.null(covering)){
      tab<-tab[!(tab$cod.classe!=covering),]
    }
    if(type=='stacked' | type=='empilhado'){
      for(i in year_begin:year_end){
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
    if(type=='normal'){
      ret<-tab[,1:7]
      for(i in year_begin:year_end){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }else if(aggregation_level=='municipality' | aggregation_level=='municipio'){
    b<-read_excel(path = p1f, sheet = 3)
    tipos<-c(11,12,13,14,15,16,17,21,51)
    `%notin%` <- Negate(`%in%`)
    b<-b[!(b$COD_ESTADO %notin% as.vector(tipos)),]
    retorno<-data.frame()
    tab<-b
    if(!is.null(code_state)){
      tab<-tab[!(tab$COD_ESTADO!=code_state),]
    }
    if(!is.null(code_mun)){
      tab<-tab[!(tab$CODIBGE!=code_mun),]
    }
    if(!is.null(covering)){
      tab<-tab[!(tab$cod.classe!=covering),]
    }
    if(type=='stacked' | type=='empilhado'){
      for(i in year_begin:year_end){
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
    if(type=='normal'){
      ret<-tab[,1:8]
      for(i in year_begin:year_end){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }
  return(retorno)
}
