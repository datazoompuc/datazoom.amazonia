#' @title load_mapbiomas_cobertura
#' 
#' @description Download and filter data on type of soil covering by year
#' 
#' @param aggregation_level A string that indicates the level of aggregation of the data. It can be by municipality or state
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param code_state Output contains only data from the state selected
#' Input has to be IBGE's coding for the state desired
#' @param code_mun Output contains only data from the municipality selected (not available for aggregation_level=='state')
#' Input has to be IBGE's coding for the municipality desired
#' @param covering Output contains only data over the selected covering
#' Input has to be the code of the desired covering
#' ATT.: This code ranges from 1 to 5 and is different from the covering code for the other functions in this package
#' @param type Decide if the output should have a column for each year (normal) or a single column for the areas of all years (stacked | empilhado)
#' @param years A vector of numbers containing the years desired in the data base
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
#' load_mapbiomas_cobertura(aggregation_level = 'municipality', path = NULL,
#'                              code_state = "PA", code_mun = NULL, covering = 3,
#'                              type = 'stacked', years = c(2000:2010))



load_mapbiomas_cobertura<-function(aggregation_level = c('municipality', 'state','municipio','estado'),path = NULL,
                                       code_state = NULL, code_mun = NULL, covering = NULL,
                                       type = c('stacked','normal','empilhado'), years = NULL){
  if(is.null(path)){
    if(aggregation_level=='state' | aggregation_level=='estado'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
    }else if(aggregation_level=='municipality' | aggregation_level=='municipio'){
      url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx'
    }
    p1f <- tempfile()
    download.file(url1, p1f, mode="wb")
  }else{
    if(aggregation_level=='state' | aggregation_level=='estado'){
      p1f<-paste0(path,'/Dados_Cobertura_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
    }else if(aggregation_level=='municipality' | aggregation_level=='municipio'){
      p1f<-paste0(path,'/Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE.xlsx')
    }
  }
  if(is.null(years)){
    years<-c(1985:2020)
  }
  if(aggregation_level=='state' | aggregation_level=='estado'){
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
    if(!is.null(code_state)){
      tab<-tab[!(tab$state!=code_state),]
    }
    if(!is.null(covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=covering),]
    }
    if(type=='stacked' | type=='empilhado'){
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
    if(type=='normal'){
      ret<-tab[,1:8]
      for(i in years){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }else if(aggregation_level=='municipality' | aggregation_level=='municipio'){
    b<-read_excel(path = p1f, sheet = 3)
    tipos<-c('RR','RO','AC','AM','MA','TO','PA','AP','MT')
    `%notin%` <- Negate(`%in%`)
    b<-b[!(b$state %notin% as.vector(tipos)),]
    retorno<-data.frame()
    tab<-b
    if(!is.null(code_state)){
      tab<-tab[!(tab$state!=code_state),]
    }
    if(!is.null(code_mun)){
      tab<-tab[!(tab$municipality!=code_mun),]
    }
    if(!is.null(covering)){
      tab<-tab[!(substring(tab$level_1,1,1)!=covering),]
    }
    if(type=='stacked' | type=='empilhado'){
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
    if(type=='normal'){
      ret<-tab[,1:8]
      for(i in years){
        ano<-as.character(i)
        ret<-cbind(ret,tab[,ano])
      }
      retorno<-ret
    }
  }
  return(retorno)
}

#' @title load_mapbiomas_transicao
#' 
#' @description Download and filter data on transition of types of soil covering by year
#' 
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param code_state Output contains only data from the state selected
#' Input has to be IBGE's coding for the state desired
#' @param covering_from Output contains only data of the transition from the covering selected to all other coverings
#' Input has to be the code of the desired covering
#' @param covering_to Output contains only data of the transition from all coverings to the covering selected
#' Input has to be the code of the desired covering
#' @param type Decide if the output should have a column for each year (normal) or a single column for the areas of all years (stacked | empilhado)
#' @param year_diff A numeric object containing the year differential desired in the data base, or have all included (NULL)
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
#' load_mapbiomas_transicao(path = NULL,
#'                              code_state = "PA", covering_from = 3, covering_to = 19,
#'                              type = 'normal', year_diff = 5)



load_mapbiomas_transicao<-function(path = NULL,
                                       code_state = NULL, covering_from = NULL, covering_to = NULL,
                                       type = c('stacked','normal','empilhado'), year_diff = NULL){
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
  if(!is.null(code_state)){
    tab<-tab[!(tab$state!=code_state),]
  }
  if(!is.null(covering_from)){
    tab<-tab[!(tab$from!=covering_from),]
  }
  if(!is.null(covering_to)){
    tab<-tab[!(tab$to!=covering_to),]
  }
  if(is.null(year_diff)){
    if(type=='stacked' | type=='empilhado'){
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
    }
    if(type=='normal'){
      ret<-c()
      ret<-tab[,1:14]
      for(i in 15:ncolu){
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi!=0){
          ret<-cbind(ret,tab[,i])
        }
      }
      retorno<-ret
    }
  }else if(!is.null(year_diff)){
    if(type=='stacked' | type=='empilhado'){
      for(i in 15:ncolu){
        ret<-c()
        ret<-tab[,1:14]
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi==year_diff){
          ret<-cbind(ret,tab[,i])
          colnames(ret)[which(colnames(ret) == nomecol)] <- 'Area'
          num<-nrow(tab[,i])
          data<-rep(c(nomecol),num)
          ret<-cbind(ret,data)
          retorno<-rbind(retorno,ret)
        }
      }
    }
    if(type=='normal'){
      ret<-c()
      ret<-tab[,1:14]
      for(i in 15:ncolu){
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi==year_diff){
          ret<-cbind(ret,tab[,i])
        }
      }
      retorno<-ret
    }
  }
  return(retorno)
}
