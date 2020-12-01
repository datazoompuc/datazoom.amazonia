#' @title datazoom_transicao_mapbiomas
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
#' 
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_transicao_mapbiomas(path = '/Desktop',
#'                              code_state = "PA", covering_from = 3, covering_to = 19,
#'                              type = 'normal', year_diff = 5)



datazoom_transicao_mapbiomas<-function(path = NULL,
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
