#' @title datazoom_transicao_mapbiomas_va
#' 
#' @description Download and filter data on transition of types of soil covering by year
#' 
#' @param path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param code_state Output contains only data from the state selected
#' Input has to be IBGE's numeric coding for the state desired
#' @param covering_from Output contains only data of the transition from the covering selected to all other coverings
#' Input has to be the code of the desired covering
#' @param covering_to Output contains only data of the transition from all coverings to the covering selected
#' Input has to be the code of the desired covering
#' @param type Decide if the output should have a column for each year (normal) or a single column for the areas of all years (stacked | empilhado)
#' @param year_diff A numeric object containing the year differential desired in the data base, or have all included (NULL)
#' 
#' @return A data base with data containing the area of each selected type of soil covering transition in each selected year
#'
#' @importFrom writexl
#' @importFrom readxl
#' @importFrom googledrive
#' 
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_transicao_mapbiomas_va(path = '/Desktop',
#'                              code_state = 11, covering_from = 3, covering_to = 19,
#'                              type = 'normal', year_diff = 5)



datazoom_transicao_mapbiomas_va<-function(path = NULL,
                                       code_state = NULL, covering_from = NULL, covering_to = NULL,
                                       type = c('stacked','normal','empilhado'), year_diff = NULL){
  if(is.null(path)){
    url1<-'https://drive.google.com/uc?export=download&id=1SF2BdX-UMTCFOIaAi9uKuf11Yg8Os_vs'
    p1f <- tempfile()
    drive_download(url1, p1f)
  }else{
    p1f<-paste0(path,'/Dados_Transicao_MapBiomas 4.1_BIOMAS-UF-MUN_SITE.xlsx')
  }
  a<-read_excel(path = p1f, sheet = 2)
  a<-a[!(a$bioma!="AMAZONIA"),]
  a[,1]<-(a[,1]-100)
  retorno<-data.frame()
  tab<-a
  ncolu<-ncol(a)
  if(!is.null(code_state)){
    tab<-tab[!(tab$codigobiomasestados!=code_state),]
  }
  if(!is.null(covering_from)){
    tab<-tab[!(tab$de!=covering_from),]
  }
  if(!is.null(covering_to)){
    tab<-tab[!(tab$para!=covering_to),]
  }
  if(is.null(year_diff)){
    if(type=='stacked' | type=='empilhado'){
      for(i in 12:ncolu){
        ret<-c()
        ret<-tab[,1:11]
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi!=0){
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
      ret<-tab[,1:11]
      for(i in 12:ncolu){
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
      for(i in 12:ncolu){
        ret<-c()
        ret<-tab[,1:11]
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi==Year_Diff){
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
      ret<-tab[,1:11]
      for(i in 12:ncolu){
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
