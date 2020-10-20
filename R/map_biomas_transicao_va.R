#' @title datazoom_transicao_mapbiomas
#' 
#' @description Download and filter data on transition of types of soil covering by year
#' 
#' @param Path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param Code_State Output contains only data from the state selected
#' Input has to be IBGE's numeric coding for the state desired
#' @param Covering_From Output contains only data of the transition from the covering selected to all other coverings
#' Input has to be the code of the desired covering
#' @param Covering_To Output contains only data of the transition from all coverings to the covering selected
#' Input has to be the code of the desired covering
#' @param Type Decide if the output should have a column for each year (Normal) or a single column for the areas of all years (Stacked | Empilhado)
#' @param Year_Diff A numeric object containing the year differential desired in the data base, or have all included (NULL)
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
#' datazoom_transicao_mapbiomas(Path = '/Desktop',
#'                              Code_State = 11, Covering_From = 3, Covering_To = 19,
#'                              Type = 'Normal', Year_Diff = 5)



datazoom_transicao_mapbiomas_va<-function(Path = NULL,
                                       Code_State = NULL, Covering_From = NULL, Covering_To = NULL,
                                       Type = c('Stacked','Normal','Empilhado'), Year_Diff = NULL){
  if(is.null(Path)){
    url1<-'https://drive.google.com/uc?export=download&id=1SF2BdX-UMTCFOIaAi9uKuf11Yg8Os_vs'
    p1f <- tempfile()
    drive_download(url1, p1f)
  }else{
    p1f<-paste0(Path,'/Dados_Transicao_MapBiomas 4.1_BIOMAS-UF-MUN_SITE.xlsx')
  }
  a<-read_excel(path = p1f, sheet = 2)
  a<-a[!(a$bioma!="AMAZONIA"),]
  a[,1]<-(a[,1]-100)
  retorno<-data.frame()
  tab<-a
  ncolu<-ncol(a)
  if(!is.null(Code_State)){
    tab<-tab[!(tab$codigobiomasestados!=Code_State),]
  }
  if(!is.null(Covering_From)){
    tab<-tab[!(tab$de!=Covering_From),]
  }
  if(!is.null(Covering_To)){
    tab<-tab[!(tab$para!=Covering_To),]
  }
  if(is.null(Year_Diff)){
    if(Type=='Stacked' | Type=='Empilhado'){
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
    if(Type=='Normal'){
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
  }else if(!is.null(Year_Diff)){
    if(Type=='Stacked' | Type=='Empilhado'){
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
    if(Type=='Normal'){
      ret<-c()
      ret<-tab[,1:11]
      for(i in 12:ncolu){
        nomecol<-colnames(a)[i]
        anoi<-substring(nomecol,1,4)
        anof<-substring(nomecol,8,12)
        anoi<-as.integer(anoi)
        anof<-as.integer(anof)
        if(anof-anoi==Year_Diff){
          ret<-cbind(ret,tab[,i])
        }
      }
      retorno<-ret
    }
  }
  return(retorno)
}