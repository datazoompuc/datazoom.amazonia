#' @title datazoom_transicao_mapbiomas
#' 
#' @description Download and filter data on transition of types of soil covering by year
#' 
#' @param Path A string indicating where the raw data is in your computer. The default is NULL which means the data will be extracted directly from the website
#' @param Code_State Output contains only data from the state selected
#' Input has to be IBGE's coding for the state desired
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
#' 
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_transicao_mapbiomas(Path = '/Desktop',
#'                              Code_State = "PA", Covering_From = 3, Covering_To = 19,
#'                              Type = 'Normal', Year_Diff = 5)



datazoom_transicao_mapbiomas<-function(Path = NULL,
                                       Code_State = NULL, Covering_From = NULL, Covering_To = NULL,
                                       Type = c('Stacked','Normal','Empilhado'), Year_Diff = NULL){
  if(is.null(Path)){
    url1<-'https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx'
    p1f <- tempfile()
    drive_download(url1, p1f)
  }else{
    p1f<-paste0(Path,'/Dados_Transicao_MapBiomas_5.0_UF-BIOMAS_SITE.xlsx')
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
  if(!is.null(Code_State)){
    tab<-tab[!(tab$state!=Code_State),]
  }
  if(!is.null(Covering_From)){
    tab<-tab[!(tab$from!=Covering_From),]
  }
  if(!is.null(Covering_To)){
    tab<-tab[!(tab$to!=Covering_To),]
  }
  if(is.null(Year_Diff)){
    if(Type=='Stacked' | Type=='Empilhado'){
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
          data<-rep(c(nomecol),num)
          ret<-cbind(ret,data)
          retorno<-rbind(retorno,ret)
        }
      }
    }
    if(Type=='Normal'){
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
  }else if(!is.null(Year_Diff)){
    if(Type=='Stacked' | Type=='Empilhado'){
      for(i in 15:ncolu){
        ret<-c()
        ret<-tab[,1:14]
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
      ret<-tab[,1:14]
      for(i in 15:ncolu){
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