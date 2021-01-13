library(foreign)
library(tidyverse)


load_deter = function(biome, aggregation_level, language) {
  
  load_deter_raw(biome)
  
  treat_deter_data(df, aggregation_level, language)
  
}


load_deter_raw = function(biome) {
  
  biome <- tolower(biome)
  
  if (biome == "amazonia") {biome <- "amz"}
  
  else if (biome != "cerrado") {warning("Invalid biome, proceeding with Amazon.")}
    
  url <- paste0("http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-", biome, "/shape")
  
  temp <- tempfile(fileext = ".zip")
  
  download.file(url, temp, mode="wb") 
  
  df <- read.dbf(unzip(temp, "deter_public.dbf"))
}


treat_deter_data = function(df, aggregation_level, language) {
  
  aggregation_level <- tolower(aggregation_level)
  
  df <- df %>%
        dplyr::select(-c(QUADRANT, PATH_ROW, SENSOR, SATELLITE))
  
  if (aggregation_level != "state") {
    
    df <- df %>%
          dplyr::select(-UF) %>%
          dplyr::arrange(MUNICIPALI, CLASSNAME, VIEW_DATE) #pode ter mais de uma ocorrencia por dia
    
    df$MUNICIPALI <- df$MUNICIPALI %>% gsub("รง", "็", .)  #consertando "็"
    
  }
    
  else if (aggregation_level == "state") {
    
    df <- df %>%
          dplyr::select(-MUNICIPALI, -UC) %>%
          dplyr::group_by(UF, CLASSNAME, VIEW_DATE) %>%
          dplyr::summarise(across(-c(AREAUCKM, AREAMUNKM)), AREAUCKM = sum(AREAUCKM), AREAMUNKM = sum(AREAMUNKM))
  }
  
  else if (aggregation_level != "municipality") {
    warning("Aggregation level not supported. Proceeding with municipality.")
  }  
  
  
  df <- df %>%
    dplyr::rename_with(dplyr::recode,
                       
                       CLASSNAME = "Classe",
                       AREAUCKM = "Area_em_UC",
                       AREAMUNKM = "Area_em_Municipio",
                       MUNICIPALI = "Municipio",
                       VIEW_DATE = "Data"
                       
    )
  
  
  language <- tolower(language)
  
  if(language == "eng") {
    df <- translate_deter_to_english(df)
  }
  else if(language != "pt"){
    warning("Selected language not supported. Proceeding with Portuguese.")
  }
  
  return(df)
}



translate_deter_to_english <- function(df) {
  
  df <- df %>%
    dplyr::rename_with(dplyr::recode,
      
      Classe = "Class",
      Data = "Date",
      UC = "ConservationUnit",
      Area_em_UC = "Area_in_CU",
      Area_em_Municipio = "rea_in_Municipality = ",
      Municipio = "Municipality",
      
    )
}