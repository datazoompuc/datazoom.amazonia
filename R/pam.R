library(tidyverse)
library(httr)
library(jsonlite)
library(sidrar)
"http://api.sidra.ibge.gov.br/values/t/1612/n6/all/v/all/p/2000,2002/c81/2702/f/u"
#request <- GET("http://api.sidra.ibge.gov.br/values/t/1613/n3/all/u/y/v/all/p/all/c82/all/d/v1000215%202,v1000216%202,v1002313%202")
#request$status_code
#If API fails request will return non-200 status code

#response <- content(request, as = "text", encoding = "UTF-8")

#df <- fromJSON(response, flatten = TRUE) %>% 
 # data.frame()

#col_names <- df[1,]
#colnames(df) <- col_names
#df = df[-1,]

#choices = c("Brasil", "Grande Região", "Unidade da Federação", "Município")
#nivter = dlgList(choices, multiple = FALSE, title = "Qual o nível territorial desejado?")$res

#load_pam_permanent - COD tabela = 1613

load_pam_permanent <- function(years, aggregation_level = "country", language = "eng"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  
  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }
  
  df <- data.frame()
  
  if (aggregation_level = "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "Brazil")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "Region")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "State")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "MesoRegion")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
   
  if (aggregation_level = "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
        for (s in sigla_uf){
          data <- sidrar::get_sidra(1613, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
          df <- rbind(df, data)
        }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1613, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
}

load_pam_temporart <- function(years, aggregation_level = "country", language = "pt"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  
  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }
  
  df <- data.frame()
  
  if (aggregation_level = "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "Brazil")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "Region")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "State")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "MesoRegion")
      df <- rbind(df, data)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1612, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1612, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
}

load_pam_main <- function(years, aggregation_level = "country", "language" = "pt"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  
  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }
  
  df <- data.frame()
  
  if (aggregation_level = "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      corn <- sidrar::get_sidra(839, period = a, geo = "Brazil")
      potato <- sidrar::get_sidra(1001, period = a, geo = "Brazil")
      peanut <- sidrar::get_sidra(1000, period = a, geo = "Brazil")
      bean <- sidrar::get_sidra(1002, period = a, geo = "Brazil")
      df <- rbind(df, corn)
      df <- rbind(df, potato)
      df <- rbind(df, peanut)
      df <- rbind(df, bean)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value")
  }
  
  if (aggregation_level = "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      corn <- sidrar::get_sidra(839, period = a, geo = "Region")
      potato <- sidrar::get_sidra(1001, period = a, geo = "Region")
      peanut <- sidrar::get_sidra(1000, period = a, geo = "Region")
      bean <- sidrar::get_sidra(1002, period = a, geo = "Region")
      df <- rbind(df, corn)
      df <- rbind(df, potato)
      df <- rbind(df, peanut)
      df <- rbind(df, bean)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      corn <- sidrar::get_sidra(839, period = a, geo = "State")
      potato <- sidrar::get_sidra(1001, period = a, geo = "State")
      peanut <- sidrar::get_sidra(1000, period = a, geo = "State")
      bean <- sidrar::get_sidra(1002, period = a, geo = "State")
      df <- rbind(df, corn)
      df <- rbind(df, potato)
      df <- rbind(df, peanut)
      df <- rbind(df, bean)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      corn <- sidrar::get_sidra(839, period = a, geo = "MesoRegion")
      potato <- sidrar::get_sidra(1001, period = a, geo = "MesoRegion")
      peanut <- sidrar::get_sidra(1000, period = a, geo = "MesoRegion")
      bean <- sidrar::get_sidra(1002, period = a, geo = "MesoRegion")
      df <- rbind(df, corn)
      df <- rbind(df, potato)
      df <- rbind(df, peanut)
      df <- rbind(df, bean)
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        corn <- sidrar::get_sidra(839, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        potato <- sidrar::get_sidra(1001, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        peanut <- sidrar::get_sidra(1000, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        bean <- sidrar::get_sidra(1002, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        df <- rbind(df, corn)
        df <- rbind(df, potato)
        df <- rbind(df, peanut)
        df <- rbind(df, bean)
      }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
  if (aggregation_level = "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        corn <- sidrar::get_sidra(839, period = a, geo = "City", geo.filter = list("State" = s))
        potato <- sidrar::get_sidra(1001, period = a, geo = "City", geo.filter = list("State" = s))
        peanut <- sidrar::get_sidra(1000, period = a, geo = "City", geo.filter = list("State" = s))
        bean <- sidrar::get_sidra(1002, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, corn)
        df <- rbind(df, potato)
        df <- rbind(df, peanut)
        df <- rbind(df, bean)
      }
    }
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }
  
}
#toString(anos[1])
#df2 = get_sidra(1613, period = c("2017", "2018"), geo = "Brazil")

#load_pam_temporary - COD tabela 1612

#load_pam_main (fei;ao, mandioca, milho, arroz, soja e cana) - COD tabelas = 1613

#aggregation_level BR GR UF ME MI MU
#baixar um ano de cada vez
#se a pessoa escolher municipio, baixar um estado de cada vez
#funcao mae com para chamar cada nivel de agregacao
