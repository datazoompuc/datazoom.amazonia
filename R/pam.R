#' @title PAM
#'
#' Loads agricultural data of permanent tillages from Brazil from 1974 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @return A \code{data frame}
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples datazoom.amazonia::load_pam_permanent(2013, aggregation_level = "country")
#'
#'

to_english1 <- function(df){
  index <- df$`Variable (Code)` == 2313
  df$Variable[index] <- "Area designated for harvest"
  index <- df$`Variable (Code)` == 216
  df$Variable[index] <- "Harvested Area"
  index <- df$`Variable (Code)` == 214
  df$Variable[index] <- "Quantity produced"
  index <- df$`Variable (Code)` == 112
  df$Variable[index] <- "Average revenue of output"
  index <- df$`Variable (Code)` == 215
  df$Variable[index] <- "Output value"

  index <- df$`Unit of measure (Code)` == 40
  df$`Unit of measure`[index] <- "Thousands of Reals"
  index <- df$`Unit of measure (Code)` == 33
  df$`Unit of measure`[index] <- "Kgs per hectare"
  index <- df$`Unit of measure (Code)` == 1017
  df$`Unit of measure`[index] <- "Tonnes"

  index <- df$`Product of permanent tillage (Code)` == 2717
  df$`Product of permanent tillage`[index] <- "Avocado"
  index <- df$`Product of permanent tillage (Code)` == 2718
  df$`Product of permanent tillage`[index] <- "Tree cotton (lump)"
  index <- df$`Product of permanent tillage (Code)` == 2719
  df$`Product of permanent tillage`[index] <- "Olives"
  index <- df$`Product of permanent tillage (Code)` == 2720
  df$`Product of permanent tillage`[index] <- "Banana (bunch)"
  index <- df$`Product of permanent tillage (Code)` == 2721
  df$`Product of permanent tillage`[index] <- "Rubber (coagulated latex)"
  index <- df$`Product of permanent tillage (Code)` == 40472
  df$`Product of permanent tillage`[index] <- "Rubber (liquid latex)"
  index <- df$`Product of permanent tillage (Code)` == 2722
  df$`Product of permanent tillage`[index] <- "Cocoa (almond)"
  index <- df$`Product of permanent tillage (Code)` == 2723
  df$`Product of permanent tillage`[index] <- "Coffee (grain) total"
  index <- df$`Product of permanent tillage (Code)` == 31619
  df$`Product of permanent tillage`[index] <- "Arabica coffee (grain)"
  index <- df$`Product of permanent tillage (Code)` == 31620
  df$`Product of permanent tillage`[index] <- "Canephora coffee (grain)"
  index <- df$`Product of permanent tillage (Code)` == 404
  df$`Product of permanent tillage`[index] <- "Avocado"
  index <- df$`Product of permanent tillage (Code)` == 404773
  df$`Product of permanent tillage`[index] <- "Cashew"
  index <- df$`Product of permanent tillage (Code)` == 2724
  df$`Product of permanent tillage`[index] <- "Khaki"
  index <- df$`Product of permanent tillage (Code)` == 2725
  df$`Product of permanent tillage`[index] <- "Cashew nuts"
  index <- df$`Product of permanent tillage (Code)` == 2726
  df$`Product of permanent tillage`[index] <- "India-tea (green leaf)"
  index <- df$`Product of permanent tillage (Code)` == 2727
  df$`Product of permanent tillage`[index] <- "Bay coconut"
  index <- df$`Product of permanent tillage (Code)` == 2728
  df$`Product of permanent tillage`[index] <- "Palm (coconut bunch)"
  index <- df$`Product of permanent tillage (Code)` == 2729
  df$`Product of permanent tillage`[index] <- "Mate herb (green leaf)"
  index <- df$`Product of permanent tillage (Code)` == 2730
  df$`Product of permanent tillage`[index] <- "Fig"
  index <- df$`Product of permanent tillage (Code)` == 2731
  df$`Product of permanent tillage`[index] <- "Guava"
  index <- df$`Product of permanent tillage (Code)` == 2732
  df$`Product of permanent tillage`[index] <- "Guarana (seed)"
  index <- df$`Product of permanent tillage (Code)` == 2733
  df$`Product of permanent tillage`[index] <- "Orange"
  index <- df$`Product of permanent tillage (Code)` == 2734
  df$`Product of permanent tillage`[index] <- "Lemon"
  index <- df$`Product of permanent tillage (Code)` == 2735
  df$`Product of permanent tillage`[index] <- "Apple"
  index <- df$`Product of permanent tillage (Code)` == 2736
  df$`Product of permanent tillage`[index] <- "Papaya"
  index <- df$`Product of permanent tillage (Code)` == 2737
  df$`Product of permanent tillage`[index] <- "Mango"
  index <- df$`Product of permanent tillage (Code)` == 2738
  df$`Product of permanent tillage`[index] <- "Passion fruit"
  index <- df$`Product of permanent tillage (Code)` == 2739
  df$`Product of permanent tillage`[index] <- "Quince"
  index <- df$`Product of permanent tillage (Code)` == 2740
  df$`Product of permanent tillage`[index] <- "Nut (dry)"
  index <- df$`Product of permanent tillage (Code)` == 90001
  df$`Product of permanent tillage`[index] <- "Palm heart"
  index <- df$`Product of permanent tillage (Code)` == 2741
  df$`Product of permanent tillage`[index] <- "Pear"
  index <- df$`Product of permanent tillage (Code)` == 2742
  df$`Product of permanent tillage`[index] <- "Peach"
  index <- df$`Product of permanent tillage (Code)` == 2743
  df$`Product of permanent tillage`[index] <- "Black pepper"
  index <- df$`Product of permanent tillage (Code)` == 2744
  df$`Product of permanent tillage`[index] <- "Sisal or agave (fiber)"
  index <- df$`Product of permanent tillage (Code)` == 2745
  df$`Product of permanent tillage`[index] <- "Tangerine"
  index <- df$`Product of permanent tillage (Code)` == 2746
  df$`Product of permanent tillage`[index] <- "Tungue (dried fruit)"
  index <- df$`Product of permanent tillage (Code)` == 2747
  df$`Product of permanent tillage`[index] <- "Urucum (seed)"
  index <- df$`Product of permanent tillage (Code)` == 2748
  df$`Product of permanent tillage`[index] <- "Grape"
  return(df)

}

to_english2 <- function(df){
  index <- df$`Variable (Code)` == 109
  df$Variable[index] <- "Planted area"
  index <- df$`Variable (Code)` == 216
  df$Variable[index] <- "Harvested Area"
  index <- df$`Variable (Code)` == 214
  df$Variable[index] <- "Quantity produced"
  index <- df$`Variable (Code)` == 112
  df$Variable[index] <- "Average revenue of output"
  index <- df$`Variable (Code)` == 215
  df$Variable[index] <- "Output value"

  index <- df$`Unit of measure (Code)` == 40
  df$`Unit of measure`[index] <- "Thousands of Reals"
  index <- df$`Unit of measure (Code)` == 33
  df$`Unit of measure`[index] <- "Kgs per hectare"
  index <- df$`Unit of measure (Code)` == 1017
  df$`Unit of measure`[index] <- "Tonnes"

  index <- df$`Product of permanent tillage (Code)` == 2717
  df$`Product of permanent tillage`[index] <- "Avocado"
  index <- df$`Product of permanent tillage (Code)` == 2718
  df$`Product of permanent tillage`[index] <- "Tree cotton (lump)"
  index <- df$`Product of permanent tillage (Code)` == 2719
  df$`Product of permanent tillage`[index] <- "Olives"
  index <- df$`Product of permanent tillage (Code)` == 2720
  df$`Product of permanent tillage`[index] <- "Banana (bunch)"
  index <- df$`Product of permanent tillage (Code)` == 2721
  df$`Product of permanent tillage`[index] <- "Rubber (coagulated latex)"
  index <- df$`Product of permanent tillage (Code)` == 40472
  df$`Product of permanent tillage`[index] <- "Rubber (liquid latex)"
  index <- df$`Product of permanent tillage (Code)` == 2722
  df$`Product of permanent tillage`[index] <- "Cocoa (almond)"
  index <- df$`Product of permanent tillage (Code)` == 2723
  df$`Product of permanent tillage`[index] <- "Coffee (grain) total"
  index <- df$`Product of permanent tillage (Code)` == 31619
  df$`Product of permanent tillage`[index] <- "Arabica coffee (grain)"
  index <- df$`Product of permanent tillage (Code)` == 31620
  df$`Product of permanent tillage`[index] <- "Canephora coffee (grain)"
  index <- df$`Product of permanent tillage (Code)` == 404
  df$`Product of permanent tillage`[index] <- "Avocado"
  index <- df$`Product of permanent tillage (Code)` == 404773
  df$`Product of permanent tillage`[index] <- "Cashew"
  index <- df$`Product of permanent tillage (Code)` == 2724
  df$`Product of permanent tillage`[index] <- "Khaki"
  index <- df$`Product of permanent tillage (Code)` == 2725
  df$`Product of permanent tillage`[index] <- "Cashew nuts"
  index <- df$`Product of permanent tillage (Code)` == 2726
  df$`Product of permanent tillage`[index] <- "India-tea (green leaf)"
  index <- df$`Product of permanent tillage (Code)` == 2727
  df$`Product of permanent tillage`[index] <- "Bay coconut"
  index <- df$`Product of permanent tillage (Code)` == 2728
  df$`Product of permanent tillage`[index] <- "Palm (coconut bunch)"
  index <- df$`Product of permanent tillage (Code)` == 2729
  df$`Product of permanent tillage`[index] <- "Mate herb (green leaf)"
  index <- df$`Product of permanent tillage (Code)` == 2730
  df$`Product of permanent tillage`[index] <- "Fig"
  index <- df$`Product of permanent tillage (Code)` == 2731
  df$`Product of permanent tillage`[index] <- "Guava"
  index <- df$`Product of permanent tillage (Code)` == 2732
  df$`Product of permanent tillage`[index] <- "Guarana (seed)"
  index <- df$`Product of permanent tillage (Code)` == 2733
  df$`Product of permanent tillage`[index] <- "Orange"
  index <- df$`Product of permanent tillage (Code)` == 2734
  df$`Product of permanent tillage`[index] <- "Lemon"
  index <- df$`Product of permanent tillage (Code)` == 2735
  df$`Product of permanent tillage`[index] <- "Apple"
  index <- df$`Product of permanent tillage (Code)` == 2736
  df$`Product of permanent tillage`[index] <- "Papaya"
  index <- df$`Product of permanent tillage (Code)` == 2737
  df$`Product of permanent tillage`[index] <- "Mango"
  index <- df$`Product of permanent tillage (Code)` == 2738
  df$`Product of permanent tillage`[index] <- "Passion fruit"
  index <- df$`Product of permanent tillage (Code)` == 2739
  df$`Product of permanent tillage`[index] <- "Quince"
  index <- df$`Product of permanent tillage (Code)` == 2740
  df$`Product of permanent tillage`[index] <- "Nut (dry)"
  index <- df$`Product of permanent tillage (Code)` == 90001
  df$`Product of permanent tillage`[index] <- "Palm heart"
  index <- df$`Product of permanent tillage (Code)` == 2741
  df$`Product of permanent tillage`[index] <- "Pear"
  index <- df$`Product of permanent tillage (Code)` == 2742
  df$`Product of permanent tillage`[index] <- "Peach"
  index <- df$`Product of permanent tillage (Code)` == 2743
  df$`Product of permanent tillage`[index] <- "Black pepper"
  index <- df$`Product of permanent tillage (Code)` == 2744
  df$`Product of permanent tillage`[index] <- "Sisal or agave (fiber)"
  index <- df$`Product of permanent tillage (Code)` == 2745
  df$`Product of permanent tillage`[index] <- "Tangerine"
  index <- df$`Product of permanent tillage (Code)` == 2746
  df$`Product of permanent tillage`[index] <- "Tungue (dried fruit)"
  index <- df$`Product of permanent tillage (Code)` == 2747
  df$`Product of permanent tillage`[index] <- "Urucum (seed)"
  index <- df$`Product of permanent tillage (Code)` == 2748
  df$`Product of permanent tillage`[index] <- "Grape"
  return(df)

}

to_long_permanent <- function(df, a){
  if( a == 0) {
    df <- split(df, df$Variável)
    for (i in 1:length(df)){
      df[[i]]$`Brasil` <- NULL
      df[[i]]$`Brasil (Código)` <- NULL
      df[[i]]$`Ano (Código)` <- NULL
      df[[i]]$`Unidade de Medida (Código)` <- NULL
      df[[i]]$`Unidade de Medida` <- NULL
      df[[i]]$`Produto das lavouras permanentes (Código)` <- NULL
      df[[i]] <- tidyr::spread(df[[i]], "Produto das lavouras permanentes", "Valor")
    }
  } elseif ( a == 1 ){
    df <- split(df, df$Variável)
    for (i in 1:length(df)){
      df[[i]]$`Brazil` <- NULL
      df[[i]]$`Brazil (Code)` <- NULL
      df[[i]]$`Year (Code)` <- NULL
      df[[i]]$`Unit of measure (Code)` <- NULL
      df[[i]]$`Unit of measure` <- NULL
      df[[i]]$`Product of permanent tillage (Code)` <- NULL
      df[[i]] <- tidyr::spread(df[[i]], "Product of permanent tillage", "Value")
    }
  }
  return(df)
}

to_long_temporary <- function(df, a){
  if( a == 0) {
    df <- split(df, df$Variável)
    for (i in 1:length(df)){
      df[[i]]$`Brasil` <- NULL
      df[[i]]$`Brasil (Código)` <- NULL
      df[[i]]$`Ano (Código)` <- NULL
      df[[i]]$`Unidade de Medida (Código)` <- NULL
      df[[i]]$`Unidade de Medida` <- NULL
      df[[i]]$`Produto das lavouras temporárias (Código)` <- NULL
      df[[i]] <- tidyr::spread(df[[i]], "Produto das lavouras temporárias", "Valor")
    }
  } elseif ( a == 1 ){
    df <- split(df, df$Variável)
    for (i in 1:length(df)){
      df[[i]]$`Brazil` <- NULL
      df[[i]]$`Brazil (Code)` <- NULL
      df[[i]]$`Year (Code)` <- NULL
      df[[i]]$`Unit of measure (Code)` <- NULL
      df[[i]]$`Unit of measure` <- NULL
      df[[i]]$`Product of temporary tillage (Code)` <- NULL
      df[[i]] <- tidyr::spread(df[[i]], "Product of temporary tillage", "Value")
    }
  }
  return(df)
}

load_pam_permanent <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

  if(language == "eng"){
    a = 1
  } else {
    a = 0
  }

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }

  df <- data.frame()

  if (aggregation_level == "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "Brazil")
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
      )
      df <- to_english1(df)
      }
  }

  if (aggregation_level == "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "Region")
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df)
    }
  }

  if (aggregation_level == "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "State")
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df)
    }
  }

  if (aggregation_level == "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1613, period = a, geo = "MesoRegion")
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df)
    }
  }

  if (aggregation_level == "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
        for (s in sigla_uf){
          data <- sidrar::get_sidra(1613, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
          df <- rbind(df, data)
        }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df)
    }
  }

  if (aggregation_level == "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1613, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df, a)
    }
  }

  if(long == TRUE){
    df <- to_long_permanent(df, a)
  }

return(df)
}

#' Loads agricultural data of temporary tillages from Brazil from 1974 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @return A \code{data frame}
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples datazoom.amazonia::load_pam_temporary(2010, aggregation_level = "country")
#'

load_pam_temporary <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

  if(language == "eng"){
    a = 1
  } else {
    a = 0
  }

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }

  df <- data.frame()

  if (aggregation_level == "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "Brazil")
      df <- rbind(df, data)
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (aggregation_level == "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "Region")
      df <- rbind(df, data)
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (aggregation_level == "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "State")
      df <- rbind(df, data)
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (aggregation_level == "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- sidrar::get_sidra(1612, period = a, geo = "MesoRegion")
      df <- rbind(df, data)
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (aggregation_level == "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1612, period = a, geo = "MicroRegion", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (aggregation_level == "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- sidrar::get_sidra(1612, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){
      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english2(df)
    }
  }

  if (long == TRUE){
    df <- to_long_temporary(df, a)
  }

return(df)
}

#' Loads agricultural data of four main tillages - corn, potato, peanut and beans - from Brazil from 2003 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @return A \code{data frame}
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples datazoom.amazonia::load_pam_main(2007, aggregation_level = "country")
#'
#'

load_pam_main <- function(years, aggregation_level = "country", language = "pt"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
  }

  df <- data.frame()

  if (aggregation_level == "country"){
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

  if (aggregation_level == "region"){
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

  if (aggregation_level == "state"){
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

  if (aggregation_level == "mesoregion"){
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

  if (aggregation_level == "microregion"){
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

  if (aggregation_level == "city"){
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

  return(df)
}
