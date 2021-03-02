#' @title PAM Permanent Tillage
#'
#' Loads agricultural data of permanent tillages from Brazil from 1974 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @param long A \code{boolean} that sets the format of the returned data. \code{long = TRUE} will return a list where each data frame represents a variable and products are transformed to columns so each geographical aggregation level/year will only have a single line. Default value is \code{FALSE}.
#'
#' @return A \code{data frame} or a \code{list} of data frames if \code{long} is set to \code{TRUE}.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_pam_permanent
#'
#' @examples \dontrun{datazoom.amazonia::load_pam_permanent(2013, aggregation_level = "country")}
#'
#'

load_pam_permanent <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")

  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  sigla_uf2 = c(12,27,13,16,23,32,21,50,51,15,25,26,22,33,24,11,14,28,17)
  micro = c(29006,29001,29007,29022,29027,29019,29002,29018,29014,29012,29026,29031,29009,
            29011,29029,29010,29024,29013,29004,29025,29005,29032,29015,29021,29003,29020,29023,29008,29016,29030,29028,52007,
            52009,52003,52017,52006,52005,52012,52010,52008,52015,52016,52004,52018,52002,52001,52013,52014,52011,
            31041,31049,31014,31055,31012,31023,31059,31030,31009,31026,31045,31011,31040,31066,31028,31034,31025,31010,
            31043,31044,31021,31037,31008,31035,31039,31031,31032,31056,31017,31004,31003,31065,31057,31061,31038,
            31007,31063,31016,31046,31033,31029,31002,31047,31020,31019,31036,31013,31006,31042,31051,31060,31052,
            31005,31053,31058,31054,31048,31027,31015,31024,31064,31022,31018,31001,31050,31062,41010,41014,41006,41005,
            41025,41023,
            41035,41003,
            41015,41037,
            41012,41008,
            41024,41026,
            41004,41029,
            41017,41032,
            41013,41016,
            41020,41036,
            41011,41009,
            41030,41038,
            41001,41027,
            41028,41021,
            41007,41031,
            41039,41034,
            41019,41022,
            41002,41033,
            41018,43022,
            43028,43030,
            43031,43029,
            43009,43016,
            43006,43011,
            43004,43003,
            43024,43014,43008,
            43034,43021,
            43035,43023,
            43012,43027,
            43010,43033,
            43026,43019,
            43005,43020,
            43018,43001,
            43017,43007,
            43025,43032,
            43013,43002,
            43015,42020,
            42012,42010,
            42006,42002,
            42005,42019,
            42009,42016,
            42013,42014,
            42004,42008,
            42011,42007,
            42001,42017,
            42015,42018,
            42003,35035,
            35033,35016,35017,
            35024,35039,35006,35022,35052,35009,
            35015,35020,35018,35023,
            35048,35032,35049,35044,
            35054,35005,
            35034,35002,
            35012,35058,
            35051,35059,
            35056,35060,
            35042,35041,
            35011,35013,
            35001,35021,
            35047,35027,
            35019,35038,
            35062,35031,
            35007,35008,
            35057,35040,
            35053,35045,
            35028,35029,
            35036,35055,
            35014,35026,
            35063,35025,
            35030,35010,
            35004,35050,
            35061,35046,
            35043,35037,35003)

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

  to_long_permanent <- function(df, a){
    if( a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`produto_das_lavouras_permanentes_codigo` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "produto_das_lavouras_permanentes", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
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


  if(language == "eng"){
    e = 1
  } else {
    e = 0
  }

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
    language = "pt"
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  if (aggregation_level == "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf2){
        data <- sidrar::get_sidra(1613, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
      for (m in micro){
        data <- sidrar::get_sidra(1613, period = a, geo = "City", geo.filter = list("MicroRegion" = m))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
        "Value"
    )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  if(long == TRUE){
    df <- to_long_permanent(df, e)
  }

return(df)
}

#' @title PAM Temporary tillage
#'
#' Loads agricultural data of temporary tillages from Brazil from 1974 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @param long A \code{boolean} that sets the format of the returned data. \code{long = TRUE} will return a list where each data frame represents a variable and products are transformed to columns so each geographical aggregation level/year will only have a single line. Default value is \code{FALSE}.
#'
#' @return A \code{data frame} or a \code{list} of data frames if \code{long} is set to \code{TRUE}.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_pam_temporary
#'
#' @examples \dontrun{datazoom.amazonia::load_pam_temporary(2010, aggregation_level = "country")}
#'

load_pam_temporary <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")

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

  to_long_temporary <- function(df, a){
    if( a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`produto_das_lavouras_temporarias_codigo` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "produto_das_lavouras_temporarias", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
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

  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  sigla_uf2 = c(12,27,13,16,23,32,21,50,51,15,25,26,22,33,24,11,14,28,17)
  micro = c(29006,29001,29007,29022,29027,29019,29002,29018,29014,29012,29026,29031,29009,
            29011,29029,29010,29024,29013,29004,29025,29005,29032,29015,29021,29003,29020,29023,29008,29016,29030,29028,52007,
            52009,52003,52017,52006,52005,52012,52010,52008,52015,52016,52004,52018,52002,52001,52013,52014,52011,
            31041,31049,31014,31055,31012,31023,31059,31030,31009,31026,31045,31011,31040,31066,31028,31034,31025,31010,
            31043,31044,31021,31037,31008,31035,31039,31031,31032,31056,31017,31004,31003,31065,31057,31061,31038,
            31007,31063,31016,31046,31033,31029,31002,31047,31020,31019,31036,31013,31006,31042,31051,31060,31052,
            31005,31053,31058,31054,31048,31027,31015,31024,31064,31022,31018,31001,31050,31062,41010,41014,41006,41005,
            41025,41023,
            41035,41003,
            41015,41037,
            41012,41008,
            41024,41026,
            41004,41029,
            41017,41032,
            41013,41016,
            41020,41036,
            41011,41009,
            41030,41038,
            41001,41027,
            41028,41021,
            41007,41031,
            41039,41034,
            41019,41022,
            41002,41033,
            41018,43022,
            43028,43030,
            43031,43029,
            43009,43016,
            43006,43011,
            43004,43003,
            43024,43014,43008,
            43034,43021,
            43035,43023,
            43012,43027,
            43010,43033,
            43026,43019,
            43005,43020,
            43018,43001,
            43017,43007,
            43025,43032,
            43013,43002,
            43015,42020,
            42012,42010,
            42006,42002,
            42005,42019,
            42009,42016,
            42013,42014,
            42004,42008,
            42011,42007,
            42001,42017,
            42015,42018,
            42003,35035,
            35033,35016,35017,
            35024,35039,35006,35022,35052,35009,
            35015,35020,35018,35023,
            35048,35032,35049,35044,
            35054,35005,
            35034,35002,
            35012,35058,
            35051,35059,
            35056,35060,
            35042,35041,
            35011,35013,
            35001,35021,
            35047,35027,
            35019,35038,
            35062,35031,
            35007,35008,
            35057,35040,
            35053,35045,
            35028,35029,
            35036,35055,
            35014,35026,
            35063,35025,
            35030,35010,
            35004,35050,
            35061,35046,
            35043,35037,35003)

  if(language == "eng"){
    e = 1
  } else {
    e = 0
  }

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
    language = "pt"
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  if (aggregation_level == "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf2){
        data <- sidrar::get_sidra(1612, period = a, geo = "City", geo.filter = list("State" = s))
        df <- rbind(df, data)
      }
      for (m in micro){
        data <- sidrar::get_sidra(1612, period = a, geo = "City", geo.filter = list("MicroRegion" = m))
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
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  if (long == TRUE){
    df <- to_long_temporary(df, e)
  }

return(df)
}

#' @title PAM Main products
#'
#' Loads agricultural data of four main tillages - corn, potato, peanut and beans - from Brazil from 2003 to the present
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @param long A \code{boolean} that sets the format of the returned data. \code{long = TRUE} will return a list where each data frame represents a variable and products are transformed to columns so each geographical aggregation level/year will only have a single line. Default value is \code{FALSE}.
#'
#' @return A \code{data frame}
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_pam_main
#'
#' @examples \dontrun{datazoom.amazonia::load_pam_main(2007, aggregation_level = "country")}
#'
#'

load_pam_main <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

  to_english3 <- function(df){
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

    index <- df$`Product of permanent tillage (Code)` == 114253
    df$`Product of permanent tillage`[index] <- "Maize (grain) - First harvest"
    index <- df$`Product of permanent tillage (Code)` == 114254
    df$`Product of permanent tillage`[index] <- "Maize (grain) - Second harvest"
    index <- df$`Product of permanent tillage (Code)` == 117989
    df$`Product of permanent tillage`[index] <- "Potato - First harvest"
    index <- df$`Product of permanent tillage (Code)` == 117990
    df$`Product of permanent tillage`[index] <- "Potato - Second harvest"
    index <- df$`Product of permanent tillage (Code)` == 117994
    df$`Product of permanent tillage`[index] <- "Potato - Third harvest"
    index <- df$`Product of permanent tillage (Code)` == 117987
    df$`Product of permanent tillage`[index] <- "Peanuts (in shell) - First harvest"
    index <- df$`Product of permanent tillage (Code)` == 117988
    df$`Product of permanent tillage`[index] <- "Peanuts (in shell) - Second harvest"
    index <- df$`Product of permanent tillage (Code)` == 117991
    df$`Product of permanent tillage`[index] <- "Beans (grain) - First harvest"
    index <- df$`Product of permanent tillage (Code)` == 117992
    df$`Product of permanent tillage`[index] <- "Beans (grain) - Second harvest"
    index <- df$`Product of permanent tillage (Code)` == 117993
    df$`Product of permanent tillage`[index] <- "Beans (grain) - Third harvest"

    return(df)

  }

  to_long_main <- function(df, a){
    if( a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`produto_das_lavouras_temporarias_codigos` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "produto_das_lavouras_temporarias", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
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

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
    language = "pt"
  }

  if(language == "eng"){
    e = 1
  } else {
    e = 0
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
      df <- janitor::clean_names(df)
      df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value")
    df <- to_english3(df)
    }
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
      df <- janitor::clean_names(df)
      df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
    df <- to_english3(df)
    }
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
      df <- janitor::clean_names(df)
      df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
    df <- to_english3(df)
    }
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
      df <- janitor::clean_names(df)
      df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
    df <- to_english3(df)
    }
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
        df <- janitor::clean_names(df)
        df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
      }
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
    df <- to_english3(df)
    }
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
        df <- janitor::clean_names(df)
        df <- df[!(df$`produto_das_lavouras_temporarias_codigo` == 31693),]
      }
    }
    if (language == "eng"){
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
    df <- to_english3(df)
    }
  }

  if (long == TRUE){
    df <- to_long_main(df, e)
  }

  return(df)
}
