#' @title PEVS Plant extraction
#'
#' Loads quantity produced and yield value of plant extraction, by type of product, from 1986 to 2019
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
#' @export load_pevs_vegextr
#'
#' @examples \dontrun{datazoom.amazonia::load_pevs_vegextr(2013, aggregation_level = "country")}

load_pevs <- function(type = NULL, geo_level = "municipality", time_period = 2014:2019, language = "pt"){

  #############################
  ## Define Basic Parameters ##
  #############################

  param = list()
  param$uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  param$time_period = time_period
  
  ## Dataset
  
  if (is.null(type)){stop('Missing Dataset!')}
  
  if (as.numeric(type) == 289){
    param$type = 289
    param$data_name = 'Vegetal extraction quantity and value (Quantidade e valor da extração vegetal)'
  }
  
  if (as.numeric(type) == 291){
    param$type = 291
    param$data_name = 'Forestry quantity and value (Quantidade e valor da silvicultura)'
  }
  
  #leave 'Forestry area (Area da silvicultura)' for last
  
  if (as.numeric(type) == 5930){
    param$type = 5930
    param$data_name = 'Forestry area (Area da silvicultura)'
  }
  
  ## Aggregation Level
  
  if (geo_level == 'country'){param$geo_reg = 'Brazil'}
  if (geo_level == 'region'){param$geo_reg = 'Region'}
  if (geo_level == 'state'){param$geo_reg = 'State'}
  if (geo_level == 'municipality'){param$geo_reg = 'City'}
  
  
  ################################################################
  ## Create Grid with every possible combination of UF and Year ##
  ################################################################
  
  input_df = expand.grid(
    x=param$uf,
    y=param$time_period
  )
  
  input_munic = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))
  input_other = list(x = as.list(as.character(param$time_period)))
  
  ###############
  ## Load Data ##
  ###############
  
  
  ## Loop Version
  # Still need to apply "purrr::safely"
  # Still need to separate municipality from country, region and state
  
 dat = list()

 for (t in 1:length(param$time_period)){

   dat[[t]] = sidrar::get_sidra(param$type, geo=param$geo_level, period = as.character(param$time_period[t]))

}
  
  
  #######################
  ## Cleaning Function ##
  #######################
  
  clean_custom = function(var){
    var = stringr::str_replace_all(string=var,pattern=' ',replacement='_')
    var = stringr::str_replace_all(string=var,pattern='-',replacement='')
    var = stringr::str_replace_all(string=var,pattern='ª',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\(',replacement='')
    var = stringr::str_replace_all(string=var,pattern='\\)',replacement='')
    var = stringr::str_to_lower(string=var)
    return(var)
  }
  
  ## Binding Rows
  #remember to add [boolean_downloaded]
  
dat  = dat %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble()
  
  

  
   sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  sigla_uf2 = c(12,27,13,16,23,32,21,50,51,15,25,26,22,33,24,11,14,28,17)
  micro = c(29006,29001,29007,29022,29027,29019,29002,29018,29014,29012,29026,29031,29009,
            29011,29029,29010,29024,29013,29004,29025,29005,29032,29015,29021,29003,29020,29023,29008,29016,29030,29028,52007,52009,52003,52017,52006,52005,52012,52010,52008,52015,52016,52004,52018,52002,52001,52013,52014,52011,
            31041,31049,31014,31055,31012,31023,31059,31030,31009,31026,31045,31011,31040,31066,31028,31034,31025,31010,31043,31044,31021,31037,31008,31035,31039,31031,31032,31056,31017,31004,31003,31065,31057,31061,31038,
            31007,31063,31016,31046,31033,31029,31002,31047,31020,31019,31036,31013,31006,31042,31051,31060,31052,31005,31053,31058,31054,31048,31027,31015,31024,31064,31022,31018,31001,31050,31062,41010,41014,41006,41005,
            41025,41023,41035,41003,41015,41037,41012,41008,41024,41026,41004,41029,41017,41032,41013,41016,41020,41036,41011,41009,41030,41038,41001,41027,41028,41021,41007,41031,41039,41034,41019,41022,
            41002,41033,41018,43022,43028,43030,43031,43029,43009,43016,43006,43011,43004,43003,43024,43014,43008,43034,43021,43035,43023,43012,43027,43010,43033,43026,43019,43005,43020,43018,43001,43017,43007,43025,43032,43013,43002,
            43015,42020,42012,42010,42006,42002,42005,42019,42009,42016,42013,42014,42004,42008,42011,42007,42001,42017,42015,42018,42003,35035,35033,35016,35017,35024,35039,35006,35022,35052,35009,
            35015,35020,35018,35023,35048,35032,35049,35044,35054,35005,35034,35002,35012,35058,35051,35059,35056,35060,35042,35041,35011,35013,35001,35021,35047,35027,35019,35038,35062,35031,35007,35008,
            35057,35040,35053,35045,35028,35029,35036,35055,35014,35026,35063,35025,35030,35010,35004,35050,35061,35046,35043,35037,35003)

  #################
  ## Translation ##
  #################

  to_english1 <- function(df){
    index <- df$`Variable (Code)` == 144
    df$Variable[index] <- "Quantity produced in plant extraction"
    index <- df$`Variable (Code)` == 145
    df$Variable[index] <- "Value produced in plant extraction"

    index <- df$`Unit of measure (Code)` == 40
    df$`Unit of measure`[index] <- "Thousands of Reals"
    index <- df$`Unit of measure (Code)` == 1016
    df$`Unit of measure`[index] <- "Cubic meters"
    index <- df$`Unit of measure (Code)` == 1017
    df$`Unit of measure`[index] <- "Tonnes"
    index <- df$`Unit of measure (Code)` == 43
    df$`Unit of measure`[index] <- "Trees (thousands)"

    index <- df$`Type of product extracted (Code)` == 3402
    df$`Type of product extracted`[index] <- "1. Alimentary (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3403
    df$`Type of product extracted`[index] <- "1.1 A\u00e7a\u00ed (fruit) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3404
    df$`Type of product extracted`[index] <- "1.2 Castanha-de-caju (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3405
    df$`Type of product extracted`[index] <- "1.3 Castanha-do-par\u00e1 (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3406
    df$`Type of product extracted`[index] <- "1.4 Erva-mate (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3407
    df$`Type of product extracted`[index] <- "1.5 Mangaba (fruit) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3408
    df$`Type of product extracted`[index] <- "1.6 Palmito (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 39409
    df$`Type of product extracted`[index] <- "1.7 Pequi (fruit) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3409
    df$`Type of product extracted`[index] <- "1.8 Pinh\u00e3o (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3410
    df$`Type of product extracted`[index] <- "1.9 Umbu (fruit) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 11296
    df$`Type of product extracted`[index] <- "1.10 Others (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3411
    df$`Type of product extracted`[index] <- "2. Aromatics, medicinals, toxic and dyes"
    index <- df$`Type of product extracted (Code)` == 3412
    df$`Type of product extracted`[index] <- "2.1 Ipecacuanha or poaia (root) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3413
    df$`Type of product extracted`[index] <- "2.2 Jaborandi (leaf) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3414
    df$`Type of product extracted`[index] <- "2.3 Urucum (seed) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3415
    df$`Type of product extracted`[index] <- "2.4 Others (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3416
    df$`Type of product extracted`[index] <- "3. Rubbers (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3417
    df$`Type of product extracted`[index] <- "3.1 Caucho (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3418
    df$`Type of product extracted`[index] <- "3.2 Hevea (coagulated latex) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3419
    df$`Type of product extracted`[index] <- "3.3 Hevea (liquid latex) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 40524
    df$`Type of product extracted`[index] <- "3.4 Mangabeira (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3420
    df$`Type of product extracted`[index] <- "4. Waxes (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3421
    df$`Type of product extracted`[index] <- "4.1 Carna\u00faba (wax) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3422
    df$`Type of product extracted`[index] <- "4.2 Carna\u00faba (powder) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 110011
    df$`Type of product extracted`[index] <- "4.3 Others (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3423
    df$`Type of product extracted`[index] <- "5. Fibers (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3424
    df$`Type of product extracted`[index] <- "5.1 Buriti (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3425
    df$`Type of product extracted`[index] <- "5.2 Carna\u00faba (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3426
    df$`Type of product extracted`[index] <- "5.3 Pia\u00e7ava (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3427
    df$`Type of product extracted`[index] <- "5.4 Others (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3428
    df$`Type of product extracted`[index] <- "6.  Non-elastic gums(Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3429
    df$`Type of product extracted`[index] <- "6.1 Balata (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3430
    df$`Type of product extracted`[index] <- "6.2 Ma\u00e7aranduba (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3431
    df$`Type of product extracted`[index] <- "6.3 Sorva (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3433
    df$`Type of product extracted`[index] <- "7.1 Charcoal (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3434
    df$`Type of product extracted`[index] <- "7.2 Firewood (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3435
    df$`Type of product extracted`[index] <- "7.3 Round wood (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3438
    df$`Type of product extracted`[index] <- "8. Oilseeds (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3439
    df$`Type of product extracted`[index] <- "8.1 Baba\u00e7u (almond) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3440
    df$`Type of product extracted`[index] <- "8.2 Copa\u00edba (oil) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3441
    df$`Type of product extracted`[index] <- "8.3 Cumaru (almond) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3442
    df$`Type of product extracted`[index] <- "8.4 Licuri (shell) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3443
    df$`Type of product extracted`[index] <- "8.5 Oiticica (seed) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3444
    df$`Type of product extracted`[index] <- "8.6 Pequi (almond) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3445
    df$`Type of product extracted`[index] <- "8.7 Tucum (almond) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3446
    df$`Type of product extracted`[index] <- "8.8 Others (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3448
    df$`Type of product extracted`[index] <- "9.1 Brazilian Pine (pine knot) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3449
    df$`Type of product extracted`[index] <- "9.2 Brazilian Pine (felled trees) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3450
    df$`Type of product extracted`[index] <- "9.3 Brazilian Pine (round wood) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3451
    df$`Type of product extracted`[index] <- "10. Tannants (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3452
    df$`Type of product extracted`[index] <- "10.1 Angico (bark) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3453
    df$`Type of product extracted`[index] <- "10.2 Barbatim\u00e3o (bark) (Tonnes)"
    index <- df$`Type of product extracted (Code)` == 3454
    df$`Type of product extracted`[index] <- "10.3 Others (Tonnes)"
    return(df)

  }

  #####################
  ## Convert to Long ##
  #####################

  to_long_permanent <- function(df, a){
    if(a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`tipo_de_produto_extrativo_codigo` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "tipo_de_produto_extrativo", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`Brazil` <- NULL
        df[[i]]$`Brazil (Code)` <- NULL
        df[[i]]$`Year (Code)` <- NULL
        df[[i]]$`Unit of measure (Code)` <- NULL
        df[[i]]$`Unit of measure` <- NULL
        df[[i]]$`Type of product extracted (Code)` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "Type of product extracted", "Value")
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

  ###################
  ## Download Data ##
  ###################

  df <- data.frame()

  ## Country Level

  if (aggregation_level == "country"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "Brazil"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  ## Region Level

  if (aggregation_level == "region"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "Region"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  ## State Level

  if (aggregation_level == "state"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "State"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  ## Mesoregion

  if (aggregation_level == "mesoregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "MesoRegion"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  ## Microregion

  if (aggregation_level == "microregion"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf){
        data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "MicroRegion", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
        "Value"
      )
      df <- to_english1(df)
    } else if (language == "pt") {
      df <- janitor::clean_names(df)
    }
  }

  ## City

  if (aggregation_level == "city"){
    for (i in 1:length(years)){
      a <-toString(years[i])
      for (s in sigla_uf2){
        data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "City", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
      for (m in micro){
        data <- suppressMessages(sidrar::get_sidra(289, period = a, geo = "City", geo.filter = list("MicroRegion" = m)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of product extracted (Code)", "Type of product extracted", "Unit of measure (Code)", "Unit of measure",
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

#' @title PEVS Forestry production
#'
#' Loads quantity produced and yield value of forestry extraction, by type of product, from 1986 to 2019
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
#' @export load_pevs_silvprod
#'
#' @examples \dontrun{datazoom.amazonia::load_pevs_silvprod(2013, aggregation_level = "country")}

load_pevs_silvprod <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")

  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  sigla_uf2 = c(12,27,13,16,23,32,21,50,51,15,25,26,22,33,24,11,14,28,17)
  micro = c(29006,29001,29007,29022,29027,29019,29002,29018,29014,29012,29026,29031,29009,
            29011,29029,29010,29024,29013,29004,29025,29005,29032,29015,29021,29003,29020,29023,29008,29016,29030,29028,52007,52009,52003,52017,52006,52005,52012,52010,52008,52015,52016,52004,52018,52002,52001,52013,52014,52011,
            31041,31049,31014,31055,31012,31023,31059,31030,31009,31026,31045,31011,31040,31066,31028,31034,31025,31010,31043,31044,31021,31037,31008,31035,31039,31031,31032,31056,31017,31004,31003,31065,31057,31061,31038,
            31007,31063,31016,31046,31033,31029,31002,31047,31020,31019,31036,31013,31006,31042,31051,31060,31052,31005,31053,31058,31054,31048,31027,31015,31024,31064,31022,31018,31001,31050,31062,41010,41014,41006,41005,
            41025,41023,41035,41003,41015,41037,41012,41008,41024,41026,41004,41029,41017,41032,41013,41016,41020,41036,41011,41009,41030,41038,41001,41027,41028,41021,41007,41031,41039,41034,41019,41022,
            41002,41033,41018,43022,43028,43030,43031,43029,43009,43016,43006,43011,43004,43003,43024,43014,43008,43034,43021,43035,43023,43012,43027,43010,43033,43026,43019,43005,43020,43018,43001,43017,43007,43025,43032,43013,43002,
            43015,42020,42012,42010,42006,42002,42005,42019,42009,42016,42013,42014,42004,42008,42011,42007,42001,42017,42015,42018,42003,35035,35033,35016,35017,35024,35039,35006,35022,35052,35009,
            35015,35020,35018,35023,35048,35032,35049,35044,35054,35005,35034,35002,35012,35058,35051,35059,35056,35060,35042,35041,35011,35013,35001,35021,35047,35027,35019,35038,35062,35031,35007,35008,
            35057,35040,35053,35045,35028,35029,35036,35055,35014,35026,35063,35025,35030,35010,35004,35050,35061,35046,35043,35037,35003)

  to_english1 <- function(df){
    index <- df$`Variable (Code)` == 142
    df$Variable[index] <- "Quantity produced in forestry"
    index <- df$`Variable (Code)` == 143
    df$Variable[index] <- "Value produced in forestry"

    index <- df$`Unit of measure (Code)` == 40
    df$`Unit of measure`[index] <- "Thousands of Reals"
    index <- df$`Unit of measure (Code)` == 1016
    df$`Unit of measure`[index] <- "Cubic meters"
    index <- df$`Unit of measure (Code)` == 1017
    df$`Unit of measure`[index] <- "Tonnes"

    index <- df$`Type of silviculture product (Code)` == 3455
    df$`Type of silviculture product`[index] <- "1.1 Charcoal (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 33247
    df$`Type of silviculture product`[index] <- "1.1.1 Eucalyptus charcoal (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 33248
    df$`Type of silviculture product`[index] <- "1.1.2 Pine charcoal (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 33249
    df$`Type of silviculture product`[index] <- "1.1.3 Charcoal from other species (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 3456
    df$`Type of silviculture product`[index] <- "1.2 firewood (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33250
    df$`Type of silviculture product`[index] <- "1.2.1 Eucalyptus firewood (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33251
    df$`Type of silviculture product`[index] <- "1.2.2 Pine firewood (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 3457
    df$`Type of silviculture product`[index] <- "1.3 Round wood (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 3458
    df$`Type of silviculture product`[index] <- "1.3.1 Round wood for paper and celulose (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33253
    df$`Type of silviculture product`[index] <- "1.3.1.1 Eucalyptus round wood for paper and celulose (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33254
    df$`Type of silviculture product`[index] <- "1.3.1.2 Pine round wood for paper and celulose (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33255
    df$`Type of silviculture product`[index] <- "1.3.1.3 Round wood from other species for paper and celulose (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 3459
    df$`Type of silviculture product`[index] <- "1.3.2 Round wood for other purposes (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33256
    df$`Type of silviculture product`[index] <- "1.3.2.1 Eucalyptus round wood for other purposes (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33257
    df$`Type of silviculture product`[index] <- "1.3.2.2 Pine round wood for other purposes (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 33258
    df$`Type of silviculture product`[index] <- "1.3.2.3 Round wood from other species for other purposes (Cubic meters)"
    index <- df$`Type of silviculture product (Code)` == 3460
    df$`Type of silviculture product`[index] <- "2. Other products (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 3461
    df$`Type of silviculture product`[index] <- "2.1 Ac\u00e1cia-negra (bark) (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 3462
    df$`Type of silviculture product`[index] <- "2.2 Eucalyptus (leaf) (Tonnes)"
    index <- df$`Type of silviculture product (Code)` == 3463
    df$`Type of silviculture product`[index] <- "2.3 Resin (Tonnes)"
    return(df)

  }

  to_long_permanent <- function(df, a){
    if(a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`tipo_de_produto_da_silvicultura_codigo` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "tipo_de_produto_da_silvicultura", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`Brazil` <- NULL
        df[[i]]$`Brazil (Code)` <- NULL
        df[[i]]$`Year (Code)` <- NULL
        df[[i]]$`Unit of measure (Code)` <- NULL
        df[[i]]$`Unit of measure` <- NULL
        df[[i]]$`Type of silviculture product  (Code)` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "Type of silviculture product", "Value")
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "Brazil"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "Region"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "State"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "MesoRegion"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "MicroRegion", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "City", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
      for (m in micro){
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "City", geo.filter = list("MicroRegion" = m)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Type of silviculture product (Code)", "Type of silviculture product", "Unit of measure (Code)", "Unit of measure",
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

#' @title PEVS Silviculture area
#'
#' Loads total existing silviculture area of each product, as of December 31th, from 2013 to 2019
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
#' @export load_pevs_areasilv
#'
#' @examples \dontrun{datazoom.amazonia::load_pevs_areasilv(2013, aggregation_level = "country")}

load_pevs_areasilv <- function(years, aggregation_level = "country", language = "pt", long = FALSE){
  message("Depending on amount of items selected function may take time to run")

  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)
  sigla_uf2 = c(12,27,13,16,23,32,21,50,51,15,25,26,22,33,24,11,14,28,17)
  micro = c(29006,29001,29007,29022,29027,29019,29002,29018,29014,29012,29026,29031,29009,
            29011,29029,29010,29024,29013,29004,29025,29005,29032,29015,29021,29003,29020,29023,29008,29016,29030,29028,52007,52009,52003,52017,52006,52005,52012,52010,52008,52015,52016,52004,52018,52002,52001,52013,52014,52011,
            31041,31049,31014,31055,31012,31023,31059,31030,31009,31026,31045,31011,31040,31066,31028,31034,31025,31010,31043,31044,31021,31037,31008,31035,31039,31031,31032,31056,31017,31004,31003,31065,31057,31061,31038,
            31007,31063,31016,31046,31033,31029,31002,31047,31020,31019,31036,31013,31006,31042,31051,31060,31052,31005,31053,31058,31054,31048,31027,31015,31024,31064,31022,31018,31001,31050,31062,41010,41014,41006,41005,
            41025,41023,41035,41003,41015,41037,41012,41008,41024,41026,41004,41029,41017,41032,41013,41016,41020,41036,41011,41009,41030,41038,41001,41027,41028,41021,41007,41031,41039,41034,41019,41022,
            41002,41033,41018,43022,43028,43030,43031,43029,43009,43016,43006,43011,43004,43003,43024,43014,43008,43034,43021,43035,43023,43012,43027,43010,43033,43026,43019,43005,43020,43018,43001,43017,43007,43025,43032,43013,43002,
            43015,42020,42012,42010,42006,42002,42005,42019,42009,42016,42013,42014,42004,42008,42011,42007,42001,42017,42015,42018,42003,35035,35033,35016,35017,35024,35039,35006,35022,35052,35009,
            35015,35020,35018,35023,35048,35032,35049,35044,35054,35005,35034,35002,35012,35058,35051,35059,35056,35060,35042,35041,35011,35013,35001,35021,35047,35027,35019,35038,35062,35031,35007,35008,
            35057,35040,35053,35045,35028,35029,35036,35055,35014,35026,35063,35025,35030,35010,35004,35050,35061,35046,35043,35037,35003)

  to_english1 <- function(df){
    index <- df$`Variable (Code)` == 6549
    df$Variable[index] <- "Total existing area of silviculture product"

    index <- df$`Forestry species (Code)` == 39326
    df$`Forestry species`[index] <- "Eucalyptus"
    index <- df$`Forestry species (Code)` == 39327
    df$`Forestry species`[index] <- "Pine"
    index <- df$`Forestry species (Code)` == 39328
    df$`Forestry species`[index] <- "Other species"
    return(df)

  }

  to_long_permanent <- function(df, a){
    if(a == 0) {
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`brasil` <- NULL
        df[[i]]$`brasil_codigo` <- NULL
        df[[i]]$`ano_codigo` <- NULL
        df[[i]]$`unidade_de_medida_codigo` <- NULL
        df[[i]]$`unidade_de_medida` <- NULL
        df[[i]]$`especie_florestal_codigo` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "especie_florestal", "valor")
      }
    } else if ( a == 1 ){
      df <- split(df, df$variavel)
      for (i in 1:length(df)){
        df[[i]]$`Brazil` <- NULL
        df[[i]]$`Brazil (Code)` <- NULL
        df[[i]]$`Year (Code)` <- NULL
        df[[i]]$`Unit of measure (Code)` <- NULL
        df[[i]]$`Unit of measure` <- NULL
        df[[i]]$`Forestry species  (Code)` <- NULL
        df[[i]] <- tidyr::spread(df[[i]], "Forestry species", "Value")
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "Brazil"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "Region"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Greater Region (Code)", "Greater Region", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "State"))
      df <- rbind(df, data)
    }

    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "State (Code)", "State", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
      data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "MesoRegion"))
      df <- rbind(df, data)
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MesoRegion (Code)", "Geographic MesoRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "MicroRegion", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Geographic MicroRegion (Code)", "Geographic MicroRegion", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "City", geo.filter = list("State" = s)))
        df <- rbind(df, data)
      }
      for (m in micro){
        data <- suppressMessages(sidrar::get_sidra(291, period = a, geo = "City", geo.filter = list("MicroRegion" = m)))
        df <- rbind(df, data)
      }
    }
    if (language == "eng"){

      colnames(df) <- c(
        "Territorial Level (Code)", "Territorial Level", "Municipality (Code)", "Municipality", "Year (Code)", "Year", "Variable (Code)",
        "Variable", "Forestry species (Code)", "Forestry species", "Unit of measure (Code)", "Unit of measure",
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
