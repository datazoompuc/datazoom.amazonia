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

load_pam_permanent <- function(years, aggregation_level = "country", language = "pt"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

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
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of permanent tillage (Code)", "Product of permanent tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }

  if (aggregation_level == "region"){
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

  if (aggregation_level == "state"){
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

  if (aggregation_level == "mesoregion"){
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

  if (aggregation_level == "microregion"){
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

  if (aggregation_level == "city"){
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

load_pam_temporary <- function(years, aggregation_level = "country", language = "pt"){
  message("Depending on amount of items selected function may take time to run")
  sigla_uf = c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)

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
    colnames(df) <- c(
      "Territorial Level (Code)", "Territorial Level", "Brazil (Code)", "Brazil", "Year (Code)", "Year", "Variable (Code)",
      "Variable", "Product of temporary tillage (Code)", "Product of temporary tillage", "Unit of measure (Code)", "Unit of measure",
      "Value"
    )
  }

  if (aggregation_level == "region"){
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

  if (aggregation_level == "state"){
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

  if (aggregation_level == "mesoregion"){
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

  if (aggregation_level == "microregion"){
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

  if (aggregation_level == "city"){
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

}
