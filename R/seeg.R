#' @title Greenhouse gases emission estimates (SEEG)
#'
#' Loads data of estimates of emission of greenhouse gases of Brazilian cities
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
#' @export load_seeg
#'
#' @examples \dontrun{datazoom.amazonia::load_seeg(2013, language = "eng")}
#'

load_seeg <- function(language = "pt"){
  message("Function requires download of large file, so it may take time to run")

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
    language = "pt"
  }

  direc <- getwd()
  url <- "https://drive.google.com/file/d/1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd/view?usp=sharing"
  urlteste <- "https://drive.google.com/file/d/1A_neQegigQuDvGHfpngk_11wsqGyR8US/view?usp=sharing"

  df <- gsheet::gsheet2tbl(urlteste)
  df <- janitor::clean_names(df)

  df <- as.data.frame(gsub("\u00ed", "i", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f5", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e1", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e7", "c", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e3", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00c1", "A", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e9", "e", as.matrix(df)))
  df <- as.data.frame(gsub("\u00fa", "u", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f4", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f3", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e2", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00d3", "O", as.matrix(df)))
  df <- as.data.frame(gsub("gabagaba", "Energia", as.matrix(df)))


  to_english <- function(df){
    index <- df$`Tier 1` == "Agropeacuaria"
    df$`Tier 1`[index] <- "agricultura and cattle raising"
    index <- df$`Tier 1` == "Energia"
    df$`Tier 1`[index] <- "Energy"
    index <- df$`Tier 1` == "Mudança de Uso da Terra e Florestas"
    df$`Tier 1`[index] <- "Changes in land and forrest use"
    index <- df$`Tier 1` == "Processos Industriais"
    df$`Tier 1`[index] <- "industrial processes"
    index <- df$`Tier 1` == "Residuos"
    df$`Tier 1`[index] <- "residues"
    ## TIER 2##
    index <- df$`Tier 2` == "Alteracoes de Uso do Solo"
    df$`Tier 2`[index] <- "Changes in Land use"
    index <- df$`Tier 2` == "Cultivo de Arroz"
    df$`Tier 2`[index] <- "Rice growing"
    index <- df$`Tier 2` == "Efluentes Liquidos"
    df$`Tier 2`[index] <- "Liquid effluents"
    index <- df$`Tier 2` == "Emissoes de HFCs"
    df$`Tier 2`[index] <- "HFC emissions"
    index <- df$`Tier 2` == "Residuos"
    df$`Tier 2`[index] <- "residues"
    index <- df$`Tier 2` == "Residuos"
    df$`Tier 2`[index] <- "residues"
    index <- df$`Tier 2` == "Residuos"
    df$`Tier 2`[index] <- "residues"
    index <- df$`Tier 2` == "Residuos"
    df$`Tier 2`[index] <- "residues"
    index <- df$`Tier 2` == "Residuos"
    df$`Tier 2`[index] <- "residues"

  }

  if (language == "eng"){
    colnames(df) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5", "Tier 6", "Type of emission",
                    "Gas", "Territory", "Municipality_Code", "Municipality", "Economic_activity", "Product",
                    "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007","2008","2009", "2010",
                    "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019_AREA")



  }
}
