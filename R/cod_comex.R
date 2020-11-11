#' Loads foreign trade data from the Brazilian government from 1986 to the present
#'
#' @param year A \code{vector} indicating what years will the data be loaded
#'
#' @param ncm A \code{boolean} that when set to \code{TRUE} returns data frame from NCM system, default is \code{FALSE}
#'
#' @param sh4 A \code{boolean} that when set to \code{TRUE} returns data frame from SH4 system, default is \code{FALSE}
#'
#' @param exp A \code{boolean} that when set to \code{TRUE} returns export data, default is \code{FALSE}
#'
#' @param imp A \code{boolean} that when set to \code{TRUE} returns import data, default is \code{FALSE}
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @return A \code{list} of data frames
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples comex_data(c(2000,2001,2002), ncm = TRUE, exp = TRUE)
#' comex_data(c(1997,2004,2008), sh4 = TRUE, imp = TRUE, language = "eng")
#'

comex_data <- function(year, ncm = FALSE, sh4 = FALSE, exp = FALSE, imp = FALSE, language = "pt") {
#Unless english is specified, columns will be renamed using Portuguese labels
    if (language != "pt" && language != "eng") {
    warning("Language selected not supported! Proceding with Portuguese")
    }
  #removes invalid years from input vector
  year1 <- year[year <= 2020]
  year1 <- year1[ year1 >= 1997]
  year2 <- year[year >= 1986]
  year2 <- year2[year2 <= 1996]
  #creates list to store data frames
  all_dtframes <- list()
  #first generates export data frames
  if (exp == TRUE) {
    #after 1997 two types of codes for goods are supported
    if (length(year1) != 0) {
      #first type in Mercosul Common Nomenclature
      if (ncm == TRUE) {
        #Generates URLs for each year to download .csv file from website
        url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_", year1, ".csv")
        #creates data frame to store
        data_exp_ncm <- data.frame()
        #for each year, concatenates data frame to all previous year
        for (i in 1:length(url)) {
          df <- read.csv2(url[i])
          data_exp_ncm <- rbind(data_exp_ncm, df)
        }
        #relabels columns in selected language
        if (language == "eng") {
          colnames(data_exp_ncm) <- c(
            "Year", "Month", "Code Mercosul Common Nomenclature", "Statistical Unit Code", "Country code",
            "Acronym of the exporting unit of the federation", "Code of the mode of transport", "Code for the customs processing unit",
            "Statistical Quantity", "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_exp_ncm) <- c(
            "Ano", "Mês", "Código Nomentaclatura Comum do Mercosul", "Código da Unidade Estatística", "Código do país",
            "Sigla da Unidade da Federação exportadora", "Código da via de Transporte", "Código da Unidade da Receita Federal",
            "Quantidade Estatística", "Quantidade em Kg Líquidos", "Valor Free on Board"
          )
        }
        #stores generated data frame in list
        all_dtframes <- list(all_dtframes, data_exp_ncm)
      }
      #same structure used above but for another system
      if (sh4 == TRUE) {
        url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/mun/EXP_", year1, "_MUN.csv")
        data_exp_sh4 <- data.frame()
        for (i in 1:length(url)) {
          df <- read.csv2(url[i])
          data_exp_sh4 <- rbind(data_exp_sh4, df)
        }
        if (language == "eng") {
          colnames(data_exp_sh4) <- c(
            "Year", "Month", "Code Harmonized System", "Country code",
            "Acronym of the exporting unit of the federation", "Code of Fiscal Domicile Municipality",
            "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_exp_sh4) <- c(
            "Ano", "Mês", "Código Sistema Harmonizado", "Código do país",
            "Sigla da Unidade da Federação exportadora", "Código Município do Domicílio Fiscal",
            "Quantidade em Kg Líquidos", "Valor Free on Board"
          )
        }
        all_dtframes <- list(all_dtframes, data_exp_sh4)
      } else if (sh4 == FALSE & ncm == FALSE) {
        #warning to help user in case neither systems are set as TRUE in input
        warning("Type of data (Nomenclatura Comum do Mercosul or Sistema Harmonizado) not selected")
      }
      #before 1997 only one other system is supported, but structure is the same
    } else if (length(year2) != 0) {
      url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/nbm/EXP_", year2, "_NBM.csv")
      data_exp_nbm <- data.frame()
      for (i in 1:length(url)) {
        df <- read.csv2(url[i])
        data_exp_nbm <- rbind(data_exp_nbm, df)
      }

      if (language == "eng") {
        colnames(data_exp_nbm) <- c(
          "Year", "Month", "Code Brazilian Goods Nomenclature", "Country code",
          "Acronym of the exporting unit of the federation",
          "Quantity in net kg", "Free on Board value"
        )
      } else {
        colnames(data_exp_nbm) <- c(
          "Ano", "Mês", "Código Nomentaclatura Brasileira de Mercadorias", "Código do país",
          "Sigla da Unidade da Federação exportadora",
          "Quantidade em Kg Líquidos", "Valor Free on Board"
        )
      }

      all_dtframes <- list(all_dtframes, data_exp_nbm)
    } else {
      warning("Year selected not valid")
    }
  } else if (imp == TRUE) {
    #same structure as exports, but for the import data
    #separate sets of data frame are generated
    if (length(year1) != 0) {
      if (ncm == TRUE) {
        url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/ncm/IMP_", year1, ".csv")
        data_imp_ncm <- data.frame()
        for (i in 1:length(url)) {
          df <- read.csv2(url[i])
          data_imp_ncm <- rbind(data_imp_ncm, df)
        }

        if (language == "eng") {
          colnames(data_imp_ncm) <- c(
            "Year", "Month", "Code Mercosul Common Nomenclature", "Statistical Unit Code", "Country code",
            "Acronym of the importing unit of the federation", "Code of the mode of transport", "Code for the customs processing unit",
            "Statistical Quantity", "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_imp_ncm) <- c(
            "Ano", "Mês", "Código Nomentaclatura Comum do Mercosul", "Código da Unidade Estatística", "Código do país",
            "Sigla da Unidade da Federação importadora", "Código da via de Transporte", "Código da Unidade da Receita Federal",
            "Quantidade Estatística", "Quantidade em Kg Líquidos", "Valor Free on Board"
          )
        }

        all_dtframes <- list(all_dtframes, data_imp_ncm)
      }
      if (sh4 == TRUE) {
        url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/mun/IMP_", year1, "_MUN.csv")
        data_imp_sh4 <- data.frame()
        for (i in 1:length(url)) {
          df <- read.csv2(url[i])
          data_imp_sh4 <- rbind(data_imp_sh4, df)
        }

        if (language == "eng") {
          colnames(data_imp_sh4) <- c(
            "Year", "Month", "Code Harmonized System", "Country code",
            "Acronym of the importing unit of the federation", "Code of Fiscal Domicile Municipality",
            "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_imp_sh4) <- c(
            "Ano", "Mês", "Código Sistema Harmonizado", "Código do país",
            "Sigla da Unidade da Federação importadora", "Código Município do Domicílio Fiscal",
            "Quantidade em Kg Líquidos", "Valor Free on Board"
          )
        }

        all_dtframes <- list(all_dtframes, data_imp_sh4)
      }
    } else if (length(year2) != 0) {
      url <- paste0("http://www.mdic.gov.br/balanca/bd/comexstat-bd/nbm/IMP_", year2, "_NBM.csv")
      data_imp_nbm <- data.frame()
      for (i in 1:length(url)) {
        df <- read.csv2(url[i])
        data_imp_nbm <- rbind(data_imp_nbm, df)
      }

      if (language == "eng") {
        colnames(data_imp_nbm) <- c(
          "Year", "Month", "Code Brazilian Goods Nomenclature", "Country code",
          "Acronym of the importing unit of the federation",
          "Quantity in net kg", "Free on Board value"
        )
      } else {
        colnames(data_imp_nbm) <- c(
          "Ano", "Mês", "Código Nomentaclatura Brasileira de Mercadorias", "Código do país",
          "Sigla da Unidade da Federação importadora",
          "Quantidade em Kg Líquidos", "Valor Free on Board"
        )
      }

      all_dtframes <- list(all_dtframes, data_imp_nbm)
    }
  } else {
    warning("Export or import not selected.")
    #just like the systems, warning is generated it neither import or export are set to TRUE in input
  }
  #removes first entry of list, which is NA
  all_dtframes[1] <- NULL
  #returns list of data frames, one for each combination of export/import and system
  return(all_dtframes)
}

# all_dtframes <- list(data_exp_ncm, data_exp_sh4, data_exp_nbm, data_imp_ncm, data_imp_sh4, data_imp_nbm)
# all_dtframes <- all_dtframes[-which(sapply(all_dtframes, is.na.data.frame))]

# CO_ANO = "Ano"
# Ano = "Year"
# CO_MES = "Mês"
# Mês = "Month"
# CO_NCM = Código Nomentaclatura Comum do Mercosul
# Código Nomenclatura Comum do Mercosul = "Code Mercosul Common Nomenclature"
# CO_UNID = Código da Unidade Estatística
# Código da Unidade Estatística = Statistical Unit Code
# CO_PAIS = Código do país
# Código do país = Country code
# SG_UF_NCM = Sigla da Unidade da Federação importadora/exportadora
# Sigla da Unidade da Federação importadora/exportadora = Acronym of the importing/exporting unit of the federation
# CO_VIA = Código da via de Transporte
# Código da via de transporte = Code of the mode of transport
# CO_URF = Código da Unidade da Receita Federal
# Código da Unidade da Receita Federal = Code for the customs processing unit
# QT_ESTAT = Quantidade Estatística
# Quantidade Estatística = Statistical Quantity
# KG_LIQUIDO = Quantidade em Kilogramas Líquidos
# Quantidade em Kilogramas Líquidos = Quantity in net kilograms
# VL_FOB = Valor Free on Board
# Valor Free on Board
# SH4 = Código do produto no Sistema Harmonizado
# Código do product no Sistema Harmonizado = Product code in the Harmonized System
# CO_MUN = código do município domicílio fiscal
# Código do município domicílio fiscal = Code of fiscal domicile municipality
