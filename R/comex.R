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
#' @examples \dontrun{load_comex(c(2000,2001,2002), ncm = TRUE, exp = TRUE)}
#'
#' @examples
#' \dontrun{
#' load_comex(c(2000, 2001, 2002),
#'   ncm = TRUE,
#'   exp = TRUE)
#' load_comex(c(1997, 2004, 2008),
#'   sh4 = TRUE,
#'   imp = TRUE,
#'   language = "eng")
#' }
#'
load_comex <- function(year, ncm = FALSE, sh4 = FALSE, exp = FALSE, imp = FALSE, language = "pt") {
  message("Depending on amount of items selected function may take time to run")
  # Unless english is specified, columns will be renamed using Portuguese labels
  if (language != "pt" && language != "eng") {
    warning("Language selected not supported! Proceding with Portuguese")
  }
  # removes invalid years from input vector
  year1 <- year[year <= 2020]
  year1 <- year1[year1 >= 1997]
  year2 <- year[year >= 1989]
  year2 <- year2[year2 <= 1996]
  # creates list to store data frames
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_3 <- data.frame()
  data_4 <- data.frame()
  data_5 <- data.frame()
  data_6 <- data.frame()
  # first generates export data frames
  if (exp == TRUE) {
    # after 1997 two types of codes for goods are supported
    if (length(year1) != 0) {
      # first type in Mercosul Common Nomenclature
      if (ncm == TRUE) {
        # Generates URLs for each year to download .csv file from website
        url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_", year1, ".csv")
        # for each year, concatenates data frame to all previous year
        for (i in 1:length(url)) {
          df <- readr::read_csv2(url[i])
          data_1 <- rbind(data_1, df)
        }
        # relabels columns in selected language
        if (language == "eng") {
          colnames(data_1) <- c(
            "Year", "Month", "Code Mercosul Common Nomenclature", "Statistical Unit Code", "Country code",
            "Acronym of the exporting unit of the federation", "Code of the mode of transport", "Code for the customs processing unit",
            "Statistical Quantity", "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_1) <- c(
            "Ano", "M\u00eass", "C\u00f3digo Nomentaclatura Comum do Mercosul", "C\u00f3digo da Unidade Estat\u00edstica", "C\u00f3digo do pa\u00eds",
            "Sigla da Unidade da Federa\u00e7\u00e3o exportadora", "C\u00f3digo da via de Transporte", "C\u00f3digo da Unidade da Receita Federal",
            "Quantidade Estat\u00edstica", "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
          )
        }
      }
      # same structure used above but for another system
      if (sh4 == TRUE) {
        url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_", year1, "_MUN.csv")

        for (i in 1:length(url)) {
          df <- readr::read_csv2(url[i])
          data_2 <- rbind(data_2, df)
        }
        if (language == "eng") {
          colnames(data_2) <- c(
            "Year", "Month", "Code Harmonized System", "Country code",
            "Acronym of the exporting unit of the federation", "Code of Fiscal Domicile Municipality",
            "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_2) <- c(
            "Ano", "M\u00eass", "C\u00f3digo Sistema Harmonizado", "C\u00f3digo do pa\u00eds",
            "Sigla da Unidade da Federa\u00e7\u00e3o exportadora", "C\u00f3digo Munic\u00edpio do Domic\u00edlio Fiscal",
            "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
          )
        }
      } else if (sh4 == FALSE & ncm == FALSE) {
        # warning to help user in case neither systems are set as TRUE in input
        warning("Type of data (Nomenclatura Comum do Mercosul or Sistema Harmonizado) not selected")
      }
      # before 1997 only one other system is supported, but structure is the same
    }
    if (length(year2) != 0) {
      url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/nbm/EXP_", year2, "_NBM.csv")

      for (i in 1:length(url)) {
        df <- readr::read_csv2(url[i])
        data_3 <- rbind(data_3, df)
      }

      if (language == "eng") {
        colnames(data_3) <- c(
          "Year", "Month", "Code Brazilian Goods Nomenclature", "Country code",
          "Acronym of the exporting unit of the federation",
          "Quantity in net kg", "Free on Board value"
        )
      } else {
        colnames(data_3) <- c(
          "Ano", "M\u00eass", "C\u00f3digo Nomentaclatura Brasileira de Mercadorias", "C\u00f3digo do pa\u00eds",
          "Sigla da Unidade da Federa\u00e7\u00e3o exportadora",
          "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
        )
      }
    } else if (length(year1) == 0 && length(year2) == 0) {
      stop("Year selected not valid")
    }
  }
  if (imp == TRUE) {
    # same structure as exports, but for the import data
    # separate sets of data frame are generated
    if (length(year1) != 0) {
      if (ncm == TRUE) {
        url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_", year1, ".csv")

        for (i in 1:length(url)) {
          df <- readr::read_csv2(url[i])
          data_4 <- rbind(data_4, df)
        }

        if (language == "eng") {
          colnames(data_4) <- c(
            "Year", "Month", "Code Mercosul Common Nomenclature", "Statistical Unit Code", "Country code",
            "Acronym of the importing unit of the federation", "Code of the mode of transport", "Code for the customs processing unit",
            "Statistical Quantity", "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_4) <- c(
            "Ano", "M\u00eass", "C\u00f3digo Nomentaclatura Comum do Mercosul", "C\u00f3digo da Unidade Estat\u00edstica", "C\u00f3digo do pa\u00eds",
            "Sigla da Unidade da Federa\u00e7\u00e3o importadora", "C\u00f3digo da via de Transporte", "C\u00f3digo da Unidade da Receita Federal",
            "Quantidade Estat\u00edstica", "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
          )
        }
      }
      if (sh4 == TRUE) {
        url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_", year1, "_MUN.csv")

        for (i in 1:length(url)) {
          df <- readr::read_csv2(url[i])
          data_5 <- rbind(data_5, df)
        }

        if (language == "eng") {
          colnames(data_5) <- c(
            "Year", "Month", "Code Harmonized System", "Country code",
            "Acronym of the importing unit of the federation", "Code of Fiscal Domicile Municipality",
            "Quantity in net kg", "Free on Board value"
          )
        } else {
          colnames(data_5) <- c(
            "Ano", "M\u00eass", "C\u00f3digo Sistema Harmonizado", "C\u00f3digo do pa\u00eds",
            "Sigla da Unidade da Federa\u00e7\u00e3o importadora", "C\u00f3digo Munic\u00edpio do Domicilio Fiscal",
            "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
          )
        }
      }
    }
    if (length(year2) != 0) {
      url <- paste0("http://www.balanca.economia.gov.br/balanca/bd/comexstat-bd/nbm/IMP_", year2, "_NBM.csv")

      for (i in 1:length(url)) {
        df <- readr::read_csv2(url[i])
        data_6 <- rbind(data_6, df)
      }

      if (language == "eng") {
        colnames(data_6) <- c(
          "Year", "Month", "Code Brazilian Goods Nomenclature", "Country code",
          "Acronym of the importing unit of the federation",
          "Quantity in net kg", "Free on Board value"
        )
      } else {
        colnames(data_6) <- c(
          "Ano", "M\u00eass", "C\u00f3digo Nomentaclatura Brasileira de Mercadorias", "C\u00f3digo do pa\u00eds",
          "Sigla da Unidade da Federa\u00e7\u00e3o importadora",
          "Quantidade em Kg L\u00edquidos", "Valor Free on Board"
        )
      }
    } else if (length(year1) == 0 && length(year2) == 0) {
      stop("Year selected not valid")
    }
  }
  if (exp == FALSE && imp == FALSE) {
    warning("Export or import not selected.")
    # just like the systems, warning is generated it neither import or export are set to TRUE in input
  }

  all_dtframes <- list(data_1, data_2, data_3, data_4, data_5, data_6)
  all_dtframes <- all_dtframes[lapply(all_dtframes, length) > 0]
  # returns list of data frames, one for each combination of export/import and system
  return(all_dtframes)
}
