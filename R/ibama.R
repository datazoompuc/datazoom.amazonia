#' @importFrom dplyr %>%
#' @importFrom rlang .data

NULL


#' Compiles environmental fines data in the Amazon Region from IBAMA
#'
#' Original data comes from \href{https://servicos.ibama.gov.br/ctf/publico/areasembargadas/ConsultaPublicaAreasEmbargadas.php}{IBAMA's website}
#'
#' @name load_ibama
#' @encoding UTF-8
#' @param download_data If \code{TRUE}, downloads the data from the original source.
#' Else, loads data from \code{load_from_where} directory.
#' @param download_directory Directory where raw data should be stored.
#' Used in case \code{download_data = TRUE}.
#' @param load_from_where Original that should be loaded. Used in
#' case \code{download_data = FALSE}.
#' @param time_aggregation Temporal level of aggregation, choose \code{year} and/or \code{month}.
#' If \code{language = 'pt'}, pick "ano" and/or "mes'"
#' @param space_aggregation Geographical level of aggregation, choose \code{municipality} and/or
#' \code{State}. For Portuguese version, \code{municipio} and/or \code{uf}
#' @param years Time period to be considered. Contemplates 2005 to the present day
#' @param language Language used in returned dataset. Use \code{language = "pt"} or
#' \code{language = "eng"}
#' @return A data frame with counts for variables related to environmental fines and
#' infractions by \code{time_aggregation}-\code{space_aggregation} chosen on the Amazon region.
#' Data include
#' counts for total number of infractions, infractions that already went to trial, and
#' number of unique perpetrators of infractions on the given place-time period.
#'
#' @examples
#'
#'  \dontrun{
#' load_ibama(
#'   download_directory = getwd(),
#'   time_aggregation = "year",
#'   space_aggregation = "state", years = 2005:2006
#' )
#'
#' load_ibama(
#'   download_directory = getwd(),
#'   time_aggregation = "ano",
#'   space_aggregation = "municipio", years = c(2010, 2012),
#'   language = "pt"
#' )
#'
#'
#' load_ibama(
#'   download_data = FALSE,
#'   load_from_where = "./Desktop/data.xls",
#'   time_aggregation = c("year", "month"),
#'   space_aggregation = c("municipality", "state")
#' )
#' }
#'
#' @export

load_ibama <- function(download_data = TRUE,
                       download_directory,
                       load_from_where,
                       time_aggregation = c("year", "month"),
                       space_aggregation = "municipality",
                       years = 2005:2021,
                       language = "eng") {
  if (download_data == TRUE) {
    download_ibama(download_dir = download_directory)
    load_from_where <- list.files(
      path = download_directory,
      pattern = "*.xls$",
      full.names = TRUE
    )
  }

  df <- clean_xml_table(file_location = load_from_where) %>%
    aggregate_fines(
      t = time_aggregation, g = space_aggregation,
      y = years, lan = language
    )


  return(df)
}



aggregate_fines <- function(dataset, t = c("year", "month"), g = "municipality",
                           y = 2005:2020, lan = "eng") {
  if ("municipality" %in% g | "municipio" %in% g) {

    if(lan == 'eng'){

    grp <- c(g, "municipality_code")

    } else{

    grp <- c(g, "cod_municipio")

    }

  } else {
    grp <- g
  }

  df <- dataset %>%
    dplyr::filter(
      .data$codigo_ibge_municipio_embargo %in% legal_amazon$CD_MUN
    ) %>%
    dplyr::select(
      .data$municipio_embargo, .data$uf_embargo,
      .data$codigo_ibge_municipio_embargo, .data$julgamento,
      .data$infracao, .data$data_de_insercao_na_lista,
      .data$cpf_ou_cnpj
    ) %>%
    dplyr::mutate(
      julgamento = ifelse(.data$julgamento == "pendente de julgamento", FALSE, TRUE),
      data_de_insercao_na_lista = lubridate::dmy(.data$data_de_insercao_na_lista),
      ano = lubridate::year(.data$data_de_insercao_na_lista),
      mes = lubridate::month(.data$data_de_insercao_na_lista)
    ) %>%
    dplyr::filter(.data$ano %in% y) %>%
    dplyr::rename(
      municipio = .data$municipio_embargo,
      uf = .data$uf_embargo,
      cod_municipio = .data$codigo_ibge_municipio_embargo
    )


  if (lan != "pt" | is.null(lan)) {
    df <- df %>%
      dplyr::rename(
        municipality = .data$municipio,
        state = .data$uf,
        month = .data$mes,
        year = .data$ano,
        municipality_code = .data$cod_municipio

      )
  }

  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(t, grp)))) %>%
    dplyr::summarise(
      n_ja_julgado = sum(!is.na(.data$julgamento), na.rm = TRUE),
      n_infracoes = dplyr::n(),
      n_cpf_cnpj_unicos = length(unique(.data$cpf_ou_cnpj)),
      .groups = "drop"
    )


  if (lan != "pt" | is.null(lan)) {
    df <- df %>%
      dplyr::rename(
        cases_already_tried = .data$n_ja_julgado,
        number_infractions = .data$n_infracoes,
        number_unique_offenders = .data$n_cpf_cnpj_unicos
      )
  }

  warning("Data from MA state considers only municipalities in the Amazon region",
    call. = FALSE
  )

  return(df)
}

download_ibama <- function(download_dir) {
  utils::download.file(
    url = "https://servicos.ibama.gov.br/ctf/publico/areasembargadas/downloadListaAreasEmbargadas.php",
    destfile = file.path(download_dir, "fines.rar"),
    mode = 'wb',
    method = 'auto'
  )

  utils::unzip(
    zipfile = file.path(download_dir, "fines.rar"),
    exdir = file.path(download_dir)
  )
}

clean_xml_table <- function(file_location) {
  doc <- XML::htmlParse(file_location,
    encoding = "UTF-8"
  )

  tableNode <- XML::getNodeSet(doc, "//table")

  dataset <- XML::readHTMLTable(tableNode[[1]])


  colnames(dataset) <- dataset[6, ]

  dataset <- dataset[-c(1:6), ] %>%
    janitor::clean_names()


  return(dataset)
}
