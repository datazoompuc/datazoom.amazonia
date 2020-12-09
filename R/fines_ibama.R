
#' @importFrom dplyr %>%
#' @importFrom rlang .data

NULL


#' Compiles environmental fines data in the Amazon Region from IBAMA
#'
#'Original data comes from \href{https://servicos.ibama.gov.br/ctf/publico/areasembargadas/ConsultaPublicaAreasEmbargadas.php}{IBAMA's website}
#'
#' @name fines_data_agg
#' @encoding UTF-8
#' @param language Language used in returned dataset. Use \code{language = "portuguese"} or
#' \code{language = "english"}
#' @param time_unit Temporal level of aggregation, choose "year" and/or "month".
#' If \code{language = 'portuguese'}, pick "ano" and/or "mes'"
#' @param geographic_unit Geographical level of aggregation, choose "city" and/or
#' "state". For portuguese version, "municipio" and/or "uf"
#' @param years Time period to be considered. Contemplates 2005 to 2020.
#' @return A dataframe with counts for variables related environmental fines and
#' infractions by \code{time_unit}-\code{geographic_unit} chosen. Data include
#' counts for total number of infractions, infractions that already went to trial, and
#' number of unique perpetrators of infractions on the given place-time period.
#'
#' @examples
#'
#' fines_data_agg(
#'   time_unit = year,
#'   geographic_unit = state, years = 2005:2006
#' )
#'
#' fines_data_agg(
#'   time_unit = c(year, month),
#'   geographic_unit = c(city, state)
#' )
#'
#' fines_data_agg(
#'   time_unit = ano,
#'   geographic_unit = municipio, years = c(2010, 2012),
#'   language = "portuguese"
#' )
#' @export


fines_data_agg <- function(time_unit = c(.data$year, .data$month), geographic_unit =.data$ city,
                           years = 2005:2020, language = "english") {
  df <- data_amazon_fines %>%
    dplyr::select(
      .data$municipio_embargo, .data$uf_embargo,
      .data$codigo_ibge_municipio_embargo, .data$julgamento,
      .data$infracao, .data$data_de_insercao_na_lista,
      .data$cpf_ou_cnpj
    ) %>%
    dplyr::mutate(
      ano = lubridate::year(.data$data_de_insercao_na_lista),
      mes = lubridate::month(.data$data_de_insercao_na_lista)
    ) %>%
    dplyr::filter(.data$ano %in% years) %>%
    dplyr::rename(
      municipio = .data$municipio_embargo,
      uf = .data$uf_embargo
    )


  if (language != "portuguese") {
    df <- df %>%
      dplyr::rename(
        city = .data$municipio,
        state = .data$uf,
        month = .data$mes,
        year = .data$ano
      )
  }

  df <- df %>%
    dplyr::group_by(dplyr::across(c({{ time_unit }}, {{ geographic_unit }}))) %>%
    dplyr::summarise(
      n_ja_julgado = sum(!is.na(.data$julgamento), na.rm = TRUE),
      n_infracoes = dplyr::n(),
      n_cpf_cnpj_unicos = length(unique(.data$cpf_ou_cnpj))
    )

  if (language != "portuguese") {
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
