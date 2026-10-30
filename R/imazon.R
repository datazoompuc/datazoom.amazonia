#' @title IMAZON - Deforestation and forest degradation alerts by municipality (SAD)
#'
#' @description Loads Imazon's SAD (Sistema de Alerta de Desmatamento): the area of
#' forest cleared (deforestation) or degraded (fire, selective logging) that Imazon's
#' satellite monitoring detected in each municipality of the Legal Amazon, month by
#' month, since 2008. Published by Imazon at \url{https://dashboards.imazongeo.org.br/sad/}.
#'
#' @param dataset A dataset name ("sad_deforestation" or "sad_degradation") with the
#' deforestation or the forest degradation alerts, respectively.
#' @param raw_data A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.
#' With \code{TRUE}, the rows are exactly as Imazon publishes them (one per month, municipality and satellite sensor), for the years in \code{time_period}.
#' @param time_period A \code{numeric} indicating for which years the data will be loaded, in the format YYYY.
#' Can be any vector of numbers, such as 2010:2012. By default (\code{NULL}) every year is returned.
#' @param language A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.
#'
#' @return A \code{tibble} with one row per municipality and month in which an alert was
#' detected: the year, the month, the municipality's IBGE code, its name and state, and the
#' area of the alerts in square kilometers (summed over the satellite sensors that reported it).
#' A municipality-month with no alert has no row.
#'
#' @details Imazon publishes municipalities by name and state only, so the IBGE code
#' (\code{municipality_code}) is added here by matching them with the package's
#' \code{municipalities} table. A name Imazon writes with a state the table does not have
#' for it (a few in the degradation alerts) keeps \code{NA} as its code, with a warning.
#'
#' This replaces the data \code{load_imazon()} returned until version 1.2.0
#' ("imazon_shp", a 0 to 3 classification of the municipalities, 2020), which was a file
#' Datazoom kept in a private Google Drive and not something Imazon publishes.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated Imazon deforestation alerts, by municipality and month
#' data <- load_imazon(
#'   dataset = "sad_deforestation",
#'   raw_data = FALSE,
#'   time_period = 2020:2024,
#'   language = "eng"
#' )
#'
#' @export

load_imazon <- function(dataset, raw_data = FALSE, time_period = NULL, language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  ANO <- MES <- ESTADO <- MUNICIPIO <- AREAKM2 <- area <- municipality_code <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "imazon"
  param$dataset <- dataset
  param$time_period <- time_period
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and time_period are valid

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  # FRAGILE: the data are the two CSV files behind Imazon's own SAD dashboard
  # (the manifest's imazon rows: .../sad/api/sad/desmatamento/municipios.csv and
  # .../degradacao/municipios.csv), already summed by Imazon's server into
  # month x municipality x sensor. They are the dashboard's data feed, not a
  # documented API: Imazon can change or retire them without notice. The
  # alternative copies on its S3 bucket are one row per alert polygon, with the
  # municipality blank for 99% of them, so they are no substitute.
  # resolve_imazon.R fetches both every week and checks the columns, the months
  # and the years against the dashboard's own /periodo endpoint.
  dat <- external_download(
    dataset = param$dataset,
    source = param$source
  )

  dat <- tibble::as_tibble(dat)

  if (!is.null(param$time_period)) {
    dat <- dat %>%
      dplyr::filter(ANO %in% param$time_period)
  }

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # FRAGILE: the raw columns named here -- ANO, MES, ESTADO, MUNICIPIO and
  # AREAKM2 -- are the ones Imazon's files carry today (ALERTA and SENSOR too,
  # unused). A renamed column stops dplyr loudly; one that changed meaning (the
  # area in hectares, say) would not, which is why resolve_imazon.R also checks
  # the values (month 1-12, year within the dashboard's period, area in
  # square kilometers within what SAD has ever reported).

  # Several satellite sensors can report in the same municipality and month; the
  # alerts are different polygons, so the municipality-month's area is their sum.
  dat <- dat %>%
    dplyr::group_by(ANO, MES, ESTADO, MUNICIPIO) %>%
    dplyr::summarise(area = sum(AREAKM2), .groups = "drop") %>%
    dplyr::arrange(ANO, MES, ESTADO, MUNICIPIO)

  dat <- imazon_add_municipality_code(dat)

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::select(
        "year" = ANO, "month" = MES, "municipality_code" = municipality_code,
        "municipality" = MUNICIPIO, "state" = ESTADO, "area" = area
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::select(
        "ano" = ANO, "mes" = MES, "cod_municipio" = municipality_code,
        "municipio" = MUNICIPIO, "uf" = ESTADO, "area" = area
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}

# The IBGE code of each row's municipality, from the package's `municipalities`
# table by name and state (Imazon gives no code). `dat` has ESTADO (two letters)
# and MUNICIPIO (the name, accents and all) and gets a `municipality_code` column.
#
# FRAGILE: the match is by name and state, not by code, so a name Imazon spells
# differently from the table (beyond accents, case and punctuation, which are
# ignored), or a state that is not the municipality's, finds no row.
# Those rows keep NA and the call warns. Checked 2026-10-30 on the whole
# history: the 624 municipalities of the deforestation alerts all match; of the
# 474 of the degradation alerts, 18 do not -- Imazon lists them under another
# state (Machadinho D'Oeste under MT, Porto Velho under AM, ...), 0.06% of the
# area. They are not guessed from the name alone: Imazon's state or its
# municipality could be the wrong one.
imazon_add_municipality_code <- function(dat) {
  ESTADO <- key <- code_muni <- name_muni <- abbrev_state <- municipality_code <- MUNICIPIO <- area <- NULL

  reference <- tibble::as_tibble(as.data.frame(datazoom.amazonia::municipalities)) %>%
    dplyr::transmute(
      municipality_code = code_muni,
      key = imazon_municipality_key(name_muni),
      ESTADO = abbrev_state
    )

  dat <- dat %>%
    dplyr::mutate(key = imazon_municipality_key(MUNICIPIO)) %>%
    dplyr::left_join(reference, by = c("key", "ESTADO")) %>%
    dplyr::select(-key)

  unmatched <- dat %>%
    dplyr::filter(is.na(municipality_code))

  if (nrow(unmatched) > 0) {
    names_unmatched <- unmatched %>%
      dplyr::distinct(MUNICIPIO, ESTADO)
    example <- utils::head(paste0(names_unmatched$MUNICIPIO, " (", names_unmatched$ESTADO, ")"), 5)
    warning(
      nrow(unmatched), " row(s), ", round(sum(unmatched$area), 1), " km2 (",
      round(100 * sum(unmatched$area) / sum(dat$area), 2), "% of the area), name ",
      nrow(names_unmatched), " municipality/state pair(s) that the package's municipalities table does not have, e.g. ",
      paste(example, collapse = ", "), ". Their municipality code is NA.",
      call. = FALSE
    )
  }

  dat
}

# "Querencia", "QUERÊNCIA" and "Querência" are one key; so are "Machadinho D'Oeste"
# and "MACHADINHO D’OESTE" (accents, case and punctuation are ignored; spaces are not).
imazon_municipality_key <- function(x) {
  toupper(gsub("[^A-Za-z0-9 ]", "", stringi::stri_trans_general(trimws(x), "Latin-ASCII")))
}
