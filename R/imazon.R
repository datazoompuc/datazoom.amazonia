#' @title IMAZON - Deforestation pressure by municipality
#'
#' @description Loads data categorizing each municipality by the level of deforestation pressure it faces
#'
#' @param dataset There is one dataset available ("imazon_shp")
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated Imazon deforestation pressure data
#' data <- load_imazon(
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' @export

load_imazon <- function(dataset = "imazon_shp", raw_data = FALSE, language = "eng") {
  # Checking for googledrive package (in Suggests)

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Package \"googledrive\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ##############################
  ## Binding Global Variables ##
  ##############################

  CD_GEOCMU <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "imazon"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset is valid

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  # FRAGILE: the data are ONE file on Google Drive (the manifest's imazon_shp url,
  # read with googledrive, no sign-in): "imazon.rds", a Datazoom conversion of
  # Imazon's Fronteiras_Amazonia_2020.shp, uploaded 2022-06-20 to a
  # contributor's personal Drive. Imazon does not publish it (no official copy was
  # found, 2026-10-29), so nothing upstream will ever tell us it moved; if that
  # account is closed or the sharing changed, this stops for every user.
  # resolve_imazon.R re-reads the file every week (md5 against Drive's, the
  # columns below) and reports a replacement through the manifest's `version`.
  dat <- external_download(
    dataset = param$dataset,
    source = param$source
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # Dropping geometry column

  dat <- dat %>%
    sf::st_drop_geometry()

  # Converting municipality codes into numeric

  dat <- dat %>%
    dplyr::mutate(dplyr::across(CD_GEOCMU, as.numeric))

  ################################
  ## Harmonizing Variable Names ##
  ################################

  # FRAGILE: the five raw columns the file carries -- CD_GEOCMU, NM_MUNICIP, Area, UF
  # and Front_2020 -- are named here and used above. Front_2020 is the file's
  # vintage written as a literal, not derived from anything: a file with Front_2023
  # would stop rename() (loudly), and one that added it beside Front_2020 would pass
  # it through unrenamed (silently). resolve_imazon.R fails on either, every week
  # (its IMAZON_COLUMNS / IMAZON_LOADER_FRONT are a copy of these names, held equal
  # by tests/testthat/test-imazon-resolver.R); adopting a newer vintage means making
  # this rename version-aware first.
  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality_code" = "CD_GEOCMU",
        "municipality" = "NM_MUNICIP",
        "area" = "Area",
        "state" = "UF",
        "deforestation_pressure" = "Front_2020"
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "cod_municipio" = "CD_GEOCMU",
        "municipio" = "NM_MUNICIP",
        "area" = "Area",
        "uf" = "UF",
        "pressao_desmatamento" = "Front_2020"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
