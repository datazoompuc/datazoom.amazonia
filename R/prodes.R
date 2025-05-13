#' @title PRODES - Deforestation Monitoring Project in the Legal Amazon by Satellite
#'
#' @description Loads data on deforestation in the Legal Amazon region.
#'
#' @param dataset A dataset name ("deforestation").
#' @inheritParams load_baci
#'
#' @return A \code{tibble} with the selected data.
#'
#' @examples
#' \dontrun{
#' # Download treated data (raw_data = FALSE)
#' # in portuguese (language = 'pt').
#' data <- load_prodes(
#'   raw_data = FALSE,
#'   language = "pt"
#' )
#' }
#'
#' @export

load_prodes <- function(dataset = "deforestation", raw_data = FALSE,
                        language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  year <- municipio <- cod_ibge <- estado <- area_km2 <- increment <- bioma <- NULL
  municipality <- municipality_code <- state <- deforestation <- desmatamento2000 <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "prodes"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language

  # check if dataset and time_period are supported

  check_params(param)

  ###################
  ## Download Data ##
  ###################

  ## Column Names come with numbers at the side - we need to clean those

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

  # keep only deforestation-related variables

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::select(
      municipio, cod_ibge, estado, area_km2, bioma, desmatamento2000, dplyr::starts_with("incremento")
    )

  # change to long format with increment variable

  dat <- dat %>%
    dplyr::rename("incremento2000" = "desmatamento2000") %>%
    tidyr::pivot_longer(
      dplyr::starts_with("incremento"),
      names_prefix = "incremento",
      names_to = "year",
      values_to = "increment"
    )

  # calculating cumulative deforestation

  dat <- dat %>%
    dplyr::arrange(municipio, year) %>%
    dplyr::mutate(
      deforestation = cumsum(increment),
      .by = c("municipio", "estado", "bioma")
    )

  dat <- dat %>%
    dplyr::mutate(
      increment = dplyr::case_when(
        year == 2000 ~ NA,
        .default = increment
      )
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename(
        "municipality" = municipio,
        "municipality_code" = cod_ibge,
        "state" = estado,
        "biome" = bioma
      )
  }
  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename(
        "ano" = year,
        "cod_municipio" = "cod_ibge",
        "uf" = estado,
        "incremento" = increment,
        "desmatamento" = deforestation
      )
  }

  return(dat_mod)
}




### ajuste_desmatamento_prodes (Vanndher) ###
library(sf)
library(dplyr)
library(stringi)

# Caminhos
path_accumulated <- "C:/Users/usuário/Downloads/accumulated_deforestation_2007.shp"
path_yearly <- "C:/Users/usuário/Downloads/yearly_deforestation.shp"
path_municipality <- "C:/Users/usuário/Downloads/BR_Municipios_2023.shp"

# Leitura
accumulated <- st_read(path_accumulated)
yearly <- st_read(path_yearly)
municipality <- st_read(path_municipality)

# Remoção das colunas por índice
accumulated <- accumulated %>%
  select(state, main_class, year, area_km, geometry)

yearly <- yearly %>%
  select(state, main_class, year, area_km, geometry)

municipality <- municipality %>%
  select(NM_MUN, NM_UF, NM_REGIAO, geometry)


teste_prodes <- function(ano = NULL, municipio = NULL, estado = NULL) {

  sf_use_s2(FALSE)

  # Padronizar município e estado buscados
  muni_std <- tolower(stri_trans_general(municipio, "Latin-ASCII"))
  estado_std <- tolower(stri_trans_general(estado, "Latin-ASCII"))

  # Padronizar os nomes do shapefile para comparar
  municipality_std <- municipality %>%
    mutate(
      nome_mun_std = tolower(stri_trans_general(NM_MUN, "Latin-ASCII")),
      nome_uf_std = tolower(stri_trans_general(NM_UF, "Latin-ASCII"))
    )

  # Filtrar
  mun <- municipality_std %>%
    filter(nome_mun_std == muni_std & nome_uf_std == estado_std)


  if (nrow(mun) == 0) {
    stop("Município não encontrado. Verifique o nome e o estado.")
  }

  # Garantir que os dados estão no mesmo CRS
  accumulated <- st_transform(accumulated, st_crs(mun))
  yearly <- st_transform(yearly, st_crs(mun))

  # Reduz a área de interesse para melhorar performance
  accumulated_crop <- st_crop(accumulated, mun)
  yearly_crop <- st_crop(yearly, mun)

  # Filtrar e intersectar o desmatamento acumulado de 2007
  accumulated_mun <- st_intersection(accumulated_crop, mun)
  accumulated_area <- sum(accumulated_mun$area_km, na.rm = TRUE)

  # Filtrar incrementos entre 2008 e o ano desejado (inclusive)
  incr_mun <- yearly %>%
    filter(year <= ano) %>%
    st_intersection(mun)
  incr_area <- sum(incr_mun$area_km, na.rm = TRUE)

  # Desmatamento apenas do ano solicitado
  year_mun <- incr_mun %>%
    filter(year == ano)
  year_area <- sum(year_mun$area_km, na.rm = TRUE)

  # Retorno organizado
  return(list(
    municipio = municipio,
    estado = estado,
    ano = ano,
    desmatamento_ano = year_area,
    acumulado_total = accumulated_area + incr_area
  ))
}

teste_prodes(ano = 2022, municipio = "Novo Aripuanã", estado = "Amazonas")
