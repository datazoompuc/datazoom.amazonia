#' IBGE codes and Legal Amazon identification of Brazilian municipalities
#'
#' A dataset containing each municipality's IBGE code, state, mesoregion, microregion, as well as a binary variable for whether it is part of the Legal Amazon. Mostly for our functions' internal use.
#'
#' @format A data frame with 5570 rows and 12 variables:
#' \describe{
#'   \item{code_muni}{IBGE 7-digit municipality code}
#'   \item{name_muni}{municipality name}
#'   \item{code_state}{2-digit state code}
#'   \item{abbrev_state}{state abbreviations (e.g. "AM")}
#'   \item{name_state}{full name of the states}
#'   \item{code_region}{1-digit regional code}
#'   \item{name_region}{name of the region}
#'   \item{legal_amazon}{takes value 1 for municipalities in the legal amazon, 0 otherwise}
#'   \item{municipality_mapbiomas}{municipality name in MAPBIOMAS data}
#'   \item{code_micro}{5-digit microregion code}
#'   \item{name_micro}{name of the microregion}
#'   \item{code_meso}{4-digit mesoregion code}
#'   \item{name_meso}{name of the mesoregion}
#' }
#' @source Package \code{geobr} and \url{https://www.ibge.gov.br/geociencias/cartas-e-mapas/mapas-regionais/15819-amazonia-legal.html?=&t=acesso-ao-produto}
"municipalities"


#' IBGE codes and MAPBIOMAS id of Brazilian municipalities and biomes
#'
#' A dataset containing each municipality-biome's IBGE code, state, biome, name and MAPBIOMAS ID. Mostly for our functions' internal use.
#'
#' @format A data frame with 6537 rows and 4 variables:
#' \describe{
#'   \item{feature_id}{MAPBIOMAS biome-municipality ID}
#'   \item{code_muni}{IBGE 7-digit municipality code}
#'   \item{abbrev_state}{state abbreviations (e.g. "AM")}
#'   \item{municipality_mapbiomas}{municipality name in MAPBIOMAS data}
#'   \item{biome}{biome}
#' }
#' @source Package \code{geobr} and \url{https://mapbiomas.org/}
"municipalities_biomes"
