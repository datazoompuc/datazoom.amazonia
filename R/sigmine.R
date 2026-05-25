#' @title SIGMINE - Mining Geographic Information System
#'
#' @description Loads information the mines being explored legally in Brazil, including their location, status, product being mined and area in square meters.
#'
#' @param dataset A dataset name ("sigmine_active")
#' @inheritParams load_baci
#'
#' @return A \code{tibble}.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' @examples
#' # Example 1: Active Mining in Amazon Region
#' \dontrun{
#' library(dplyr)
#' mining <- load_sigmine(dataset = "sigmine_active", raw_data = FALSE, language = "eng")
#' amazon_states <- c("AC", "AM", "AP", "MA", "MT", "MS", "PA", "RO", "RR", "TO")
#' amazon_mining <- mining %>%
#'   filter(state %in% amazon_states) %>%
#'   select(mine_name, state, municipality, mineral_type, area_m2, status)
#' print("Active mining operations in the Amazon:")
#' print(amazon_mining)
#' print(paste("Total mining operations in Amazon:", nrow(amazon_mining)))
#' }
#'
#' # Example 2: Mineral Types and Distribution
#' \dontrun{
#' library(dplyr)
#' mining <- load_sigmine(dataset = "sigmine_active", raw_data = FALSE, language = "eng")
#' mineral_distribution <- mining %>%
#'   group_by(mineral_type) %>%
#'   summarize(num_operations = n(),
#'             total_area_hectares = sum(area_m2, na.rm = TRUE) / 10000,
#'             num_states = n_distinct(state), .groups = 'drop') %>%
#'   arrange(desc(num_operations))
#' print("Mining operations by mineral type:")
#' print(mineral_distribution)
#' precious_metals <- mining %>%
#'   filter(grepl("gold|silver|diamond", mineral_type, ignore.case = TRUE)) %>%
#'   group_by(state) %>%
#'   summarize(num_operations = n(),
#'             total_area = sum(area_m2, na.rm = TRUE) / 10000, .groups = 'drop') %>%
#'   arrange(desc(num_operations))
#' print("Precious metal mining by state:")
#' print(precious_metals)
#' }
#'
#' # Example 3: Largest Mining Operations
#' \dontrun{
#' library(dplyr)
#' mining <- load_sigmine(dataset = "sigmine_active", raw_data = FALSE, language = "eng")
#' largest_mines <- mining %>%
#'   mutate(area_hectares = area_m2 / 10000) %>%
#'   arrange(desc(area_hectares)) %>%
#'   select(mine_name, state, municipality, mineral_type, area_hectares, status) %>%
#'   head(30)
#' print("Largest mining operations in Brazil:")
#' print(largest_mines)
#' state_mining_area <- mining %>%
#'   mutate(area_hectares = area_m2 / 10000) %>%
#'   group_by(state) %>%
#'   summarize(total_mining_area = sum(area_hectares, na.rm = TRUE),
#'             num_operations = n(),
#'             avg_operation_size = mean(area_hectares, na.rm = TRUE), .groups = 'drop') %>%
#'   arrange(desc(total_mining_area))
#' print("Mining footprint by state:")
#' print(state_mining_area)
#' }
#'
#' #' @export
#'
load_sigmine <- function(dataset = "sigmine_active",
                         raw_data = FALSE,
                         language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- nome <- uf <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "sigmine"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset is supported

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  dat <- external_download(dataset = param$dataset, source = param$source) %>%
    janitor::clean_names()

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  a <- dat %>%
    dplyr::mutate(dplyr::across(
      nome:uf,
      ~ ifelse(stringr::str_detect(.x, "DADO N.O CADASTRADO"),
        NA, .x
      )
    ))

  if (language == "pt") {
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "ult_evento"] <- "ultimo_evento"
    names(a)[names(a) == "nome"] <- "empresa"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "uso"
    names(a)[names(a) == "area_ha"] <- "area_m2"
  } else if (language == "eng") {
    a$area_ha <- a$area_ha * 10000
    names(a)[names(a) == "numero"] <- "number"
    names(a)[names(a) == "ult_evento"] <- "last_event"
    names(a)[names(a) == "uf"] <- "state"
    names(a)[names(a) == "ano"] <- "year"
    names(a)[names(a) == "processo"] <- "process"
    names(a)[names(a) == "id"] <- "id"
    names(a)[names(a) == "fase"] <- "phase"
    names(a)[names(a) == "nome"] <- "company"
    names(a)[names(a) == "subs"] <- "mineral"
    names(a)[names(a) == "uso"] <- "use"
    names(a)[names(a) == "area_ha"] <- "area_m2"
  }

  return(a)
}
