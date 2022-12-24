#' @title EPE
#'
#' @description Electrical Energy Monthly Consumption per Class
#'
#' @param dataset A dataset name, ("energy_consumption_per_class") or ("national_energy_balance")
#' @param geo_level A geographical level, ("state") or ("subsystem"), only available for "energy_consumption_per_class"
#' @inheritParams load_baci
#'
#' @examples
#' \dontrun{
#' # download treated data about energy consumption at the state level
#' clean_epe <- load_epe(
#'   dataset = "energy_consumption_per_class",
#'   geo_level = "state",
#'   raw_data = FALSE
#' )
#' }
#'
#' @export

load_epe <- function(dataset, raw_data = FALSE,
                     geo_level = "state", language = "eng") {
  ###########################
  ## Bind Global Variables ##
  ###########################

  Sistema <- ano <- Total <- Quantidade <- available_time <- NULL
  mes <- uf <- geo <- region_subsystem <- v1 <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$geo_level <- geo_level

  #################
  ## Downloading ##
  #################

  # Choosing which sheets to read

  if (param$geo_level == "state") {
    sheets_selected <- c(
      "CONSUMO POR UF",
      "CONSUMO CATIVO POR UF",
      "CONSUMO RESIDENCIAL POR UF",
      "CONSUMO INDUSTRIAL POR UF",
      "CONSUMO COMERCIAL POR UF",
      "CONSUMO OUTROS POR UF",
      "CONSUMIDORES RESIDENCIAIS POR F", # Ha um erro de escrita na planilha de origem: 'F' ao invés de 'UF'
      "CONSUMIDORES INDUSTRIAIS POR UF",
      "CONSUMIDORES COMERCIAIS POR UF",
      "CONSUMIDORES OUTROS POR UF"
    )
  }
  if (param$geo_level == "region" | param$geo_level == "subsystem") {
    sheets_selected <- c(
      "TOTAL",
      "RESIDENCIAL",
      "INDUSTRIAL",
      "COMERCIAL",
      "OUTROS",
      "CATIVO",
      "CONSUMIDORES RESIDENCIAIS",
      "CONSUMIDORES TOTAIS"
    )
  }

  dat <- external_download(
    source = "EPE",
    dataset = param$dataset,
    year = param$time_period,
    sheet = sheets_selected
  )

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "energy_consumption_per_class") {

    # removing accents from each sheet

    dat <- dat %>%
      purrr::map(
        ~ dplyr::mutate_if(., is.character, function(var) {
          stringi::stri_trans_general(str = var, id = "Latin-ASCII")
        })
      )

    if (geo_level == "state") {

      # removes first three rows of each data frame + last row

      dat <- dat %>%
        purrr::map(dplyr::slice_tail, n = -3) %>%
        purrr::map(dplyr::slice_head, n = -1)

      # turns each sheet sideways, easier to work with

      dat <- dat %>%
        purrr::map(t) # applies t() function for transpose

      base::suppressMessages(
        dat <- dat %>%
          purrr::map(tibble::as_tibble, .name_repair = "unique")
      )

      # creates list with column names (initially from first row)

      var_names <- dat %>%
        purrr::map(dplyr::slice_head, n = 1) %>%
        purrr::map(as.character)

      # first and second are Year and Month respectively

      var_names <- var_names %>%
        purrr::map(
          function(names) {
            names[1] <- "ano"
            names[2] <- "mes"

            names
          }
        )

      # removing first row and turning into column names

      dat <- dat %>%
        purrr::map(dplyr::slice_tail, n = -1) %>%
        purrr::map2(var_names, stats::setNames)

      # extending year variable for all the missing rows

      dat <- dat %>%
        purrr::map(tidyr::fill, ano, .direction = "down")

      # shifting into long format

      dat <- dat %>%
        purrr::imap(
          ~ tidyr::pivot_longer(.x, -c("ano", "mes"), names_to = "uf", values_to = .y)
        )

      # merging everything into one dataframe

      dat <- dat %>%
        purrr::reduce(dplyr::full_join, by = c("ano", "mes", "uf"))

      # general data cleaning

      month_numbers <- c(
        "JAN" = 1,
        "FEV" = 2,
        "MAR" = 3,
        "ABR" = 4,
        "MAI" = 5,
        "JUN" = 6,
        "JUL" = 7,
        "AGO" = 8,
        "SET" = 9,
        "OUT" = 10,
        "NOV" = 11,
        "DEZ" = 12
      )

      dat <- dat %>%
        dplyr::mutate(dplyr::across(ano, stringr::str_remove, "\\*"))

      dat <- dat %>%
        janitor::clean_names() %>%
        dplyr::mutate(dplyr::across(mes, dplyr::recode, !!!month_numbers)) %>%
        dplyr::mutate(dplyr::across(-uf, as.numeric))

      dat <- dat %>%
        dplyr::mutate(uf = dplyr::case_when(
          uf == "TOTAL POR UF" ~ "Total",
          TRUE ~ uf
        ))
    }

    if (param$geo_level %in% c("subsystem", "region")) {

      # removes first 3 rows of each sheet

      dat <- dat %>%
        purrr::map(dplyr::slice_tail, n = -3)

      # if sheet has an extra column, remove last useless one

      dat <- dat %>%
        purrr::map(
          function(df) {
            if (ncol(df) == 14) {
              df <- df[, -14]
            }

            df
          }
        )

      # creating year variable from second column

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., ano = .[[2]])
        )

      # removing annoying asterisk

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., dplyr::across(ano, stringr::str_remove, "\\*"))
        )

      # keeping only actual years in year variable

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., ano = dplyr::case_when(
            ano %in% as.character(2000:2030) ~ ano
          ))
        )

      # extending year to missing values

      dat <- dat %>%
        purrr::map(tidyr::fill, ano, .direction = "down")

      # now creating a variable to identify regions vs. subsystems

      # creating geo variable from first column

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., geo = .[[1]])
        )

      # it can only take two values: REGIAO GEOGRAFICA or SUBSISTEMA ELETRICO

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., geo = dplyr::case_when(
            geo %in% c("REGIAO GEOGRAFICA", "SUBSISTEMA ELETRICO") ~ geo,
            geo == "SUBSISTEMA" ~ "SUBSISTEMA ELETRICO" # sometimes this alt name is used
          ))
        )

      # extending to missing values

      dat <- dat %>%
        purrr::map(tidyr::fill, geo, .direction = "down")

      # making region/subsystem variable

      region_system_list <- c(
        "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste", "Sudeste/C.Oeste", "Sistemas Isolados"
      )

      dat <- dat %>%
        purrr::map(
          ~ dplyr::mutate(., region_subsystem = dplyr::case_when(
            .[[1]] %in% region_system_list ~ .[[1]]
          ))
        )

      dat <- dat %>%
        purrr::map(dplyr::select, -1) %>% # remove first column
        purrr::map(tidyr::drop_na, region_subsystem) # remove rows with missing regions

      # column names

      var_names <- c(1:12, "ano", "geo", "region_subsystem") # twelve months and the three artificial variables

      # removing first row and setting column names

      dat <- dat %>%
        purrr::map(dplyr::slice_tail, n = -1) %>%
        purrr::map(stats::setNames, var_names)


      # shifting into long format

      dat <- dat %>%
        purrr::imap(
          ~ tidyr::pivot_longer(.x, -c("ano", "geo", "region_subsystem"), names_to = "mes", values_to = .y)
        )

      # merging everything into one dataframe

      dat <- dat %>%
        purrr::reduce(dplyr::full_join, by = c("geo", "ano", "mes", "region_subsystem"))

      dat <- dat %>%
        janitor::clean_names() %>%
        dplyr::mutate(dplyr::across(-c("geo", "ano", "region_subsystem"), as.numeric))

      # Finally picking geo_level
      if (param$geo_level == "subsystem") {
        dat <- dat %>%
          dplyr::filter(geo == "SUBSISTEMA ELETRICO") %>%
          dplyr::rename("subsistema" = "region_subsystem")
      }
      if (param$geo_level == "region") {
        dat <- dat %>%
          dplyr::filter(geo == "REGIAO GEOGRAFICA") %>%
          dplyr::rename("regiao" = "region_subsystem")
      }

      dat <- dat %>%
        dplyr::select(-geo)
    }
  }

  if (param$dataset == "national_energy_balance") {
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::select(-v1)
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat <- dat %>%
    dplyr::rename_with(
      function(name) {
        name %>%
          stringr::str_remove("_por_uf") %>%
          stringr::str_remove("_por_f") %>%
          stringr::str_remove("consumo_")
      }
    )

  names(dat) %>%
    stringr::str_remove("_por_uf") %>%
    stringr::str_remove("_por_f")

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "total" = "consumo_total",
        "residencial" = "consumo_residencial",
        "industrial" = "consumo_industrial",
        "comercial" = "consumo_comercial",
        "outros" = "consumo_outros",
        "cativo" = "consumo_cativo",
        "consumo" = "consumo_total"
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "estado" = "state",
        "amz_legal" = "legal_amazon",
        "ano" = "year",
        "hidro" = "hydro",
        "eolica" = "wind",
        "termo" = "thermal",
        "cana" = "sugar_cane_bagasse",
        "lenha" = "firewood",
        "lixivia" = "black_liquor",
        "outras_fontes_renovaveis" = "other_renewable_sources",
        "carvao_vapor" = "steam_coal",
        "gas_natural" = "natural_gas",
        "gas_de_coqueira" = "coke_oven_gas",
        "combustivel" = "fuel_oil",
        "diesel" = "diesel",
        "outras_fontes_nao_renovaveis" = "other_non_renewable_sources",
        "regiao" = "region",
        "subsistema" = "subsystem",
        "mes" = "month",
        "total" = "total_consumption",
        "residencial" = "residential_consumption",
        "industrial" = "industrial_consumption",
        "comercial" = "comercial_consumption",
        "outros" = "other_consumption",
        "cativo" = "captive_consumption",
        "consumidores_residenciais" = "residential_consumers",
        "consumidores_industriais" = "industrial_consumers",
        "consumidores_totais" = "total_consumers",
        "consumidores_comerciais" = "commercial_consumers",
        "consumidores_outros" = "other_consumers",
        "consumo" = "total_consumption"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
