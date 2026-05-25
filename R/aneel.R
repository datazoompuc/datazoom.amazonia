#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("energy_development_budget", "energy_generation" or "energy_enterprises_distributed")
#' @param year A numeric value or vector of years (2017-2022).
#'   Required for the "energy_development_budget" dataset.
#'   Ignored for the other datasets.
#' @inheritParams load_baci
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated data about energy generation
#' clean_aneel <- load_aneel(
#'   dataset = "energy_generation",
#'   raw_data = FALSE
#' )
#'
#' # download raw annual CDE budget data
#' raw_cde <- load_aneel(
#'   dataset = "energy_development_budget",
#'   year = 2021,
#'   raw_data = TRUE
#' )
#'
#' @export

load_aneel <- function(dataset,
                       raw_data = FALSE,
                       language = "eng",
                       year = NULL) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  ano <- variable <- label <- var_code <- operation_start <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "aneel"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$year <- year

  skip <- NULL

if (param$dataset == "energy_development_budget") {
  if (is.null(param$year) || length(param$year) == 0) {
    stop("For 'energy_development_budget', you must provide 'year'.")
  }

  invalid_years <- setdiff(as.integer(param$year), 2017:2022)

  if (length(invalid_years) > 0) {
    stop(
      "Year(s) not available for 'energy_development_budget': ",
      paste(invalid_years, collapse = ", "),
      ". Valid years: 2017-2022."
    )
  }
}

  if (param$dataset == "energy_generation") {
    skip <- 1
    # skips first row of excel sheet for this dataset
  }

  # check if dataset is valid

  check_params(param)

  #################
  ## Downloading ##
  #################

  if (param$dataset == "energy_development_budget" && length(param$year) > 1) {
    dat <- purrr::map_dfr(param$year, function(y) {
      df_y <- external_download(
        source = param$source,
        dataset = param$dataset,
        year = y,
        skip_rows = skip
      )

      if (!"year" %in% names(df_y)) {
        df_y$year <- y
      }

      df_y
    })
  } else {
    dat <- external_download(
      source = param$source,
      dataset = param$dataset,
      year = param$year,
      skip_rows = skip
    )

    if (param$dataset == "energy_development_budget" &&
        !"year" %in% names(dat) &&
        !"ano" %in% names(dat)) {
      dat$year <- param$year
    }
  }

  if (param$raw_data) {
    return(dat)
  }


  ######################
  ## Data Engineering ##
  ######################

  if (param$dataset == "energy_enterprises_distributed") {
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::mutate_if(is.character, function(var) {
        iconv(var, from = "ISO-8859-1", to = "UTF-8")
      }) %>%
      dplyr::mutate_if(is.character, function(var) {
        stringi::stri_trans_general(str = var, id = "Latin-ASCII")
      }) %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "")),
        dplyr::across(dplyr::starts_with("mda"), .fns = ~ gsub("[.]", "", .x)),
        dplyr::across(dplyr::starts_with("num_coord"), .fns = ~ gsub("[.]", "", .x))
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("mda"), ~ gsub("[,]", ".", .x) %>% as.numeric()),
        dplyr::across(dplyr::starts_with("num_coord"), ~ gsub("[,]", ".", .x) %>% as.numeric())
      ) %>%
      dplyr::mutate(
        sig_modalidade_empreendimento = dplyr::case_when(
          dsc_modalidade_habilitado == "Geracao na propria UC" ~ "Microgera\u00e7\u00e3o ou Minigera\u00e7\u00e3o distribu\u00edda",
          dsc_modalidade_habilitado == "Auto consumo remoto"   ~ "Autoconsumo remoto",
          dsc_modalidade_habilitado == "Compartilhada"         ~ "Gera\u00e7\u00e3o compartilhada",
          dsc_modalidade_habilitado == "Condom\u00ednio"      ~ "Condom\u00ednio",
          TRUE                                                 ~ "Indefinido"
        )
      )
  } else {
    dat <- dat %>%
      janitor::clean_names() %>%
      dplyr::mutate_if(is.character, function(var) {
        stringi::stri_trans_general(str = var, id = "Latin-ASCII")
      })
  }

  # Loading dictionary to recode variable values

  dic <- load_dictionary(param$dataset)

  if (param$language == "pt") {
    dic <- dic %>%
      dplyr::rename("label" = "label_pt")
  }
  if (param$language == "eng") {
    dic <- dic %>%
      dplyr::rename("label" = "label_eng")
  }

  available_vars <- dic %>%
    dplyr::select(variable) %>%
    unlist()

  dat <- names(dat) %>%
    purrr::map_dfc(
      function(var) {
        df <- dat %>%
          dplyr::select(var)

        if (var %in% available_vars) {
          dic <- dic %>%
            dplyr::filter(variable == var)

          var_labels <- dic %>%
            dplyr::select(label) %>%
            unlist()

          var_codes <- dic %>%
            dplyr::select(var_code) %>%
            unlist()

          names(var_labels) <- var_codes

          df <- df %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(var), \(x) dplyr::recode(x, !!!var_labels)))
        }

        return(df)
      }
    )

  # changing operation_start to date format

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(operation_start, \(x) as.Date(x, format = "%d/%m/%Y", origin = "1970-01-01"))
    )

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (param$language == "pt") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "soma_de_valor" = "value",
        "participacao" = "participacao_no_total",
        "municipio_s" = "municipios",
        "potencia_outorgada_k_w" = "potencia_outorgada_kw",
        "potencia_fiscalizada_k_w" = "potencia_fiscalizada_kw",
        "garantia_fisica_k_w" = "garantia_fisica_kw",
        "estado" = "sig_uf"
      )
  }

  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "ano" = "year",
        "tipo_de_despesa" = "type_of_expense",
        "soma_de_valor" = "value",
        "participacao" = "share_of_total",
        "empreendimento" = "venture",
        "uf" = "state",
        "fonte" = "source",
        "fase" = "stage",
        "origem" = "origin",
        "tipo" = "type",
        "tipo_de_atuacao" = "type_of_permission",
        "combustivel_final" = "final_fuel",
        "entrada_em_operacao" = "operation_start",
        "potencia_outorgada_k_w" = "granted_power_kw",
        "potencia_fiscalizada_k_w" = "fiscalized_power_kw",
        "garantia_fisica_k_w" = "physical_guarantee_kw",
        "geracao_qualificada" = "qualified_generation",
        "latitude_decimal" = "latitude_dd",
        "longitude_decimal" = "longitude_dd",
        "inicio_vigencia" = "validity_start",
        "fim_vigencia" = "validity_end",
        "proprietario_regime_de_exploracao" = "owner_or_exploration_regime",
        "sub_bacia" = "sub_basin",
        "municipio_s" = "municipalities",
        "dat_geracao_conjunto_dados" = "generation_date",
        "anm_periodo_referencia" = "reference_period",
        "num_cnpj_distribuidora" = "distributor_cnpj",
        "sig_agente" = "sig_agent",
        "nom_agente" = "agent_name",
        "cod_classe_consumo" = "consumption_class_code",
        "dsc_classe_consumo" = "consumption_class_description",
        "cod_sub_grupo_tarifario" = "tariff_subgroup_code",
        "dsc_sub_grupo_tarifario" = "tariff_subgroup_description",
        "cod_u_fibge" = "ibge_state_code",
        "sig_uf" = "state",
        "cod_regiao" = "region_code",
        "nom_regiao" = "region_name",
        "cod_municipio_ibge" = "ibge_municipality_code",
        "nom_municipio" = "municipality_name",
        "cod_cep" = "zip_code",
        "sig_tipo_consumidor" = "consumer_type",
        "num_cpfcnpj" = "cpf_cnpj_number",
        "nom_titular_empreendimento" = "business_owner_name",
        "cod_empreendimento" = "business_code",
        "dth_atualiza_cadastral_empreend" = "update_date",
        "sig_modalidade_empreendimento" = "business_type",
        "dsc_modalidade_habilitado" = "business_type_description",
        "qtd_uc_recebe_credito" = "receiving_unit_credit_amount",
        "sig_tipo_geracao" = "generation_type",
        "dsc_fonte_geracao" = "generation_source_description",
        "dsc_porte" = "size_description",
        "mda_potencia_instalada_kw" = "installed_power_kw",
        "num_coord_n_empreendimento" = "business_north_coordinate",
        "num_coord_e_empreendimento" = "business_east_coordinate",
        "nom_sub_estacao" = "substation_name",
        "num_coord_e_sub" = "substation_east_coordinate",
        "num_coord_n_sub" = "substation_north_coordinate"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
