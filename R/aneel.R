#' @title ANEEL
#'
#' @description National Electric Energy Agency - ANEEL
#'
#' @param dataset A dataset name ("energy_development_budget", "energy_generation" or "energy_enterprises_distributed")
#' @param year A numeric value or vector of years (2017-2024; tracks the manifest's
#'   \code{available_time} for "energy_development_budget" -- see
#'   \code{inst/extdata/manifest/v1/datasets_link.csv} -- and widens automatically as ANEEL
#'   publishes new years, so re-check this range here if it goes stale again).
#'   Required for the "energy_development_budget" dataset.
#'   Ignored for the other datasets.
#' @inheritParams load_baci
#'
#' @return A \code{data.frame}: the raw source data when \code{raw_data = TRUE}, or a treated \code{tibble} when \code{raw_data = FALSE}.
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

  ano <- variable <- label <- var_code <- NULL

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

  # valid years come from the manifest's available_time -- one row per year
  # (see inst/extdata/manifest/v1/datasets_link.csv) -- instead of a
  # hardcoded range that must be edited in R code whenever ANEEL adds a year.
  # available_time is the same across all of this dataset's year rows (a
  # dataset-wide fact, not a per-row one), so this is the one legitimate
  # cross-row read -- dataset_meta(), not dataset_field() with no key (see
  # R/manifest.R).
  available <- dataset_meta(param$source, param$dataset, "available_time")
  valid_years <- parse_years(available)

  invalid_years <- setdiff(as.integer(param$year), valid_years)

  if (length(invalid_years) > 0) {
    stop(
      "Year(s) not available for 'energy_development_budget': ",
      paste(invalid_years, collapse = ", "),
      ". Valid years: ", available, "."
    )
  }
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

  # All three aneel datasets are now read with fread(encoding = "UTF-8")
  # (see R/download.R's aneel branch) -- verified live 2026-09-21 by
  # inspecting each source file's raw bytes. No iconv() re-decoding is
  # needed for any of them; an ISO-8859-1 -> UTF-8 iconv() step used to run
  # here for energy_enterprises_distributed/energy_development_budget on
  # the assumption those files were Latin-1, which was wrong (their real
  # bytes are UTF-8) and produced mojibake like "Ã­" for "í" -- removed
  # along with switching those two fread() calls to encoding = "UTF-8".
  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  if (param$dataset == "energy_enterprises_distributed") {
    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))
      ) %>%
      # FRAGILE: exact string match on dsc_modalidade_habilitado categories.
      # If ANEEL alters or introduces new modality labels, case_when() silently
      # assigns 'Indefinido' to all unmapped rows.
      dplyr::mutate(
        sig_modalidade_empreendimento = dplyr::case_when(
          dsc_modalidade_habilitado == "Geracao na propria UC" ~ "Microgera\u00e7\u00e3o ou Minigera\u00e7\u00e3o distribu\u00edda",
          dsc_modalidade_habilitado == "Auto consumo remoto"   ~ "Autoconsumo remoto",
          dsc_modalidade_habilitado == "Compartilhada"         ~ "Gera\u00e7\u00e3o compartilhada",
          dsc_modalidade_habilitado == "Condom\u00ednio"      ~ "Condom\u00ednio",
          TRUE                                                 ~ "Indefinido"
        )
      )
  }

  if (param$dataset %in% c("energy_enterprises_distributed", "energy_generation", "energy_development_budget")) {
    # FRAGILE: assumes numeric coordinate/power/value columns (mda_*/
    # num_coord_*/vlr_*) use Brazilian thousand separators (.) and decimal
    # commas (,) to strip/convert. If ANEEL changes formatting or exports
    # clean numerics directly, gsub() may alter valid values. Guarded on
    # is.character() because fread() can already auto-parse a comma-free
    # column (e.g. energy_generation's mda_potencia_fiscalizada_kw) as
    # numeric on its own -- gsub()ing a numeric column would coerce it
    # through as.character() unnecessarily.
    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with(c("mda", "num_coord", "vlr")) & dplyr::where(is.character), .fns = ~ gsub("[.]", "", .x))
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with(c("mda", "num_coord", "vlr")) & dplyr::where(is.character), ~ gsub("[,]", ".", .x) %>% as.numeric())
      )
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

  # Dead code, removed: this used to across(operation_start, as.Date(...,
  # format = "%d/%m/%Y")) here, before any rename runs -- but no column is
  # ever literally named "operation_start" pre-rename in any of the three
  # aneel datasets (that's the ENG rename target, see below), so
  # across(operation_start, ...) always matched zero columns and this step
  # was a permanent no-op. Verified live 2026-09-21: energy_generation's
  # real date column (dat_entrada_operacao) is already IDate/Date straight
  # out of fread() -- data.table auto-detects its ISO YYYY-MM-DD values --
  # so no manual as.Date() conversion is needed for it anyway, and the
  # "%d/%m/%Y" format this block assumed was also wrong for that column.

  ################################
  ## Harmonizing Variable Names ##
  ################################

  # FRAGILE: exact upstream column names expected in Portuguese harmonization.
  # If ANEEL renames any of these columns, recode silently leaves them untouched.
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

  # FRAGILE: exact upstream column names expected in English harmonization.
  # If ANEEL changes database attribute names, recode silently leaves columns in Portuguese.
  #
  # Several keys below used to be short/generic names ("fonte", "fase",
  # "origem", "tipo", "tipo_de_atuacao", "combustivel_final",
  # "geracao_qualificada", "inicio_vigencia", "fim_vigencia",
  # "proprietario_regime_de_exploracao", "sub_bacia", "municipio_s",
  # "entrada_em_operacao") that never matched any real column name after
  # janitor::clean_names() -- same root cause, and largely the same wrong
  # names, as R/dictionary.R's energy_generation ~variable mismatch fixed
  # 2026-09-21 (see NEWS.md). Fixed here by pointing each key at the real
  # column, keeping the original target name where one was already chosen.
  # "fonte" = "source" had no real column to reassign to (sig_tipo_geracao
  # is already "generation_type" below) and is left as dead weight.
  # "tipo_de_despesa" = "type_of_expense" used to be here too, for the same
  # reason -- removed 2026-09-21 alongside R/dictionary.R's matching
  # ~variable rows once it was confirmed (live, and against a locally saved
  # copy of the current resource) that energy_development_budget's real
  # data has no expense-by-category dimension at all; see NEWS.md.
  if (param$language == "eng") {
    dat_mod <- dat %>%
      dplyr::rename_with(dplyr::recode,
        "ano" = "year",
        "soma_de_valor" = "value",
        "participacao" = "share_of_total",
        "empreendimento" = "venture",
        "uf" = "state",
        "fonte" = "source",
        "dsc_fase_usina" = "stage",
        "dsc_origem_combustivel" = "origin",
        "dsc_fonte_combustivel" = "type",
        "dsc_tipo_outorga" = "type_of_permission",
        "nom_fonte_combustivel" = "final_fuel",
        "dat_entrada_operacao" = "operation_start",
        "potencia_outorgada_k_w" = "granted_power_kw",
        "potencia_fiscalizada_k_w" = "fiscalized_power_kw",
        "garantia_fisica_k_w" = "physical_guarantee_kw",
        "mda_potencia_outorgada_kw" = "granted_power_kw",
        "mda_potencia_fiscalizada_kw" = "fiscalized_power_kw",
        "mda_garantia_fisica_kw" = "physical_guarantee_kw",
        "idc_geracao_qualificada" = "qualified_generation",
        "latitude_decimal" = "latitude_dd",
        "longitude_decimal" = "longitude_dd",
        "dat_inicio_vigencia" = "validity_start",
        "dat_fim_vigencia" = "validity_end",
        "dsc_propri_regime_pariticipacao" = "owner_or_exploration_regime",
        "dsc_sub_bacia" = "sub_basin",
        "dsc_muninicpios" = "municipalities",
        "dat_geracao_conjunto_dados" = "generation_date",
        "anm_periodo_referencia" = "reference_period",
        "num_cnpj_distribuidora" = "distributor_cnpj",
        "sig_agente" = "sig_agent",
        "nom_agente" = "agent_name",
        "nom_empreendimento" = "venture_name",
        "ide_nucleo_ceg" = "ceg_core_id",
        "cod_ceg" = "ceg_code",
        "sig_uf_principal" = "main_state",
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
        "num_ano" = "reference_year",
        "nom_mes" = "reference_month",
        "num_cnpj" = "agent_cnpj",
        "dsc_grupo_tarifario" = "tariff_group_description",
        "idc_classe_consumidor" = "consumer_class_indicator",
        "idc_tipo" = "type_indicator",
        "vlr_desconto" = "discount_value",
        "vlr_cobranca" = "charge_value",
        "num_coord_e_sub" = "substation_east_coordinate",
        "num_coord_n_sub" = "substation_north_coordinate"
      )
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
