#' @title IBAMA - Brazilian Institute for the Environment and Renewable Natural Resources
#'
#' @description Loads IBAMA environmental embargo polygons and fine records. Treated embargo polygons can be associated with every municipality they spatially intersect.
#'
#' @param dataset A dataset name ("embargoed_areas", "distributed_fines", or "collected_fines")
#' @inheritParams load_baci
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM"). Does not apply to the "embargoed_areas" dataset.
#' @param language A \code{string} that indicates in which language the data will be returned. Currently, only Portuguese ("pt") and English ("eng") are supported.
#'
#' @param geo_level For \code{dataset = "embargoed_areas"}, use \code{"municipality"} to assign each embargo polygon to every municipality it intersects. Other datasets are already tabular and ignore this option.
#'
#' @return A \code{tibble}. For treated \code{"embargoed_areas"} data, there is one row per embargo-municipality pair with positive-area overlap. The source area value is repeated for each municipality and is not additive; \code{intersection_area_m2} is the geometric area inside that municipality.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated embargoed areas data in english
#' data <- load_ibama(
#'   dataset = "embargoed_areas",
#'   raw_data = FALSE,
#'   language = "eng"
#' )
#'
#' # download treated collected fines data from Bahia
#' data <- load_ibama(
#'   dataset = "collected_fines",
#'   raw_data = FALSE,
#'   states = "BA",
#'   language = "pt"
#' )
#'
#' @export

load_ibama <- function(dataset,
                       raw_data = FALSE,
                       states = "all",
                       language = "eng",
                       geo_level = "municipality") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  survey <- link <- legal_amazon <- codigo_ibge_municipio_embargo <- NULL
  municipio_embargo <- uf_embargo <- julgamento <- infracao <- data_de_insercao_na_lista <- NULL
  cpf_ou_cnpj <- cod_municipio <- ano <- mes <- month <- year <- municipality_code <- NULL
  n_infracoes <- n_ja_julgado <- n_cpf_cnpj_unicos <- NULL
  municipio_infracao <- uf_infracao <- uf <- NULL
  municipio <- name_muni <- code_muni <- municipality <- NULL
  data_auto <- abbrev_state <- ultima_atualizacao_relatorio <- tipo_auto <- valor_do_auto <- NULL
  acao_fiscalizatoria <- amazonia_legal <- cod_tipo_bioma <- cod_uf_tad <- cpf_cnpj_embargado <- NULL
  dat_embargo <- dat_impressao <- dat_ult_alter_geom <- data_embargo <- data_impressao <- NULL
  data_ult_alter_geom <- des_infracao <- des_localizacao <- des_locali <- des_tad <- hora_embargo <- NULL
  hora_impressao <- hora_ult_alter_geom <- nome_pessoa_embargada <- nome_embar <- cpf_cnpj_e <- NULL
  num_auto_infracao <- num_auto_i <- serie_auto <- ser_auto_infracao <- ser_tad <- NULL
  num_processo <- num_proces <- ordem_fiscalizacao <- ordem_fisc <- cd_acao_fi <- NULL
  qtd_area_desmatada <- qtd_area_d <- qtd_area_embargada <- qtd_area_e <- NULL
  sit_desmatamento <- sit_desmat <- state <- location_description <- NULL
  tad_description <- embargo_date <- embargo_time <- print_date <- print_time <- NULL
  last_geom_update_date <- last_geom_update_time <- infringement_number <- infringement_series <- NULL
  deforested_area_amount <- embargoed_area_amount <- biome_type_code <- fiscal_action <- fiscal_order <- NULL
  infringement_description <- longitude <- latitude <- NULL
  dat_embarg <- dat_impres <- dat_ult_al <- cod_tipo_b <- des_infrac <- num_longit <- num_latitu <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "ibama"
  param$dataset <- dataset
  param$language <- language
  param$raw_data <- raw_data
  param$states <- states
  param$geo_level <- geo_level

  # check if dataset is valid

  check_params(param)

  if (any(states == "all")) {
    param$states <- c(
      "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN",
      "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC",
      "RS", "MS", "MT", "GO", "DF"
    )
  }
  if (dataset == "embargoed_areas") {
    param$states <- "all"
  }

  ##############
  ## Download ##
  ##############

  # Downloads each state separately

  if (dataset == "embargoed_areas") {
    # The embargo shapefile is a single national archive, not one archive per
    # state. Download it once so the same national records are not repeated
    # 27 times before the municipal overlay.
    dat <- list(all = external_download(
      source = param$source,
      dataset = param$dataset,
      state = "all"
    ))
  } else {
    dat <- param$states %>%
      purrr::imap(
        function(uf, iteration) {
          base::message(paste0("Downloading ", uf, " (", iteration, " out of ", length(param$states), ")"))

          external_download(
            source = param$source,
            dataset = param$dataset,
            state = uf
          )
        }
      )
    names(dat) <- param$states
  }

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  if (dataset %in% c("collected_fines", "distributed_fines")) {
    dat <- dat %>%
      purrr::map(
        ~ dplyr::mutate(
          .x,
          dplyr::across(dplyr::starts_with("valor"), as.character)
        )
      )
  }

  if (dataset == "embargoed_areas") {
    dat <- dat[[1]] %>%
      janitor::clean_names()
  } else {
    dat <- dat %>%
      dplyr::bind_rows() %>%
      janitor::clean_names()
  }

  # data come without municipality codes, only their names. +so we turn everything lowercase and remove accents to try and match.
  dat <- dat %>%
    dplyr::mutate(
      name_muni = municipio,
      # removing accents from municipality names
      dplyr::across(municipio, ~ stringi::stri_trans_general(., id = "Latin-ASCII"))
    ) %>%
    dplyr::mutate(dplyr::across(municipio, tolower)) # making all municipality names lowercase

  geo <- datazoom.amazonia::municipalities %>%
    dplyr::select(
      code_muni,
      "municipio" = name_muni,
      "uf" = abbrev_state,
      legal_amazon
    )

  # 41 municipalities don't match due to spelling inconsistencies
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "sao luiz do anuaa" & uf == "RR" ~ "sao luiz",
        municipio == "eldorado dos carajas" & uf == "PA" ~ "eldorado do carajas",
        municipio == "santa isabel do para" & uf == "PA" ~ "santa izabel do para",
        municipio == "couto de magalhaes" & uf == "TO" ~ "couto magalhaes",
        municipio == "fortaleza do tabocao" & uf == "TO" ~ "tabocao",
        municipio == "pau d arco" & uf == "TO" ~ "pau d'arco",
        municipio == "sao valerio da natividade" & uf == "TO" ~ "sao valerio",
        municipio == "governador edson lobao" & uf == "MA" ~ "governador edison lobao",
        municipio == "assu" & uf == "RN" ~ "acu",
        municipio == "presidente juscelino" & uf == "RN" ~ "serra caiada",
        municipio == "olho-d'agua do borges" & uf == "RN" ~ "olho d'agua do borges",
        municipio == "augusto severo" & uf == "RN" ~ "campo grande",
        municipio == "campo de santana" & uf == "PB" ~ "tacima",
        municipio == "santarem" & uf == "PB" ~ "joca claudino",
        municipio == "sao domingos de pombal" & uf == "PB" ~ "sao domingos",
        municipio == "lagoa do itaenga" & uf == "PE" ~ "lagoa de itaenga",
        municipio == "iguaraci" & uf == "PE" ~ "iguaracy",
        municipio == "belem de sao francisco" & uf == "PE" ~ "belem do sao francisco",
        municipio == "amparo de sao francisco" & uf == "SE" ~ "amparo do sao francisco",
        municipio == "santa teresinha" & uf == "BA" ~ "santa terezinha",
        municipio == "muquem de sao francisco" & uf == "BA" ~ "muquem do sao francisco",
        municipio == "brasopolis" & uf == "MG" ~ "brazopolis",
        municipio == "passa-vinte" & uf == "MG" ~ "passa vinte",
        municipio == "sao thome das letras" & uf == "MG" ~ "sao tome das letras",
        municipio == "dona eusebia" & uf == "MG" ~ "dona euzebia",
        municipio == "justinopolis" & uf == "MG" ~ "ribeirao das neves",
        municipio == "trajano de morais" & uf == "RJ" ~ "trajano de moraes",
        municipio == "embu" & uf == "SP" ~ "embu das artes",
        municipio == "moji mirim" & uf == "SP" ~ "mogi mirim",
        municipio == "florinia" & uf == "SP" ~ "florinea",
        municipio == "biritiba-mirim" & uf == "SP" ~ "biritiba mirim",
        municipio == "sao luis do paraitinga" & uf == "SP" ~ "sao luiz do paraitinga",
        municipio == "grao para" & uf == "SC" ~ "grao-para",
        municipio == "presidente castelo branco" & uf == "SC" ~ "presidente castello branco",
        municipio == "passos de torres" & uf == "SC" ~ "passo de torres",
        municipio == "santana do livramento" & uf == "RS" ~ "sant'ana do livramento",
        municipio == "poxoreo" & uf == "MT" ~ "poxoreu",
        municipio == "colinas de goiais" & uf == "GO" ~ "colinas do sul",
        TRUE ~ municipio
      )
    )

  dat <- dat %>%
    dplyr::mutate(uf = dplyr::case_when(
      municipio == "ponte alta do norte" & uf == "GO" ~ "SC",
      TRUE ~ uf
    ))

  # Merging with IBGE municipalities
  dat <- dat %>%
    dplyr::left_join(geo, by = c("municipio", "uf"))


  if (dataset %in% c("collected_fines", "distributed_fines")) {
    # Changing dates to date format
    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("data_auto", "data_pagamento")), \(x) as.Date(x, format = "%d/%m/%Y"))
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "")),
        dplyr::across(ultima_atualizacao_relatorio, \(x) as.POSIXct(x, format = "%d/%m/%Y %H:%M"))
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("valor"), .fns = ~ gsub("[.]", "", as.character(.x)))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("valor"), ~ gsub("[,]", ".", as.character(.x)) %>% as.numeric()))
  }

  if (dataset == "embargoed_areas") {
    ## Minor corrections
    dat <- dat %>%
      tidyr::separate(col = dat_embarg, into = c("data_embargo", "hora_embargo"), sep = " ") %>%
      tidyr::separate(col = dat_impres, into = c("data_impressao", "hora_impressao"), sep = " ") %>%
      tidyr::separate(col = dat_ult_al, into = c("data_ult_alter_geom", "hora_ult_alter_geom"), sep = " ") %>%
      suppressWarnings() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "")),
        data_embargo = as.Date(data_embargo, "%d/%m/%y"),
        data_impressao = as.Date(data_impressao, "%d/%m/%y"),
        data_ult_alter_geom = as.Date(data_ult_alter_geom, "%d/%m/%y"),
        dplyr::across(dplyr::starts_with("qtd"), ~ gsub("[,]", ".", .x) %>% as.numeric())
      )

    municipalities <- external_download(
      source = "internal",
      dataset = "geo_municipalities"
    ) %>%
      dplyr::left_join(
        datazoom.amazonia::municipalities %>%
          dplyr::select(code_muni, legal_amazon),
        by = "code_muni"
      )
    dat <- ibama_embargo_municipality_intersection(dat, municipalities)
    dat$uf <- dat$abbrev_state
    dat$abbrev_state <- NULL
  }

  dat <- dat %>%
    tibble::as_tibble()

  ################################
  ## Harmonizing Variable Names ##
  ################################

  if (dataset == "embargoed_areas") {
    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::select(-municipio, -dplyr::any_of("cod_uf_tad")) %>%
        dplyr::rename(
          municipio      = name_muni,
          cod_municipio  = code_muni,
          amazonia_legal = legal_amazon,
          area_intersecao_m2 = intersection_area_m2,
          numero_poligonais_origem = source_polygon_count
        ) %>%
        dplyr::relocate(c(municipio, cod_municipio, amazonia_legal), .before = uf)
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::select(-municipio, -dplyr::any_of("cod_uf_tad")) %>%
        dplyr::rename(
          municipality              = name_muni,
          municipality_code         = code_muni,
          state                     = uf,
          location_description      = des_locali,
          embargoed_person_name     = nome_embar,
          embargoed_cpf_cnpj        = cpf_cnpj_e,
          process_number            = num_proces,
          tad_description           = des_tad,
          embargo_date              = data_embargo,
          embargo_time              = hora_embargo,
          print_date                = data_impressao,
          print_time                = hora_impressao,
          last_geom_update_date     = data_ult_alter_geom,
          last_geom_update_time     = hora_ult_alter_geom,
          infringement_number       = num_auto_i,
          infringement_series       = serie_auto,
          deforested_area_amount    = qtd_area_d,
          embargoed_area_amount     = qtd_area_e,
          biome_type_code           = cod_tipo_b,
          fiscal_action             = cd_acao_fi,
          fiscal_order              = ordem_fisc,
          infringement_description  = des_infrac,
          longitude                 = num_longit,
          latitude                  = num_latitu
        ) %>%
        dplyr::relocate(c(municipality, municipality_code, legal_amazon), .before = state)
    }
  }

  if (dataset %in% c("collected_fines", "distributed_fines")) {
    dat <- dat %>%
      dplyr::select(-municipio) %>%
      dplyr::relocate(code_muni, name_muni, uf, data_auto)

    if (param$language == "pt") {
      dat_mod <- dat %>%
        dplyr::rename(
          "cod_municipio" = name_muni,
          "municipio" = name_muni,
          "penalidade" = tipo_auto,
          "data_penalidade" = data_auto,
          "valor_penalidade" = valor_do_auto,
          "amazonia_legal" = legal_amazon
        )
    }

    if (param$language == "eng") {
      dat_mod <- dat %>%
        dplyr::rename_with(dplyr::recode,
          code_muni = "municipality_code",
          name_muni = "municipality",
          uf = "state",
          data_auto = "penalty_date",
          no_ai = "report_number",
          nome_ou_razao_social = "name_or_corporate_name",
          status_debito = "debt_status",
          tipo_auto = "penalty",
          tipo_infracao = "infraction",
          enquadramento_legal = "legal_framework",
          valor_do_auto = "penalty_value",
          moeda = "currency",
          parcela = "installment",
          quantidade_de_parcelas = "number_of_installments",
          valor_base_da_parcela = "base_installment_value",
          valor_pago = "value_paid",
          data_pagamento = "payment_date",
          ultima_atualizacao_relatorio = "report_last_update"
        )
    }
  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}

# Associate each IBAMA embargo term with the municipalities its polygon
# actually reaches. seq_tad is the source's primary key; a row key is used
# only when that field is absent or missing.
ibama_embargo_municipality_intersection <- function(embargoes, municipalities) {
  if (!inherits(embargoes, "sf")) {
    stop("The IBAMA embargo dataset must contain polygon geometries.", call. = FALSE)
  }
  if (is.na(sf::st_crs(embargoes)) || is.na(sf::st_crs(municipalities))) {
    stop("Both IBAMA and municipal geometries must have a defined CRS.", call. = FALSE)
  }

  municipality_columns <- c("code_muni", "name_muni", "abbrev_state", "legal_amazon")
  missing_columns <- setdiff(municipality_columns, names(municipalities))
  if (length(missing_columns)) {
    stop("The municipal layer is missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  operation_crs <- sf::st_crs(
    "+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
  )
  embargoes <- sf::st_transform(sf::st_make_valid(embargoes), operation_crs)
  municipalities <- sf::st_transform(sf::st_make_valid(municipalities), operation_crs)

  key <- if ("seq_tad" %in% names(embargoes)) as.character(embargoes$seq_tad) else rep(NA_character_, nrow(embargoes))
  missing_key <- is.na(key) | !nzchar(key)
  key[missing_key] <- paste0(".source_row_", which(missing_key))
  groups <- split(seq_len(nrow(embargoes)), factor(key, levels = unique(key)))

  embargoes <- lapply(groups, function(ix) {
    feature <- embargoes[ix[1], ]
    sf::st_geometry(feature) <- sf::st_union(sf::st_geometry(embargoes[ix, ]))
    feature$source_polygon_count <- length(ix)
    feature
  })
  embargoes <- do.call(rbind, embargoes)
  embargoes$.embargo_key <- names(groups)

  # Replace the municipality previously matched by name with the municipality
  # determined by the polygon overlay.
  for (column in intersect(c("name_muni", "code_muni", "legal_amazon"), names(embargoes))) {
    embargoes[[column]] <- NULL
  }
  if ("abbrev_state" %in% names(embargoes)) embargoes$abbrev_state <- NULL

  intersections <- suppressWarnings(sf::st_intersection(
    embargoes,
    municipalities[, municipality_columns]
  ))
  intersections$intersection_area_m2 <- as.numeric(sf::st_area(intersections))
  intersections <- intersections[is.finite(intersections$intersection_area_m2) & intersections$intersection_area_m2 > 0, ]

  matched <- unique(intersections$.embargo_key)
  unmatched <- embargoes[!embargoes$.embargo_key %in% matched, ]
  if (nrow(unmatched)) {
    for (column in municipality_columns) unmatched[[column]] <- municipalities[[column]][NA_integer_]
    unmatched$intersection_area_m2 <- NA_real_
    intersections <- rbind(intersections, unmatched)
  }

  intersections <- sf::st_drop_geometry(intersections)
  intersections$.embargo_key <- NULL
  intersections
}
