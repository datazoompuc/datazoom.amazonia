#' @title Greenhouse gas emission estimates (SEEG)
#'
#' @description Loads data of estimates of emission of greenhouse gases
#'
#' @param dataset A dataset name ("seeg", seeg_farming", "seeg_industry", "seeg_energy", "seeg_land", "seeg_residuals"). On which "seeg" contains all five sectors (only works with raw_data = TRUE) and the others are filtered specifically by a main source of emission.
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data. Can be one of "country", "state" or "municipality".
#'
#' @return A \code{tibble}.
#'
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download raw SEEG data (all sectors) at the country level
#' all_emissions <- load_seeg(
#'   dataset = "seeg",
#'   raw_data = TRUE,
#'   geo_level = "country",
#'   language = "eng"
#' )
#'
#' # download treated agricultural emissions at the state level
#' farming <- load_seeg(
#'   dataset = "seeg_farming",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "eng"
#' )
#'
#' # download treated land use change emissions at the state level
#' land_use <- load_seeg(
#'   dataset = "seeg_land",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "eng"
#' )
#'
#' # download treated energy emissions at the municipality level
#' energy <- load_seeg(
#'   dataset = "seeg_energy",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "eng"
#' )
#'
#' # download treated industrial process emissions at the state level
#' industry <- load_seeg(
#'   dataset = "seeg_industry",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "eng"
#' )
#'
#' # download treated waste emissions at the state level
#' residuals <- load_seeg(
#'   dataset = "seeg_residuals",
#'   raw_data = FALSE,
#'   geo_level = "state",
#'   language = "eng"
#' )
#'
#' @export

load_seeg <- function(dataset, raw_data = FALSE,
                      geo_level, language = "eng") {
  ##############################
  ## Binding Global Variables ##
  ##############################

  ibge <- city <- state <- municipio <- estado <- setor <- id_territorio <- NULL
  setor_de_emissao <- categoria_emissora <- sub_categoria_emissora <- NULL
  emissao_remocao_bunker <- categoria_emissao <- subcategoria_emissao <- NULL
  recorte <- atividade_geral <- tipo_residuo <- tipo_emissao <- via_emissao <- NULL
  year <- Ano <- Valor <- value <- NULL
  sector <- emission_category <- emission_subcategory <- waste_stream <- general_activity <- NULL
  emission_type <- emission_pathway <- emission_scope <- x1970 <- x2024 <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "seeg"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and geo_level are supported

  check_params(param)

  ## Dataset

  if (param$dataset == "seeg" & param$raw_data == FALSE) {
    stop("This dataset only works with raw_data = TRUE")
  }
  if (param$dataset == "seeg_farming" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_energy" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_industry" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_land" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }
  if (param$dataset == "seeg_residuals" & param$raw_data == TRUE) {
    stop("This dataset only works with raw_data = FALSE")
  }

  # Picking which sheet to download
  #
  # 2026-09-15: "Dados" replaces the pre-v13.0 "BD GEE Municipios GWP-AR5"
  # sheet -- see this file's header for the full schema migration (NIVEL
  # 1..6 restructured into Setor de emissão/Categoria emissora/Sub-categoria
  # emissora/Recorte/Atividade geral; year range widened from 2000-2018 to
  # 1970-2024; gas/produto dropped at municipality grain). Verified live via
  # resolve_seeg.R's structural check before this became the manifest's
  # real url (actions/scrapers/resolve_seeg.R).
  #
  # FRAGILE: every pivot_longer()/across() call below hardcodes the column
  # range x1970:x2024. If a future upload adds a year (e.g. 2025), this
  # range does NOT expand on its own -- tidyselect just excludes the new
  # column from output silently, no error, so the new year's data would
  # vanish from every sector's output. resolve_seeg.R's live year-field
  # check (against the manifest's available_time) is the thing that's
  # actually supposed to catch this drift and force a manual range update
  # here -- this hardcoded range is only as safe as that check staying wired
  # up.

  sheet <- "Dados"

  ##############
  ## Download ##
  ##############

  dat <- external_download(
    dataset = param$dataset,
    source = param$source,
    geo_level = param$geo_level,
    sheet = sheet
  )


  dat <- dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  # FRAGILE: one row per (municipality x full category path) that has any
  # non-zero value across 1970-2024 -- confirmed live NOT to be a full
  # cross product (most, not all, categories apply to all 5570
  # municipalities). Also confirmed live to include exactly one
  # non-municipality row (id_territorio == "1BR", municipio == "BR (NA)",
  # estado == "Nao Alocado" after Latin-ASCII stripping) representing
  # emissions the source itself doesn't attribute to any municipality --
  # kept (not dropped) at every geo_level so a real quantity is never
  # silently discarded from a total; at geo_level == "municipality" it
  # surfaces as its own row with municipio/estado/ibge coerced to NA (same
  # convention the pre-v13.0 code already used for its own "NA"-string
  # placeholders, see the ibge/municipio/estado case_when() calls below).
  #
  # FRAGILE: every sector/geo_level/language branch below repeats the same
  # three id_territorio/municipio assumptions about the source file's
  # current format, none of them re-derived from the data itself:
  #   - `id_territorio == "1BR"` is a hardcoded literal sentinel for the
  #     unallocated-emissions row. If a future upload spells this sentinel
  #     differently, the comparison just never matches -- the row stops
  #     being coerced to NA and is instead treated as a real (garbage)
  #     municipality.
  #   - `substring(id_territorio, 2)` assumes id_territorio is always
  #     exactly a 1-character prefix + the IBGE code (confirmed against 5
  #     real municipalities, not all 5570). A future format with a
  #     differently-sized prefix would silently produce a wrong, truncated
  #     IBGE code rather than erroring.
  #   - `sub(" \\([A-Za-z]{2}\\)$", "", municipio)` assumes municipality
  #     names always end in a literal " (XX)" two-letter suffix. A format
  #     change there (suffix dropped, 3 letters, full state name in
  #     parens) either silently leaves the suffix in `municipio` or strips
  #     the wrong substring.

  ## Return Raw Data
  #
  # The Dados sheet is already municipality-grained -- state/country levels
  # are real sums over municipality rows, computed here exactly like the
  # pre-v13.0 code did, just against the new column names.

  if (param$geo_level == "state") {
    dat <- dat %>%
      dplyr::group_by(
        setor_de_emissao, categoria_emissora, sub_categoria_emissora,
        recorte, atividade_geral, emissao_remocao_bunker, estado
      ) %>%
      dplyr::summarise(dplyr::across(x1970:x2024, \(x) sum(x, na.rm = FALSE)), .groups = "drop")
  }

  if (param$geo_level == "country") {
    dat <- dat %>%
      dplyr::group_by(
        setor_de_emissao, categoria_emissora, sub_categoria_emissora,
        recorte, atividade_geral, emissao_remocao_bunker
      ) %>%
      dplyr::summarise(dplyr::across(x1970:x2024, \(x) sum(x, na.rm = FALSE)), .groups = "drop")
  }

  if (param$dataset == "seeg" & param$raw_data) {
    return(dat)
  }


  # -------------------------------------------------------------------------
  # seeg_residuals ("Residuos" sector)
  #
  # Verified live against the real v13.0 file (66,559 residuals rows) cross-
  # tabulated against the pre-v13.0 file's equivalent "Residuos" rows
  # (71,865 rows) -- see the plan/session notes for the full crosstab. What
  # was confirmed:
  #
  #   - categoria_emissora (5 values) is a clean 1:1 relabel of the old
  #     NIVEL 3 (e.g. "Efluentes Liquidos Domesticos" -> "Efluentes
  #     domesticos"), no case_when needed for PT output.
  #   - sub_categoria_emissora (13 values) is a near-1:1 relabel of the old
  #     NIVEL 4 for every category EXCEPT "Disposicao final": there, the OLD
  #     file split by WASTE TYPE (sewage sludge / municipal solid waste /
  #     healthcare waste, 3 old NIVEL 4 values, row counts don't total the
  #     same as the new file's 2 subcategories), the NEW file instead splits
  #     by DISPOSAL SITE TYPE ("aterros controlados ou lixoes" / "aterros
  #     sanitarios") -- a genuine methodology change for that one category,
  #     not a renaming, so no old-to-new value mapping is attempted for it.
  #   - recorte (Diretas/Efluentes Liquidos/Residuos solidos) and
  #     atividade_geral (Producao Industrial/Saneamento Basico) are BOTH
  #     genuinely new dimensions with no old-schema equivalent (old NIVEL 2,
  #     which had a similar-looking 2-value shape, groups categories
  #     differently -- e.g. it lumped domestic AND industrial effluents
  #     together, recorte/atividade_geral do not) -- kept as their own new
  #     columns rather than forced into old NIVEL 2's name/shape.
  #   - old atividade_economica/produto/gas are confirmed absent from the
  #     v13.0 Dados sheet entirely (not folded into any other field) --
  #     dropped, not reconstructed.
  #   - id_territorio = "1" + the old 7-digit IBGE municipality code,
  #     confirmed against 5 real municipalities across 5 different states
  #     (Abatia/PR, Abaetetuba/PA, Abadia de Goias/GO, Manaus/AM, Rio
  #     Branco/AC) -- stripped back to the classic ibge code here.
  #   - municipio embeds " (UF)" as a suffix (e.g. "Abatia (PR)") and estado
  #     is now the full Portuguese state name (e.g. "Parana") instead of the
  #     old 2-letter UF code.
  # FRAGILE: a real, confirmed format change on `estado` -- municipio is
  # stripped back to a plain name (matching the old shape) but estado is
  # passed through as the source gives it, NOT reduced back to a 2-letter
  # code, since that would require a name->UF lookup this session did not
  # verify against every one of the 27 real values. Every sector/language
  # branch shares this same estado passthrough.
  #   - Confirmed live: exactly one row (id_territorio == "1BR", municipio ==
  #     "BR (NA)", estado == "Nao Alocado" after this file's ASCII-strip) is
  #     not a real municipality -- it represents emissions the source itself
  #     doesn't attribute to any municipality. Kept at every geo_level (a
  #     real quantity, dropping it would understate totals); at
  #     geo_level == "municipality" its municipio/estado/ibge are coerced to
  #     NA (same convention the pre-v13.0 code already used for its own
  #     "NA"-string geography placeholders).
  # FRAGILE: every branch below hardcodes the literal filter
  #     setor_de_emissao == "Residuos". A future rename of this sector's
  #     label makes every seeg_residuals branch silently return zero rows
  #     instead of erroring.
  # -------------------------------------------------------------------------

  if (param$dataset == "seeg_residuals" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Residuos") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_residuo = recorte,
        tipo_emissao = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        municipio = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        estado = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio) %>%
      dplyr::relocate(
        Ano, municipio, estado, ibge, setor, categoria_emissao, subcategoria_emissao,
        tipo_residuo, atividade_geral, tipo_emissao, Valor
      )
  }

  if (param$dataset == "seeg_residuals" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Residuos") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_residuo = recorte,
        tipo_emissao = emissao_remocao_bunker
      )

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::relocate(
          Ano, estado, setor, categoria_emissao, subcategoria_emissao,
          tipo_residuo, atividade_geral, tipo_emissao, Valor
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          Ano, setor, categoria_emissao, subcategoria_emissao,
          tipo_residuo, atividade_geral, tipo_emissao, Valor
        )
    }
  }

  if (param$dataset == "seeg_residuals" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Residuos") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        waste_stream = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        city = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        state = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio, -municipio, -estado) %>%
      seeg_residuals_translate() %>%
      dplyr::relocate(
        year, city, state, ibge, sector, emission_category, emission_subcategory,
        waste_stream, general_activity, emission_type, value
      )
  }

  if (param$dataset == "seeg_residuals" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Residuos") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        waste_stream = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      seeg_residuals_translate()

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::mutate(state = estado) %>%
        dplyr::select(-estado) %>%
        dplyr::relocate(
          year, state, sector, emission_category, emission_subcategory,
          waste_stream, general_activity, emission_type, value
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          year, sector, emission_category, emission_subcategory,
          waste_stream, general_activity, emission_type, value
        )
    }
  }

  # -------------------------------------------------------------------------
  # seeg_farming ("Agropecuaria" sector)
  #
  # Verified live against the real v13.0 file (128,272 farming rows) cross-
  # tabulated against the pre-v13.0 file's equivalent "Agropecuaria" rows
  # (473,078 rows). What was confirmed:
  #
  #   - categoria_emissora (5 values) is a clean 1:1 relabel of the old
  #     NIVEL 2 (Cultivo de Arroz, Fermentacao Enterica, Manejo de Dejetos
  #     Animais, Queima de Residuos Agricolas, Solos Manejados).
  #   - sub_categoria_emissora (14 values) is a near-1:1 relabel of the old
  #     NIVEL 4 for the values old NIVEL 4 actually named (e.g. "Deposicao
  #     de dejetos em pastagem", "Mineralizacao de N associado a perda de C
  #     no solo" match verbatim) -- 4 categories that only ever had the
  #     generic placeholder NIVEL 4 value "Outros" in the old file
  #     (Cultivo de Arroz, Fermentacao Enterica, Manejo de Dejetos Animais,
  #     Queima de Residuos Agricolas) now get a real, specific
  #     sub_categoria_emissora instead -- a genuine improvement, not a loss.
  #   - recorte (3 values: Diretas/Indiretas (deposicao atmosferica)/
  #     Indiretas (lixiviacao/escorrimento superficial)) folds old NIVEL 3
  #     (Diretas/Indiretas) together with old NIVEL 4's own further split of
  #     "Indiretas" into "Deposicao Atmosferica"/"Lixiviacao" -- kept as one
  #     new field rather than force-split back into two old-shaped ones.
  #   - atividade_geral (3 values: Agricultura/Pecuaria/Silvicultura) is the
  #     closest survivor of old atividade_economica (AGR/PEC) -- confirmed
  #     to add a genuinely new third value, "Silvicultura" (forestry),
  #     appearing only under "Solos Manejados" > soil-carbon-stock
  #     sub-categories (a value old atividade_economica never had).
  #   - OLD NIVEL 5/NIVEL 6 (the Animal/Vegetal type split and the actual
  #     livestock species or crop, e.g. "Bubalino", "Soja") are confirmed
  #     absent from the v13.0 Dados sheet entirely, same as
  #     atividade_economica/produto/gas -- species/crop-level detail is a
  #     real, confirmed loss at municipality grain, not reconstructed.
  #   - Bunker values seen here: Emissao/Emissao NCI/Remocao NCI (no plain
  #     "Remocao") -- kept as 3 distinct translated values rather than
  #     merged into old's Emissao/Remocao pair (see this file's Part 2 open
  #     items: NCI's exact accounting meaning wasn't independently
  #     confirmed this session, so nothing is silently folded into it).
  # FRAGILE: every branch below hardcodes the literal filter
  #     setor_de_emissao == "Agropecuaria". A future rename of this
  #     sector's label makes every seeg_farming branch silently return
  #     zero rows instead of erroring.
  # -------------------------------------------------------------------------

  if (param$dataset == "seeg_farming" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Agropecuaria") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        via_emissao = recorte,
        tipo_emissao = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        municipio = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        estado = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio) %>%
      dplyr::relocate(
        Ano, municipio, estado, ibge, setor, categoria_emissao, subcategoria_emissao,
        via_emissao, atividade_geral, tipo_emissao, Valor
      )
  }

  if (param$dataset == "seeg_farming" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Agropecuaria") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        via_emissao = recorte,
        tipo_emissao = emissao_remocao_bunker
      )

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::relocate(
          Ano, estado, setor, categoria_emissao, subcategoria_emissao,
          via_emissao, atividade_geral, tipo_emissao, Valor
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          Ano, setor, categoria_emissao, subcategoria_emissao,
          via_emissao, atividade_geral, tipo_emissao, Valor
        )
    }
  }

  if (param$dataset == "seeg_farming" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Agropecuaria") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_pathway = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        city = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        state = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio, -municipio, -estado) %>%
      seeg_farming_translate() %>%
      dplyr::relocate(
        year, city, state, ibge, sector, emission_category, emission_subcategory,
        emission_pathway, general_activity, emission_type, value
      )
  }

  if (param$dataset == "seeg_farming" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Agropecuaria") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_pathway = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      seeg_farming_translate()

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::mutate(state = estado) %>%
        dplyr::select(-estado) %>%
        dplyr::relocate(
          year, state, sector, emission_category, emission_subcategory,
          emission_pathway, general_activity, emission_type, value
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          year, sector, emission_category, emission_subcategory,
          emission_pathway, general_activity, emission_type, value
        )
    }
  }

  # -------------------------------------------------------------------------
  # seeg_industry ("Processos Industriais" sector)
  #
  # Verified live against the real v13.0 file (5,683 industry rows) cross-
  # tabulated against the pre-v13.0 file's equivalent "Processos Industriais"
  # rows (only 165 rows -- this sector is genuinely sparse, a handful of real
  # facilities per category). What was confirmed:
  #
  #   - categoria_emissora (7 values) is a near-1:1 relabel of the old
  #     NIVEL 2 (Industria Quimica, Produtos Minerais, Producao de Metais,
  #     Uso Nao-Energetico de Combustiveis e Uso de Solventes, and old
  #     "Uso de SF6"/"Emissoes de HFCs" split into their own better-named
  #     categories: "Uso de SF6 em equipamentos eletricos", "Producao e uso
  #     de HFCs") -- PLUS one genuinely new category, "Producao e uso de
  #     CFs", with no old-schema equivalent at all -- see the FRAGILE tag on
  #     SEEG_INDUSTRY_CATEGORY_EN below for what's uncertain about it.
  #   - sub_categoria_emissora is a near-1:1 relabel of the old NIVEL 3
  #     (e.g. "Producao de Amonia" -> "Producao de amonia" verbatim).
  #   - recorte (Emissao efetiva/Emissao potencial -- "actual"/"potential"
  #     emission) is a GENUINELY NEW field: the old schema had no equivalent
  #     dimension for this sector at all (NIVEL 3 was already the finest
  #     category level, no further actual/potential split existed).
  #   - atividade_geral (7 values: Quimica, Cimento, Metalurgia, Edificacoes,
  #     Transporte de carga, Transporte de passageiros, Outras materias
  #     primas e industrias) is a real ENHANCEMENT over old
  #     atividade_economica (CIM/MET/ENE_ELET/Outra_IND/HFC, 5 codes) -- old
  #     lumped every HFC use into one "HFC" bucket; the new file breaks it
  #     out by the actual END-USE sector (buildings/freight/passenger
  #     transport), confirmed via real co-occurring rows, not assumed from
  #     field names.
  #   - OLD NIVEL 4 (input/technology detail -- e.g. "Cal Calcitica"/"Cal
  #     Dolomitica"/"Cal Magnesiana" lime types, "Tecnologia Soderberg"/
  #     "Tecnologia Prebaked Anode" aluminum smelting tech) and
  #     atividade_economica/produto/gas are confirmed absent from the v13.0
  #     Dados sheet entirely -- dropped, not reconstructed, same treatment
  #     as every other migrated sector's confirmed real losses.
  #   - Bunker: only "Emissao" appears for this sector (no NCI values,
  #     unlike farming/land).
  # FRAGILE: every branch below hardcodes the literal filter
  #     setor_de_emissao == "Processos Industriais". A future rename of
  #     this sector's label makes every seeg_industry branch silently
  #     return zero rows instead of erroring.
  # -------------------------------------------------------------------------

  if (param$dataset == "seeg_industry" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Processos Industriais") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        municipio = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        estado = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio) %>%
      dplyr::relocate(
        Ano, municipio, estado, ibge, setor, categoria_emissao, subcategoria_emissao,
        recorte, atividade_geral, tipo_emissao, Valor
      )
  }

  if (param$dataset == "seeg_industry" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Processos Industriais") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      )

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::relocate(
          Ano, estado, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          Ano, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    }
  }

  if (param$dataset == "seeg_industry" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Processos Industriais") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        city = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        state = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio, -municipio, -estado) %>%
      seeg_industry_translate() %>%
      dplyr::relocate(
        year, city, state, ibge, sector, emission_category, emission_subcategory,
        emission_scope, general_activity, emission_type, value
      )
  }

  if (param$dataset == "seeg_industry" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Processos Industriais") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      seeg_industry_translate()

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::mutate(state = estado) %>%
        dplyr::select(-estado) %>%
        dplyr::relocate(
          year, state, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          year, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    }
  }

  # -------------------------------------------------------------------------
  # seeg_energy -- confirmed live against the real v13.0 "Dados" sheet
  # (52,771 rows for setor_de_emissao == "Energia") cross-tabulated against
  # the OLD file's NIVEL 1..6 for the same sector (85,793 rows):
  #   - categoria_emissora (8 values: Comercial, Industrial, Publico,
  #     Residencial, Transportes, Agropecuaria, Geracao de eletricidade
  #     (servico publico), Producao de combustiveis) is a clean 1:1 relabel
  #     of the old NIVEL 3 (the end-use sector).
  #   - sub_categoria_emissora (26 values, e.g. Rodoviario, Aereo, Cimento,
  #     Quimica, Refino de petroleo) is a clean 1:1 relabel of the old
  #     NIVEL 4 (the subsector).
  #   - recorte (2 values: "Emissoes pela queima de combustiveis" / "Emissoes
  #     fugitivas") is a clean 1:1 relabel of the old NIVEL 2 (combustion vs
  #     fugitive emissions) -- a DIFFERENT meaning of "recorte" than
  #     seeg_industry's actual/potential split; this field is sector-scoped,
  #     not a global vocabulary.
  #   - atividade_geral (10 values) is a coarser, cross-cutting grouping that
  #     does not map 1:1 to any single old level: e.g. "Transporte de carga"/
  #     "Transporte de passageiros" both pull from categoria == "Transportes"
  #     + subcategoria == "Rodoviario" alike (every municipality has BOTH an
  #     atividade_geral == "Transporte de carga" AND a "Transporte de
  #     passageiros" row for the same Rodoviario subcategoria -- confirmed
  #     live, not a duplicate).
  #   - OLD NIVEL 5 (fuel type -- e.g. Gasolina/Diesel/GLP/Gas natural/Lenha)
  #     and NIVEL 6 (further sub-detail, e.g. "Consumo Final Energetico" vs
  #     "Centrais Eletricas de Servico Publico"), plus atividade_economica
  #     and produto, are confirmed absent from the v13.0 Dados sheet
  #     entirely.
  # FRAGILE: this is a real loss of the fuel-type dimension for this sector
  # specifically (the old file let you split energy emissions by fuel
  # burned; the new one does not) -- same treatment as every other migrated
  # sector's confirmed real losses, documented rather than reconstructed.
  #   - Bunker: both "Emissao" and "Bunker" appear for this sector (2 rows
  #     total tagged "Bunker", both under categoria == "Transportes" --
  #     confirmed live, passed through via the shared tipo_emissao/
  #     emission_type translation like every other sector).
  # FRAGILE: every branch below hardcodes the literal filter
  #     setor_de_emissao == "Energia". A future rename of this sector's
  #     label makes every seeg_energy branch silently return zero rows
  #     instead of erroring.
  # -------------------------------------------------------------------------

  if (param$dataset == "seeg_energy" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Energia") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        municipio = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        estado = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio) %>%
      dplyr::relocate(
        Ano, municipio, estado, ibge, setor, categoria_emissao, subcategoria_emissao,
        recorte, atividade_geral, tipo_emissao, Valor
      )
  }

  if (param$dataset == "seeg_energy" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Energia") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      )

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::relocate(
          Ano, estado, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          Ano, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    }
  }

  if (param$dataset == "seeg_energy" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Energia") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        city = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        state = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio, -municipio, -estado) %>%
      seeg_energy_translate() %>%
      dplyr::relocate(
        year, city, state, ibge, sector, emission_category, emission_subcategory,
        emission_scope, general_activity, emission_type, value
      )
  }

  if (param$dataset == "seeg_energy" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Energia") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      seeg_energy_translate()

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::mutate(state = estado) %>%
        dplyr::select(-estado) %>%
        dplyr::relocate(
          year, state, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          year, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    }
  }

  # -------------------------------------------------------------------------
  # seeg_land -- confirmed live against the real v13.0 "Dados" sheet
  # (104,205 rows for setor_de_emissao == "Mudanca de Uso da Terra e
  # Floresta") cross-tabulated against the OLD file's NIVEL 1..6 for the
  # same sector (129,993 rows):
  #   - categoria_emissora (7 values: Alteracoes de uso da terra, Carbono
  #     organico no solo, Queimadas nao associadas a desmatamento, Remocao
  #     em areas protegidas, Remocao por mudanca de uso da terra, Remocao
  #     por vegetacao secundaria, Residuos florestais) is a relabel of the
  #     old NIVEL 2.
  #   - sub_categoria_emissora (5 values: Desmatamento, Outras mudancas de
  #     uso da terra, Queimadas nao associadas a desmatamento, Regeneracao,
  #     Vegetacao nativa estavel) corresponds to the old NIVEL 5.
  #   - recorte (Em area protegida / Fora de area protegida / a literal
  #     "NA" string for Residuos florestais rows, where the old file's
  #     equivalent NIVEL 4 split simply doesn't apply) corresponds to the
  #     old NIVEL 4 protected-area split; the literal "NA" is translated to
  #     "Not applicable" in English rather than a real NA, so no valid row
  #     is ever left with a missing value post-translation.
  #   - atividade_geral (4 values: Agricultura, Outros (geral), Pecuaria,
  #     Vegetacao nativa) is a coarse new grouping with no direct old-schema
  #     equivalent.
  # FRAGILE: the largest confirmed loss across every migrated sector -- OLD
  # NIVEL 3 (BIOME -- Amazonia/Cerrado/Mata Atlantica/etc.) is confirmed
  # absent from the v13.0 Dados sheet entirely, and OLD NIVEL 6 (the full
  # origin->destination land-cover TRANSITION MATRIX, e.g. "Floresta
  # primaria -- Silvicultura") collapses into the four coarser fields
  # above. Both are dropped, not reconstructed -- called out explicitly to
  # the user at migration time, not just documented here (see 2026-09-15
  # session).
  #   - Bunker: "Emissao", "Emissao NCI", and (unlike every other migrated
  #     sector) plain "Remocao" all appear for this sector -- confirmed the
  #     shared SEEG_EMISSION_TYPE_EN dictionary needed a "Remocao" ->
  #     "Removal" entry added (it previously only had "Remocao NCI").
  # FRAGILE: every branch below hardcodes the literal filter
  #     setor_de_emissao == "Mudanca de Uso da Terra e Floresta". A future
  #     rename of this sector's label makes every seeg_land branch
  #     silently return zero rows instead of erroring.
  # -------------------------------------------------------------------------

  if (param$dataset == "seeg_land" & param$geo_level == "municipality" & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Mudanca de Uso da Terra e Floresta") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        municipio = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        estado = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio) %>%
      dplyr::relocate(
        Ano, municipio, estado, ibge, setor, categoria_emissao, subcategoria_emissao,
        recorte, atividade_geral, tipo_emissao, Valor
      )
  }

  if (param$dataset == "seeg_land" & param$geo_level %in% c("country", "state") & param$language == "pt") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Mudanca de Uso da Terra e Floresta") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "Ano",
        names_prefix = "x",
        values_to = "Valor"
      ) %>%
      dplyr::rename(
        setor = setor_de_emissao,
        categoria_emissao = categoria_emissora,
        subcategoria_emissao = sub_categoria_emissora,
        tipo_emissao = emissao_remocao_bunker
      )

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::relocate(
          Ano, estado, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          Ano, setor, categoria_emissao, subcategoria_emissao,
          recorte, atividade_geral, tipo_emissao, Valor
        )
    }
  }

  if (param$dataset == "seeg_land" & param$geo_level == "municipality" & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Mudanca de Uso da Terra e Floresta") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      dplyr::mutate(
        ibge = dplyr::if_else(id_territorio == "1BR", NA_character_, substring(id_territorio, 2)),
        city = dplyr::if_else(id_territorio == "1BR", NA_character_, sub(" \\([A-Za-z]{2}\\)$", "", municipio)),
        state = dplyr::if_else(id_territorio == "1BR", NA_character_, estado)
      ) %>%
      dplyr::select(-id_territorio, -municipio, -estado) %>%
      seeg_land_translate() %>%
      dplyr::relocate(
        year, city, state, ibge, sector, emission_category, emission_subcategory,
        emission_scope, general_activity, emission_type, value
      )
  }

  if (param$dataset == "seeg_land" & param$geo_level %in% c("country", "state") & param$language == "eng") {
    dat <- dat %>%
      dplyr::filter(setor_de_emissao == "Mudanca de Uso da Terra e Floresta") %>%
      tidyr::pivot_longer(
        cols = x1970:x2024,
        names_to = "year",
        names_prefix = "x",
        values_to = "value"
      ) %>%
      dplyr::rename(
        sector = setor_de_emissao,
        emission_category = categoria_emissora,
        emission_subcategory = sub_categoria_emissora,
        emission_scope = recorte,
        general_activity = atividade_geral,
        emission_type = emissao_remocao_bunker
      ) %>%
      seeg_land_translate()

    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::mutate(state = estado) %>%
        dplyr::select(-estado) %>%
        dplyr::relocate(
          year, state, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    } else {
      dat <- dat %>%
        dplyr::relocate(
          year, sector, emission_category, emission_subcategory,
          emission_scope, general_activity, emission_type, value
        )
    }
  }

  return(dat)
}

# -----------------------------------------------------------------------
# PT -> EN value translation, shared machinery for every seeg_* sector.
#
# One hashed named-vector lookup per field instead of a dplyr::case_when()
# chain: case_when() evaluates every `==` branch (N string comparisons per
# row, for a field with N known values); a named-vector lookup resolves
# each row in one hash lookup regardless of N. Matters more as sectors with
# larger vocabularies get migrated (e.g. seeg_industry's ~35-value
# emitters_production field) than it does for residuals' small ones, so
# this is the shared shape every later sector's own dictionary plugs into,
# not just a residuals-only tweak.
#
# seeg_translate(x, dict): dict is a named character vector, PT value =
# name, EN value = value (e.g. c("Emissao" = "Emission")). Any x value NOT
# a name in dict passes through unchanged (mirrors case_when()'s old
# `TRUE ~ x` fallback) -- this matters because a handful of fields (setor,
# tipo_emissao) have exactly one confirmed real value today but could see
# more added upstream before this code is revisited.
#
# FRAGILE: that same passthrough-on-unmatched behavior means every
# SEEG_*_EN dictionary below is a closed list, silently. If a future SEEG
# upload adds a new category/subcategory/activity value, language = "eng"
# output does NOT error -- it just shows the untranslated Portuguese string
# for that one value, mixed in with otherwise-English output, easy to miss
# in a quick look at the data.
# -----------------------------------------------------------------------

seeg_translate <- function(x, dict) {
  out <- unname(dict[x])
  unmatched <- is.na(out) & !is.na(x)
  out[unmatched] <- x[unmatched]
  out
}

# Every dictionary below was confirmed live against the real v13.0 file's
# actual distinct "Residuos"-sector values (see load_seeg()'s header
# comment on seeg_residuals) -- not guessed from field names.

SEEG_SECTOR_EN <- c(
  "Residuos" = "Waste",
  "Agropecuaria" = "Agriculture and Livestock",
  "Processos Industriais" = "Industrial Processes",
  "Energia" = "Energy",
  "Mudanca de Uso da Terra e Floresta" = "Land Use Change and Forestry"
)

SEEG_EMISSION_TYPE_EN <- c(
  "Emissao" = "Emission",
  "Emissao NCI" = "Emission NCI",
  "Remocao" = "Removal",
  "Remocao NCI" = "Removal NCI"
)

SEEG_STATE_EN <- c(
  "Nao Alocado" = "Not Allocated"
)

SEEG_RESIDUALS_CATEGORY_EN <- c(
  "Disposicao final" = "Final disposal",
  "Efluentes domesticos" = "Domestic effluents",
  "Efluentes liquidos industriais" = "Industrial liquid effluents",
  "Incineracao ou queima a ceu aberto" = "Incineration or open-air burning",
  "Tratamento biologico de residuos solidos" = "Biological treatment of solid waste"
)

SEEG_RESIDUALS_SUBCATEGORY_EN <- c(
  "Compostagem" = "Composting",
  "Disposicao em aterros controlados ou lixoes" = "Disposal in controlled landfills or open dumps",
  "Disposicao final em aterros sanitarios" = "Final disposal in sanitary landfills",
  "Incineracao" = "Incineration",
  "Producao de carne avicola" = "Poultry meat production",
  "Producao de carne bovina" = "Beef production",
  "Producao de carne suina" = "Pork production",
  "Producao de Celulose" = "Pulp production",
  "Producao de Cerveja" = "Beer production",
  "Producao de leite cru" = "Raw milk production",
  "Producao de leite pasteurizado" = "Pasteurized milk production",
  "Queima de residuos a ceu aberto" = "Open-air waste burning",
  "Tratamento e despejo de efluentes domesticos" = "Domestic effluent treatment and disposal"
)

SEEG_RESIDUALS_WASTE_STREAM_EN <- c(
  "Diretas" = "Direct",
  "Efluentes Liquidos" = "Liquid effluents",
  "Residuos solidos" = "Solid waste"
)

SEEG_RESIDUALS_GENERAL_ACTIVITY_EN <- c(
  "Producao Industrial" = "Industrial Production",
  "Saneamento Basico" = "Basic Sanitation"
)

# English value translations for seeg_residuals, shared by the municipality
# and country/state branches above.
seeg_residuals_translate <- function(dat) {
  sector <- emission_category <- emission_subcategory <- NULL
  waste_stream <- general_activity <- emission_type <- estado <- NULL

  dat %>%
    dplyr::mutate(
      sector = seeg_translate(sector, SEEG_SECTOR_EN),
      emission_category = seeg_translate(emission_category, SEEG_RESIDUALS_CATEGORY_EN),
      emission_subcategory = seeg_translate(emission_subcategory, SEEG_RESIDUALS_SUBCATEGORY_EN),
      waste_stream = seeg_translate(waste_stream, SEEG_RESIDUALS_WASTE_STREAM_EN),
      general_activity = seeg_translate(general_activity, SEEG_RESIDUALS_GENERAL_ACTIVITY_EN),
      emission_type = seeg_translate(emission_type, SEEG_EMISSION_TYPE_EN)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of("estado"),
      ~ seeg_translate(., SEEG_STATE_EN)
    ))
}

# Every dictionary below was confirmed live against the real v13.0 file's
# actual distinct "Agropecuaria"-sector values (see load_seeg()'s header
# comment on seeg_farming) -- not guessed from field names.

SEEG_FARMING_CATEGORY_EN <- c(
  "Cultivo de arroz" = "Rice cultivation",
  "Fermentacao enterica" = "Enteric fermentation",
  "Manejo de dejetos animais" = "Animal waste management",
  "Queima de residuos agricolas" = "Agricultural residue burning",
  "Solos manejados" = "Managed soils"
)

SEEG_FARMING_SUBCATEGORY_EN <- c(
  "Cultivo em sistema irrigado inundado" = "Flooded irrigated rice cultivation",
  "Processo de digestao de animais ruminantes" = "Ruminant animal digestion process",
  "Tratamento e disposicao de dejetos animais" = "Animal waste treatment and disposal",
  "Pratica de queima dos residuos agricolas" = "Agricultural residue burning practice",
  "Aplicacao de residuos organicos" = "Application of organic residues",
  "Aumento do estoque de C no solo" = "Increase in soil carbon stock",
  "Corretivo agricola" = "Agricultural soil amendment",
  "Deposicao de dejetos em pastagem" = "Manure deposition on pasture",
  "Fertilizantes sinteticos nitrogenados" = "Synthetic nitrogen fertilizers",
  "Mineralizacao de N associado a perda de C no solo" = "N mineralization associated with soil carbon loss",
  "Ocorrencia de queima de pasto" = "Pasture burning occurrence",
  "Reducao do estoque de C no solo" = "Decrease in soil carbon stock",
  "Residuos agricolas" = "Agricultural residues",
  "Solos organicos" = "Organic soils"
)

SEEG_FARMING_PATHWAY_EN <- c(
  "Diretas" = "Direct",
  "Indiretas (deposicao atmosferica)" = "Indirect (atmospheric deposition)",
  "Indiretas (lixiviacao/escorrimento superficial)" = "Indirect (leaching/runoff)"
)

SEEG_FARMING_GENERAL_ACTIVITY_EN <- c(
  "Agricultura" = "Agriculture",
  "Pecuaria" = "Livestock",
  "Silvicultura" = "Forestry"
)

# English value translations for seeg_farming, shared by the municipality
# and country/state branches above.
seeg_farming_translate <- function(dat) {
  sector <- emission_category <- emission_subcategory <- NULL
  emission_pathway <- general_activity <- emission_type <- estado <- NULL

  dat %>%
    dplyr::mutate(
      sector = seeg_translate(sector, SEEG_SECTOR_EN),
      emission_category = seeg_translate(emission_category, SEEG_FARMING_CATEGORY_EN),
      emission_subcategory = seeg_translate(emission_subcategory, SEEG_FARMING_SUBCATEGORY_EN),
      emission_pathway = seeg_translate(emission_pathway, SEEG_FARMING_PATHWAY_EN),
      general_activity = seeg_translate(general_activity, SEEG_FARMING_GENERAL_ACTIVITY_EN),
      emission_type = seeg_translate(emission_type, SEEG_EMISSION_TYPE_EN)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of("estado"),
      ~ seeg_translate(., SEEG_STATE_EN)
    ))
}

# Every dictionary below was confirmed live against the real v13.0 file's
# actual distinct "Processos Industriais"-sector values (see load_seeg()'s
# header comment on seeg_industry) -- not guessed from field names.

SEEG_INDUSTRY_CATEGORY_EN <- c(
  "Industria quimica" = "Chemical industry",
  "Produtos minerais" = "Mineral products",
  "Producao de metais" = "Metal production",
  # FRAGILE: exactly which compounds "CFs" covers was not independently
  # confirmed this session -- translated literally, not expanded.
  "Producao e uso de CFs" = "Production and use of CFs",
  "Producao e uso de HFCs" = "Production and use of HFCs",
  "Uso de SF6 em equipamentos eletricos" = "SF6 use in electrical equipment",
  "Uso nao-energetico de combustiveis e solventes em outros setores" = "Non-energy use of fuels and solvents in other sectors"
)

SEEG_INDUSTRY_SUBCATEGORY_EN <- c(
  "Producao de acrilonitrila" = "Acrylonitrile production",
  "Producao de amonia" = "Ammonia production",
  "Producao de caprolactama" = "Caprolactam production",
  "Producao de carbureto de calcio" = "Calcium carbide production",
  "Producao de cloreto de vinila" = "Vinyl chloride production",
  "Producao de coque de petroleo calcinado" = "Calcined petroleum coke production",
  "Producao de eteno" = "Ethylene production",
  "Producao de metanol" = "Methanol production",
  "Producao de negro-de-fumo" = "Carbon black production",
  "Producao de acido adipico" = "Adipic acid production",
  "Producao de acido fosforico" = "Phosphoric acid production",
  "Producao de acido nitrico" = "Nitric acid production",
  "Producao de oxido de eteno" = "Ethylene oxide production",
  "Consumo de barrilha" = "Soda ash consumption",
  "Producao de cal" = "Lime production",
  "Producao de ceramica" = "Ceramics production",
  "Producao de cimento" = "Cement production",
  "Producao de magnesia nao metalurgica" = "Non-metallurgical magnesia production",
  "Producao de vidro" = "Glass production",
  "Producao de aluminio" = "Aluminum production",
  "Producao de ferro gusa e aco" = "Pig iron and steel production",
  "Producao de ferroligas" = "Ferroalloy production",
  "Producao de magnesio" = "Magnesium production",
  "Producao de outros nao-ferrosos" = "Production of other non-ferrous metals",
  "Setor eletronico" = "Electronics sector",
  "Montagem de equipamento ou produto" = "Equipment or product assembly",
  "Sucateamento de equipamento ou produto" = "Equipment or product scrapping",
  "Uso em equipamento ou produto" = "Use in equipment or product",
  "Setor eletrico" = "Electrical sector",
  "Uso nao-energetico de combustiveis e solventes em outros setores" = "Non-energy use of fuels and solvents in other sectors"
)

SEEG_INDUSTRY_SCOPE_EN <- c(
  "Emissao efetiva" = "Actual emission",
  "Emissao potencial" = "Potential emission"
)

SEEG_INDUSTRY_GENERAL_ACTIVITY_EN <- c(
  "Quimica" = "Chemical",
  "Outras materias primas e industrias" = "Other raw materials and industries",
  "Cimento" = "Cement",
  "Metalurgia" = "Metallurgy",
  "Edificacoes" = "Buildings",
  "Transporte de carga" = "Freight transport",
  "Transporte de passageiros" = "Passenger transport"
)

# English value translations for seeg_industry, shared by the municipality
# and country/state branches above.
seeg_industry_translate <- function(dat) {
  sector <- emission_category <- emission_subcategory <- NULL
  emission_scope <- general_activity <- emission_type <- estado <- NULL

  dat %>%
    dplyr::mutate(
      sector = seeg_translate(sector, SEEG_SECTOR_EN),
      emission_category = seeg_translate(emission_category, SEEG_INDUSTRY_CATEGORY_EN),
      emission_subcategory = seeg_translate(emission_subcategory, SEEG_INDUSTRY_SUBCATEGORY_EN),
      emission_scope = seeg_translate(emission_scope, SEEG_INDUSTRY_SCOPE_EN),
      general_activity = seeg_translate(general_activity, SEEG_INDUSTRY_GENERAL_ACTIVITY_EN),
      emission_type = seeg_translate(emission_type, SEEG_EMISSION_TYPE_EN)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of("estado"),
      ~ seeg_translate(., SEEG_STATE_EN)
    ))
}

SEEG_ENERGY_CATEGORY_EN <- c(
  "Comercial" = "Commercial",
  "Industrial" = "Industrial",
  "Publico" = "Public",
  "Residencial" = "Residential",
  "Transportes" = "Transport",
  "Agropecuaria" = "Agriculture and Livestock",
  "Geracao de eletricidade (servico publico)" = "Electricity generation (public utility)",
  "Producao de combustiveis" = "Fuel production"
)

SEEG_ENERGY_SUBCATEGORY_EN <- c(
  "Aereo" = "Air",
  "Agropecuaria" = "Agriculture and Livestock",
  "Alimentos e bebidas" = "Food and beverages",
  "Ceramica" = "Ceramics",
  "Cimento" = "Cement",
  "Comercial" = "Commercial",
  "Exploracao de petroleo e gas natural" = "Oil and natural gas exploration",
  "Ferro gusa e aco" = "Pig iron and steel",
  "Ferro ligas" = "Ferroalloys",
  "Ferroviario" = "Rail",
  "Geracao de eletricidade (servico publico)" = "Electricity generation (public utility)",
  "Hidroviario" = "Waterway",
  "Mineracao e pelotizacao" = "Mining and pelletizing",
  "Nao ferrosos e outros da metalurgia" = "Non-ferrous and other metallurgy",
  "Outras industrias" = "Other industries",
  "Papel e celulose" = "Pulp and paper",
  "Producao de alcool" = "Alcohol production",
  "Producao de carvao mineral e outros" = "Coal and other mineral production",
  "Producao de carvao vegetal" = "Charcoal production",
  "Publico" = "Public",
  "Quimica" = "Chemical",
  "Refino de petroleo" = "Oil refining",
  "Residencial" = "Residential",
  "Rodoviario" = "Road",
  "Textil" = "Textile",
  "Transporte de petroleo e gas natural" = "Oil and natural gas transport"
)

SEEG_ENERGY_SCOPE_EN <- c(
  "Emissoes pela queima de combustiveis" = "Fuel combustion emissions",
  "Emissoes fugitivas" = "Fugitive emissions"
)

SEEG_ENERGY_GENERAL_ACTIVITY_EN <- c(
  "Edificacoes" = "Buildings",
  "Geracao de eletricidade" = "Electricity generation",
  "Outras materias primas e industrias" = "Other raw materials and industries",
  "Transporte de carga" = "Freight transport",
  "Transporte de passageiros" = "Passenger transport",
  "Agropecuaria (finalidade nao identificada)" = "Agriculture and Livestock (unidentified purpose)",
  "Cimento" = "Cement",
  "Metalurgia" = "Metallurgy",
  "Quimica" = "Chemical",
  "Producao de combustiveis" = "Fuel production"
)

# English value translations for seeg_energy, shared by the municipality
# and country/state branches above.
seeg_energy_translate <- function(dat) {
  sector <- emission_category <- emission_subcategory <- NULL
  emission_scope <- general_activity <- emission_type <- estado <- NULL

  dat %>%
    dplyr::mutate(
      sector = seeg_translate(sector, SEEG_SECTOR_EN),
      emission_category = seeg_translate(emission_category, SEEG_ENERGY_CATEGORY_EN),
      emission_subcategory = seeg_translate(emission_subcategory, SEEG_ENERGY_SUBCATEGORY_EN),
      emission_scope = seeg_translate(emission_scope, SEEG_ENERGY_SCOPE_EN),
      general_activity = seeg_translate(general_activity, SEEG_ENERGY_GENERAL_ACTIVITY_EN),
      emission_type = seeg_translate(emission_type, SEEG_EMISSION_TYPE_EN)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of("estado"),
      ~ seeg_translate(., SEEG_STATE_EN)
    ))
}

SEEG_LAND_CATEGORY_EN <- c(
  "Alteracoes de uso da terra" = "Land use change",
  "Carbono organico no solo" = "Soil organic carbon",
  "Queimadas nao associadas a desmatamento" = "Fires not associated with deforestation",
  "Remocao em areas protegidas" = "Removal in protected areas",
  "Remocao por mudanca de uso da terra" = "Removal due to land use change",
  "Remocao por vegetacao secundaria" = "Removal by secondary vegetation",
  "Residuos florestais" = "Forest residues"
)

SEEG_LAND_SUBCATEGORY_EN <- c(
  "Desmatamento" = "Deforestation",
  "Outras mudancas de uso da terra" = "Other land use changes",
  "Queimadas nao associadas a desmatamento" = "Fires not associated with deforestation",
  "Regeneracao" = "Regeneration",
  "Vegetacao nativa estavel" = "Stable native vegetation"
)

# NB: the literal "NA" string (not a missing value -- see R/seeg.R's
# seeg_land header comment) is translated to "Not applicable" rather than
# a real NA, so no valid row is ever left with a missing emission_scope.
SEEG_LAND_SCOPE_EN <- c(
  "Em area protegida" = "In protected area",
  "Fora de area protegida" = "Outside protected area",
  "NA" = "Not applicable"
)

SEEG_LAND_GENERAL_ACTIVITY_EN <- c(
  "Agricultura" = "Agriculture",
  "Outros (geral)" = "Other (general)",
  "Pecuaria" = "Livestock",
  "Vegetacao nativa" = "Native vegetation"
)

# English value translations for seeg_land, shared by the municipality
# and country/state branches above.
seeg_land_translate <- function(dat) {
  sector <- emission_category <- emission_subcategory <- NULL
  emission_scope <- general_activity <- emission_type <- estado <- NULL

  dat %>%
    dplyr::mutate(
      sector = seeg_translate(sector, SEEG_SECTOR_EN),
      emission_category = seeg_translate(emission_category, SEEG_LAND_CATEGORY_EN),
      emission_subcategory = seeg_translate(emission_subcategory, SEEG_LAND_SUBCATEGORY_EN),
      emission_scope = seeg_translate(emission_scope, SEEG_LAND_SCOPE_EN),
      general_activity = seeg_translate(general_activity, SEEG_LAND_GENERAL_ACTIVITY_EN),
      emission_type = seeg_translate(emission_type, SEEG_EMISSION_TYPE_EN)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of("estado"),
      ~ seeg_translate(., SEEG_STATE_EN)
    ))
}
