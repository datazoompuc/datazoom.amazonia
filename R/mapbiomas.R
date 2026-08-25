#' @title MAPBIOMAS - The Annual Land Cover and Use Mapping Project in Brazil
#'
#' @description Loads information about land cover and use
#'
#' @param dataset A dataset name ("mapbiomas_cover", "mapbiomas_transition", "mapbiomas_irrigation", "mapbiomas_deforestation_regeneration", "mapbiomas_secondary_vegetation", "mapbiomas_mining", "mapbiomas_water" or "mapbiomas_fire")
#' @inheritParams load_baci
#' @param geo_level A \code{string} that defines the geographic level of the data
#'   * For dataset "mapbiomas_cover", can only be "municipality"
#'   * For dataset "mapbiomas_transition", can be "municipality" or "biome" (faster download)
#'   * For dataset "mapbiomas_deforestation_regeneration", can only be "municipality"
#'   * For dataset "mapbiomas_secondary_vegetation", can only be "municipality"
#'   * For dataset "mapbiomas_mining", can be "indigenous_land" or "municipality"
#'   * For dataset "mapbiomas_irrigation" (temporarily unavailable, a new collection will be soon delivered), can be "state" or "biome"
#'   * For dataset "mapbiomas_water", can be "municipality" or "biome" ("state" is currently unavailable -- the upstream source link is dead; check the manifest, `inst/extdata/manifest/v1/datasets_link.csv`, before assuming this has changed)
#'   * For dataset "mapbiomas_fire", can only be "state"
#'
#' @return A \code{tibble}.
#'
#' @examplesIf interactive()
#' ### DO NOT RUN ###
#' # download treated MapBiomas land cover data by municipality
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_cover",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "eng"
#' )
#'
#' # download treated data on mining on indigenous lands
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_mining",
#'   raw_data = FALSE,
#'   geo_level = "indigenous_land",
#'   language = "eng"
#' )
#'
#' # download treated data on secondary vegetation by municipality
#' data <- load_mapbiomas(
#'   dataset = "mapbiomas_secondary_vegetation",
#'   raw_data = FALSE,
#'   geo_level = "municipality",
#'   language = "eng"
#' )
#'
#' @export

load_mapbiomas <- function(dataset, raw_data = FALSE, geo_level = "municipality",
                           language = "eng") {
  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()
  param$source <- "mapbiomas"
  param$dataset <- dataset
  param$geo_level <- geo_level
  param$language <- language
  param$raw_data <- raw_data

  # check if dataset and geo_level are supported

  check_params(param)

  # plucking sheet corresponding to each dataset/geo_level (one manifest row
  # per (dataset, geo_level) combination -- see
  # inst/extdata/manifest/v1/datasets_link.csv)

  sheet <- dataset_field(param$source, param$dataset, "sheet", geo_level = param$geo_level)

  ## MapBiomas collection number -- NOT dataset-wide: several MapBiomas
  ## datasets deliberately pin one geo_level to an older Dataverse
  ## collection than its siblings (e.g. mapbiomas_mining/indigenous_land is
  ## intentionally one collection behind municipality's -- see
  ## actions/scrapers/resolve_mapbiomas.R). Reading this with the row's own
  ## geo_level (dataset_field(), not dataset_meta()) is what makes the
  ## printed collection number actually match the file being downloaded for
  ## every geo_level, not just the ones that happen to share the base row's
  ## old value.

  collection <- dataset_field(param$source, param$dataset, "version", geo_level = param$geo_level)
  message("Data from MapBiomas - Collection ", collection, "\n")

  #################
  ## Downloading ##
  #################

  dat <- external_download(
    dataset = param$dataset,
    source = param$source,
    geo_level = param$geo_level,
    sheet = sheet
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat_mod <- mapbiomas_treat(dat, param)

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}

# Split out of load_mapbiomas() so it can be unit-tested against small
# hand-built tibbles carrying real source headers, without a
# multi-hundred-megabyte download -- see
# tests/testthat/test-mapbiomas-schema.R.
#
# Every assumption below about the SHAPE of the incoming file (not just its
# column names) is tagged "# FRAGILE:". actions/scripts/build_manifest.R
# greps this file for that tag (collect_mapbiomas_fragility_notes()) and
# names the affected lines in the PR body whenever a MapBiomas manifest row
# changes -- so a reviewer knows exactly what to re-check even when the
# resolver's own diff looks like "just a URL". Keep the tag on its own
# comment line directly above the code it describes; the collector pairs
# each tag with the next non-comment line number.
mapbiomas_treat <- function(dat, param) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  value <- NULL

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, function(var) {
      stringi::stri_trans_general(str = var, id = "Latin-ASCII")
    })

  # FRAGILE: assumes exactly one redundant state column to drop. Collection
  # 10's COVERAGE sheets carry BOTH state (full name) and state_acronym;
  # the rename maps below send state_acronym -> "state" (eng) / both ->
  # "uf" (pt), so keeping both aborts rename_with() on a duplicate name.
  # Keep the full name, which is what "state" meant in every
  # pre-Collection-10 mapbiomas output and still means in every other
  # sheet. If a future collection adds a THIRD state-like column (e.g.
  # "state_iso"), this guard won't catch it and rename_with() will abort
  # the same way again.
  if (all(c("state", "state_acronym") %in% names(dat))) {
    dat <- dat %>% dplyr::select(-"state_acronym")
  }

  # FRAGILE: a hardcoded per-dataset structural fork. A future dataset that
  # also ships pre-aggregated/wide data (like water) needs a human to
  # notice this branch exists at all -- nothing here errors on its own if
  # a new dataset silently falls into the wrong side.
  if (param$dataset != "mapbiomas_water") {
    # FRAGILE: substring match, not exact-name match. Drops ANY future
    # column whose name merely CONTAINS "id" (not just literal ID/
    # feature_id) -- a real data column could be silently lost if a
    # collection adds one (e.g. a hypothetical "valid_flag").
    dat <- dat %>%
      dplyr::select(-dplyr::contains("id"))

    # reshaping

    # FRAGILE: assumes year/window columns are always x- or p-prefixed
    # 4-digit (optionally underscore-paired) after janitor::clean_names().
    # Collection 9 gave bare "1985"/"1985_1986", which janitor prefixes to
    # "x1985"/"x1985_1986"; Collection 10's TRANSITION_10 gives
    # "p1985_1986", already letter-initial, so janitor leaves it alone and
    # a plain starts_with("x") selector matched nothing. A future naming
    # convention change (dash-separated windows, a new prefix letter)
    # would silently select ZERO columns here -- drop_na(value) then
    # empties the whole result instead of erroring.
    dat <- dat %>%
      tidyr::pivot_longer(
        tidyselect::matches("^[xp][0-9]{4}(_[0-9]{4})?$"),
        names_to = "year",
        values_to = "value",
        names_prefix = "[xp]"
      ) %>%
      tidyr::drop_na(value)
  } else {
    if (param$geo_level == "municipality") {
      dat <- dat %>%
        dplyr::rename("municipality_code" = "code")
    }
    if (param$geo_level == "biome") {
      # FRAGILE: Collection 4's biome sheet (WATER_BIOME_ANNUAL) is wide
      # (BIOME + one column per year) and has no code/name pair, unlike
      # the city sheet -- pivot it like every non-water dataset instead of
      # renaming columns that aren't there.
      dat <- dat %>%
        tidyr::pivot_longer(
          tidyselect::matches("^[xp][0-9]{4}$"),
          names_to = "year",
          values_to = "value",
          names_prefix = "[xp]"
        ) %>%
        tidyr::drop_na(value)
    }
    if (param$geo_level == "state") {
      dat <- dat %>%
        dplyr::rename("state_code" = "code", "state" = "name")
    }
  }


  if (param$dataset == "mapbiomas_cover" & param$geo_level == "indigenous_land") {
    # FRAGILE: (position-based, not name-based) Collection 9 called this column "territory" and
    # it sat at position 2; Collection 10 calls it "indigenous_territories"
    # and position 2 is now "biome" -- extracting by position parsed biome
    # names with a territory regex and silently produced three all-NA
    # columns. Find it by name instead.
    terr_col <- intersect(c("indigenous_territories", "territory"), names(dat))[1]
    if (!is.na(terr_col)) {
      dat <- dat %>%
        tidyr::extract(
          col = terr_col,
          into = c("territory_name", "territory_sub_name", "territory_code"),
          # FRAGILE: assumes the "Name (sub) (code)" parenthetical format,
          # verified live against "Alto Rio Purus (1201)". If MapBiomas
          # ever drops the trailing code, territory_code silently becomes
          # NA for every row instead of erroring.
          regex = "^(.*?)\\s*(?:\\(([^()]+)\\))?\\s*\\((\\d+)\\)$"
        )
    }
  }

  ################################
  ## Harmonizing Variable Names ##
  ################################

  # FRAGILE: a literal name list. If a collection renames one of these
  # (e.g. "from_class" -> "class_from_label"), the old name is NOT dropped
  # and leaks into the final output silently -- no error either way.
  rm_vars <- c(
    "biome_municipality", "color", "category", "biome_state",
    "to_color", "from_color", "from_class", "to_class",
    "class_lulc", "group"
  )

  dat_mod <- dat %>%
    dplyr::select(
      -dplyr::any_of(c(rm_vars))
    )

  # FRAGILE: case_match() below is an EXACT name match. A future source
  # rename means the intended pt/eng label silently never applies -- the
  # column survives under its raw source name instead of erroring. There
  # is no fallback and no warning.
  if (param$language == "pt") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "municipality" ~ "municipio",
        "city" ~ "municipio",
        "biome" ~ "bioma",
        "geocode" ~ "cod_municipio",
        "geocode_municipality" ~ "cod_municipio",
        "state_acronym" ~ "uf",
        "value" ~ "valor",
        "year" ~ "ano",
        "dr_class_name" ~ "classe_desmatamento",
        "class_irrig" ~ "classe_irrigacao",
        "il" ~ "terra_indigena",
        "municipality_code" ~ "cod_municipio",
        "biome_code" ~ "cod_bioma",
        "state_code" ~ "cod_uf",
        "state" ~ "uf",
        "territory_name" ~ "nome_territorio",
        "territory_subname" ~ "sub_nome_territorio",
        "territory_code" ~ "cod_territorio",
        .default = .
      )) %>%
      # FRAGILE: depends on the literal substrings "to_level"/"from_level"
      # appearing as PREFIXES. Collection 10's transition columns are
      # already suffixed (class_level_N_from/_to, not from_level_N) --
      # this rule may already be silently inert post-migration; worth
      # confirming directly, not just watching for a future break.
      dplyr::rename_with(~ stringr::str_replace(., "to_level", "para_level")) %>%
      dplyr::rename_with(~ stringr::str_replace(., "from_level", "de_level"))
  }

  if (param$language == "eng") {
    dat_mod <- dat_mod %>%
      dplyr::rename_with(~ dplyr::case_match(.,
        "city" ~ "municipality",
        "geocode" ~ "municipality_code",
        "geocode_municipality" ~ "municipality_code",
        "state_acronym" ~ "state",
        "dr_class_name" ~ "deforestation_class",
        "classe_irrig" ~ "irrigation_class",
        "il" ~ "indigenous_land",
        .default = .
      ))
  }

  dat_mod <- dat_mod %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("^class_level_\\d+$"),
        # FRAGILE: assumes labels look like "1.1. Forest Formation"
        # (numeric-dot prefix). A labeling change (no numeric prefix, or a
        # different separator) leaves the prefix in silently, changing
        # displayed values without any error.
        ~ sub("^\\s*\\d+(?:\\.\\d+)*\\.?\\s*", "", as.character(.))
      )
    )

  dat_mod
}
