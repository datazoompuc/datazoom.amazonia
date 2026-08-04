# One-off (but re-runnable) migration: restructures the manifest from
# "one self-contained row per dataset/override" to the 5-tier field-
# coalescing schema (see R/manifest.R and the "Normalizar o schema do
# manifest" plan). No value is typed by hand here -- every change is a
# mechanical transformation of the existing CSV, so the result is auditable
# and the script can be re-run if the pre-normalization CSV is ever needed
# again (e.g. to regenerate the golden fixtures in
# tests/testthat/fixtures/resolution_matrix_pre.rds).
#
# What it does, in order:
#   1. Renames "link" -> "url" and "collection" -> "version" (see the plan's
#      column dictionary for why: "link" conflated a real download URL with
#      a SIDRA documentation landing page; "version" generalizes beyond
#      MapBiomas).
#   2. Splits the ~100 SIDRA rows' landing-page-only "link" into the new
#      "docs_url" column, leaving "url" NA (SIDRA data is never downloaded
#      by URL -- it comes from the sidrar API via sidra_code).
#   3. Lowercases "available_geo" (check_params.R already did this at read
#      time; normalizing storage removes the need to trust every writer to
#      also lowercase, and lets values now match for hoisting below).
#   4. Adds the 4 EPE per-geo_level "sheet" override rows that used to be a
#      hardcoded if/else ladder in R/epe.R (consumer/industrial energy
#      consumption x state/subsystem) -- the same override pattern already
#      used for MapBiomas.
#   5. Hoists a value column to a new "survey default" row (dataset = NA)
#      when a MAJORITY (>50%, and at least 2 rows) of a survey's dataset
#      rows share the identical value -- e.g. every PAM crop points at the
#      same SIDRA docs page, every TerraClimate variable shares one host.
#      Datasets whose value differs keep it explicit on their own row,
#      overriding the default (5-tier resolution walks the dataset's own
#      row before the survey default). `sidra_code` and `resolver` are
#      deliberately never hoisted: they identify/tag a specific row, they
#      are not a "shared default" in the same sense (see the plan's Risk
#      #1: hoisting `resolver` would make an unresolved dataset silently
#      "inherit" a resolver's ownership through NA-means-inherit coalescing).
#   6. Blanks any geo_level/year override cell whose value is identical to
#      its OWN dataset's base-row value -- this is what collapses DEGRAD's
#      10 repeated URLs and ANEEL's already-lean year rows into the same
#      shape (see the plan's "Modelo de resolução" section).

library(dplyr)
library(readr)
library(tibble)

MANIFEST_PATH <- "inst/extdata/manifest/v1/datasets_link.csv"

VALUE_COLS <- c(
  "sidra_code", "url", "docs_url", "available_time", "available_geo",
  "archive_file", "sheet", "layer_name", "version", "resolver"
)
# resolver/sidra_code are per-row identifiers, never hoisted to a shared
# survey default (see step 5 above).
HOIST_ELIGIBLE_COLS <- setdiff(VALUE_COLS, c("sidra_code", "resolver"))

old <- read_csv(
  MANIFEST_PATH,
  col_types = cols(.default = col_character()),
  na = c("", "NA"),
  locale = locale(encoding = "UTF-8")
)

# ---- 1. rename --------------------------------------------------------------

old <- old %>% rename(url = link, version = collection)

# ---- 2. split SIDRA docs pages ----------------------------------------------

old$docs_url <- NA_character_
is_sidra <- !is.na(old$sidra_code)
old$docs_url[is_sidra] <- old$url[is_sidra]
old$url[is_sidra] <- NA_character_

# ---- 3. lowercase available_geo ---------------------------------------------

old$available_geo <- tolower(old$available_geo)

# ---- 4. add the 4 EPE sheet-by-geo_level override rows ----------------------
# Literal values are the ones previously hardcoded in R/epe.R's if/else
# ladder for consumer_energy_consumption / industrial_energy_consumption.

epe_sheet_overrides <- tribble(
  ~survey, ~dataset, ~geo_level, ~sheet,
  "epe", "consumer_energy_consumption", "state", "CONSUMO E NUMCONS SAM UF",
  "epe", "consumer_energy_consumption", "subsystem", "CONSUMO E NUMCONS SAM",
  "epe", "industrial_energy_consumption", "state", "SETOR INDUSTRIAL POR UF",
  "epe", "industrial_energy_consumption", "subsystem", "SETOR INDUSTRIAL POR RG"
) %>%
  mutate(year = NA_character_) %>%
  bind_rows(tibble(!!!setNames(as.list(rep(NA_character_, length(VALUE_COLS))), VALUE_COLS))[0, ]) %>%
  select(survey, dataset, geo_level, year, all_of(VALUE_COLS))

old <- bind_rows(old, epe_sheet_overrides)

# ---- 5. hoist shared values to survey-default rows --------------------------

base_idx <- !is.na(old$dataset) & is.na(old$geo_level) & is.na(old$year)
base <- old[base_idx, ]
overrides <- old[!base_idx, ]

# keep the PRE-hoist base values around -- step 6 needs them to recognize
# an override cell that duplicates its dataset's original value, even after
# that same value has been blanked out of the base row by hoisting.
orig_base <- base %>% select(survey, dataset, all_of(VALUE_COLS))

default_list <- list()

for (s in unique(base$survey)) {
  idx <- which(base$survey == s)
  if (length(idx) < 2) next # single-dataset survey: nothing to hoist

  for (col in HOIST_ELIGIBLE_COLS) {
    vals <- base[[col]][idx]
    non_na <- vals[!is.na(vals)]
    if (length(non_na) < 2) next

    # Require EVERY dataset row to have a value (no NA holes) before
    # hoisting is even considered. A dataset whose value is genuinely NA
    # -- not "missing", but meaning something (e.g. EPE's
    # national_energy_balance has available_geo = NA because it is NOT
    # restricted to any geo_level, checked via is.na() in
    # check_params.R:85) -- must never start silently inheriting a
    # sibling's value just because coalescing treats an empty cell as
    # "inherit". This is the one gap in "empty cell = inherit" the plan's
    # Risk #1 warns about; every hoist candidate found by inspection
    # happened to already be fully populated, so this costs nothing today
    # and only prevents a future regression.
    if (length(non_na) < length(idx)) next

    tab <- sort(table(non_na), decreasing = TRUE)
    top_val <- names(tab)[1]
    top_n <- unname(tab[1])

    if (top_n >= 2 && (top_n / length(non_na)) > 0.5) {
      if (is.null(default_list[[s]])) default_list[[s]] <- list()
      default_list[[s]][[col]] <- top_val
      match_idx <- idx[!is.na(vals) & vals == top_val]
      base[[col]][match_idx] <- NA_character_
    }
  }
}

default_rows <- if (length(default_list) > 0) {
  bind_rows(lapply(names(default_list), function(s) {
    row <- setNames(as.list(rep(NA_character_, length(VALUE_COLS))), VALUE_COLS)
    row[names(default_list[[s]])] <- default_list[[s]]
    tibble(survey = s, dataset = NA_character_, geo_level = NA_character_, year = NA_character_) %>%
      bind_cols(as_tibble(row))
  }))
} else {
  NULL
}

cat(sprintf(
  "hoisted %d field(s) across %d survey default row(s)\n",
  sum(vapply(default_list, length, integer(1))), length(default_list)
))

# ---- 6. blank override cells that duplicate their dataset's base value ------

for (i in seq_len(nrow(overrides))) {
  s <- overrides$survey[i]
  d <- overrides$dataset[i]
  ob <- orig_base[orig_base$survey == s & orig_base$dataset == d, ]
  if (nrow(ob) != 1) next

  for (col in VALUE_COLS) {
    ov <- overrides[[col]][i]
    bv <- ob[[col]]
    if (!is.na(ov) && !is.na(bv) && identical(ov, bv)) {
      overrides[[col]][i] <- NA_character_
    }
  }
}

# ---- 7. reassemble, order, write ---------------------------------------------
#
# Row order within a survey: the survey-default row (dataset = NA) first,
# then for each dataset its own base row (geo_level = year = NA) BEFORE its
# geo_level/year overrides. dplyr::arrange() sorts NA last by default, which
# would put every override row ahead of the base row it overrides (e.g.
# ANEEL's 2017-2022 rows ahead of the row they're overriding) -- the
# explicit `is_override` key below overrides that default so the "mother"
# row always reads first, exactly the way a human editing this file would
# expect to find it.

result <- bind_rows(default_rows, base, overrides) %>%
  mutate(is_override = !is.na(geo_level) | !is.na(year)) %>%
  arrange(survey, !is.na(dataset), dataset, is_override, geo_level, year) %>%
  select(survey, dataset, geo_level, year, all_of(VALUE_COLS))

stopifnot(
  # every original (survey, dataset) pair must still resolve to a base row
  all(unique(old$dataset[base_idx]) %in% result$dataset[!is.na(result$dataset)])
)

write_csv(result, MANIFEST_PATH, na = "")

cat(sprintf(
  "wrote %d rows (%d survey defaults, %d dataset base rows, %d overrides) to %s\n",
  nrow(result),
  sum(is.na(result$dataset)),
  sum(!is.na(result$dataset) & is.na(result$geo_level) & is.na(result$year)),
  sum(!is.na(result$dataset) & (!is.na(result$geo_level) | !is.na(result$year))),
  MANIFEST_PATH
))

filled_before <- sum(!is.na(old %>% select(all_of(VALUE_COLS))))
filled_after <- sum(!is.na(result %>% select(all_of(VALUE_COLS))))
cat(sprintf("filled value cells: %d -> %d\n", filled_before, filled_after))
