# One-off (but re-runnable) migration: the INVERSE of normalize_manifest.R.
# Restructures the manifest from the 5-tier field-coalescing schema (survey
# default -> dataset base row -> geo_level/year overrides, each cell
# inheriting from the tier below when blank) to fully self-sufficient rows,
# per the "self-sufficient rows + PR-only refresh" plan
# (how-come-the-manifest-witty-nest.md). No value is typed by hand -- every
# change is a mechanical transformation of the existing CSV, so the result
# is auditable and the script can be re-run.
#
# What it does, in order:
#   1. For each of the 12 survey-default rows (dataset = NA), copies every
#      non-NA value down onto every dataset row of that survey that doesn't
#      already have its own value for that column, then drops the default
#      row entirely. This is the exact inverse of normalize_manifest.R's
#      step 5 (the hoist) -- undoing it, not re-deriving it independently,
#      keeps every value byte-identical to what dataset_field() already
#      resolved under the old 5-tier code (see
#      tests/testthat/fixtures/resolution_matrix_tiered.rds, captured
#      BEFORE this script ran).
#   2. For each geo_level/year override row, fills every blank value cell
#      from its own dataset's base row. Inverse of normalize_manifest.R's
#      step 6 (the blank-if-duplicate pass).
#   3. Adds 3 rows for (survey, dataset, geo_level) combinations that
#      dataset_field()'s old tier-2-miss fallthrough answered from the base
#      row, but which the new exact-key-match lookup needs an explicit row
#      for (see R/manifest.R and the plan's "3 missing rows" table):
#        - epe / consumer_energy_consumption / region
#        - epe / industrial_energy_consumption / region
#        - mapbiomas / mapbiomas_cover / municipality
#      Each is seeded as a full copy of its dataset's (now fully-populated)
#      base row, geo_level set, year left NA.
#   4. Reorders (survey, dataset, geo_level, year) and writes.

library(dplyr)
library(readr)
library(tibble)

MANIFEST_PATH <- "inst/extdata/manifest/v1/datasets_link.csv"

VALUE_COLS <- c(
  "sidra_code", "url", "docs_url", "available_time", "available_geo",
  "archive_file", "sheet", "layer_name", "version", "resolver"
)

old <- read_csv(
  MANIFEST_PATH,
  col_types = cols(.default = col_character()),
  na = c("", "NA"),
  locale = locale(encoding = "UTF-8")
)

# ---- 1. copy survey defaults down, then drop them ----------------------------
#
# Filled ONLY onto each dataset's BASE row (geo_level = year = NA), never
# directly onto override rows -- under the old 5-tier order a survey
# default (tier 5) was the LAST fallback, reached only after a dataset's
# own base row (tier 4) AND that dataset's geo_level/year override (tier
# 2/3) both came up blank. Filling an override row straight from the
# survey default here would skip tier 4 entirely and use the wrong source
# whenever the base row had its own value the override was actually
# supposed to inherit (step 2 below fills overrides from their base row,
# but only AFTER this step has finished settling what that base row's
# value actually is).

defaults <- old %>% filter(is.na(dataset))
rest <- old %>% filter(!is.na(dataset))
is_base <- is.na(rest$geo_level) & is.na(rest$year)

n_filled_from_default <- 0

for (s in defaults$survey) {
  def_row <- defaults[defaults$survey == s, ]
  idx <- which(rest$survey == s & is_base)
  if (length(idx) == 0) next

  for (col in VALUE_COLS) {
    dv <- def_row[[col]][1]
    if (is.na(dv)) next
    blank <- idx[is.na(rest[[col]][idx])]
    if (length(blank) == 0) next
    rest[[col]][blank] <- dv
    n_filled_from_default <- n_filled_from_default + length(blank)
  }
}

cat(sprintf(
  "step 1: filled %d cell(s) from %d survey-default row(s), then dropped them\n",
  n_filled_from_default, nrow(defaults)
))

# ---- 2. fill override rows from their own dataset's base row -----------------

base_idx <- is.na(rest$geo_level) & is.na(rest$year)
base <- rest[base_idx, ]
overrides <- rest[!base_idx, ]

n_filled_from_base <- 0

for (i in seq_len(nrow(overrides))) {
  s <- overrides$survey[i]
  d <- overrides$dataset[i]
  b <- base[base$survey == s & base$dataset == d, ]
  if (nrow(b) != 1) next

  for (col in VALUE_COLS) {
    if (is.na(overrides[[col]][i]) && !is.na(b[[col]])) {
      overrides[[col]][i] <- b[[col]]
      n_filled_from_base <- n_filled_from_base + 1
    }
  }
}

cat(sprintf("step 2: filled %d cell(s) on override rows from their base row\n", n_filled_from_base))

# ---- 3. add the 3 rows the old tier-2-miss fallthrough used to answer --------

gap_specs <- tribble(
  ~survey, ~dataset, ~geo_level,
  "epe", "consumer_energy_consumption", "region",
  "epe", "industrial_energy_consumption", "region",
  "mapbiomas", "mapbiomas_cover", "municipality"
)

gap_rows <- list()
for (i in seq_len(nrow(gap_specs))) {
  s <- gap_specs$survey[i]
  d <- gap_specs$dataset[i]
  g <- gap_specs$geo_level[i]
  b <- base[base$survey == s & base$dataset == d, ]
  stopifnot(nrow(b) == 1)

  new_row <- b
  new_row$geo_level <- g
  gap_rows[[i]] <- new_row
}
gap_rows <- bind_rows(gap_rows)

cat(sprintf("step 3: added %d row(s) for tier-2-miss combinations\n", nrow(gap_rows)))

# ---- 4. reassemble, order, write ---------------------------------------------

result <- bind_rows(base, overrides, gap_rows) %>%
  arrange(survey, dataset, coalesce(geo_level, ""), coalesce(year, "")) %>%
  select(survey, dataset, geo_level, year, all_of(VALUE_COLS))

stopifnot(
  # every original dataset must still be present
  all(unique(old$dataset[!is.na(old$dataset)]) %in% result$dataset),
  # no dataset = NA row survives
  !any(is.na(result$dataset)),
  # no duplicate (survey, dataset, geo_level, year) key
  !any(duplicated(result[, c("survey", "dataset", "geo_level", "year")]))
)

write_csv(result, MANIFEST_PATH, na = "")

cat(sprintf(
  "wrote %d rows (%d base rows, %d overrides) to %s\n",
  nrow(result),
  sum(is.na(result$geo_level) & is.na(result$year)),
  sum(!is.na(result$geo_level) | !is.na(result$year)),
  MANIFEST_PATH
))

filled_before <- sum(!is.na(old %>% select(all_of(VALUE_COLS))))
filled_after <- sum(!is.na(result %>% select(all_of(VALUE_COLS))))
cat(sprintf("filled value cells: %d -> %d\n", filled_before, filled_after))
