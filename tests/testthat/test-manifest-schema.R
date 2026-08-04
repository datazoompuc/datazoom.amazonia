# Structural tests for the normalized manifest schema (5-tier field
# coalescing -- see R/manifest.R and the "Normalizar o schema do manifest"
# plan). These guard the two invariants the whole redesign depends on:
#
#   1. At most one survey-default row (dataset == NA) per survey -- two
#      would make resolution ambiguous (which one does a dataset inherit
#      from?).
#   2. No cell duplicates the value it would already inherit from the tier
#      below it -- this is the test that stops the original defect (DEGRAD
#      repeating its URL ten times, MapBiomas repeating available_time on
#      every geo_level override) from creeping back in.

manifest <- link_table()

test_that("the packaged manifest has the expected 14-column schema", {
  expect_equal(
    names(manifest),
    c(
      "survey", "dataset", "geo_level", "year", "sidra_code",
      "url", "docs_url", "available_time", "available_geo",
      "archive_file", "sheet", "layer_name", "version", "resolver"
    )
  )
  expect_true(all(vapply(manifest, is.character, logical(1))))
})

test_that("at most one survey-default row exists per survey", {
  defaults <- manifest[is.na(manifest$dataset), ]
  expect_false(any(duplicated(defaults$survey)))
})

test_that("at most one dataset-base row exists per (survey, dataset)", {
  base <- manifest[!is.na(manifest$dataset) & is.na(manifest$geo_level) & is.na(manifest$year), ]
  expect_false(any(duplicated(base[, c("survey", "dataset")])))
})

test_that("no (survey, dataset, geo_level, year) key is duplicated", {
  keys <- manifest[, c("survey", "dataset", "geo_level", "year")]
  expect_false(any(duplicated(keys)))
})

test_that("no dataset-base row duplicates a value its survey default already provides", {
  # This is the anti-regression test: it fails the moment a dataset row
  # repeats a value that is only there because the survey default already
  # supplies it -- exactly the duplication data-raw/normalize_manifest.R
  # collapsed, and exactly what would silently creep back in if someone
  # hand-edited a dataset row to "be explicit" instead of leaving it blank.
  value_cols <- c(
    "sidra_code", "url", "docs_url", "available_time", "available_geo",
    "archive_file", "sheet", "layer_name", "version", "resolver"
  )

  base <- manifest[!is.na(manifest$dataset) & is.na(manifest$geo_level) & is.na(manifest$year), ]
  defaults <- manifest[is.na(manifest$dataset), ]

  offenders <- character(0)
  for (i in seq_len(nrow(base))) {
    def <- defaults[defaults$survey == base$survey[i], ]
    if (nrow(def) != 1) next
    for (col in value_cols) {
      if (!is.na(base[[col]][i]) && !is.na(def[[col]]) && identical(base[[col]][i], def[[col]])) {
        offenders <- c(offenders, paste(base$survey[i], base$dataset[i], col, sep = "/"))
      }
    }
  }

  expect_equal(offenders, character(0))
})

test_that("no geo_level/year override row duplicates a value its own dataset row already provides", {
  value_cols <- c(
    "sidra_code", "url", "docs_url", "available_time", "available_geo",
    "archive_file", "sheet", "layer_name", "version", "resolver"
  )

  base <- manifest[!is.na(manifest$dataset) & is.na(manifest$geo_level) & is.na(manifest$year), ]
  overrides <- manifest[!is.na(manifest$dataset) & (!is.na(manifest$geo_level) | !is.na(manifest$year)), ]

  offenders <- character(0)
  for (i in seq_len(nrow(overrides))) {
    b <- base[base$survey == overrides$survey[i] & base$dataset == overrides$dataset[i], ]
    if (nrow(b) != 1) next
    for (col in value_cols) {
      if (!is.na(overrides[[col]][i]) && !is.na(b[[col]]) && identical(overrides[[col]][i], b[[col]])) {
        offenders <- c(offenders, paste(
          overrides$survey[i], overrides$dataset[i],
          overrides$geo_level[i], overrides$year[i], col,
          sep = "/"
        ))
      }
    }
  }

  expect_equal(offenders, character(0))
})
