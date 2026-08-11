# Structural tests for the self-sufficient-rows manifest schema (see
# R/manifest.R and the "self-sufficient rows + PR-only refresh" plan). These
# guard the invariants dataset_field()'s single exact-key lookup depends on:
#
#   1. No survey-default row (dataset == NA) exists -- every row is a real
#      dataset row, so there is nothing left to disambiguate.
#   2. No (survey, dataset, geo_level, year) key is duplicated.
#   3. Every geo_level/year value a dataset's base row DECLARES (via
#      available_geo/available_time) has an actual row backing it -- there
#      is no more fallthrough to catch a missing one at read time (this is
#      what used to silently work via 5-tier inheritance and is now a hard
#      requirement instead).
#   4. Redundancy across a dataset's own rows is now EXPECTED, not
#      forbidden -- this replaces the old anti-duplication tests, which
#      actively required the opposite.

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

test_that("no survey-default row (dataset = NA) exists", {
  expect_false(any(is.na(manifest$dataset)))
})

test_that("at most one dataset-base row exists per (survey, dataset)", {
  base <- manifest[is.na(manifest$geo_level) & is.na(manifest$year), ]
  expect_false(any(duplicated(base[, c("survey", "dataset")])))
})

test_that("no (survey, dataset, geo_level, year) key is duplicated", {
  keys <- manifest[, c("survey", "dataset", "geo_level", "year")]
  expect_false(any(duplicated(keys)))
})

test_that("every geo_level/year a dataset declares has a matching row", {
  # Mirrors actions/scripts/manifest_validate.R's validate_key_completeness()
  # -- run again here so a hand-edited manifest snapshot (not just a
  # resolver-generated candidate) is caught by the regular test suite too.
  parse_years_local <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(integer(0))
    }
    toks <- trimws(strsplit(x, ",")[[1]])
    unlist(lapply(toks, function(tok) {
      if (grepl("-", tok, fixed = TRUE)) {
        b <- as.integer(trimws(strsplit(tok, "-", fixed = TRUE)[[1]]))
        seq.int(b[1], b[2])
      } else {
        as.integer(tok)
      }
    }))
  }

  base <- manifest[is.na(manifest$geo_level) & is.na(manifest$year), ]
  overrides <- manifest[!is.na(manifest$geo_level) | !is.na(manifest$year), ]

  offenders <- character(0)
  for (i in seq_len(nrow(base))) {
    s <- base$survey[i]
    d <- base$dataset[i]
    ov <- overrides[overrides$survey == s & overrides$dataset == d, ]
    if (nrow(ov) == 0) next

    if (any(!is.na(ov$geo_level))) {
      declared <- base$available_geo[i]
      declared <- if (is.na(declared)) character(0) else trimws(strsplit(declared, ",")[[1]])
      missing <- setdiff(tolower(declared[nzchar(declared)]), tolower(ov$geo_level[!is.na(ov$geo_level)]))
      if (length(missing) > 0) offenders <- c(offenders, paste(s, d, "geo_level", paste(missing, collapse = "+"), sep = "/"))
    }
    if (any(!is.na(ov$year))) {
      declared <- parse_years_local(base$available_time[i])
      missing <- setdiff(declared, as.integer(ov$year[!is.na(ov$year)]))
      if (length(missing) > 0) offenders <- c(offenders, paste(s, d, "year", paste(missing, collapse = "+"), sep = "/"))
    }
  }

  expect_equal(offenders, character(0))
})

test_that("no dataset mixes geo_level and year overrides", {
  base <- manifest[is.na(manifest$geo_level) & is.na(manifest$year), ]
  overrides <- manifest[!is.na(manifest$geo_level) | !is.na(manifest$year), ]

  offenders <- character(0)
  for (i in seq_len(nrow(base))) {
    ov <- overrides[overrides$survey == base$survey[i] & overrides$dataset == base$dataset[i], ]
    if (any(!is.na(ov$geo_level)) && any(!is.na(ov$year))) {
      offenders <- c(offenders, paste(base$survey[i], base$dataset[i], sep = "/"))
    }
  }

  expect_equal(offenders, character(0))
})
