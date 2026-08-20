# Structural tests for the keyed/unkeyed manifest schema (see R/manifest.R
# and the "delete manifest base rows" migration, 2026-08-19). These guard
# the invariants dataset_field()/dataset_meta()'s exact-key lookup and
# agreed-value collapse depend on:
#
#   1. No survey-default row (dataset == NA) exists -- every row is a real
#      dataset row, so there is nothing left to disambiguate.
#   2. No (survey, dataset, geo_level, year) key is duplicated.
#   3. A (survey, dataset) group is either UNKEYED (exactly one row, blank
#      key) or KEYED (every row has a real geo_level or year) -- NEVER
#      both. This replaces the old "at most one base row" test: a base row
#      (blank key) mixed with real override rows is no longer just
#      redundant, it is not allowed at all (see R/manifest.R's header for
#      why -- a base row could silently drift stale relative to its own
#      overrides, confirmed live for mapbiomas_mining before this
#      migration).
#   4. Every geo_level/year a keyed dataset's rows collectively declare
#      (via available_geo/available_time) has an actual row backing it,
#      AND every row that exists is actually declared -- there is no more
#      fallthrough to catch a missing one at read time, and no base row
#      left to read the declaration off of (the declaration is now the
#      AGREED value across the dataset's own rows).
#   5. Every field dataset_meta() is allowed to read (MANIFEST_META_COLS)
#      actually agrees within each keyed dataset -- this is what makes
#      dataset_meta()'s stop() unreachable against a manifest that passes
#      this test (one narrow, documented exception: see
#      KNOWN_META_DISAGREEMENTS below).
#   6. Redundancy across a dataset's own rows is EXPECTED, not forbidden --
#      this replaces the old anti-duplication tests, which required the
#      opposite (see R/manifest.R's schema note).

manifest <- link_table()

manifest_groups <- function(m) {
  unique(m[!is.na(m$dataset), c("survey", "dataset")])
}

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

test_that("no (survey, dataset, geo_level, year) key is duplicated", {
  keys <- manifest[, c("survey", "dataset", "geo_level", "year")]
  expect_false(any(duplicated(keys)))
})

test_that("a dataset is either unkeyed (one blank-key row) or keyed (no blank-key row), never both", {
  # Rewritten from "at most one dataset-base row exists per (survey,
  # dataset)" -- that test only checked for DUPLICATE base rows, which
  # silently passed (vacuously) once base rows started being removed one
  # migration at a time; it never actually verified the row IS gone for a
  # keyed dataset. This checks the real invariant directly.
  groups <- manifest_groups(manifest)
  offenders <- character(0)

  for (i in seq_len(nrow(groups))) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- manifest[manifest$survey == s & !is.na(manifest$dataset) & manifest$dataset == d, ]
    has_blank <- any(is.na(rows$geo_level) & is.na(rows$year))
    has_real <- any(!is.na(rows$geo_level) | !is.na(rows$year))

    if (has_blank && has_real) {
      offenders <- c(offenders, paste0(s, "/", d, ": mixes a blank-key row with real override rows"))
    }
    if (has_blank && nrow(rows) > 1) {
      offenders <- c(offenders, paste0(s, "/", d, ": unkeyed dataset has more than one row"))
    }
  }

  expect_equal(offenders, character(0))
})

test_that("no dataset mixes geo_level and year overrides", {
  # Rewritten to iterate (survey, dataset) GROUPS directly instead of
  # looping over base rows to find their overrides -- the base-row loop
  # would silently check nothing at all for any dataset that no longer has
  # one, which is exactly the "stays green while testing nothing" failure
  # mode this migration's own plan flagged as the highest risk.
  groups <- manifest_groups(manifest)
  offenders <- character(0)

  for (i in seq_len(nrow(groups))) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- manifest[manifest$survey == s & !is.na(manifest$dataset) & manifest$dataset == d, ]
    if (any(!is.na(rows$geo_level)) && any(!is.na(rows$year))) {
      offenders <- c(offenders, paste(s, d, sep = "/"))
    }
  }

  expect_equal(offenders, character(0))
})

test_that("every keyed dataset's declared geo_level/year set exactly matches its rows (both directions)", {
  # Rewritten from a base-row-reading version (the declaration used to
  # live on ONE row; now it's the AGREED value across the dataset's own
  # rows -- see agreed_value() in R/manifest.R). Also adds the reverse
  # check the old version never had: a row existing that ISN'T declared is
  # now caught too, not just a declared value missing its row.
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

  agreed <- function(x) {
    vals <- unique(x[!is.na(x)])
    if (length(vals) == 1) vals else NA_character_
  }

  groups <- manifest_groups(manifest)
  offenders <- character(0)

  for (i in seq_len(nrow(groups))) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- manifest[manifest$survey == s & !is.na(manifest$dataset) & manifest$dataset == d, ]
    if (nrow(rows) <= 1) next

    if (any(!is.na(rows$geo_level))) {
      declared_raw <- agreed(rows$available_geo)
      if (!is.na(declared_raw)) {
        declared <- tolower(trimws(strsplit(declared_raw, ",")[[1]]))
        declared <- declared[nzchar(declared)]
        present <- tolower(rows$geo_level[!is.na(rows$geo_level)])
        missing <- setdiff(declared, present)
        extra <- setdiff(present, declared)
        if (length(missing) > 0) offenders <- c(offenders, paste(s, d, "geo_level missing", paste(missing, collapse = "+"), sep = "/"))
        if (length(extra) > 0) offenders <- c(offenders, paste(s, d, "geo_level undeclared", paste(extra, collapse = "+"), sep = "/"))
      }
    }

    if (any(!is.na(rows$year))) {
      declared_raw <- agreed(rows$available_time)
      if (!is.na(declared_raw)) {
        declared <- parse_years_local(declared_raw)
        present <- as.integer(rows$year[!is.na(rows$year)])
        missing <- setdiff(declared, present)
        extra <- setdiff(present, declared)
        if (length(missing) > 0) offenders <- c(offenders, paste(s, d, "year missing", paste(missing, collapse = "+"), sep = "/"))
        if (length(extra) > 0) offenders <- c(offenders, paste(s, d, "year undeclared", paste(extra, collapse = "+"), sep = "/"))
      }
    }
  }

  expect_equal(offenders, character(0))
})

test_that("every MANIFEST_META_COLS field agrees within each keyed dataset (one documented exception)", {
  # New: this is what makes dataset_meta()'s stop() (R/manifest.R)
  # unreachable against a manifest that passes this test -- mirrors
  # actions/scripts/manifest_validate.R's validate_dataset_level_agreement()
  # exactly, including its one narrow, deliberate exception (see that
  # file's KNOWN_META_DISAGREEMENTS for why mapbiomas_transition's
  # available_time is allowed to differ: its municipality row is sourced
  # from an older collection with genuinely shorter time coverage than its
  # siblings, and no code reads that specific (dataset, field) pair
  # without a key today).
  meta_cols <- c("sidra_code", "available_time", "available_geo", "layer_name", "resolver")
  known_exceptions <- list(
    list(survey = "mapbiomas", dataset = "mapbiomas_transition", field = "available_time")
  )
  is_known_exception <- function(s, d, f) {
    any(vapply(known_exceptions, function(k) {
      identical(k$survey, s) && identical(k$dataset, d) && identical(k$field, f)
    }, logical(1)))
  }

  groups <- manifest_groups(manifest)
  offenders <- character(0)

  for (i in seq_len(nrow(groups))) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- manifest[manifest$survey == s & !is.na(manifest$dataset) & manifest$dataset == d, ]
    if (nrow(rows) <= 1) next

    for (col in meta_cols) {
      if (is_known_exception(s, d, col)) next
      vals <- unique(rows[[col]][!is.na(rows[[col]])])
      if (length(vals) > 1) {
        offenders <- c(offenders, paste(s, d, col, sep = "/"))
      }
    }
  }

  expect_equal(offenders, character(0))
})

test_that("dataset_field() stops when a keyed dataset is queried without its key", {
  # New: the core runtime guard this migration adds. A keyed dataset called
  # with neither geo_level nor year used to silently resolve the (now
  # deleted) base row -- it now stop()s instead, per dataset (geo_level-
  # keyed vs. year-keyed).
  expect_error(dataset_field("mapbiomas", "mapbiomas_cover", "url"), "keyed by geo_level")
  expect_error(dataset_field("aneel", "energy_development_budget", "url"), "keyed by year")

  # An unkeyed dataset must be completely unaffected -- it has no base row
  # to lose, it just has one real row.
  expect_false(is.na(dataset_field("degrad", "degrad", "url", year = 2010)))
})
