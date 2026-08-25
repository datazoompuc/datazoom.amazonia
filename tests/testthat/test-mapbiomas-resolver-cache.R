# tests/testthat/test-mapbiomas-resolver-cache.R
#
# Tests for actions/scripts/mapbiomas_resolver_cache.R -- the verification
# cache that lets resolve_mapbiomas.R skip re-downloading a Dataverse
# candidate whose pass/fail verdict is already known, safely (see that
# file's header for the checksum + rules-fingerprint safety property).
#
# actions/ is .Rbuildignore'd -- CI tooling, never shipped in the built
# package. Skipped entirely when absent, same posture as
# test-site-inventory.R and test-mapbiomas-fragility.R.
#
# ---- Scope, stated honestly ------------------------------------------------
#
# This file tests the CACHE MODULE itself in isolation: read/write,
# fingerprinting, and the lookup/upsert safety properties (a changed
# checksum or a changed rules_fingerprint must always miss, never trust a
# stale verdict). That's the part that's genuinely unit-testable without
# network.
#
# It does NOT attempt to mock resolve_mapbiomas.R's dv_file_layout() to
# prove the resolver LOOP actually skips a download on a cache hit --
# dv_file_layout(), dv_dataset_files(), dv_search(), and
# resolve_via_dataverse() are all closures defined INSIDE
# resolve_mapbiomas(rows), never assigned to an accessible top-level
# binding. testthat::local_mocked_bindings() (and any other
# binding-replacement mock) can only intercept a call that's resolved
# through a lookup it can reach; a function that's called from entirely
# within its own enclosing function's local scope has no such seam. Writing
# a test that CLAIMS to verify "no download happens on a cache hit" via a
# mock that can't actually reach the call it's supposed to intercept would
# silently pass regardless of whether the real wiring works -- worse than
# no test at all. That end-to-end property is instead verified live (see
# this feature's plan: a cold run followed by a warm run, comparing wall-
# clock time and asserting the resulting candidate manifest is byte-
# identical either way) -- a real proof, just not an automated one.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "mapbiomas_resolver_cache.R"), local = TRUE)

test_that("read_resolver_cache() returns an empty, correctly-shaped tibble for a missing file", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  expect_equal(nrow(cache), 0)
  expect_equal(names(cache), RESOLVER_CACHE_COLS)
})

test_that("rules_fingerprint() is stable for identical inputs and sensitive to either pattern changing", {
  fp1 <- rules_fingerprint("^COVERAGE", "^geocode$")
  fp2 <- rules_fingerprint("^COVERAGE", "^geocode$")
  expect_identical(fp1, fp2)

  fp_diff_sheet <- rules_fingerprint("^TRANSITION", "^geocode$")
  fp_diff_col <- rules_fingerprint("^COVERAGE", "^municipality_code$")
  fp_no_col <- rules_fingerprint("^COVERAGE", NULL)
  expect_false(identical(fp1, fp_diff_sheet))
  expect_false(identical(fp1, fp_diff_col))
  expect_false(identical(fp1, fp_no_col))
})

test_that("cache_lookup() misses on an empty cache", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")
  expect_null(cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp))
})

test_that("cache_upsert() then cache_lookup() round-trips a verdict, keyed on geo_level", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")

  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp, "fail", NA_character_)
  hit <- cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp)
  expect_equal(hit$verdict, "fail")

  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "254", "def456", fp, "pass", "COVERAGE_10")
  hit2 <- cache_lookup(cache, "mapbiomas_cover", "municipality", "254", "def456", fp)
  expect_equal(hit2$verdict, "pass")
  expect_equal(hit2$sheet, "COVERAGE_10")
  expect_equal(nrow(cache), 2) # two distinct file_ids, two rows
})

test_that("a changed checksum is a cache MISS -- never trusts a verdict for content that's since changed", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")
  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp, "fail", NA_character_)

  expect_null(cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "DIFFERENT_CHECKSUM", fp))
})

test_that("a changed rules_fingerprint is a cache MISS -- never trusts a verdict computed under different rules", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")
  fp_tightened <- rules_fingerprint("^COVERAGE", "^municipality_code$")
  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp, "pass", "COVERAGE_10.1")

  # Same dataset/geo_level/file_id/checksum -- ONLY the rule changed. A
  # maintainer who tightens required_col_pattern must get a real re-check,
  # not a cached "pass" that was true under the old, looser rule.
  expect_null(cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp_tightened))
})

test_that("cache_upsert() replaces an existing (dataset, geo_level, file_id) row rather than duplicating it", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")
  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp, "fail", NA_character_)
  expect_equal(nrow(cache), 1)

  # Same file_id, checksum changed (Dataverse replaced the file's content
  # under the same id) -- must overwrite, not accumulate a second row.
  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "535", "NEWCHECKSUM", fp, "pass", "COVERAGE_10.1")
  expect_equal(nrow(cache), 1)
  expect_null(cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "abc123", fp)) # old checksum: gone
  expect_equal(cache_lookup(cache, "mapbiomas_cover", "municipality", "535", "NEWCHECKSUM", fp)$verdict, "pass")
})

test_that("an unkeyed dataset (geo_level = NULL/NA) round-trips correctly", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^a_ANNUAL", NULL)
  cache <- cache_upsert(cache, "mapbiomas_fire", NULL, "230", "xyz789", fp, "pass", "a_ANNUAL")

  hit <- cache_lookup(cache, "mapbiomas_fire", NULL, "230", "xyz789", fp)
  expect_equal(hit$verdict, "pass")
  # A DIFFERENT dataset with the same NA geo_level must not collide.
  expect_null(cache_lookup(cache, "mapbiomas_deforestation_regeneration", NULL, "230", "xyz789", fp))
})

test_that("write_resolver_cache() then read_resolver_cache() round-trips through disk", {
  cache <- read_resolver_cache(file.path(tempdir(), "does_not_exist_cache.csv"))
  fp <- rules_fingerprint("^COVERAGE", "^geocode$")
  cache <- cache_upsert(cache, "mapbiomas_cover", "municipality", "254", "def456", fp, "pass", "COVERAGE_10")
  cache <- cache_upsert(cache, "mapbiomas_fire", NULL, "230", "xyz789", rules_fingerprint("^a_ANNUAL", NULL), "pass", "a_ANNUAL")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_resolver_cache(cache, tmp)

  reread <- read_resolver_cache(tmp)
  expect_equal(nrow(reread), nrow(cache))
  hit <- cache_lookup(reread, "mapbiomas_cover", "municipality", "254", "def456", fp)
  expect_equal(hit$verdict, "pass")
  expect_equal(hit$sheet, "COVERAGE_10")
})
