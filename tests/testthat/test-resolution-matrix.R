# RETIRED 2026-08-19 by the "delete manifest base rows" migration -- see
# R/manifest.R and tests/testthat/test-resolution-matrix-v2.R, which
# replaces this file's job.
#
# This file used to be the behavior-preservation golden for the
# self-sufficient-rows migration: resolution_matrix_tiered.rds (captured
# by data-raw/capture_resolution_matrix.R, itself kept for provenance) asked
# dataset_field() for every (survey, dataset, geo_level, year, field)
# combination reachable from real R/ code -- INCLUDING a blank-key
# (geo_level = NULL, year = NULL) query for every dataset, keyed or not,
# since at capture time that query legitimately resolved to the dataset's
# base row.
#
# The base-row-deletion migration makes that exact query type -- a blank
# key against a KEYED dataset -- a hard stop() (see R/manifest.R's
# dataset_field()). Replaying resolution_matrix_tiered.rds's ~90 blank-key
# entries for the 9 datasets that used to have a base row would all now
# "fail" -- but every one of those failures is the new guard correctly
# firing, not a regression. Patching around that with more case_when
# exceptions here would be exactly the kind of mismatch the manifest-
# maintenance skill says to go fix (or, here, retire) rather than paper
# over: this fixture's blank-key entries test a query shape the migration
# deliberately made illegal, not a value the migration might have gotten
# wrong.
#
# resolution_matrix_tiered.rds and capture_resolution_matrix.R are left in
# place for historical reference (they document what the OLD 5-tier
# resolver returned, prior to two later migrations), but nothing replays
# them anymore. test-resolution-matrix-v2.R (fixture:
# resolution_matrix_keyed.rds, generator: data-raw/
# capture_resolution_matrix_v2.R) is the current behavior-preservation
# golden -- captured with the same real-row-only query set this migration
# actually needed, and it explicitly asserts the ~90 former-base-row
# queries now stop() instead of silently expecting them to still resolve.

test_that("resolution_matrix_tiered.rds is retired -- see test-resolution-matrix-v2.R", {
  skip("Retired 2026-08-19: this fixture's blank-key queries against now-keyed datasets are exactly what the base-row-deletion migration made illegal. See test-resolution-matrix-v2.R.")
})
