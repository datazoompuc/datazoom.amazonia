# tests/testthat/test-mapbiomas-fragility.R
#
# Smoke test for actions/scripts/mapbiomas_fragility.R's
# collect_mapbiomas_fragility_notes() -- the function build_manifest.R uses
# to list, by line number, which parts of R/mapbiomas.R's mapbiomas_treat()
# a reviewer should re-check whenever a scheduled run changes a MapBiomas
# manifest row (see that function's header and NEWS.md for the full story).
#
# actions/ is .Rbuildignore'd -- CI tooling, never shipped in the built
# package. Skipped entirely when absent, same posture as
# test-site-inventory.R.
#
# This does NOT re-verify every individual fragile point (test-mapbiomas-
# schema.R already exercises the actual behavior each one describes). It
# only guards against the tagging mechanism itself silently breaking --
# e.g. someone deletes a "# FRAGILE:" comment during a future refactor
# without deleting the code it warned about. That specific failure mode
# (tag removed, risk still there) isn't caught by any assertion here either
# -- it's caught by code review noticing the comment count drop in the
# diff. What this test catches is the mechanical stuff: the tag format
# assumption drifting, or the collector erroring outright.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "mapbiomas_fragility.R"), local = TRUE)

mapbiomas_r_path <- file.path(pkg_root, "R", "mapbiomas.R")

test_that("collect_mapbiomas_fragility_notes() finds every FRAGILE tag currently in R/mapbiomas.R", {
  notes <- collect_mapbiomas_fragility_notes(mapbiomas_r_path)

  # 11 tags were placed as of this fix (state_acronym guard, per-dataset
  # structural fork, id-substring match, year-pivot regex, water/biome
  # pivot, territory-by-position, territory regex format, rm_vars literal
  # list, case_match exact-match, to_level/from_level substring rename,
  # class_level numeric-prefix regex) -- asserting >= 8 (not an exact
  # count) so a future maintainer adding MORE tags doesn't have to also
  # update this number, while still catching the tagging mechanism going
  # silent entirely.
  expect_gte(length(notes), 8)

  # Every note must actually carry a real line number pointing somewhere
  # inside the file, and the explanatory text (not just the tag).
  expect_true(all(grepl("^R/mapbiomas\\.R:[0-9]+ -- .+", notes)))

  n_lines <- length(readLines(mapbiomas_r_path, warn = FALSE))
  line_nos <- as.integer(sub("^R/mapbiomas\\.R:([0-9]+) --.*$", "\\1", notes))
  expect_true(all(line_nos >= 1 & line_nos <= n_lines))
})

test_that("collect_mapbiomas_fragility_notes() returns character(0), not an error, for a missing file", {
  expect_equal(
    collect_mapbiomas_fragility_notes(file.path(tempdir(), "does_not_exist_mapbiomas.R")),
    character(0)
  )
})

test_that("collect_mapbiomas_fragility_notes() is read live -- a new tag is picked up without any other change", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "some_function <- function() {",
    "  # FRAGILE: a made-up example tag for this test only.",
    "  1 + 1",
    "}"
  ), tmp)
  on.exit(unlink(tmp), add = TRUE)

  notes <- collect_mapbiomas_fragility_notes(tmp)
  expect_length(notes, 1)
  expect_match(notes, "a made-up example tag for this test only", fixed = TRUE)
})
