# tests/testthat/test-fragility-notes.R
#
# Smoke test for actions/scripts/fragility_notes.R's collect_fragility_notes()
# -- the one generic function build_manifest.R uses (via FRAGILITY_SOURCES)
# to list, by line number, which parts of a source's treatment code a
# reviewer should re-check whenever a scheduled run changes one of that
# source's manifest rows. Originally mapbiomas-only
# (mapbiomas_fragility.R / collect_mapbiomas_fragility_notes(), tested only
# against R/mapbiomas.R); generalized so the mechanism itself is tested
# once here, and every source that adopts the "# FRAGILE:" tagging
# convention (R/mapbiomas.R, R/epe.R today) is exercised through the same
# table-driven test instead of a per-source copy of this file.
#
# actions/ is .Rbuildignore'd -- CI tooling, never shipped in the built
# package. Skipped entirely when absent, same posture as
# test-site-inventory.R.
#
# This does NOT re-verify every individual fragile point (test-mapbiomas-
# schema.R / test-epe-schema.R already exercise the actual behavior each one
# describes). It only guards against the tagging mechanism itself silently
# breaking -- e.g. someone deletes a "# FRAGILE:" comment during a future
# refactor without deleting the code it warned about. That specific failure
# mode (tag removed, risk still there) isn't caught by any assertion here
# either -- it's caught by code review noticing the comment count drop in
# the diff. What this test catches is the mechanical stuff: the tag format
# assumption drifting, or the collector erroring outright.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "fragility_notes.R"), local = TRUE)

# One row per tagged source, minimum tag count last verified against the
# live file (>= , not exact, so a future maintainer adding MORE tags
# doesn't have to also bump this number -- only the tagging mechanism going
# silent entirely should fail these).
tagged_sources <- list(
  list(r_path = file.path(pkg_root, "R", "mapbiomas.R"), label = "R/mapbiomas.R", min_tags = 8),
  list(r_path = file.path(pkg_root, "R", "epe.R"), label = "R/epe.R", min_tags = 3),
  list(r_path = file.path(pkg_root, "R", "prodes.R"), label = "R/prodes.R", min_tags = 4),
  list(r_path = file.path(pkg_root, "R", "baci.R"), label = "R/baci.R", min_tags = 3),
  # Added 2026-10-21 alongside PAM's two FRAGILITY_SOURCES rows: pam.R has 3
  # (the hardcoded SIDRA table ids, the label-derived column renames, the six
  # named IBGE variable ids) and R/download.R gained a second tag, counted in
  # its entry below (sidra_download()'s classific = "all" default).
  list(r_path = file.path(pkg_root, "R", "pam.R"), label = "R/pam.R", min_tags = 3),
  # Added 2026-10-22 alongside PPM's two FRAGILITY_SOURCES rows: ppm.R has 3
  # (the hardcoded SIDRA table ids, the IBGE variable ids each dataset names, the
  # classification-label columns the pivots use) and R/download.R gained a second
  # tag, counted in its entry below (sidra_get_table()'s no-classification route).
  list(r_path = file.path(pkg_root, "R", "ppm.R"), label = "R/ppm.R", min_tags = 3),
  # Added 2026-10-31 alongside PEVS's three FRAGILITY_SOURCES rows: pevs.R has 3 (the
  # hardcoded SIDRA table ids, the label-derived column names and pivot range, the
  # variable ids each dataset names); R/download.R's tags are the ones already counted
  # in its entry below and R/dictionary.R's the ones in its entry (the PEVS variables
  # note is one of them).
  list(r_path = file.path(pkg_root, "R", "pevs.R"), label = "R/pevs.R", min_tags = 3),
  # R/dictionary.R holds every source's dictionary: 2 tags as of 2026-10-22, the
  # ANEEL one (the ~variable column names) and the PPM section's (its codes are
  # copies of IBGE's classifications), the latter wired into PPM's
  # FRAGILITY_SOURCES rows.
  list(r_path = file.path(pkg_root, "R", "dictionary.R"), label = "R/dictionary.R", min_tags = 2),
  # Added 2026-10-19 alongside TerraClimate's two FRAGILITY_SOURCES rows (its
  # treatment code spans two files): climate.R has 3 (the hard-coded bounding
  # boxes, the whole-calendar-year time window, the layer naming the pivot
  # relies on) and download.R has 1 (the file-name pattern with its embedded
  # start year). R/download.R is shared by every source: its tags reach a PR
  # body only through a FRAGILITY_SOURCES row pointing at it, and today those
  # are TerraClimate's and PAM's (2 tags as of 2026-10-21).
  list(r_path = file.path(pkg_root, "R", "climate.R"), label = "R/climate.R", min_tags = 3),
  list(r_path = file.path(pkg_root, "R", "download.R"), label = "R/download.R", min_tags = 2),
  # Added 2026-10-21 for the certificate fallback (R/balanca_tls.R): 2 tags, the
  # host name its pattern accepts and the hard-coded copy of the certificates it
  # trusts. It is wired to the "comex_dic" and "baci_dic" rows in FRAGILITY_SOURCES.
  list(r_path = file.path(pkg_root, "R", "balanca_tls.R"), label = "R/balanca_tls.R", min_tags = 2),
  # 2026-09-21: dropped from 5 to 4 -- the removed tag covered a dead
  # as.Date(operation_start, ...) step (never matched a real column in any
  # of the three aneel datasets, verified live) that was deleted rather
  # than kept as a tagged-but-inert assumption. See NEWS.md.
  list(r_path = file.path(pkg_root, "R", "aneel.R"), label = "R/aneel.R", min_tags = 4),
  # Added 2026-10-12 alongside degrad's FRAGILITY_SOURCES row: 3 tags as of
  # that date (the missing-CRS/SAD69 datum assumption, the operation_crs
  # fixed to IBGE's Albers projection, and the st_intersection() splitting
  # caveat).
  list(r_path = file.path(pkg_root, "R", "degrad.R"), label = "R/degrad.R", min_tags = 3),
  # Added 2026-10-08 alongside DETER's FRAGILITY_SOURCES row: 3 tags as of
  # that date (class_name/classname rename, the hardcoded CRS, and the
  # municipality st_intersection() alert-splitting behavior).
  list(r_path = file.path(pkg_root, "R", "deter.R"), label = "R/deter.R", min_tags = 3),
  # Added 2026-10-17 alongside COMEX's FRAGILITY_SOURCES row: 3 tags that day
  # (raw MUN column names, raw NCM column names + integer assumptions, and the
  # dictionary host/encoding in load_trade_dic()). 4 as of 2026-10-19: that
  # last one was split in two when the dictionary URLs moved into the manifest
  # (the manifest-sourced URL and its fake-200 failure mode; the Latin-1
  # encoding and column names).
  list(r_path = file.path(pkg_root, "R", "comex.R"), label = "R/comex.R", min_tags = 4)
)

for (src in tagged_sources) {
  local({
    r_path <- src$r_path
    label <- src$label
    min_tags <- src$min_tags

    test_that(sprintf("collect_fragility_notes() finds every FRAGILE tag currently in %s", label), {
      notes <- collect_fragility_notes(r_path, label = label)

      expect_gte(length(notes), min_tags)

      # Every note must actually carry a real line number pointing
      # somewhere inside the file, and the explanatory text (not just the
      # tag).
      expect_true(all(grepl(paste0("^", label, ":[0-9]+ -- .+"), notes, fixed = FALSE)))

      n_lines <- length(readLines(r_path, warn = FALSE))
      line_nos <- as.integer(sub(paste0("^", label, ":([0-9]+) --.*$"), "\\1", notes))
      expect_true(all(line_nos >= 1 & line_nos <= n_lines))
    })
  })
}

test_that("collect_fragility_notes() returns character(0), not an error, for a missing file", {
  expect_equal(
    collect_fragility_notes(file.path(tempdir(), "does_not_exist.R"), label = "R/does_not_exist.R"),
    character(0)
  )
})

test_that("collect_fragility_notes() is read live -- a new tag is picked up without any other change", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "some_function <- function() {",
    "  # FRAGILE: a made-up example tag for this test only.",
    "  1 + 1",
    "}"
  ), tmp)
  on.exit(unlink(tmp), add = TRUE)

  notes <- collect_fragility_notes(tmp, label = "R/made_up.R")
  expect_length(notes, 1)
  expect_match(notes, "a made-up example tag for this test only", fixed = TRUE)
  expect_match(notes, "^R/made_up\\.R:3 -- ")
})

test_that("FRAGILITY_SOURCES entries all point at real files with a real changed_prefix/label/treat_fn", {
  expect_true(length(FRAGILITY_SOURCES) >= 2)
  for (fsrc in FRAGILITY_SOURCES) {
    expect_true(all(c("changed_prefix", "r_path", "label", "treat_fn") %in% names(fsrc)))
    expect_true(file.exists(file.path(pkg_root, fsrc$r_path)))
    expect_true(nzchar(fsrc$changed_prefix))
  }
})
