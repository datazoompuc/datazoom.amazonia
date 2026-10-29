# tests/testthat/test-prodes-resolver-legend.R
#
# actions/scrapers/resolve_prodes.R runs without the package loaded, so it
# keeps its own copy of R/prodes.R's raster legend (codes, labels and the
# .qml parser). This file fails whenever the two copies disagree -- change
# both together. Also covers the resolver-only checks: every d<year>/r<year>
# entry, and clouds. Offline (same real .qml fixture as test-prodes-legend.R).

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scrapers", "resolve_prodes.R"), local = TRUE)

qml_txt <- paste(readLines(test_path("fixtures", "prodes_amazonia_legal_2025.qml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("the resolver's codes and labels match R/prodes.R's, for any release year", {
  for (y in 2020:2035) {
    for (item in prodes_expected_legend(y)) {
      expect_equal(item$code, prodes_raster_code(item$dataset, y), info = paste(item$dataset, y))
      expect_equal(item$label, prodes_legend_label(item$dataset, y), info = paste(item$dataset, y))
    }
  }
})

test_that("the resolver checks every PRODES dataset but clouds", {
  checked <- vapply(prodes_expected_legend(2025L), `[[`, character(1), "dataset")
  expect_setequal(c(checked, "clouds"), datasets_link(source = "prodes")$dataset)
})

test_that("the resolver parses a .qml exactly like R/prodes.R does", {
  tricky <- paste(
    '<paletteEntry color="#000000" value="91" label="91 Hidrografia/rios" alpha="255"/>',
    '<paletteEntry value="100" label="100 Vegeta&#xe7;&#227;o nativa florestal" alpha="255"/>',
    '<paletteEntry alpha="255" label="7 d2007 &amp; antes" value="7"/>',
    '<paletteEntry value="x" label="no code"/>'
  )
  for (txt in list(qml_txt, tricky)) {
    expect_identical(prodes_qml_palette(txt), prodes_parse_qml(txt))
  }
})

test_that("the real 2025 legend passes the resolver's check", {
  expect_length(prodes_legend_mismatches(prodes_qml_palette(qml_txt), 2025L), 0)
})

test_that("a relabelled or missing code fails the check", {
  pal <- prodes_qml_palette(qml_txt)
  pal_bad <- pal
  pal_bad["91"] <- "91 Nuvem"
  expect_match(prodes_legend_mismatches(pal_bad, 2025L), "code 91: expected label '91 Hidrografia', got '91 Nuvem'", all = FALSE)
  expect_match(prodes_legend_mismatches(pal[names(pal) != "101"], 2025L), "code 101: .* not present", all = FALSE)
})

test_that("an older year's entry under the wrong code fails, not only the release year's", {
  pal <- c(prodes_qml_palette(qml_txt), "77" = "77 d2008")
  expect_match(prodes_legend_mismatches(pal, 2025L), "code 77 is labelled '77 d2008', but d2008 should be code 8", all = FALSE)
})

test_that("clouds fails only when code 99 means something else", {
  pal <- prodes_qml_palette(qml_txt)
  expect_length(prodes_legend_mismatches(c(pal, "99" = "99 Nuvem"), 2025L), 0)
  expect_match(prodes_legend_mismatches(c(pal, "99" = "99 Agua"), 2025L), "code 99 \\(clouds\\)", all = FALSE)
})
