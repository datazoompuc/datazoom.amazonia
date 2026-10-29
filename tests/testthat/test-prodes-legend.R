# tests/testthat/test-prodes-legend.R
#
# R/prodes.R's raster legend helpers: the hardcoded codes
# (prodes_raster_code()), the .qml parser, and the guard load_prodes() runs
# against the .qml in the zip a user downloaded (prodes_check_qml()). Offline:
# fixtures/prodes_amazonia_legal_2025.qml is the real legend from
# prodes_amazonia_legal_2025_v20260408.zip, extracted 2026-10-20.
# The resolver's copy of the same codes is held to these in
# test-prodes-resolver-legend.R.

qml_fixture <- test_path("fixtures", "prodes_amazonia_legal_2025.qml")
read_fixture <- function() paste(readLines(qml_fixture, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("prodes_raster_code() returns PRODES' encoding for every dataset", {
  expect_equal(prodes_raster_code("deforestation", 2007:2009), 7:9)
  expect_equal(prodes_raster_code("residual_deforestation", 2010), 50)
  expect_equal(prodes_raster_code("native_vegetation", 2025), 100)
  expect_equal(prodes_raster_code("non_forest", 2025), 101)
  expect_equal(prodes_raster_code("hydrography", 2025), 91)
  expect_equal(prodes_raster_code("clouds", 2025), 99)
  expect_error(prodes_raster_code("not_a_dataset", 2025), "unknown PRODES dataset")
})

test_that("every manifest PRODES dataset has a code", {
  for (ds in datasets_link(source = "prodes")$dataset) {
    expect_length(prodes_raster_code(ds, 2025), 1)
  }
})

test_that("the real 2025 legend agrees with every code load_prodes() can use", {
  legend <- prodes_parse_qml(read_fixture())
  expect_gt(length(legend), 0)
  # every year the manifest declares, not just the release year
  expect_length(prodes_legend_problems(legend, "deforestation", 2007:2025), 0)
  expect_length(prodes_legend_problems(legend, "residual_deforestation", 2010:2025), 0)
  for (ds in c("native_vegetation", "non_forest", "hydrography", "clouds")) {
    expect_length(prodes_legend_problems(legend, ds, 2025), 0)
  }
})

test_that("prodes_parse_qml() survives '/' in a label and XML escapes", {
  txt <- paste(
    '<paletteEntry color="#000000" value="91" label="91 Hidrografia/rios" alpha="255"/>',
    '<paletteEntry value="100" label="100 Vegeta&#xe7;&#227;o nativa florestal" alpha="255"/>',
    '<paletteEntry alpha="255" label="7 d2007 &amp; antes" value="7"/>'
  )
  legend <- prodes_parse_qml(txt)
  expect_equal(unname(legend["91"]), "91 Hidrografia/rios")
  expect_equal(unname(legend["100"]), "100 Vegetação nativa florestal")
  expect_equal(unname(legend["7"]), "7 d2007 & antes")
})

test_that("prodes_legend_problems() flags missing and relabelled codes", {
  legend <- c("91" = "91 Hidrografia", "25" = "25 d2025", "26" = "26 d2026")
  expect_length(prodes_legend_problems(legend, "hydrography", 2025), 0)
  expect_match(prodes_legend_problems(legend, "native_vegetation", 2025), "not in the legend")
  expect_match(prodes_legend_problems(legend, "deforestation", 2024), "code 24 .* not in the legend")
  legend["25"] <- "25 r2025"
  expect_match(prodes_legend_problems(legend, "deforestation", 2025), "labelled '25 r2025', expected '25 d2025'")
})

test_that("clouds only counts when code 99 is there meaning something else", {
  expect_length(prodes_legend_problems(c("91" = "91 Hidrografia"), "clouds", 2025), 0)
  expect_length(prodes_legend_problems(c("99" = "99 Nuvem"), "clouds", 2025), 0)
  expect_match(prodes_legend_problems(c("99" = "99 Hidrografia"), "clouds", 2025), "not clouds")
})

test_that("prodes_check_qml() warns only when the downloaded legend disagrees", {
  skip_if_not_installed("terra")
  dir <- withr::local_tempdir()
  tif <- file.path(dir, "prodes_amazonia_legal_2025_v20260408.tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = c(25, 91, 100, 101)), tif)
  r <- terra::rast(tif)

  # no .qml next to the .tif: nothing to compare, stay quiet
  expect_silent(prodes_check_qml(r, "hydrography", 2025))

  qml <- file.path(dir, "prodes_amazonia_legal_2025_v20260408.qml")
  file.copy(qml_fixture, qml)
  expect_silent(prodes_check_qml(r, "hydrography", 2025))
  expect_silent(prodes_check_qml(r, "deforestation", 2007:2025))
  expect_silent(prodes_check_qml(r, "clouds", 2025))

  # the legend now says 91 is something else
  txt <- readLines(qml, encoding = "UTF-8")
  writeLines(sub('label="91 Hidrografia"', 'label="91 Nuvem"', txt, fixed = TRUE), qml, useBytes = TRUE)
  expect_warning(prodes_check_qml(r, "hydrography", 2025), "code 91 is labelled '91 Nuvem'")
})
