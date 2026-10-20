# tests/testthat/test-prodes-download.R
#
# external_download()'s prodes branch used to read list.files(dir, "*.tif"):
# with the cache off, `dir` is the session's tempdir(), where terra leaves its
# own spat_*.tif temp rasters, and one left over from an earlier load_prodes()
# call (a derivative of the PRODES raster, so carrying the same layer name)
# was stacked in as a 2nd layer -- load_prodes() then failed with "Can't
# transform a data frame with duplicate names" (confirmed 2026-10-20 in a real
# session). Offline: perform_download() is mocked to hand over a small zip
# shaped like INPE's (the raster named after layer_name, plus its .qml).

withr::defer(clear_download_cache(), teardown_env())

layer <- dataset_meta("prodes", "deforestation", "layer_name")

make_prodes_zip <- function(dir, tif_name = layer) {
  src <- file.path(dir, "src")
  dir.create(src)
  # named like the real file's single layer (terra names it after the file)
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = c(21, 25, 91, 100), names = tif_name), file.path(src, paste0(tif_name, ".tif")))
  file.copy(test_path("fixtures", "prodes_amazonia_legal_2025.qml"), file.path(src, paste0(tif_name, ".qml")))
  zipfile <- file.path(dir, "prodes.zip")
  withr::with_dir(src, utils::zip(zipfile, list.files(src), flags = "-q"))
  zipfile
}

mock_download_of <- function(zipfile, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    perform_download = function(path, temp, ...) file.copy(zipfile, temp, overwrite = TRUE),
    .env = .env
  )
}

test_that("the raster is read by its exact name, not every .tif in tempdir() (cache off)", {
  skip_if_not_installed("terra")
  skip_if(!nzchar(Sys.which("zip")) && !nzchar(Sys.getenv("R_ZIPCMD")), "no zip program")
  clear_download_cache()
  withr::local_options(datazoom.amazonia.cache = FALSE)
  zipfile <- make_prodes_zip(withr::local_tempdir())
  withr::defer(unlink(file.path(tempdir(), paste0(layer, c(".tif", ".qml")))))

  # what was left behind in the real session: a raster with the same layer
  # name. There it was terra's own spat_*.tif, which survived tmpFiles()
  # only because Windows won't delete a file still held open; named spat_*
  # here, tmpFiles() would just delete it, so it gets another name -- the old
  # pattern picked up any .tif in tempdir() anyway.
  decoy <- file.path(tempdir(), "leftover_from_an_earlier_call.tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1, names = layer), decoy, overwrite = TRUE)
  withr::defer(unlink(decoy))

  mock_download_of(zipfile)
  r <- external_download(source = "prodes", dataset = "deforestation")
  expect_equal(terra::nlyr(r), 1)
  expect_equal(names(r), layer)
})

test_that("load_prodes() deletes the terra temp files it made, and only those", {
  skip_if_not_installed("terra")
  skip_if_not_installed("units")
  clear_download_cache()
  # small rasters stay in memory; make terra write every step to disk, as it
  # does with the real full-resolution raster
  old_opt <- terra::terraOptions(print = FALSE)$todisk
  terra::terraOptions(todisk = TRUE)
  withr::defer(terra::terraOptions(todisk = old_opt))

  tif <- file.path(withr::local_tempdir(), paste0(layer, ".tif"))
  terra::writeRaster(terra::rast(nrows = 40, ncols = 40, xmin = -60, xmax = -59, ymin = -5, ymax = -4, crs = "EPSG:4326", vals = 25, names = layer), tif)
  munic <- sf::st_sf(
    code_muni = 1, name_muni = "A", code_state = 15, abbrev_state = "PA",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(-60, -5), c(-59, -5), c(-59, -4), c(-60, -4), c(-60, -5)))), crs = 4326)
  )
  testthat::local_mocked_bindings(
    external_download = function(dataset, source, ...) if (source == "prodes") terra::rast(tif) else munic
  )

  # a raster of the user's own, on disk among terra's temp files
  users_raster <- terra::rast(nrows = 2, ncols = 2, vals = 1:4) * 2
  users_files <- terra::tmpFiles()
  expect_gt(length(users_files), 0)

  out <- suppressMessages(load_prodes(dataset = "deforestation", time_period = 2025))
  expect_equal(nrow(out), 1)
  expect_setequal(terra::tmpFiles(), users_files)
  expect_equal(terra::values(users_raster)[, 1], c(2, 4, 6, 8))
})

test_that("a zip without the expected raster stops with a clear error", {
  skip_if_not_installed("terra")
  skip_if(!nzchar(Sys.which("zip")) && !nzchar(Sys.getenv("R_ZIPCMD")), "no zip program")
  clear_download_cache()
  zipfile <- make_prodes_zip(withr::local_tempdir(), tif_name = "prodes_renamed_by_inpe")

  mock_download_of(zipfile)
  expect_error(
    external_download(source = "prodes", dataset = "deforestation"),
    "does not contain the expected raster '.*'. Found: prodes_renamed_by_inpe.tif"
  )
})
