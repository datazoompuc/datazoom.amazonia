# tests/testthat/test-download-dir.R
#
# Where external_download() puts a download. With the cache off it used to be
# the session's tempdir() itself, and the zip readers find their file by
# pattern -- sigmine/ibama/aneel with recursive = TRUE, which also reaches the
# datazoom-cache/ dirs earlier cached calls left there. So a shapefile one
# source extracted could be read back as another's. Offline: perform_download()
# is mocked to hand over a small zipped shapefile per source.

withr::defer(clear_download_cache(), teardown_env())

skip_if(!nzchar(Sys.which("zip")) && !nzchar(Sys.getenv("R_ZIPCMD")), "no zip program")

# a zip holding one shapefile, `name`.shp, whose one row says where it came from
make_shp_zip <- function(dir, name) {
  src <- file.path(dir, name)
  dir.create(src)
  pt <- sf::st_sf(origin = name, geometry = sf::st_sfc(sf::st_point(c(-60, -5)), crs = 4326))
  sf::st_write(pt, file.path(src, paste0(name, ".shp")), quiet = TRUE)
  zipfile <- file.path(dir, paste0(name, ".zip"))
  withr::with_dir(src, utils::zip(zipfile, list.files(src), flags = "-q"))
  zipfile
}

test_that("with the cache off, a download never reads another source's extracted files", {
  clear_download_cache()
  work <- withr::local_tempdir()
  zips <- list(
    sigmine = make_shp_zip(work, "sigmine_layer"),
    ibama = make_shp_zip(work, "ibama_layer")
  )
  testthat::local_mocked_bindings(
    perform_download = function(path, temp, download_method, source, ...) file.copy(zips[[source]], temp, overwrite = TRUE)
  )

  # sigmine first, cached: its shapefile now sits under tempdir()/datazoom-cache/
  withr::with_options(list(datazoom.amazonia.cache = TRUE), {
    sig <- external_download(source = "sigmine", dataset = "sigmine_active")
  })
  expect_equal(sig$origin, "sigmine_layer")

  # then ibama with the cache off -- a recursive search of tempdir() found
  # sigmine's .shp first ("datazoom-cache/..." sorts before "ibama_layer.shp")
  withr::with_options(list(datazoom.amazonia.cache = FALSE), {
    iba <- external_download(source = "ibama", dataset = "embargoed_areas")
    iba_again <- external_download(source = "ibama", dataset = "embargoed_areas")
  })
  expect_equal(iba$origin, "ibama_layer")
  expect_equal(iba_again$origin, "ibama_layer")
})

test_that("a cache miss starts from an empty dir, not the files a dropped entry left", {
  clear_download_cache()
  work <- withr::local_tempdir()
  zipfile <- make_shp_zip(work, "sigmine_layer")
  testthat::local_mocked_bindings(
    perform_download = function(path, temp, ...) file.copy(zipfile, temp, overwrite = TRUE)
  )
  withr::local_options(datazoom.amazonia.cache = TRUE)

  external_download(source = "sigmine", dataset = "sigmine_active")
  # all.names: keys start with the extension (".zip||<url>"), hidden from ls()
  key <- ls(download_cache_files_env(), all.names = TRUE)
  expect_length(key, 1)
  dir <- get(key, envir = download_cache_files_env())$dir
  # an unrelated shapefile lands in that dir, and the entry is dropped
  # (its zip deleted -- file_cache_get() then treats it as a miss)
  stray <- make_shp_zip(work, "aaa_stray")
  utils::unzip(stray, exdir = dir)
  unlink(list.files(dir, "\\.zip$", full.names = TRUE))
  .dz_download_cache$parsed_key <- NULL

  again <- external_download(source = "sigmine", dataset = "sigmine_active")
  expect_equal(again$origin, "sigmine_layer")
  expect_false(file.exists(file.path(dir, "aaa_stray.shp")))
})
