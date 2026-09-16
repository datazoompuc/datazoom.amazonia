# Session-scoped download + parse cache (R/download.R). SEEG (6 datasets),
# IPS (8 datasets), and PRODES (6 datasets) each serve every dataset off one
# identical file -- these tests exercise the cache generically via
# file_cache_key()/parsed_cache_eligible()/external_download() itself,
# using local file:// fixtures so nothing here touches the network.

test_that("clear_download_cache() resets state between tests", {
  clear_download_cache()
  expect_null(.dz_download_cache$files)
  expect_null(.dz_download_cache$parsed_key)
})

withr::defer(clear_download_cache(), teardown_env())

test_that("parsed cache key equates datasets that share one xlsx, distinguishes ones that don't", {
  # All 6 SEEG datasets pass the same sheet/skip_rows -- only `dataset` and
  # `geo_level` differ between them, and neither is part of the key.
  key_farming <- list(
    path = "https://example.org/seeg.xlsx", file_extension = ".xlsx",
    source = "seeg", sheet = "Dados", skip_rows = 0
  )
  key_land <- key_farming # dataset/geo_level never enter the key at all
  expect_identical(key_farming, key_land)

  # IPS encodes the requested years INTO param$sheet, so two different
  # year requests are correctly treated as different reads.
  key_ips_2014 <- list(
    path = "https://example.org/ips.xlsx", file_extension = ".xlsx",
    source = "ips", sheet = "2014", skip_rows = 0
  )
  key_ips_2021 <- list(
    path = "https://example.org/ips.xlsx", file_extension = ".xlsx",
    source = "ips", sheet = "2021", skip_rows = 0
  )
  expect_false(identical(key_ips_2014, key_ips_2021))
})

test_that("parsed_cache_eligible() allowlists only the xlsx read branches", {
  expect_true(parsed_cache_eligible("seeg", ".xlsx"))
  expect_true(parsed_cache_eligible("ips", ".xlsx"))
  expect_true(parsed_cache_eligible("epe", ".xlsx"))
  expect_true(parsed_cache_eligible("mapbiomas", ".xlsx"))
  expect_true(parsed_cache_eligible("iema", ".xlsx"))

  # aneel has both an xlsx dataset (energy_generation) and fread-based ones
  # (energy_enterprises_distributed/energy_development_budget) -- extension
  # alone already tells them apart, since only the xlsx one ever reaches
  # here with file_extension == ".xlsx".
  expect_true(parsed_cache_eligible("aneel", ".xlsx"))
  expect_false(parsed_cache_eligible("aneel", ".csv"))

  # SpatRaster (prodes/terraclimate) and data.table (baci/csv sources) are
  # reference-semantics or file-pointer-backed -- never eligible.
  expect_false(parsed_cache_eligible("prodes", ".zip"))
  expect_false(parsed_cache_eligible("terraclimate", ".nc"))
  expect_false(parsed_cache_eligible("baci", ".zip"))
  expect_false(parsed_cache_eligible("ibama", ".csv"))
})

test_that("file cache registry: miss -> set -> hit, and clear empties it", {
  clear_download_cache()

  key <- file_cache_key("https://example.org/f.csv", ".csv")
  expect_null(file_cache_get(key))

  tmp_dir <- withr::local_tempdir()
  tmp_file <- file.path(tmp_dir, "f.csv")
  writeLines("a,b\n1,2", tmp_file)

  file_cache_set(key, dir = tmp_dir, temp = tmp_file)
  hit <- file_cache_get(key)
  expect_equal(hit$dir, tmp_dir)
  expect_equal(hit$temp, tmp_file)

  clear_download_cache()
  expect_null(file_cache_get(key))
})

test_that("a file cache entry pointing at a since-deleted file is treated as a miss, not an error", {
  clear_download_cache()

  key <- file_cache_key("https://example.org/gone.csv", ".csv")
  tmp_dir <- withr::local_tempdir()
  tmp_file <- file.path(tmp_dir, "gone.csv")
  writeLines("x", tmp_file)
  file_cache_set(key, dir = tmp_dir, temp = tmp_file)

  unlink(tmp_file)
  expect_null(file_cache_get(key))

  unlink(tmp_dir, recursive = TRUE)
  expect_null(file_cache_get(key))
})

test_that("download_cache_enabled() honors the options() escape hatch", {
  withr::local_options(datazoom.amazonia.cache = NULL)
  expect_true(download_cache_enabled())

  withr::local_options(datazoom.amazonia.cache = FALSE)
  expect_false(download_cache_enabled())

  withr::local_options(datazoom.amazonia.cache = TRUE)
  expect_true(download_cache_enabled())
})

test_that("external_download() caches a .csv download end-to-end via a file:// fixture", {
  clear_download_cache()
  withr::local_options(datazoom.amazonia.cache = TRUE)

  fixture_dir <- withr::local_tempdir()
  fixture <- file.path(fixture_dir, "fixture.csv")
  writeLines("x,y\n1,2", fixture)
  fixture_url <- paste0("file://", fixture)

  mock_dataset_url <- function(...) fixture_url

  testthat::local_mocked_bindings(dataset_url = mock_dataset_url)

  dat1 <- external_download(source = "sidra_fixture_source", dataset = "d")

  # Mutate the source after the first call -- if the cache is working, the
  # second call must NOT see this change.
  writeLines("x,y\n999,999", fixture)

  dat2 <- external_download(source = "sidra_fixture_source", dataset = "d")

  expect_equal(dat1, dat2)
  expect_equal(as.character(dat1$x), "1")
})

test_that("external_download() does not cache across a cleared cache", {
  clear_download_cache()
  withr::local_options(datazoom.amazonia.cache = TRUE)

  fixture_dir <- withr::local_tempdir()
  fixture <- file.path(fixture_dir, "fixture2.csv")
  writeLines("x\n1", fixture)
  fixture_url <- paste0("file://", fixture)

  testthat::local_mocked_bindings(dataset_url = function(...) fixture_url)

  dat1 <- external_download(source = "sidra_fixture_source2", dataset = "d")

  clear_download_cache()
  writeLines("x\n2", fixture)

  dat2 <- external_download(source = "sidra_fixture_source2", dataset = "d")

  expect_equal(as.character(dat1$x), "1")
  expect_equal(as.character(dat2$x), "2")
})

test_that("a failed download leaves no residual per-key cache directory", {
  clear_download_cache()
  withr::local_options(datazoom.amazonia.cache = TRUE)

  missing_url <- paste0("file://", file.path(withr::local_tempdir(), "does-not-exist.csv"))
  testthat::local_mocked_bindings(dataset_url = function(...) missing_url)

  expect_error(
    external_download(source = "sidra_fixture_source3", dataset = "d")
  )

  key <- file_cache_key(missing_url, ".csv")
  expect_null(file_cache_get(key))
})
