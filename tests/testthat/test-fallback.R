# Proves the central promise of the manifest design: when the remote manifest
# cannot be fetched (unreachable host, timeout, malformed response), the
# package falls back to the packaged snapshot SILENTLY -- no message, no
# warning -- and still returns a fully functional, complete table.
#
# These tests deliberately flip datazoom.amazonia.use_remote_manifest back to
# TRUE (setup.R turns it off for every other test) because they are the ones
# meant to exercise the remote-fetch code path.

test_that("an unreachable manifest URL falls back silently to the snapshot", {
  withr::local_options(
    datazoom.amazonia.use_remote_manifest = TRUE,
    # Port 1 on loopback: nothing listens there, connection refused immediately.
    datazoom.amazonia.manifest_url = "http://127.0.0.1:1/nope.csv"
  )
  datazoom.amazonia:::clear_manifest_cache()

  expect_silent(result <- datasets_link())
  expect_true(nrow(result) >= 150)
  expect_equal(datazoom.amazonia:::manifest_info()$source, "snapshot")

  datazoom.amazonia:::clear_manifest_cache()
})

test_that("a malformed remote manifest (too small / not a CSV) falls back silently", {
  bad_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("this is not a manifest", bad_file)

  withr::local_options(
    datazoom.amazonia.use_remote_manifest = TRUE,
    datazoom.amazonia.manifest_url = paste0("file://", bad_file)
  )
  datazoom.amazonia:::clear_manifest_cache()

  expect_silent(result <- datasets_link())
  expect_true(nrow(result) >= 150)
  expect_equal(datazoom.amazonia:::manifest_info()$source, "snapshot")

  datazoom.amazonia:::clear_manifest_cache()
})

test_that("a valid local manifest file is used as the 'remote' source", {
  candidate <- withr::local_tempfile(fileext = ".csv")
  file.copy(
    system.file("extdata", "manifest", "v1", "datasets_link.csv", package = "datazoom.amazonia"),
    candidate,
    overwrite = TRUE
  )

  withr::local_options(
    datazoom.amazonia.use_remote_manifest = TRUE,
    datazoom.amazonia.manifest_url = paste0("file://", candidate)
  )
  datazoom.amazonia:::clear_manifest_cache()

  expect_silent(result <- datasets_link())
  expect_true(nrow(result) >= 150)
  expect_equal(datazoom.amazonia:::manifest_info()$source, "remote")

  datazoom.amazonia:::clear_manifest_cache()
})

test_that("the manifest is cached: a second call does not re-fetch", {
  withr::local_options(
    datazoom.amazonia.use_remote_manifest = TRUE,
    datazoom.amazonia.manifest_url = "http://127.0.0.1:1/nope.csv"
  )
  datazoom.amazonia:::clear_manifest_cache()

  datasets_link()
  info_1 <- datazoom.amazonia:::manifest_info()
  datasets_link()
  info_2 <- datazoom.amazonia:::manifest_info()

  expect_equal(info_1, info_2)

  datazoom.amazonia:::clear_manifest_cache()
})
