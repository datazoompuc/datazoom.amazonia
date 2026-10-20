# TerraClimate's THREDDS server answers a request it cannot serve with HTTP 200
# and an EMPTY body (a dataset name it does not have -- how release 1.2.0's
# "..._1958_CurrentYear_GLOBE.nc" fails) or with HTTP 400 and a plain-text
# explanation (a time range outside the data). utils::download.file() saves
# either as if it were the netCDF, and terra::rast() then fails with the
# unhelpful "[rast] cannot open this file as a SpatRaster". external_download()
# now checks the file first. Offline: perform_download() is mocked to write
# whatever bytes the server would have sent.

withr::defer(clear_download_cache(), teardown_env())

tc_coords <- list(
  lat_min = "-18.04177", lat_max = "5.272225",
  lon_min = "-73.99094", lon_max = "-44"
)
tc_years <- list(
  initial_time = "2030-01-01T00%3A00%3A00Z",
  final_time = "2030-12-01T00%3A00%3A00Z"
)

# external_download() for TerraClimate, the transfer replaced by `write_body`
# (a function that writes the response body to the file it is handed)
tc_download <- function(write_body, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    dataset_url = function(...) "https://example.org/thredds/ncss/terraclimate",
    perform_download = function(path, temp, ...) write_body(temp),
    .env = .env
  )
  external_download(
    source = "terraclimate", dataset = "precipitation", dataset_code = "ppt",
    coords = tc_coords, year = tc_years
  )
}

# The error tc_download() raises, or NULL if it raises none (warnings from
# terra refusing a fake netCDF file are not what these tests are about)
tc_error <- function(write_body) {
  tryCatch(
    {
      suppressWarnings(tc_download(write_body))
      NULL
    },
    error = function(e) e
  )
}

write_text <- function(txt) function(temp) writeLines(txt, temp)

# A file that starts like a netCDF-3 one; terra cannot read it, which is fine --
# these tests only care whether the check lets it through.
fake_netcdf_bytes <- c(charToRaw("CDF"), as.raw(1), raw(60))

test_that("is_netcdf_file() recognises classic, 64-bit-offset and HDF5 netCDF headers", {
  classic <- withr::local_tempfile()
  writeBin(fake_netcdf_bytes, classic)
  expect_true(is_netcdf_file(classic))

  offset64 <- withr::local_tempfile()
  writeBin(c(charToRaw("CDF"), as.raw(2), raw(60)), offset64)
  expect_true(is_netcdf_file(offset64))

  hdf5 <- withr::local_tempfile()
  writeBin(c(as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a)), raw(60)), hdf5)
  expect_true(is_netcdf_file(hdf5))
})

test_that("is_netcdf_file() rejects an empty, missing, truncated, text or wrong-version file", {
  empty <- withr::local_tempfile()
  file.create(empty)
  expect_false(is_netcdf_file(empty))
  expect_false(is_netcdf_file(withr::local_tempfile())) # never created

  truncated <- withr::local_tempfile()
  writeBin(charToRaw("CDF"), truncated)
  expect_false(is_netcdf_file(truncated))

  wrong_version <- withr::local_tempfile()
  writeBin(c(charToRaw("CDF"), as.raw(3), raw(60)), wrong_version)
  expect_false(is_netcdf_file(wrong_version))

  txt <- withr::local_tempfile()
  writeLines("Requested time range does not intersect actual time range", txt)
  expect_false(is_netcdf_file(txt))
})

test_that("text_snippet() returns one line of text, truncated, and NULL for non-text", {
  expect_equal(text_snippet(charToRaw("a  b\r\n\tc\n")), "a b c")

  long <- text_snippet(charToRaw(strrep("x", 500)))
  expect_equal(nchar(long), 203)
  expect_true(endsWith(long, "..."))

  expect_null(text_snippet(raw(0)))
  expect_null(text_snippet(charToRaw("  \n\n ")))
  expect_null(text_snippet(fake_netcdf_bytes)) # NUL and \001 are not text
  expect_null(text_snippet(as.raw(c(0x50, 0x4b, 0x03, 0x04)))) # a zip header

  # Non-ASCII text (a Portuguese error page) survives; a character cut in half
  # at the read boundary becomes "?" instead of breaking the regex step.
  expect_equal(text_snippet(charToRaw(enc2utf8("Não encontrado"))), enc2utf8("Não encontrado"))
  expect_equal(text_snippet(c(charToRaw("abc"), as.raw(0xc3))), "abc?")
})

test_that("an empty response (a dataset name the server does not have) stops with a clear error", {
  for (cache in c(TRUE, FALSE)) {
    clear_download_cache()
    withr::local_options(datazoom.amazonia.cache = cache)

    err <- tc_error(function(temp) file.create(temp))

    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "TerraClimate did not return a netCDF file")
    expect_match(conditionMessage(err), "response was empty")
    expect_match(conditionMessage(err), "github.com/datazoompuc/datazoom.amazonia/issues", fixed = TRUE)
    expect_false(grepl("SpatRaster", conditionMessage(err), fixed = TRUE))
  }
})

test_that("an HTTP 400 text body (a time range outside the data) is quoted in the error", {
  clear_download_cache()

  body <- paste(
    "Requested time range 2030-01-01T00:00:00Z - 2030-12-01T00:00:00Z",
    "does not intersect actual time range",
    "1950-01-01T00:00:00Z - 2025-12-01T00:00:00Z"
  )
  err <- tc_error(write_text(body))

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "TerraClimate did not return a netCDF file")
  expect_match(conditionMessage(err), body, fixed = TRUE)
})

test_that("an HTML error page is reported with the start of its text", {
  clear_download_cache()

  page <- c(
    "<!DOCTYPE html>",
    "<html><head><title>Error 503 Service Unavailable</title></head>",
    "<body><h1>Service Unavailable</h1><p>", strrep("The server is down. ", 40), "</p></body></html>"
  )
  err <- tc_error(write_text(page))

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "TerraClimate did not return a netCDF file")
  expect_match(conditionMessage(err), "Error 503 Service Unavailable", fixed = TRUE)
  expect_match(conditionMessage(err), "...", fixed = TRUE) # cut at 200 characters
  expect_lt(nchar(conditionMessage(err)), 400)
})

test_that("a binary file that is not a netCDF one is reported by size", {
  clear_download_cache()

  err <- tc_error(function(temp) writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, rep(0, 20))), temp))

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "unrecognized 24-byte file")
})

test_that("a file that starts like a netCDF one is not rejected by the check", {
  clear_download_cache()

  err <- tc_error(function(temp) writeBin(fake_netcdf_bytes, temp))

  # terra may still refuse the padding after the header; the check must not
  if (!is.null(err)) {
    expect_false(grepl("did not return a netCDF file", conditionMessage(err), fixed = TRUE))
  }
})

test_that("a genuine netCDF file comes back as a SpatRaster", {
  skip_if_not_installed("terra")
  clear_download_cache()

  # Written through GDAL's netCDF driver (terra::writeCDF() needs the ncdf4
  # package), which produces a netCDF-4 file: the HDF5 signature branch.
  nc <- withr::local_tempfile(fileext = ".nc")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = -74, xmax = -44, ymin = -18, ymax = 5, vals = 1:4)
  written <- tryCatch(
    {
      suppressWarnings(terra::writeRaster(r, nc, filetype = "netCDF", overwrite = TRUE))
      file.exists(nc)
    },
    error = function(e) FALSE
  )
  skip_if_not(written, "terra cannot write netCDF files here")

  expect_true(is_netcdf_file(nc))

  res <- tc_download(function(temp) file.copy(nc, temp, overwrite = TRUE))

  expect_s4_class(res, "SpatRaster")
  expect_equal(terra::ncell(res), 4)
})

test_that("a rejected download is not cached: the retry downloads again", {
  clear_download_cache()
  withr::local_options(datazoom.amazonia.cache = TRUE)

  calls <- 0
  flaky <- function(temp) {
    calls <<- calls + 1
    if (calls == 1) file.create(temp) else writeBin(fake_netcdf_bytes, temp)
  }

  expect_error(tc_download(flaky), "did not return a netCDF file")
  err <- tc_error(flaky)

  expect_equal(calls, 2)
  if (!is.null(err)) {
    expect_false(grepl("did not return a netCDF file", conditionMessage(err), fixed = TRUE))
  }
})
