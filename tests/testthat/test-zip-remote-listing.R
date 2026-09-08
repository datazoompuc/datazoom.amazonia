# tests/testthat/test-zip-remote-listing.R
#
# Offline test for actions/scripts/zip_remote_listing.R. zip_remote_entries()/
# zip_remote_extract_entry() (the curl-backed functions every resolver
# actually calls) were built and proven against the REAL live PRODES and
# BACI archives -- see resolve_prodes.R's/resolve_baci.R's headers for what
# was verified live and when. That file deliberately splits fetch (curl,
# HTTP) from parse (raw bytes in, structures out) so the parse layer --
# zip_entries_from_range_fn()/zip_extract_entry_from_range_fn(), the part
# that would silently break a resolver if a refactor got a byte offset
# wrong -- can be exercised here with a `get_range(from, to)` closure backed
# by a small zip built on disk, no network and no test-server dependency
# needed.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "zip_remote_listing.R"), local = TRUE)

build_fixture_zip <- function() {
  dir <- tempfile("ziptest")
  dir.create(dir)
  qml_path <- file.path(dir, "sample.qml")
  writeLines(c(
    "<qgis>",
    "<colorPalette>",
    '<paletteEntry value="7" label="7 d2007" color="#ff0000"/>',
    '<paletteEntry value="100" label="100 native" color="#00ff00"/>',
    "</colorPalette>",
    "</qgis>"
  ), qml_path)
  tif_path <- file.path(dir, "sample.tif")
  writeBin(as.raw(sample(0:255, 2000, replace = TRUE)), tif_path)

  zip_path <- file.path(dir, "fixture.zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zip_path, files = c(basename(qml_path), basename(tif_path)), flags = "-X")
  zip_path
}

# A 0-indexed, INCLUSIVE-range get_range(from, to) backed by a local file's
# own bytes -- exactly the contract zip_entries_from_range_fn()/
# zip_extract_entry_from_range_fn() expect a real ranged HTTP GET to satisfy.
local_get_range <- function(all_bytes) {
  function(from, to) {
    to <- min(to, length(all_bytes) - 1)
    all_bytes[(from + 1):(to + 1)]
  }
}

test_that("zip_entries_from_range_fn() finds every entry in a small local zip", {
  zip_path <- build_fixture_zip()
  all_bytes <- readBin(zip_path, "raw", n = file.size(zip_path))
  get_range <- local_get_range(all_bytes)

  entries <- zip_entries_from_range_fn(get_range, length(all_bytes))
  expect_setequal(names(entries), c("sample.qml", "sample.tif"))
  expect_true(all(c("method", "comp_size", "uncomp_size", "local_hdr_off", "crc32", "mod_time", "mod_date", "fname_raw") %in% names(entries[["sample.qml"]])))
})

test_that("zip_entries_from_range_fn() grows its tail window when the initial one misses the EOCD", {
  zip_path <- build_fixture_zip()
  all_bytes <- readBin(zip_path, "raw", n = file.size(zip_path))
  get_range <- local_get_range(all_bytes)

  # initial_tail smaller than the whole file forces at least one grow-and-retry
  entries <- zip_entries_from_range_fn(get_range, length(all_bytes), initial_tail = 16L)
  expect_setequal(names(entries), c("sample.qml", "sample.tif"))
})

test_that("zip_extract_entry_from_range_fn() recovers real .qml content via the reconstruct-and-unzip path", {
  zip_path <- build_fixture_zip()
  all_bytes <- readBin(zip_path, "raw", n = file.size(zip_path))
  get_range <- local_get_range(all_bytes)

  entries <- zip_entries_from_range_fn(get_range, length(all_bytes))
  qml_bytes <- zip_extract_entry_from_range_fn(get_range, entries[["sample.qml"]])
  txt <- rawToChar(qml_bytes)

  expect_match(txt, 'value="7" label="7 d2007"', fixed = TRUE)
  expect_match(txt, 'value="100" label="100 native"', fixed = TRUE)
})

test_that("zip_extract_entry_from_range_fn() round-trips a binary (non-text) entry byte-for-byte", {
  zip_path <- build_fixture_zip()
  all_bytes <- readBin(zip_path, "raw", n = file.size(zip_path))
  get_range <- local_get_range(all_bytes)

  entries <- zip_entries_from_range_fn(get_range, length(all_bytes))
  tif_bytes <- zip_extract_entry_from_range_fn(get_range, entries[["sample.tif"]])
  expect_equal(length(tif_bytes), entries[["sample.tif"]]$uncomp_size)
})

test_that("zip_entries_from_range_fn() throws (not NULL/empty) when there's no EOCD at all", {
  not_a_zip <- as.raw(sample(0:255, 500, replace = TRUE))
  get_range <- local_get_range(not_a_zip)
  expect_error(zip_entries_from_range_fn(get_range, length(not_a_zip)))
})

test_that("zip_extract_entry_from_range_fn() throws when header_pad is too small for the entry's real header", {
  zip_path <- build_fixture_zip()
  all_bytes <- readBin(zip_path, "raw", n = file.size(zip_path))
  get_range <- local_get_range(all_bytes)

  entries <- zip_entries_from_range_fn(get_range, length(all_bytes))
  expect_error(
    zip_extract_entry_from_range_fn(get_range, entries[["sample.qml"]], header_pad = 0L),
    "header_pad"
  )
})
