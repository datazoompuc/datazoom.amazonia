# tests/testthat/test-baci-resolver.R
#
# Offline test for actions/scrapers/resolve_baci.R: the one row it owns, baci/
# HS92 -- CEPII's archive, found by HEAD-probing "V<YYYYMM>" stamps, with url,
# archive_file, version and available_time all derived from the same stamp.
#
# (From 2026-10-19 to 2026-10-28 this file mostly tested a second thing, the
# baci_dic/product_codes row: Brazil's COMEX product dictionary NCM_SH.csv,
# which R/baci.R's load_baci_dic() joins BACI's product codes with. That was
# the same file as comex_dic/hs_codes, so the duplicate row and its check are
# gone; load_baci_dic() reads comex_dic/hs_codes and resolve_comex.R guards the
# columns it selects -- see test-comex-resolver.R.)
#
# curl::curl_fetch_memory is mocked (no network), and so is zip_remote_entries()
# (defined below, in the same environment the resolver was sourced into).

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_baci.R"), local = TRUE)

# A stand-in for zip_remote_listing.R's zip_remote_entries(): names only (the
# resolver only reads the entry names), with the stamp taken from the URL.
zip_remote_entries <- function(url, ...) {
  stamp <- sub(".*_V([0-9]{6})\\.zip$", "\\1", url)
  stats::setNames(vector("list", 3), sprintf("BACI_HS92_Y%d_V%s.csv", 1995:1997, stamp))
}

baci_rows <- tibble::tibble(
  survey = "baci", dataset = "HS92",
  geo_level = NA_character_, year = NA_character_
)

# A fake world: CEPII's zip answers `status` for every stamp probed (the
# resolver takes the newest). Any other URL is an error: this resolver talks to
# CEPII and nobody else.
fake_world <- function(status = 200L, seen = NULL) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("^https://www\\.cepii\\.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V[0-9]{6}\\.zip$", url)) {
      return(list(status_code = status, headers = raw(0), content = raw(0)))
    }
    stop("the fake world has no such URL: ", url)
  }
}
with_world <- function(world, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = world, .package = "curl")
}

test_that("resolve_baci() returns the HS92 row alone, its url, version, archive_file and years all from one stamp", {
  seen <- new.env()
  out <- with_world(fake_world(seen = seen), resolve_baci(baci_rows))

  expect_equal(nrow(out), 1)
  expect_equal(out$survey, "baci")
  expect_equal(out$dataset, "HS92")
  expect_match(out$version, "^[0-9]{6}$")
  expect_equal(out$url, sprintf("https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V%s.zip", out$version))
  expect_equal(out$archive_file, sprintf("*$year$_V%s.csv", out$version))
  expect_equal(out$available_time, "1995-1997") # read from the zip's own file names
  expect_null(attr(out, "partial_failures"))

  # nothing but CEPII is ever contacted: the product dictionary is comex's row now
  expect_true(length(seen$urls) > 0)
  expect_true(all(grepl("^https://www\\.cepii\\.fr/", seen$urls)))
})

test_that("with no stamp CEPII answers for, it stops instead of keeping a stale one", {
  expect_error(
    with_world(fake_world(status = 404L), resolve_baci(baci_rows)),
    "no valid BACI_HS92_V<YYYYMM>.zip found",
    fixed = TRUE
  )
})

test_that("the committed manifest: one row owned by this resolver, and no baci_dic row any more", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")

  owned <- m[!is.na(m$resolver) & m$resolver == "baci", ]
  expect_equal(owned$survey, "baci")
  expect_equal(owned$dataset, "HS92")

  # load_baci_dic() reads comex_dic/hs_codes (owned by resolve_comex.R), not a row of its own
  expect_false("baci_dic" %in% m$survey)
  hs <- m[m$survey == "comex_dic" & m$dataset == "hs_codes", ]
  expect_equal(nrow(hs), 1)
  expect_equal(hs$resolver, "comex")
})
