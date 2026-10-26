# tests/testthat/test-baci-resolver.R
#
# Offline test for the baci_dic/product_codes part of actions/scrapers/
# resolve_baci.R (added 2026-10-19, issue #353; see that file's note of the
# same date). baci_dic is the product-code dictionary R/baci.R's load_baci_dic()
# joins BACI's codes with -- Brazil's COMEX table NCM_SH.csv, not CEPII's. Its
# manifest row used to have NO resolver and point at balanca.economia.gov.br;
# when that host's TLS certificate expired (2026-10-03) load_baci() began to
# stop, after the 2.4 GB download, with a certificate error, and nothing
# noticed. The resolver now checks the row once per run, on balanca.mdic.gov.br:
# one ranged GET that must be `text/csv` (that server answers a missing path
# with 200/206 text/plain, so a status proves nothing) with the raw columns
# load_baci_dic() selects. A dictionary problem is a PARTIAL failure: the
# HS92 row, CEPII's own version stamp, must still be merged.
#
# curl::curl_fetch_memory is mocked (no network), and so is zip_remote_entries()
# (defined below, in the same environment the resolver was sourced into). The
# responses mirror what was measured live on 2026-10-18/19.

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
  survey = c("baci", "baci_dic"), dataset = c("HS92", "product_codes"),
  geo_level = NA_character_, year = NA_character_
)
dic_rows <- baci_rows[baci_rows$survey == "baci_dic", ]

# The header NCM_SH.csv really starts with (read live 2026-10-19).
ncm_sh_columns <- c(
  "CO_SH6", "NO_SH6_POR", "NO_SH6_ESP", "NO_SH6_ING", "CO_SH4", "NO_SH4_POR", "NO_SH4_ESP",
  "NO_SH4_ING", "CO_SH2", "NO_SH2_POR", "NO_SH2_ESP", "NO_SH2_ING", "CO_NCM_SECROM",
  "NO_SEC_POR", "NO_SEC_ESP", "NO_SEC_ING"
)
csv_response <- function(drop = character(0)) {
  list(
    status_code = 206L,
    headers = charToRaw("HTTP/1.1 206 Partial Content\r\ncontent-type: text/csv\r\ncontent-range: bytes 0-4095/7675822\r\n\r\n"),
    content = charToRaw(paste0(
      paste0('"', setdiff(ncm_sh_columns, drop), '"', collapse = ";"),
      '\n"010110";"Animais vivos";"x";"y"\n'
    ))
  )
}
# What the server answers for a path that does not exist: 200 (206 when the
# request carries a Range), text/plain -- the Joomla index.php source.
placeholder_response <- function(status = 200L) {
  list(
    status_code = status,
    headers = charToRaw(paste0("HTTP/1.1 ", status, " X\r\ncontent-type: text/plain; charset=UTF-8\r\n\r\n")),
    content = charToRaw("<?php\n/**\n * @package    Joomla.Site\n */\n")
  )
}
status_response <- function(status) {
  list(
    status_code = status,
    headers = charToRaw(paste0("HTTP/1.1 ", status, " X\r\ncontent-type: text/html\r\n\r\n")),
    content = charToRaw("<html>nope</html>")
  )
}

# A fake world: CEPII's zip answers 200 for every stamp probed (the resolver
# takes the newest), and the dictionary URL answers `dictionary`, which may be
# a response or a function(url) that errors like curl does.
fake_world <- function(dictionary = csv_response(), seen = NULL) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("BACI_HS92_V[0-9]{6}\\.zip$", url)) {
      return(list(status_code = 200L, headers = raw(0), content = raw(0)))
    }
    if (identical(url, BACI_DIC_URL)) {
      return(if (is.function(dictionary)) dictionary(url) else dictionary)
    }
    stop("the fake world has no such URL: ", url)
  }
}
with_world <- function(world, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = world, .package = "curl")
}
dictionary_message <- function(dictionary) {
  tryCatch(
    with_world(fake_world(dictionary), baci_check_dictionary(dic_rows)),
    error = function(e) conditionMessage(e)
  )
}

test_that("a real dictionary resolves to its manifest row on balanca.mdic.gov.br, with no available_time", {
  out <- with_world(fake_world(), baci_check_dictionary(dic_rows))

  expect_equal(out$survey, "baci_dic")
  expect_equal(out$dataset, "product_codes")
  expect_equal(out$url, "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv")
  expect_equal(out$docs_url, BACI_DIC_DOCS_URL)
  # no years, no version: not columns of this row at all ("leave the cell alone")
  expect_false(any(c("available_time", "version", "archive_file") %in% names(out)))
})

test_that("columns load_baci_dic() does not select may come and go; the ones it selects may not", {
  expect_no_error(with_world(fake_world(csv_response(drop = c("CO_SH4", "NO_SEC_ING"))), baci_check_dictionary(dic_rows)))

  msg <- dictionary_message(csv_response(drop = "NO_SH6_ING"))
  expect_match(msg, "no longer carries the columns R/baci.R's load_baci_dic() selects", fixed = TRUE)
  expect_match(msg, "missing NO_SH6_ING", fixed = TRUE)
  expect_match(dictionary_message(csv_response(drop = c("CO_SH6", "NO_SH6_POR"))), "missing CO_SH6, NO_SH6_POR", fixed = TRUE)
})

test_that("the server's text/plain placeholder is a missing file, whether it comes as 200 or 206", {
  for (status in c(200L, 206L)) {
    msg <- dictionary_message(placeholder_response(status))
    expect_match(msg, "no real CSV (text/csv)", fixed = TRUE)
    expect_match(msg, paste0("HTTP ", status), fixed = TRUE)
    expect_match(msg, "text/plain placeholder", fixed = TRUE)
  }
})

test_that("any other status stops instead of being read as success", {
  expect_match(dictionary_message(status_response(404L)), "no real CSV (text/csv)", fixed = TRUE)
  expect_match(dictionary_message(status_response(403L)), "HTTP 403", fixed = TRUE)
  # a redirect is the host saying the layout moved; it is not followed
  expect_match(dictionary_message(status_response(302L)), "HTTP 302", fixed = TRUE)
})

test_that("transport failures name their cause: expired certificate, unverifiable chain, anything else", {
  expired <- function(url) stop("schannel: next InitializeSecurityContext failed: SEC_E_CERT_EXPIRED (0x80090328) - The received certificate has expired.")
  msg <- dictionary_message(expired)
  expect_match(msg, "could not reach https://balanca.mdic.gov.br/", fixed = TRUE)
  expect_match(msg, "TLS certificate has expired", fixed = TRUE)
  expect_match(msg, "curl said: schannel", fixed = TRUE)

  chain <- function(url) stop("SSL peer certificate or SSH remote key was not OK: [balanca.mdic.gov.br] SSL certificate problem: unable to get local issuer certificate")
  msg <- dictionary_message(chain)
  expect_match(msg, "cannot verify the server's TLS chain", fixed = TRUE)
  expect_match(msg, "AC SERPRO AR46 OV TLS CA 2025", fixed = TRUE)
  expect_match(msg, "never turn verification off", fixed = TRUE)

  other <- function(url) stop("Timeout was reached")
  msg <- dictionary_message(other)
  expect_match(msg, "no HTTP response", fixed = TRUE)
  expect_match(msg, "curl said: Timeout was reached", fixed = TRUE)
})

test_that("resolve_baci() returns HS92 and the dictionary row together when both check out", {
  seen <- new.env()
  out <- with_world(fake_world(seen = seen), resolve_baci(baci_rows))

  expect_equal(out$survey, c("baci", "baci_dic"))
  expect_equal(out$dataset, c("HS92", "product_codes"))
  expect_null(attr(out, "partial_failures"))
  # HS92 is as before: its own url/version/archive_file/available_time...
  expect_match(out$url[1], "^https://www\\.cepii\\.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V[0-9]{6}\\.zip$")
  expect_match(out$version[1], "^[0-9]{6}$")
  expect_equal(out$available_time[1], "1995-1997")
  # ... and none of it leaks into the dictionary row (NA = "leave the cell alone" downstream)
  expect_true(is.na(out$version[2]) && is.na(out$archive_file[2]) && is.na(out$available_time[2]))
  expect_true(is.na(out$docs_url[1]))
  expect_equal(out$url[2], BACI_DIC_URL)
  expect_equal(out$docs_url[2], BACI_DIC_DOCS_URL)
  expect_true(BACI_DIC_URL %in% seen$urls)
})

test_that("a dictionary problem is a PARTIAL failure: HS92 is still returned and the baci_dic row is left alone", {
  for (bad in list(placeholder_response(206L), csv_response(drop = "CO_SH6"), function(url) stop("Timeout was reached"))) {
    out <- with_world(fake_world(bad), resolve_baci(baci_rows))

    expect_equal(out$survey, "baci") # only HS92: CEPII's own version stamp must not be frozen by this
    expect_equal(out$dataset, "HS92")
    partial <- attr(out, "partial_failures")
    expect_length(partial, 1)
    expect_match(partial, "^baci_dic/product_codes: ")
  }
  # and the reason is the one the dictionary check produced
  out <- with_world(fake_world(function(url) stop("SSL certificate problem: unable to get local issuer certificate")), resolve_baci(baci_rows))
  expect_match(attr(out, "partial_failures"), "AC SERPRO AR46 OV TLS CA 2025", fixed = TRUE)
})

test_that("without a baci_dic row the dictionary is never requested and nothing else changes", {
  seen <- new.env()
  only_hs92 <- baci_rows[baci_rows$survey == "baci", ]
  out <- with_world(fake_world(seen = seen), resolve_baci(only_hs92))

  expect_equal(out$survey, "baci")
  expect_null(attr(out, "partial_failures"))
  expect_false(BACI_DIC_URL %in% seen$urls)
  # the HS92 row is the same one the full call returns (which only adds a
  # docs_url column, NA for HS92, when it binds the dictionary row)
  full <- with_world(fake_world(), resolve_baci(baci_rows))
  expect_equal(out[1, ], full[1, names(out)], ignore_attr = TRUE)
})

test_that("the committed baci_dic row is owned by this resolver and agrees with what it emits", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")

  d <- m[m$survey == "baci_dic", ]
  expect_equal(d$dataset, "product_codes")
  expect_equal(d$resolver, "baci")
  expect_equal(d$url, BACI_DIC_URL)
  expect_equal(d$docs_url, BACI_DIC_DOCS_URL)
  expect_true(is.na(d$available_time) && is.na(d$geo_level) && is.na(d$year))

  # CEPII's own row is untouched by all this
  h <- m[m$survey == "baci", ]
  expect_equal(h$dataset, "HS92")
  expect_equal(h$resolver, "baci")
})

test_that("R/baci.R takes the dictionary URL from the manifest, hard-codes none, and selects what the resolver demands", {
  baci_src <- file.path(pkg_root, "R", "baci.R")
  skip_if_not(file.exists(baci_src), "R/baci.R not present")

  # (a) statically: no URL in the code (comments may cite examples), and the
  # raw columns the resolver requires are the ones the loader names
  code <- readLines(baci_src, warn = FALSE)
  code <- code[!grepl("^\\s*#", code) & !grepl("^#'", code)]
  expect_false(any(grepl("https?://", code)), info = "R/baci.R's code must not contain a URL literal")
  expect_true(any(grepl('dataset_url(source = "baci_dic", dataset = "product_codes")', code, fixed = TRUE)))
  src <- tolower(paste(code, collapse = "\n"))
  for (col in BACI_DIC_REQUIRED) {
    expect_true(grepl(paste0("\\b", tolower(col), "\\b"), src), info = paste(col, "is required by the resolver but R/baci.R never selects it"))
  }

  skip_if_not(exists("load_baci_dic", mode = "function"), "package code not loaded")

  # (b) behaviorally: what load_baci_dic() reads is exactly what the manifest
  # lookup returns (a made-up manifest drives it)
  seen <- character(0)
  read_spy <- function(file, ...) {
    seen <<- c(seen, file)
    tibble::tibble(co_sh6 = "010110", no_sh6_por = "Cavalos", no_sh6_ing = "Horses")
  }
  load_with <- function(dataset_url_fn) {
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(suppressMessages(load_baci_dic(language = "eng")), read_delim = read_spy, .package = "readr"),
      dataset_url = dataset_url_fn, .package = "datazoom.amazonia"
    )
  }
  out <- load_with(function(source, dataset, ...) paste0("manifest://", source, "/", dataset))
  expect_equal(seen, "manifest://baci_dic/product_codes")
  expect_equal(names(out), c("product_code", "product"))
  expect_equal(out$product, "Horses")

  # (c) the committed manifest supplies it (setup.R forces the local snapshot)
  expect_equal(dataset_url(source = "baci_dic", dataset = "product_codes"), BACI_DIC_URL)

  # (d) a manifest without the row fails with a clear message, not read_delim(NA)
  expect_error(
    load_with(function(source, dataset, ...) NA_character_),
    "No download URL found for source 'baci_dic', dataset 'product_codes'",
    fixed = TRUE
  )
})
