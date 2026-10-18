# tests/testthat/test-comex-resolver.R
#
# Offline test for actions/scrapers/resolve_comex.R (added 2026-10-17; see
# that file's header for the live investigation every assumption below comes
# from). COMEX publishes one bulk CSV per dataset per year; the resolver
# probes the candidate years with ONE ranged GET each and keeps available_time
# and the host current. The server's behaviors that shaped it, mocked here:
#   * a file that does not exist still answers 200 -- `text/plain`, a 1420-byte
#     Joomla index.php -- so a status code (or a HEAD) proves nothing and only
#     `text/csv` means "file";
#   * the old host's certificate expired and both hosts send an incomplete TLS
#     chain, so the resolver must stop() with a message that names the cause,
#     never weaken verification or guess a range;
#   * the loader (R/comex.R) reads specific raw columns, so a header missing
#     one must stop the resolver before it publishes that year.
#
# curl::curl_fetch_memory is mocked (no network). The fake world mirrors what
# was measured live on 2026-10-17: files for 1997-2026 in every dataset, the
# import-by-NCM files carrying two extra columns (VL_FRETE, VL_SEGURO).

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_comex.R"), local = TRUE)

comex_datasets <- c("export_mun", "export_prod", "import_mun", "import_prod")
comex_rows <- tibble::tibble(
  survey = "comex", dataset = comex_datasets,
  geo_level = NA_character_, year = NA_character_
)

# The header line each dataset's real files start with (measured live).
header_line <- function(dataset, drop = character(0)) {
  cols <- switch(dataset,
    export_mun = , import_mun = COMEX_MUN_COLUMNS,
    export_prod = COMEX_NCM_COLUMNS,
    import_prod = c(COMEX_NCM_COLUMNS, "VL_FRETE", "VL_SEGURO")
  )
  paste0('"', setdiff(cols, drop), '"', collapse = ";")
}

csv_response <- function(dataset, drop = character(0)) {
  list(
    status_code = 206L,
    headers = charToRaw("HTTP/1.1 206 Partial Content\r\ncontent-type: text/csv\r\ncontent-range: bytes 0-4095/60077097\r\n\r\n"),
    content = charToRaw(paste0(header_line(dataset, drop), '\n"2025";"07";"27011200"\n'))
  )
}
# What the server answers for a path that does not exist: 200, text/plain.
placeholder_response <- function() {
  list(
    status_code = 200L,
    headers = charToRaw("HTTP/1.1 200 OK\r\ncontent-type: text/plain; charset=UTF-8\r\ncontent-length: 1420\r\n\r\n"),
    content = charToRaw("<?php\n/**\n * @package    Joomla.Site\n */\ndefine('_JEXEC', 1);\n")
  )
}
status_response <- function(status, ctype = "text/html") {
  list(
    status_code = status,
    headers = charToRaw(paste0("HTTP/1.1 ", status, " X\r\ncontent-type: ", ctype, "\r\n\r\n")),
    content = charToRaw("<html>nope</html>")
  )
}

# A fake COMEX server. `years` = which years exist per dataset; `drop` =
# columns missing from a given dataset's files (optionally only some years);
# `override` = function(dataset, year) -> a response, or NULL to fall through.
fake_comex <- function(years = NULL, drop = list(), override = function(dataset, year) NULL, seen = NULL) {
  if (is.null(years)) {
    years <- stats::setNames(rep(list(1997:2026), length(comex_datasets)), comex_datasets)
  }
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    m <- regmatches(url, regexec("/comexstat-bd/(mun|ncm)/(EXP|IMP)_([0-9]{4})(_MUN)?\\.csv$", url))[[1]]
    if (length(m) == 0) {
      return(placeholder_response())
    }
    dataset <- paste0(
      c(EXP = "export", IMP = "import")[[m[3]]], "_",
      c(mun = "mun", ncm = "prod")[[m[2]]]
    )
    year <- as.integer(m[4])
    o <- override(dataset, year)
    if (!is.null(o)) {
      return(o)
    }
    if (year %in% years[[dataset]]) csv_response(dataset, drop[[dataset]]) else placeholder_response()
  }
}

run <- function(fetch, rows = comex_rows) {
  testthat::with_mocked_bindings(
    resolve_comex(rows),
    curl_fetch_memory = fetch, .package = "curl"
  )
}
error_message <- function(fetch, rows = comex_rows) {
  tryCatch(run(fetch, rows), error = function(e) conditionMessage(e))
}

test_that("every dataset gets its own verified range and the `$year$` URL on the current host", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen))

  expect_named(out, c("survey", "dataset", "geo_level", "year", "url", "docs_url", "available_time"))
  expect_equal(out$dataset, comex_datasets)
  expect_equal(out$available_time, rep("1997-2026", 4))
  expect_equal(out$url, c(
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/mun/EXP_$year$_MUN.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_$year$.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/mun/IMP_$year$_MUN.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/IMP_$year$.csv"
  ))
  expect_true(all(out$docs_url == COMEX_DOCS_URL))
  # blank-key rows only, never a dataset = NA row and no monthly `version`
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  expect_false("version" %in% names(out))

  # nothing was probed anywhere but the current host, and `ncmv2/` never is
  expect_true(all(startsWith(seen$urls, COMEX_HOST)))
  expect_false(any(grepl("ncmv2", seen$urls, fixed = TRUE)))
})

test_that("a 200 text/plain placeholder is a missing file, not a year (the server's catch-all)", {
  years <- stats::setNames(rep(list(1997:2025), 4), comex_datasets) # 2026 and everything else: placeholder
  out <- run(fake_comex(years))
  expect_equal(out$available_time, rep("1997-2025", 4))
})

test_that("ranges are per dataset, not shared", {
  years <- list(
    export_mun = 1997:2026, export_prod = 1997:2026,
    import_mun = 1997:2025, import_prod = 1998:2025
  )
  out <- run(fake_comex(years))
  expect_equal(stats::setNames(out$available_time, out$dataset), c(
    export_mun = "1997-2026", export_prod = "1997-2026",
    import_mun = "1997-2025", import_prod = "1998-2025"
  ))
})

test_that("only the manifest rows it owns are returned, and only their files are probed", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen), rows = comex_rows[comex_rows$dataset == "import_mun", ])
  expect_equal(out$dataset, "import_mun")
  expect_true(all(grepl("/mun/IMP_", seen$urls, fixed = TRUE)))
})

test_that("no real file at all stops, naming the dataset and the path probed", {
  msg <- error_message(fake_comex(stats::setNames(rep(list(integer(0)), 4), comex_datasets)))
  expect_match(msg, "no real CSV (text/csv) found for 'export_mun'", fixed = TRUE)
  expect_match(msg, "mun/EXP_$year$_MUN.csv", fixed = TRUE)
})

test_that("a hole inside the range stops: available_time cannot express a gap", {
  years <- stats::setNames(rep(list(setdiff(1997:2026, 2010L)), 4), comex_datasets)
  msg <- error_message(fake_comex(years))
  expect_match(msg, "except year(s) 2010", fixed = TRUE)
})

test_that("a header missing a column R/comex.R reads stops, naming the column and the year", {
  msg <- error_message(fake_comex(drop = list(import_mun = "CO_MUN")))
  expect_match(msg, "'import_mun' files no longer carry the columns R/comex.R reads", fixed = TRUE)
  expect_match(msg, "missing CO_MUN", fixed = TRUE)

  # only recent years broken: still caught, because every year is checked
  recent_only <- function(dataset, year) {
    if (dataset == "export_prod" && year >= 2025) csv_response(dataset, drop = "KG_LIQUIDO") else NULL
  }
  msg <- error_message(fake_comex(override = recent_only))
  expect_match(msg, "2025: missing KG_LIQUIDO", fixed = TRUE)
})

test_that("extra columns are fine (import-by-NCM files carry VL_FRETE and VL_SEGURO)", {
  expect_equal(header_line("import_prod"), paste0('"', c(COMEX_NCM_COLUMNS, "VL_FRETE", "VL_SEGURO"), '"', collapse = ";"))
  expect_equal(run(fake_comex())$dataset, comex_datasets)
})

test_that("a status that is neither a CSV nor a clean 404 stops instead of being read as 'absent'", {
  msg <- error_message(fake_comex(override = function(dataset, year) if (year == 2026) status_response(403L) else NULL))
  expect_match(msg, "unexpected HTTP 403", fixed = TRUE)

  # a 302 is the host saying the layout moved: neither "file" nor "absent".
  # (That the resolver's curl handle does not FOLLOW redirects is a handle
  # option, invisible to this mock -- not covered here; see the resolver.)
  msg <- error_message(fake_comex(override = function(dataset, year) if (year == 1995) status_response(302L) else NULL))
  expect_match(msg, "unexpected HTTP 302", fixed = TRUE)

  # a clean 404 (should the server ever stop faking 200) is simply absent
  not_found <- function(dataset, year) if (year > 2025 || year < 1997) status_response(404L) else NULL
  expect_equal(run(fake_comex(override = not_found))$available_time, rep("1997-2025", 4))
})

test_that("an expired certificate stops with the cause named", {
  expired <- function(url, handle = NULL, ...) {
    stop("schannel: next InitializeSecurityContext failed: SEC_E_CERT_EXPIRED (0x80090328) - The received certificate has expired.")
  }
  msg <- error_message(expired)
  expect_match(msg, "could not reach https://balanca.mdic.gov.br/", fixed = TRUE)
  expect_match(msg, "TLS certificate has expired", fixed = TRUE)
  expect_match(msg, "curl said: schannel", fixed = TRUE)
})

test_that("an unverifiable chain (OpenSSL, intermediate not sent) stops and says what to add", {
  chain <- function(url, handle = NULL, ...) {
    stop("SSL peer certificate or SSH remote key was not OK: [balanca.mdic.gov.br] SSL certificate problem: unable to get local issuer certificate")
  }
  msg <- error_message(chain)
  expect_match(msg, "cannot verify the server's TLS certificate chain", fixed = TRUE)
  expect_match(msg, "AC SERPRO AR46 OV TLS CA 2025", fixed = TRUE)
  expect_match(msg, "never turn verification off", fixed = TRUE)
})

test_that("any other transport failure is reported as no HTTP response, with curl's text", {
  msg <- error_message(function(url, handle = NULL, ...) stop("Timeout was reached"))
  expect_match(msg, "no HTTP response", fixed = TRUE)
  expect_match(msg, "curl said: Timeout was reached", fixed = TRUE)
})

test_that("a dataset this resolver does not know, or no rows, stops", {
  rows <- tibble::tibble(survey = "comex", dataset = "export_sh6", geo_level = NA_character_, year = NA_character_)
  expect_match(error_message(fake_comex(), rows), "only knows export_mun, import_mun, export_prod, import_prod", fixed = TRUE)
  expect_match(error_message(fake_comex(), comex_rows[0, ]), "no manifest rows tagged resolver == 'comex'", fixed = TRUE)
})

test_that("header parsing tolerates a BOM, quotes, case and garbage bytes", {
  expect_equal(
    comex_header_columns(charToRaw("﻿\"co_ano\";\"CO_MES\";SH4\r\n\"2025\";\"01\"\n")),
    c("CO_ANO", "CO_MES", "SH4")
  )
  expect_equal(comex_header_columns(raw(0)), character(0))
  expect_no_error(comex_header_columns(as.raw(c(0xe9, 0xff, 0x3b, 0x00, 0x41, 0x0a))))
})

test_that("the columns the resolver demands are the ones R/comex.R actually reads", {
  comex_src <- file.path(pkg_root, "R", "comex.R")
  skip_if_not(file.exists(comex_src), "R/comex.R not present")
  src <- tolower(paste(readLines(comex_src, warn = FALSE), collapse = "\n"))
  for (dataset in comex_datasets) {
    for (col in COMEX_DATASETS[[dataset]]$required) {
      expect_true(
        grepl(paste0("\\b", tolower(col), "\\b"), src),
        info = paste(dataset, col, "is required by resolve_comex.R but R/comex.R never mentions it")
      )
    }
  }
})

test_that("R/comex.R's hard-coded dictionary host is the one the resolver publishes", {
  # load_trade_dic() builds NCM.csv / NCM_SH.csv URLs from a literal no
  # resolver touches; on 2026-10-03 the old host's certificate expired and
  # that literal broke load_br_trade() while the manifest's data URLs were
  # fine. Keep them on the same host.
  comex_src <- file.path(pkg_root, "R", "comex.R")
  skip_if_not(file.exists(comex_src), "R/comex.R not present")
  lines <- readLines(comex_src, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)] # code only, not the comments citing old URLs
  path_line <- grep('path <- "https://', lines, fixed = TRUE, value = TRUE)
  expect_length(path_line, 1)
  dictionary_base <- sub('.*path <- "([^"]+)".*', "\\1", path_line)
  expect_equal(dictionary_base, paste0(COMEX_HOST, "/balanca/bd/"))
  expect_false(any(grepl("economia.gov.br", lines, fixed = TRUE)))
})

test_that("the committed manifest rows agree with what the resolver owns and emits", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")
  m <- m[m$survey == "comex", ]
  expect_setequal(m$dataset, comex_datasets)
  expect_true(all(m$resolver == "comex"))
  expect_true(all(startsWith(m$url, COMEX_BASE)))
  expect_true(all(grepl("$year$", m$url, fixed = TRUE)))
  expect_true(all(m$docs_url == COMEX_DOCS_URL))
  expect_true(all(grepl("^1997-[0-9]{4}$", m$available_time)))
})
