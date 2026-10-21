# tests/testthat/test-balanca-tls.R
#
# Offline tests for R/balanca_tls.R -- the certificate-error fallback for
# balanca.mdic.gov.br, which sends its leaf TLS certificate but not the
# intermediate CA that issued it (see that file's header). OpenSSL-based systems
# (Linux) cannot verify such a server on their own: measured 2026-10-21, the
# package's own load_baci_dic()/load_trade_dic()/download.file() all stopped with
# "unable to get local issuer certificate" on an OpenSSL libcurl with the stock
# Ubuntu 22.04/24.04 CA bundles, while the same calls worked on Windows.
#
# Nothing here touches the network: requests are stand-ins, and the bundled
# certificates are inspected with the openssl command line where it exists.

cert_error <- function() {
  stop(
    "SSL peer certificate or SSH remote key was not OK: [balanca.mdic.gov.br] ",
    "SSL certificate problem: unable to get local issuer certificate",
    call. = FALSE
  )
}
BALANCA <- "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv"
pkg_root_balanca <- function() normalizePath(test_path("..", ".."), mustWork = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- which URLs the fallback may ever apply to -------------------------------

test_that("is_balanca_url() accepts balanca.mdic.gov.br itself and nothing that merely resembles it", {
  yes <- c(
    BALANCA, "https://balanca.mdic.gov.br", "https://BALANCA.mdic.gov.br/x", "https://balanca.mdic.gov.br:443/x",
    "https://balanca.mdic.gov.br?a=1", "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_1997.csv"
  )
  no <- c(
    "http://balanca.mdic.gov.br/x", # not https
    "https://balanca.economia.gov.br/x", # the old, different host
    "https://balanca.mdic.gov.br.example.com/x", # look-alike suffix
    "https://balanca.mdic.gov.br@example.com/x", # user-info trick: the host is example.com
    "https://example.com/https://balanca.mdic.gov.br/x",
    "https://www.mdic.gov.br/", "https://mdic.gov.br/", "https://balanca.mdic.gov.br:8443/x",
    NA_character_, ""
  )
  for (u in yes) expect_true(is_balanca_url(u), info = u)
  for (u in no) expect_false(is_balanca_url(u), info = u)
  # not a single string
  expect_false(is_balanca_url(NULL))
  expect_false(is_balanca_url(c(BALANCA, BALANCA)))
  expect_false(is_balanca_url(1))
})

# ---- the fallback's decisions ------------------------------------------------

test_that("another host is never retried, and its errors and warnings pass through untouched", {
  calls <- 0
  testthat::local_mocked_bindings(new_handle = function(...) stop("no handle may be built for another host"), .package = "curl")
  attempt <- function(handle) {
    calls <<- calls + 1
    warning("URL 'https://example.com/x': status was 'SSL peer certificate or SSH remote key was not OK'")
    cert_error()
  }
  expect_warning(
    expect_error(balanca_with_fallback("https://example.com/x", attempt), "unable to get local issuer certificate"),
    "status was"
  )
  expect_equal(calls, 1)
})

test_that("a request that works the first time is made once, with no handle and no notice", {
  rec <- list()
  testthat::local_mocked_bindings(new_handle = function(...) stop("not needed"), .package = "curl")
  out <- balanca_with_fallback(BALANCA, function(handle) {
    rec[length(rec) + 1] <<- list(handle) # list(): assigning NULL with [[<- would drop it
    "data"
  })
  expect_equal(out, "data")
  expect_length(rec, 1)
  expect_null(rec[[1]])
})

test_that("a certificate error on balanca.mdic.gov.br is retried once with a handle that trusts only the bundled chain", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  built <- NULL
  testthat::local_mocked_bindings(
    new_handle = function(...) {
      built <<- list(...)
      structure(list(), class = "fake_handle")
    },
    .package = "curl"
  )
  rec <- list()
  attempt <- function(handle) {
    rec[length(rec) + 1] <<- list(handle)
    if (is.null(handle)) cert_error() else "data"
  }
  expect_message(out <- balanca_with_fallback(BALANCA, attempt), "TLS verification stays on")
  expect_equal(out, "data")
  expect_length(rec, 2)
  expect_null(rec[[1]])
  expect_s3_class(rec[[2]], "fake_handle")
  # the ONLY thing configured is the CA file: verification is never switched off
  expect_named(built, "cainfo")
  expect_equal(built$cainfo, balanca_chain_file())
  expect_true(nzchar(built$cainfo) && file.exists(built$cainfo))

  # the notice is once per session, not once per file
  expect_message(balanca_with_fallback(BALANCA, attempt), NA)
})

test_that("download.file() style failures (a warning naming the certificate, then a plain error) trigger the fallback, and its warning is dropped when the fallback works", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  testthat::local_mocked_bindings(new_handle = function(...) structure(list(), class = "fake_handle"), .package = "curl")
  attempt <- function(handle) {
    if (is.null(handle)) {
      warning("URL 'https://balanca.mdic.gov.br/x': status was 'SSL peer certificate or SSH remote key was not OK'")
      stop("cannot open URL 'https://balanca.mdic.gov.br/x'", call. = FALSE) # note: this message has no "certificate" in it
    }
    "data"
  }
  expect_message(
    expect_warning(out <- balanca_with_fallback(BALANCA, attempt), NA),
    "bundled with datazoom.amazonia"
  )
  expect_equal(out, "data")
})

test_that("when the fallback fails too, the error shows both reasons and says verification is never turned off", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  testthat::local_mocked_bindings(new_handle = function(...) structure(list(), class = "fake_handle"), .package = "curl")
  attempt <- function(handle) {
    if (is.null(handle)) cert_error() else stop("SSL certificate problem: certificate has expired", call. = FALSE)
  }
  err <- tryCatch(suppressMessages(balanca_with_fallback(BALANCA, attempt)), error = function(e) conditionMessage(e))
  expect_match(err, "with this system's certificate store: .*unable to get local issuer certificate")
  expect_match(err, "bundled with datazoom.amazonia .*: SSL certificate problem: certificate has expired")
  expect_match(err, "never turned off", fixed = TRUE)
  expect_match(err, "SERPRO has changed its intermediate CA", fixed = TRUE)
})

test_that("an error that is not about a certificate is not retried: a 404 or a timeout would only be repeated", {
  testthat::local_mocked_bindings(new_handle = function(...) stop("no handle may be built"), .package = "curl")
  for (msg in c("HTTP status was '404 Not Found'", "Timeout was reached", "Could not resolve host: balanca.mdic.gov.br")) {
    calls <- 0
    attempt <- function(handle) {
      calls <<- calls + 1
      warning("URL: ", msg)
      stop(msg, call. = FALSE)
    }
    expect_warning(expect_error(balanca_with_fallback(BALANCA, attempt), msg, fixed = TRUE), msg, fixed = TRUE)
    expect_equal(calls, 1, info = msg)
  }
})

test_that("without the bundled file the original error is raised, not a confusing second one", {
  testthat::local_mocked_bindings(balanca_chain_file = function() "")
  testthat::local_mocked_bindings(new_handle = function(...) stop("no handle may be built"), .package = "curl")
  calls <- 0
  expect_error(
    balanca_with_fallback(BALANCA, function(handle) {
      calls <<- calls + 1
      cert_error()
    }),
    "unable to get local issuer certificate"
  )
  expect_equal(calls, 1)
})

# ---- the two ways the package reads balanca.mdic.gov.br ------------------------

test_that("balanca_download_file() makes the usual download.file() call, then curl_download() with the bundle after a certificate failure", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  seen <- new.env()
  seen$download_file <- list()
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode = "w", ...) {
      seen$download_file[[length(seen$download_file) + 1]] <- list(url = url, mode = mode)
      warning("URL '", url, "': status was 'SSL peer certificate or SSH remote key was not OK'")
      stop("cannot open URL '", url, "'", call. = FALSE)
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet = TRUE, mode = "wb", handle) {
      seen$curl <- list(url = url, mode = mode, handle_class = class(handle))
      writeLines("co_ano;value", destfile)
      invisible(destfile)
    },
    .package = "curl"
  )
  dest <- tempfile(fileext = ".csv")
  suppressMessages(balanca_download_file(BALANCA, dest))
  expect_length(seen$download_file, 1)
  expect_equal(seen$download_file[[1]], list(url = BALANCA, mode = "wb"))
  expect_equal(seen$curl$url, BALANCA)
  expect_equal(seen$curl$mode, "wb")
  expect_true("curl_handle" %in% seen$curl$handle_class)
  expect_equal(readLines(dest), "co_ano;value")
})

test_that("external_download()'s download step reaches the fallback for a COMEX URL but not for another host's", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  n_curl <- 0
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode = "w", ...) {
      warning("status was 'SSL peer certificate or SSH remote key was not OK'")
      stop("cannot open URL", call. = FALSE)
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      n_curl <<- n_curl + 1
      writeLines("ok", destfile)
    },
    .package = "curl"
  )
  dest <- tempfile()
  suppressMessages(perform_download(
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_1997.csv", dest, "standard",
    source = "comex"
  ))
  expect_equal(n_curl, 1)
  expect_warning(
    expect_error(perform_download("https://example.com/x.csv", tempfile(), "standard", source = "pam"), "cannot open URL"),
    "SSL peer certificate"
  )
  expect_equal(n_curl, 1) # unchanged: no fallback for another host
})

test_that("balanca_read_delim() reads a URL as readr always did, and a curl connection carrying the bundle after a certificate failure", {
  rlang::reset_message_verbosity("datazoom.amazonia.balanca_tls")
  rec <- list()
  testthat::local_mocked_bindings(
    read_delim = function(file, ...) {
      rec[[length(rec) + 1]] <<- list(file = file, args = list(...))
      if (is.character(file)) cert_error()
      tibble::tibble(co_sh6 = "010110")
    },
    .package = "readr"
  )
  out <- suppressMessages(balanca_read_delim(BALANCA, delim = ";", progress = FALSE))
  expect_equal(out, tibble::tibble(co_sh6 = "010110"))
  expect_length(rec, 2)
  expect_identical(rec[[1]]$file, BALANCA)
  expect_s3_class(rec[[2]]$file, "connection")
  # the caller's arguments reach readr both times
  expect_equal(rec[[1]]$args, list(delim = ";", progress = FALSE))
  expect_equal(rec[[2]]$args, list(delim = ";", progress = FALSE))
  close(rec[[2]]$file)
})

test_that("both dictionary readers and the COMEX data download go through the fallback, not straight to readr/download.file", {
  src <- function(f) paste(readLines(file.path(pkg_root_balanca(), "R", f), warn = FALSE), collapse = "\n")
  skip_if_not(file.exists(file.path(pkg_root_balanca(), "R", "baci.R")), "package sources not present")
  expect_match(src("baci.R"), "balanca_read_delim(url,", fixed = TRUE)
  expect_match(src("comex.R"), "balanca_read_delim(final,", fixed = TRUE)
  expect_false(grepl("readr::read_delim(url,", src("baci.R"), fixed = TRUE))
  expect_false(grepl("readr::read_delim(final,", src("comex.R"), fixed = TRUE))
  expect_match(src("download.R"), "balanca_download_file(path, temp)", fixed = TRUE)
  expect_false(grepl('utils::download.file(url = path, destfile = temp, mode = "wb")', src("download.R"), fixed = TRUE))
})

test_that("no R code turns TLS verification off, and the bundled chain is configured in exactly one place", {
  skip_if_not(dir.exists(file.path(pkg_root_balanca(), "R")), "package sources not present")
  files <- list.files(file.path(pkg_root_balanca(), "R"), pattern = "\\.R$", full.names = TRUE)
  code <- vapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), character(1))
  # comments may mention these words; only code counts
  code <- vapply(strsplit(code, "\n"), function(l) paste(sub("^\\s*#.*$", "", l), collapse = "\n"), character(1))
  expect_false(any(grepl("ssl_verifypeer|ssl_verifyhost|verifypeer|verifyhost|--insecure|CURLOPT_SSL_VERIFY|NODE_TLS_REJECT", code)))
  expect_identical(basename(names(code))[grepl("cainfo", code)], "balanca_tls.R")
})

# ---- the bundled certificates themselves ---------------------------------------

pem_blocks <- function(path) {
  x <- readLines(path, warn = FALSE)
  starts <- grep("^-----BEGIN CERTIFICATE-----$", x)
  ends <- grep("^-----END CERTIFICATE-----$", x)
  Map(function(s, e) x[s:e], starts, ends)
}
openssl <- function(...) {
  out <- suppressWarnings(system2("openssl", c(...), stdout = TRUE, stderr = TRUE))
  list(text = paste(out, collapse = "\n"), status = attr(out, "status") %||% 0L)
}

test_that("the bundled chain is installed with the package and holds exactly the root and the intermediate", {
  chain <- balanca_chain_file()
  expect_true(nzchar(chain), info = "system.file() found nothing: inst/extdata/tls/balanca-serpro-chain.pem is not installed")
  expect_true(file.exists(chain))
  blocks <- pem_blocks(chain)
  expect_length(blocks, 2) # [1] GlobalSign Root R46, [2] AC SERPRO AR46 OV TLS CA 2025
})

test_that("the bundled certificates are what they claim: a self-signed R46 root and the SERPRO intermediate it signed, not about to expire", {
  skip_if(Sys.which("openssl") == "", "the openssl command line is not available")
  blocks <- pem_blocks(balanca_chain_file())
  skip_if(length(blocks) != 2, "covered by the previous test")
  root <- tempfile(fileext = ".pem")
  inter <- tempfile(fileext = ".pem")
  writeLines(blocks[[1]], root)
  writeLines(blocks[[2]], inter)

  expect_match(openssl("x509", "-in", shQuote(root), "-noout", "-subject")$text, "GlobalSign Root R46")
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-subject")$text, "AC SERPRO AR46 OV TLS CA 2025")
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-issuer")$text, "GlobalSign Root R46")
  # signatures: the root is self-signed, and it signed the intermediate -- using ONLY the bundled root
  expect_match(openssl("verify", "-no-CApath", "-CAfile", shQuote(root), shQuote(root))$text, ": OK")
  expect_match(openssl("verify", "-no-CApath", "-CAfile", shQuote(root), shQuote(inter))$text, ": OK")
  # a certificate authority that can only sign server certificates directly (pathlen:0)
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-text")$text, "CA:TRUE, pathlen:0")

  # Not expired, and not about to be: the intermediate's notAfter is 2030-10-15. If this fails, SERPRO's
  # intermediate is near the end of its life -- fetch the current one from the AIA URL of balanca.mdic.gov.br's
  # certificate (see the header of inst/extdata/tls/balanca-serpro-chain.pem) and replace it.
  day90 <- 90 * 24 * 3600
  expect_equal(openssl("x509", "-in", shQuote(inter), "-noout", "-checkend", day90)$status, 0L,
    info = "the bundled SERPRO intermediate expires within 90 days: renew inst/extdata/tls/balanca-serpro-chain.pem"
  )
  expect_equal(openssl("x509", "-in", shQuote(root), "-noout", "-checkend", day90)$status, 0L)
})

test_that("the bundled root is one the system already trusts, so the fallback adds no new trust anchor", {
  skip_if(Sys.which("openssl") == "", "the openssl command line is not available")
  bundles <- c("/etc/ssl/certs/ca-certificates.crt", "/etc/pki/tls/certs/ca-bundle.crt", "/etc/ssl/cert.pem")
  bundle <- bundles[file.exists(bundles)][1]
  skip_if(is.na(bundle), "no system CA bundle in a standard Linux/macOS location")
  root <- tempfile(fileext = ".pem")
  writeLines(pem_blocks(balanca_chain_file())[[1]], root)
  # a self-signed certificate verifies against a store only if that very certificate is in it
  expect_match(openssl("verify", "-no-CApath", "-CAfile", shQuote(bundle), shQuote(root))$text, ": OK")
})
