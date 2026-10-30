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
  # A self-signed root, then the intermediate it signed (today GlobalSign Root R46 and
  # AC SERPRO AR46 OV TLS CA 2025). A chain of another shape is not something the
  # automatic refresh (see refresh-bundled-chain.sh) should be trusted to judge: a person decides.
  expect_length(blocks, 2)
})

# What the file's own header says about each certificate: "# [n] <name> -- <role>" and "#     SHA-256 <fingerprint>".
# That header is written by hand or by refresh-bundled-chain.sh, never by these tests, so that a rotation does not
# need this file edited: the tests hold the header to what the certificates really are instead of to a name.
header_claims <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- x[grepl("^#", x)]
  names <- sub("^# \\[[0-9]+\\] (.*?) -- .*$", "\\1", grep("^# \\[[0-9]+\\] ", x, value = TRUE), perl = TRUE)
  sha <- sub("^#\\s+SHA-256 ", "", grep("^#\\s+SHA-256 ", x, value = TRUE))
  list(names = names, sha = sha)
}

test_that("the bundled certificates are what they claim: a self-signed root and the intermediate it signed, as the file's own header names them, not about to expire", {
  skip_if(Sys.which("openssl") == "", "the openssl command line is not available")
  blocks <- pem_blocks(balanca_chain_file())
  skip_if(length(blocks) != 2, "covered by the previous test")
  root <- tempfile(fileext = ".pem")
  inter <- tempfile(fileext = ".pem")
  writeLines(blocks[[1]], root)
  writeLines(blocks[[2]], inter)

  claims <- header_claims(balanca_chain_file())
  expect_length(claims$names, 2)
  expect_length(claims$sha, 2)
  expect_match(openssl("x509", "-in", shQuote(root), "-noout", "-subject")$text, claims$names[1], fixed = TRUE)
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-subject")$text, claims$names[2], fixed = TRUE)
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-issuer")$text, claims$names[1], fixed = TRUE)
  # the fingerprints the header prints are the certificates' own
  fingerprint <- function(f) sub("^.*Fingerprint=", "", openssl("x509", "-in", shQuote(f), "-noout", "-fingerprint", "-sha256")$text)
  expect_equal(c(fingerprint(root), fingerprint(inter)), claims$sha)
  # signatures: the root is self-signed, and it signed the intermediate -- using ONLY the bundled root
  expect_match(openssl("verify", "-no-CApath", "-CAfile", shQuote(root), shQuote(root))$text, ": OK")
  expect_match(openssl("verify", "-no-CApath", "-CAfile", shQuote(root), shQuote(inter))$text, ": OK")
  # a certificate authority that can only sign server certificates directly (pathlen:0)
  expect_match(openssl("x509", "-in", shQuote(inter), "-noout", "-text")$text, "CA:TRUE, pathlen:0")

  # Not expired, and not about to be (the intermediate's notAfter is in the header). If this fails, the
  # intermediate is near the end of its life -- the weekly update-manifest.yaml run opens a pull request
  # with the one the host presents now, as long as the host still does not send it; otherwise fetch the current one
  # from the AIA URL of balanca.mdic.gov.br's certificate (see the header of
  # inst/extdata/tls/balanca-serpro-chain.pem) and replace it.
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


# ---- refresh-bundled-chain.sh: what the weekly pull request contains, on a throwaway CA hierarchy ----------
#
# Run by trust-intermediate.sh when the certificate chain balanca.mdic.gov.br presents is not the bundled one
# (.github/actions/trust-serpro-intermediate). Nothing here is a real certificate and nothing touches the network.

refresh_script <- function() {
  file.path(pkg_root_balanca(), ".github", "actions", "trust-serpro-intermediate", "refresh-bundled-chain.sh")
}
fingerprint_of <- function(f) sub("^.*Fingerprint=", "", openssl("x509", "-in", shQuote(f), "-noout", "-fingerprint", "-sha256")$text)

# root A (self-signed); intermediates "old" and "new" under A; a leaf for `localhost` issued by "new";
# root B with an intermediate "other" and a leaf issued by it. Returns the paths.
fake_pki <- function(dir) {
  p <- function(...) file.path(dir, paste0(...))
  key_csr <- function(name, subj) {
    openssl("req", "-new", "-newkey", "ec", "-pkeyopt", "ec_paramgen_curve:prime256v1", "-nodes",
      "-keyout", shQuote(p(name, ".key")), "-out", shQuote(p(name, ".csr")), "-subj", shQuote(subj))
  }
  root <- function(name, cn) {
    openssl("req", "-x509", "-newkey", "ec", "-pkeyopt", "ec_paramgen_curve:prime256v1", "-nodes",
      "-keyout", shQuote(p(name, ".key")), "-out", shQuote(p(name, ".pem")), "-days", "3650", "-subj", shQuote(paste0("/O=Fake/CN=", cn)),
      "-addext", shQuote("basicConstraints=critical,CA:TRUE"), "-addext", shQuote("keyUsage=critical,keyCertSign,cRLSign"))
  }
  signed <- function(name, cn, issuer, ext) {
    key_csr(name, paste0("/O=Fake/CN=", cn))
    writeLines(ext, p(name, ".ext"))
    openssl("x509", "-req", "-in", shQuote(p(name, ".csr")), "-CA", shQuote(p(issuer, ".pem")), "-CAkey", shQuote(p(issuer, ".key")),
      "-CAcreateserial", "-out", shQuote(p(name, ".pem")), "-days", "1800", "-extfile", shQuote(p(name, ".ext")))
  }
  ca_ext <- c("basicConstraints=critical,CA:TRUE,pathlen:0", "keyUsage=critical,keyCertSign,cRLSign", "extendedKeyUsage=serverAuth")
  leaf_ext <- function(aia) c("basicConstraints=CA:FALSE", "keyUsage=digitalSignature", "extendedKeyUsage=serverAuth", "subjectAltName=DNS:localhost",
    paste0("authorityInfoAccess=caIssuers;URI:", aia))
  root("rootA", "Fake Root A")
  signed("old", "Fake Intermediate Old", "rootA", ca_ext)
  signed("new", "Fake Intermediate New", "rootA", ca_ext)
  signed("leaf", "localhost", "new", leaf_ext("http://example.invalid/new.crt"))
  root("rootB", "Fake Root B")
  signed("other", "Fake Intermediate Other", "rootB", ca_ext)
  signed("leafB", "localhost", "other", leaf_ext("http://example.invalid/other.crt"))
  stats::setNames(lapply(c("rootA", "old", "new", "leaf", "rootB", "other", "leafB"), p, ".pem"), c("rootA", "old", "new", "leaf", "rootB", "other", "leafB"))
}

# A bash that understands the paths R hands it. On Windows, Sys.which("bash") can be the WSL launcher in
# System32, which does not (it prints a blank line and fails): probe each candidate by having it read a file.
find_bash <- function() {
  probe <- tempfile()
  writeLines("probe", probe)
  probe <- normalizePath(probe, winslash = "/")
  candidates <- unique(c(Sys.which("bash"), "C:/Program Files/Git/bin/bash.exe", "C:/Program Files/Git/usr/bin/bash.exe"))
  for (b in candidates[nzchar(candidates) & file.exists(candidates)]) {
    out <- suppressWarnings(tryCatch(system2(b, c("-c", shQuote(paste("cat", shQuote(probe)))), stdout = TRUE, stderr = TRUE), error = function(e) "error"))
    if (identical(trimws(out), "probe") && is.null(attr(out, "status"))) {
      return(normalizePath(b, winslash = "/"))
    }
  }
  NA_character_
}
run_refresh <- function(bundle, leaf, issuers, body = NULL) {
  unix <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)
  # (system2's `env` is a Unix-shell prefix and does not work on Windows; withr sets the process environment)
  vars <- c(REFRESH_DATE = "2026-11-01", REFRESH_BODY = if (is.null(body)) "" else unix(body))
  out <- withr::with_envvar(vars, suppressWarnings(system2(find_bash(), # (system2 quotes a command with spaces itself)
    c(shQuote(unix(refresh_script())), shQuote(unix(bundle)), shQuote(unix(leaf)), "localhost", shQuote(unix(issuers))),
    stdout = TRUE, stderr = TRUE
  )))
  list(text = paste(out, collapse = "\n"), status = attr(out, "status") %||% 0L)
}
tools_available <- function() {
  skip_if(Sys.which("openssl") == "", "the openssl command line is not available")
  skip_if(is.na(find_bash()), "no bash that can read R's file paths")
  skip_if_not(file.exists(refresh_script()), ".github/ not present (stripped build)")
}

test_that("a rotated intermediate replaces the old one, the root is kept, and the new file verifies the host BY ITSELF", {
  tools_available()
  dir <- withr::local_tempdir()
  pki <- fake_pki(dir)
  bundle <- file.path(dir, "bundle.pem")
  body <- file.path(dir, "body.md")
  writeLines(c("# the old header", readLines(pki$rootA), readLines(pki$old)), bundle)

  res <- run_refresh(bundle, pki$leaf, pki$new, body)
  expect_equal(res$status, 0L, info = res$text)

  blocks <- pem_blocks(bundle)
  expect_length(blocks, 2)
  written <- vapply(blocks, function(b) {
    f <- tempfile(fileext = ".pem")
    writeLines(b, f)
    fingerprint_of(f)
  }, character(1))
  expect_equal(written, c(fingerprint_of(pki$rootA), fingerprint_of(pki$new)))
  expect_false(fingerprint_of(pki$old) %in% written)

  # the header was regenerated from the certificates: names, roles, dates, and the fingerprints are the real ones
  claims <- header_claims(bundle)
  expect_equal(claims$names, c("Fake Root A", "Fake Intermediate New"))
  expect_equal(claims$sha, written)
  head_text <- paste(readLines(bundle)[grepl("^#", readLines(bundle))], collapse = "\n")
  expect_match(head_text, "Regenerated 2026-11-01", fixed = TRUE)
  expect_match(head_text, "[2] Fake Intermediate New -- issued by Fake Root A", fixed = TRUE)
  expect_match(head_text, "source: http://example.invalid/new.crt", fixed = TRUE) # the AIA URL of the certificate it issued
  expect_match(head_text, "source: kept from the previous bundle", fixed = TRUE)

  # what R does with the file: it is the ONLY trust store, and the host name and purpose are checked
  v <- openssl("verify", "-CAfile", shQuote(bundle), "-no-CApath", "-purpose", "sslserver", "-verify_hostname", "localhost", shQuote(pki$leaf))
  expect_match(v$text, ": OK")

  # the pull request description says what changed, in both directions
  text <- paste(readLines(body), collapse = "\n")
  expect_match(text, fingerprint_of(pki$old), fixed = TRUE)
  expect_match(text, fingerprint_of(pki$new), fixed = TRUE)
  expect_match(text, "Fake Intermediate Old", fixed = TRUE)
  expect_match(text, "Fake Intermediate New", fixed = TRUE)
  expect_match(text, "Before merging", fixed = TRUE)
})

test_that("a second run on the refreshed bundle changes nothing (exit 2), so the weekly pull request does not churn", {
  tools_available()
  dir <- withr::local_tempdir()
  pki <- fake_pki(dir)
  bundle <- file.path(dir, "bundle.pem")
  writeLines(c("# the old header", readLines(pki$rootA), readLines(pki$old)), bundle)
  expect_equal(run_refresh(bundle, pki$leaf, pki$new)$status, 0L)
  before <- readLines(bundle)
  res <- run_refresh(bundle, pki$leaf, pki$new)
  expect_equal(res$status, 2L, info = res$text)
  expect_equal(readLines(bundle), before)
})

test_that("a chain that leads to a root the bundle does not carry is refused: nothing is written, a person decides", {
  tools_available()
  dir <- withr::local_tempdir()
  pki <- fake_pki(dir)
  bundle <- file.path(dir, "bundle.pem")
  writeLines(c("# the old header", readLines(pki$rootA), readLines(pki$old)), bundle)
  before <- readLines(bundle)
  body <- file.path(dir, "body.md")
  res <- run_refresh(bundle, pki$leafB, pki$other, body)
  expect_equal(res$status, 3L, info = res$text)
  expect_match(res$text, "needs a person", fixed = TRUE)
  expect_equal(readLines(bundle), before)
  expect_false(file.exists(body))
})
