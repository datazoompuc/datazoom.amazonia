# TLS fallback for balanca.mdic.gov.br -- the ministry's COMEX host, which
# serves load_br_trade()'s data and the product dictionaries read by
# load_br_trade() and load_baci().
#
# THE PROBLEM. That server sends its leaf certificate but NOT the intermediate
# CA that issued it ("AC SERPRO AR46 OV TLS CA 2025"). Windows and macOS fetch a
# missing intermediate themselves (the certificate's AIA field); OpenSSL-based
# systems -- Linux, i.e. servers, CI and RStudio Server -- do not, so every
# request there stops with "SSL certificate problem: unable to get local issuer
# certificate". Measured 2026-10-21 with the package's own code on an OpenSSL
# libcurl and the stock Ubuntu 22.04 / 24.04 ca-certificates bundles (none of
# them contains the intermediate; all contain its root, GlobalSign Root R46).
# The CI workflows get around it by installing the intermediate into the
# runner's trust store (.github/actions/trust-serpro-intermediate), which can
# never run on a user's machine -- hence this file.
#
# WHAT IT DOES. A request to balanca.mdic.gov.br is made exactly as before. Only
# if it fails with a certificate error is it repeated ONCE, with libcurl told to
# trust inst/extdata/tls/balanca-serpro-chain.pem -- GlobalSign Root R46 plus
# the SERPRO intermediate -- instead of the system store. Consequences:
#   * Nothing changes where the first attempt works (Windows, macOS, and any
#     Linux whose server-side problem is ever fixed): the fallback never runs.
#   * The fallback applies to balanca.mdic.gov.br only (is_balanca_url()); no
#     other host's verification is touched, and nothing is set process-wide.
#   * Verification is NEVER turned off. The handshake must still produce a
#     chain from the server's leaf, through the bundled intermediate, to the
#     bundled root, with valid signatures, dates and the host name; a leaf from
#     any other CA, for any other host, or an expired one, is still rejected
#     (measured: the same bundle rejects google.com, the ministry's own
#     www.mdic.gov.br, a missing intermediate, and a host-name mismatch).
#   * The root is included, rather than trusting the intermediate alone: with
#     the intermediate alone, libcurl 8.x (measured) accepts it as a trust
#     anchor, but older libcurl (Ubuntu 22.04 ships 7.81; not tested here) is
#     only guaranteed to accept a chain that ends in a self-signed root. With
#     the root, the signatures are checked all the way up on any version. R46 is
#     also in Mozilla's store (checked: byte-identical).
#
# MAINTENANCE. The bundled intermediate is valid until 2030-10-15. If SERPRO
# rotates it the fallback fails (with the message below), and the package needs
# the new certificate: tests/testthat/test-balanca-tls.R fails before the old
# one expires, and the CI action warns when the live intermediate differs from
# the bundled one. Chosen over fetching the intermediate at run time (from the
# leaf's AIA URL) because that needs an unverified TLS handshake in R just to
# read a URL, plus certificate-chain code in R, for a certificate that changes
# every few years.

# Is `url` an https URL on balanca.mdic.gov.br itself? (Not a look-alike:
# "balanca.mdic.gov.br.example.com" and "balanca.mdic.gov.br@example.com" are not.)
is_balanca_url <- function(url) {
  is.character(url) && length(url) == 1L && !is.na(url) &&
    grepl("^https://balanca\\.mdic\\.gov\\.br(:443)?([/?#]|$)", url, ignore.case = TRUE)
}

balanca_chain_file <- function() {
  system.file("extdata", "tls", "balanca-serpro-chain.pem", package = "datazoom.amazonia")
}

# libcurl words this differently per backend ("SSL certificate problem: ...",
# "SSL peer certificate or SSH remote key was not OK", Schannel's variants), and
# R's own download.file() reports only the generic one as a warning -- hence
# matching on the word, and looking at warnings as well as the error.
is_certificate_failure <- function(messages) {
  any(grepl("certificate", messages, ignore.case = TRUE))
}

# Runs `attempt(NULL)`; if it fails on a certificate error and `url` is on
# balanca.mdic.gov.br, runs `attempt(handle)` once, where `handle` is a curl
# handle that trusts the bundled chain (and nothing else). `attempt` must use
# the handle only when it is not NULL. Any other URL: plain `attempt(NULL)`,
# untouched.
balanca_with_fallback <- function(url, attempt) {
  if (!is_balanca_url(url)) {
    return(attempt(NULL))
  }

  # R's download.file() reports why it failed as a warning before the error.
  # Hold the warnings back: drop them if the fallback works, pass them on if not.
  held <- character(0)
  first <- tryCatch(
    withCallingHandlers(
      attempt(NULL),
      warning = function(w) {
        held <<- c(held, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  pass_on_warnings <- function() for (w in held) warning(w, call. = FALSE)

  if (!inherits(first, "error")) {
    pass_on_warnings()
    return(first)
  }

  chain <- balanca_chain_file()
  if (!is_certificate_failure(c(conditionMessage(first), held)) || !nzchar(chain)) {
    pass_on_warnings()
    stop(first)
  }

  rlang::inform(
    paste(
      "balanca.mdic.gov.br does not send its intermediate certificate and this system does not look it up;",
      "verifying it with the SERPRO/GlobalSign chain bundled with datazoom.amazonia. TLS verification stays on."
    ),
    .frequency = "once", .frequency_id = "datazoom.amazonia.balanca_tls"
  )
  second <- tryCatch(attempt(curl::new_handle(cainfo = chain)), error = function(e) e)
  if (!inherits(second, "error")) {
    return(second)
  }

  stop(
    "Could not verify the TLS certificate of ", url, ".\n",
    "- with this system's certificate store: ", conditionMessage(first), "\n",
    "- with the SERPRO chain bundled with datazoom.amazonia (inst/extdata/tls/balanca-serpro-chain.pem): ",
    conditionMessage(second), "\n",
    "TLS verification is never turned off. If the second message says the certificate has expired, the ",
    "server's certificate is the problem; if it says the issuer cannot be found, SERPRO has changed its ",
    "intermediate CA and the bundled chain needs updating (please report it at ",
    "https://github.com/datazoompuc/datazoom.amazonia/issues).",
    call. = FALSE
  )
}

# utils::download.file(url, destfile, mode = "wb") -- the call every "standard"
# download in external_download() makes -- with the fallback above for
# balanca.mdic.gov.br. For every other URL it is that call, unchanged.
balanca_download_file <- function(url, destfile) {
  balanca_with_fallback(url, function(handle) {
    if (is.null(handle)) {
      utils::download.file(url = url, destfile = destfile, mode = "wb")
    } else {
      curl::curl_download(url, destfile, quiet = FALSE, mode = "wb", handle = handle)
    }
  })
}

# readr::read_delim(url, ...) with the fallback above for balanca.mdic.gov.br.
# readr reads a URL through curl::curl() with a default handle, so the fallback
# hands it a connection that carries the bundle instead. For every other URL it
# is readr::read_delim(url, ...), unchanged.
balanca_read_delim <- function(url, ...) {
  balanca_with_fallback(url, function(handle) {
    readr::read_delim(if (is.null(handle)) url else curl::curl(url, handle = handle), ...)
  })
}
