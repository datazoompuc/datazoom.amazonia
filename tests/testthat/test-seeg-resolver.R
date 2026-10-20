# tests/testthat/test-seeg-resolver.R
#
# Offline test for actions/scrapers/resolve_seeg.R's failure diagnostics
# (added 2026-10-17, see that file's header): resolve_seeg() fails with HTTP
# 403 from GitHub Actions runners (issue #344) although the same request works
# from an ordinary connection, and a bare "HTTP 403" cannot say whether an
# IP/ASN rule, a JavaScript challenge or a dropped connection is behind it. The
# stop() message now carries the evidence that tells them apart -- status,
# the few headers that matter (server, cf-*, x-*, ...), the HTML <title> and a
# short body excerpt -- while stop() itself stays the final behaviour.
#
# curl::curl_fetch_memory is mocked, so no network is touched. What is real and
# what is not: seeg.eco.br sits behind Amazon CloudFront, and fixtures/
# seeg_cloudfront_403_{headers.txt,body.html} are the exact bytes of its
# refusal of a request WITHOUT the browser User-Agent, captured live on
# 2026-10-17. That is not the CI-IP block itself -- no response from a blocked
# CI run has been captured yet. Every other response below is SYNTHETIC
# (shaped after Cloudflare's documented challenge page, a deliberately
# different product, so the formatting is not tuned to one edge). These tests
# pin the formatting and the failure modes (valid UTF-8, no cookie values,
# bounded size, curl errors kept), not what the CI block will turn out to be.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("jsonlite")
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_seeg.R"), local = TRUE)

seeg_rows <- tibble::tibble(
  survey = "seeg", dataset = "seeg",
  geo_level = NA_character_, year = NA_character_
)

# A curl response shaped like what curl_fetch_memory() returns.
fake_response <- function(status = 403L, headers = raw(0), content = raw(0),
                          url = "https://seeg.eco.br/dados/") {
  list(url = url, status_code = status, headers = headers, content = content)
}

header_block <- function(lines) charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n\r\n"))

# What a Cloudflare managed-challenge refusal looks like (documented shape).
challenge_headers <- header_block(c(
  "HTTP/2 403 ",
  "date: Sat, 17 Oct 2026 12:00:00 GMT",
  "content-type: text/html; charset=UTF-8",
  "server: cloudflare",
  "cf-ray: 9f0c1d2e3a4b5c6d-GRU",
  "cf-mitigated: challenge",
  "x-frame-options: SAMEORIGIN",
  "set-cookie: __cf_bm=SECRETVALUE123; path=/; HttpOnly; Secure",
  "set-cookie: _other=ANOTHERSECRET; path=/"
))
challenge_body <- charToRaw(paste0(
  '<!DOCTYPE html><html lang="en-US"><head>\n<title>Just a moment...</title>\n',
  '<meta charset="UTF-8"></head><body>\n',
  strrep("<p>padding line</p>\n", 400), "</body></html>"
))

# The error message resolve_seeg() produces for a given fake curl behaviour.
failure_message <- function(fetch, rows = seeg_rows) {
  tryCatch(
    testthat::with_mocked_bindings(
      resolve_seeg(rows),
      curl_fetch_memory = fetch,
      .package = "curl"
    ),
    error = function(e) conditionMessage(e)
  )
}
respond_with <- function(resp) function(url, handle = NULL, ...) resp

test_that("a 403 still stops, now with status, key headers, title and a body excerpt", {
  resp <- fake_response(403L, challenge_headers, challenge_body)

  expect_error(
    testthat::with_mocked_bindings(
      resolve_seeg(seeg_rows),
      curl_fetch_memory = respond_with(resp), .package = "curl"
    ),
    "could not fetch https://seeg.eco.br/dados/ \\(HTTP 403\\)"
  )

  msg <- failure_message(respond_with(resp))
  lines <- strsplit(msg, "\n")[[1]]
  # the first line is the one-line summary that becomes the issue's list item
  expect_match(lines[1], "HTTP 403")
  expect_false(grepl("<", lines[1], fixed = TRUE))

  expect_match(msg, "HTTP 403 from https://seeg.eco.br/dados/", fixed = TRUE)
  expect_match(msg, "  server: cloudflare", fixed = TRUE)
  expect_match(msg, "  cf-ray: 9f0c1d2e3a4b5c6d-GRU", fixed = TRUE)
  expect_match(msg, "  cf-mitigated: challenge", fixed = TRUE)
  expect_match(msg, "  x-frame-options: SAMEORIGIN", fixed = TRUE)
  expect_match(msg, "  content-type: text/html; charset=UTF-8", fixed = TRUE)
  expect_match(msg, "html <title>: Just a moment...", fixed = TRUE)
  expect_match(msg, "<!DOCTYPE html>", fixed = TRUE)

  # cookie NAMES only, never their values; unlisted headers (date) left out
  expect_match(msg, "set-cookie (names only): __cf_bm, _other", fixed = TRUE)
  expect_false(grepl("SECRETVALUE123|ANOTHERSECRET", msg))
  expect_false(grepl("date:", msg, fixed = TRUE))

  # a ~9 KB page must not balloon the message: 600-char excerpt, whole thing small
  expect_match(msg, "first 600 chars", fixed = TRUE)
  expect_lt(nchar(msg), 2500)
})

test_that("SEEG's real CloudFront refusal (captured live 2026-10-17) is reported legibly", {
  read_fixture <- function(name) {
    path <- test_path("fixtures", name)
    readBin(path, "raw", n = file.size(path))
  }
  resp <- fake_response(
    403L, read_fixture("seeg_cloudfront_403_headers.txt"), read_fixture("seeg_cloudfront_403_body.html")
  )

  msg <- failure_message(respond_with(resp))

  expect_match(msg, "(HTTP 403)", fixed = TRUE)
  expect_match(msg, "  server: CloudFront", fixed = TRUE)
  expect_match(msg, "  x-cache: Error from cloudfront", fixed = TRUE)
  expect_match(msg, "(CloudFront)", fixed = TRUE) # the via header
  expect_match(msg, "  x-amz-cf-id: ", fixed = TRUE)
  expect_match(msg, "html <title>: ERROR: The request could not be satisfied", fixed = TRUE)
  expect_match(msg, "Request blocked.", fixed = TRUE)
  expect_false(grepl("set-cookie", msg, fixed = TRUE)) # this response sets none
  expect_true(validUTF8(msg))
  expect_lt(nchar(msg), 2500)
})

test_that("a non-403 status is reported as such", {
  msg <- failure_message(respond_with(fake_response(503L, challenge_headers, challenge_body)))
  expect_match(msg, "(HTTP 503)", fixed = TRUE)
  expect_match(msg, "HTTP 503 from https://seeg.eco.br/dados/", fixed = TRUE)
})

test_that("a curl error (no HTTP response at all) keeps curl's own message", {
  msg <- failure_message(function(url, handle = NULL, ...) stop("SSL connect error"))
  expect_match(msg, "(request failed)", fixed = TRUE)
  expect_match(msg, "no HTTP response was received -- curl error: SSL connect error", fixed = TRUE)
})

test_that("only the last redirect hop's headers are reported", {
  two_hops <- charToRaw(paste0(
    "HTTP/2 301 \r\nlocation: https://old.example/dados/\r\nserver: first-hop\r\n\r\n",
    "HTTP/2 403 \r\nserver: cloudflare\r\ncf-ray: abc-GRU\r\n\r\n"
  ))
  msg <- failure_message(respond_with(fake_response(403L, two_hops, charToRaw("blocked"))))
  expect_match(msg, "  server: cloudflare", fixed = TRUE)
  expect_false(grepl("first-hop|old.example", msg))
})

test_that("the message is valid UTF-8 even for a cut multibyte char or non-UTF-8 bytes", {
  # 2000 x "€" is 6000 bytes of 3-byte characters; the 4096-byte cap lands
  # mid-character (4096 %% 3 == 1). The 0xe9 bytes are Latin-1, invalid UTF-8.
  euro <- paste(rep("€", 2000), collapse = "")
  body <- c(charToRaw("<title>Acesso negado "), as.raw(0xe9), charToRaw("</title>"), charToRaw(enc2utf8(euro)))
  headers <- c(charToRaw("HTTP/2 403 \r\nserver: cloud"), as.raw(0xe9), charToRaw("flare\r\n\r\n"))

  msg <- failure_message(respond_with(fake_response(403L, headers, body)))

  expect_true(validUTF8(msg))
  # manifest_report.json is written with jsonlite -- an invalid string would
  # break it on exactly the run the evidence is wanted from
  expect_no_error(json <- jsonlite::toJSON(list(seeg = msg), auto_unbox = TRUE))
  expect_equal(jsonlite::fromJSON(json)$seeg, msg)
})

test_that("an empty body and missing headers do not crash the diagnostics", {
  msg <- failure_message(respond_with(fake_response(403L, raw(0), raw(0))))
  expect_match(msg, "(none of those headers present)", fixed = TRUE)
  expect_match(msg, "body: (empty)", fixed = TRUE)
  expect_false(grepl("html <title>", msg, fixed = TRUE))
})

test_that("a successful page never goes through the diagnostics path", {
  page <- function(hrefs) {
    charToRaw(paste0(
      "<html><body>",
      paste0('<a href="', hrefs, '">x</a>', collapse = ""),
      "</body></html>"
    ))
  }
  base <- "https://seeg.eco.br/wp-content/uploads/2025/12/"

  # 200, but no municipal workbook link: fails on its own, later, check
  msg <- failure_message(respond_with(fake_response(200L, raw(0), page(paste0(base, "Dados-nacionais-13.0.xlsx")))))
  expect_match(msg, "no municipal-level '.xlsx' link found", fixed = TRUE)
  expect_false(grepl("could not fetch|response headers", msg))

  # 200 with two candidates: still the ambiguity stop, not the fetch one
  msg <- failure_message(respond_with(fake_response(200L, raw(0), page(paste0(base, c("Dados-municipais-13.0.xlsx", "Dados-municipais-12.0.xlsx"))))))
  expect_match(msg, "found 2 candidate municipal", fixed = TRUE)
  expect_false(grepl("could not fetch|response headers", msg))
})
