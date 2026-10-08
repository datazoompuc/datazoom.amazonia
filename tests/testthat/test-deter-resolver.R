# tests/testthat/test-deter-resolver.R
#
# Offline test for actions/scrapers/resolve_deter.R's keyword fallback (added
# 2026-10-08, see that file's header): when a dataset's known slug is no
# longer in TerraBrasilis' download API, the resolver may recover from a
# rename -- but only through ONE unambiguous /deter-namespaced entry whose
# `name` carries the dataset's biome keyword, and only among links no other
# dataset's known slug still claims. Anything else must stop(), never guess.
#
# The API (curl::curl_fetch_memory) is mocked with fixtures/
# terrabrasilis_download_api_deter.json: the 8 real /deter entries read from
# the live API on 2026-10-08 (4 products x PT/EN) plus 5 real NON-deter
# entries whose names happen to match a keyword ("Amazon TerraClass", a PT
# "... nao florestais - Bioma Amazonia" PRODES layer, ...) -- 135 of the
# API's 231 entries do, which is why the /deter scoping matters. A rename is
# simulated by editing the fake API's LINKS (the world changes, the resolver
# code doesn't), which is exactly what a real TerraBrasilis rename looks like.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("jsonlite")
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_deter.R"), local = TRUE)

api <- jsonlite::fromJSON(test_path("fixtures", "terrabrasilis_download_api_deter.json"), simplifyVector = FALSE)

deter_rows <- tibble::tibble(
  survey = "deter",
  dataset = c("deter_amz", "deter_cerrado", "deter_pantanal", "deter_non_forest"),
  geo_level = NA_character_, year = NA_character_
)
host <- "https://terrabrasilis.dpi.inpe.br/file-delivery/download/"
expected_url <- stats::setNames(
  paste0(host, c("deter-amz", "deter-cerrado-nb", "deter-pantanal", "deter-nf"), "/shape"),
  deter_rows$dataset
)

# Run resolve_deter() against a fake API response.
run_with_api <- function(entries, status = 200L) {
  body <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(entries, auto_unbox = TRUE))))
  testthat::with_mocked_bindings(
    resolve_deter(deter_rows),
    curl_fetch_memory = function(url, handle = NULL, ...) list(status_code = status, content = body),
    .package = "curl"
  )
}
urls <- function(res) stats::setNames(res$url, res$dataset)

# Edits to the fake API.
rename_link <- function(entries, from, to) {
  lapply(entries, function(e) { e$link <- sub(from, to, e$link, fixed = TRUE); e })
}
drop_entries <- function(entries, fragment) Filter(function(e) !grepl(fragment, e$link, fixed = TRUE), entries)
disable_entries <- function(entries, fragment) {
  lapply(entries, function(e) { if (grepl(fragment, e$link, fixed = TRUE)) e$enabled <- FALSE; e })
}
set_name <- function(entries, fragment, new_name) {
  lapply(entries, function(e) { if (grepl(fragment, e$link, fixed = TRUE)) e$name <- new_name; e })
}

test_that("the fixture is the real API shape: 8 /deter entries + keyword-matching decoys", {
  links <- vapply(api, function(e) e$link, "")
  expect_equal(sum(grepl("/deter", links, fixed = TRUE)), 8)
  expect_true(any(grepl("Floresta", vapply(api, function(e) e$name, ""), fixed = TRUE))) # accent-bearing PT name
})

test_that("happy path: every known slug matches, no fallback, no message", {
  expect_no_message(res <- run_with_api(api))
  expect_equal(urls(res), expected_url)
  expect_equal(unique(res$resolver), "deter")
  # nothing unusual happened, so nothing for build_manifest.R to put in the PR body
  expect_null(attr(res, "fallback_notes"))
  expect_null(attr(res, "partial_failures"))
})

test_that("a renamed slug is recovered by the keyword fallback, with a warning message", {
  for (slug in c("deter-amz", "deter-cerrado-nb", "deter-pantanal", "deter-nf")) {
    ds <- deter_rows$dataset[match(slug, c("deter-amz", "deter-cerrado-nb", "deter-pantanal", "deter-nf"))]
    renamed <- rename_link(api, paste0("/", slug, "/"), paste0("/", slug, "-v2/"))

    expect_message(res <- run_with_api(renamed), "keyword fallback", info = ds)

    want <- expected_url
    want[[ds]] <- sub(paste0("/", slug, "/"), paste0("/", slug, "-v2/"), want[[ds]], fixed = TRUE)
    expect_equal(urls(res), want, info = ds)

    # ... and the same fact is handed to build_manifest.R for the PR body: one
    # single-line note, naming the dataset and the URL it fell back to.
    notes <- attr(res, "fallback_notes")
    expect_type(notes, "character")
    expect_length(notes, 1)
    expect_match(notes, ds, fixed = TRUE, info = ds)
    expect_match(notes, paste0("/", slug, "-v2/shape"), fixed = TRUE, info = ds)
    expect_false(grepl("\n", notes, fixed = TRUE), info = ds)
    # informational only: a fallback is a success, never a "partial failure"
    # (that attribute would trip exit code 11 and open an issue)
    expect_null(attr(res, "partial_failures"), info = ds)
  }
})

test_that("each dataset that falls back gets its own note", {
  both <- rename_link(rename_link(api, "/deter-cerrado-nb/", "/deter-cerrado-v2/"), "/deter-pantanal/", "/deter-pantanal-v2/")
  suppressMessages(res <- run_with_api(both))
  notes <- attr(res, "fallback_notes")
  expect_length(notes, 2)
  expect_length(grep("^deter_cerrado:", notes), 1)
  expect_length(grep("^deter_pantanal:", notes), 1)
  # the datasets that still matched their slug directly are not mentioned
  expect_false(any(grepl("deter_amz|deter_non_forest", notes)))
})

test_that("deter_amz's keyword also matches the non-forest entry -- that link must NOT be a candidate", {
  # Regression for a bug found by reading the live names: the English
  # non-forest entry is "Amazon Biome notices for non-forest areas", so
  # "amaz" matches it too. Before links claimed by another dataset's slug
  # were excluded, a renamed deter_amz slug saw TWO candidates and failed.
  res <- suppressMessages(run_with_api(rename_link(api, "/deter-amz/", "/deter-amz-v2/")))
  expect_equal(urls(res)[["deter_amz"]], paste0(host, "deter-amz-v2/shape"))
  expect_equal(urls(res)[["deter_non_forest"]], expected_url[["deter_non_forest"]])
})

test_that("entries that VANISH are never papered over with a neighbour's URL", {
  # Worst case of the same bug: with the Amazon entries removed, deter-nf was
  # the only "amaz" candidate left and got accepted as deter_amz's URL,
  # silently giving two datasets one download.
  expect_error(run_with_api(drop_entries(api, "/deter-amz/")), "deter_amz.*found 0 distinct candidate")
  expect_error(run_with_api(disable_entries(api, "/deter-amz/")), "deter_amz.*found 0 distinct candidate")
  expect_error(run_with_api(drop_entries(api, "/deter-cerrado-nb/")), "deter_cerrado")
})

test_that("a rename the keyword can't follow stops instead of guessing", {
  lost_name <- set_name(
    rename_link(api, "/deter-cerrado-nb/", "/deter-cerrado-v2/"),
    "/deter-cerrado-v2/", "Alertas de desmatamento"
  )
  expect_error(run_with_api(lost_name), "deter_cerrado.*found 0 distinct candidate")
})

test_that("an ambiguous fallback stops instead of picking one", {
  # amz AND non-forest renamed at once: nothing claims deter-nf any more, so
  # "amaz" finds two distinct links for deter_amz.
  both <- rename_link(rename_link(api, "/deter-amz/", "/deter-amz-v2/"), "/deter-nf/", "/deter-nf-v2/")
  expect_error(run_with_api(both), "deter_amz.*found 2 distinct candidate")
})

test_that("non-/deter entries never become fallback candidates", {
  # The fixture's decoys include real "Amazon ..." / "Cerrado ..." /
  # "Pantanal ..." non-DETER layers; with the real DETER entry gone they must
  # still not be picked up.
  decoy_names <- vapply(
    Filter(function(e) !grepl("/deter", e$link, fixed = TRUE), api), function(e) e$name, ""
  )
  expect_true(any(grepl("amaz", decoy_names, ignore.case = TRUE)))
  expect_true(any(grepl("pantanal", decoy_names, ignore.case = TRUE)))
  expect_error(run_with_api(drop_entries(api, "/deter-pantanal/")), "deter_pantanal.*found 0 distinct candidate")
})

test_that("two datasets can never resolve to the same URL", {
  # INPE merging the Amazon and non-forest downloads into one link would send
  # both fallbacks to it -- refuse rather than write a duplicated manifest.
  merged <- rename_link(rename_link(api, "/deter-amz/", "/deter-both/"), "/deter-nf/", "/deter-both/")
  expect_error(suppressMessages(run_with_api(merged)), "more than one dataset resolved to the same URL")
})

test_that("request-level failures and bad input still stop as before", {
  expect_error(run_with_api(api, status = 500L), "HTTP 500")
  expect_error(resolve_deter(NULL), "no manifest rows")
  expect_error(resolve_deter(deter_rows[0, ]), "no manifest rows")
  expect_error(
    testthat::with_mocked_bindings(
      resolve_deter(dplyr::mutate(deter_rows[1, ], dataset = "deter_mystery")),
      curl_fetch_memory = function(url, handle = NULL, ...) {
        list(status_code = 200L, content = charToRaw(as.character(jsonlite::toJSON(api, auto_unbox = TRUE))))
      },
      .package = "curl"
    ),
    "doesn't know how to resolve: deter_mystery"
  )
})

test_that("resolve_deter.R registers exactly one resolver in build_manifest.R's registry", {
  src_env <- environment(resolve_deter)
  expect_identical(grep("^resolve_", ls(src_env), value = TRUE), "resolve_deter")
  expect_identical(grep("^watch_", ls(src_env), value = TRUE), character(0))
})
