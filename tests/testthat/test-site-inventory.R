# tests/testthat/test-site-inventory.R
#
# Tests for actions/scripts/site_inventory.R and actions/scrapers/
# watch_mapbiomas.R -- the site-link inventory mechanism that replaced the
# old docs_url-writing WordPress watcher in resolve_mapbiomas.R (see that
# file's header, and watch_mapbiomas.R's header, for the full story).
#
# actions/ is .Rbuildignore'd -- it is CI tooling, never shipped in the
# built package, and won't exist in an installed copy or a CRAN tarball.
# Every test in this file is skipped entirely when it's absent, same
# posture as a test that needs network access being skipped offline.
#
# The two HTML fixtures under fixtures/site_html/ are real evidence, not
# invented markup:
#   - mapbiomas_202608_table.html is a trimmed capture of the LIVE page
#     (fetched 2026-08-19), keeping only its real <tr class="dl-row">
#     blocks.
#   - mapbiomas_202606_accordion.html reconstructs the OLDER template
#     documented from a 2026-06-23 Wayback Machine capture the user found
#     mid-investigation (an accordion layout with inline DOI citations and
#     a Google-Drive-hosted file) -- the exact evidence that proved this
#     page's markup fully rebuilds without notice, which is the entire
#     reason this file runs four independent strategies instead of one.
#   - mapbiomas_empty.html has neither -- it exercises the total-miss guard.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "site_inventory.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "watch_mapbiomas.R"), local = TRUE)

read_fixture_html <- function(name) {
  txt <- readLines(test_path("fixtures", "site_html", name), warn = FALSE, encoding = "UTF-8")
  txt <- paste(txt, collapse = "\n")
  Encoding(txt) <- "UTF-8"
  txt
}

today_html <- read_fixture_html("mapbiomas_202608_table.html")
june_html <- read_fixture_html("mapbiomas_202606_accordion.html")
empty_html <- read_fixture_html("mapbiomas_empty.html")

site_url <- "https://brasil.mapbiomas.org/downloads/estatisticas/"

empty_inventory <- tibble::tibble(
  source = character(0), page = character(0), label = character(0),
  collection = character(0), url = character(0), host = character(0),
  strategy = character(0), first_seen = character(0)
)

# Temporarily replaces fetch_page() inside watch_mapbiomas()'s own closure
# environment (both were source()'d into this file's local env above, so
# watch_mapbiomas() resolves fetch_page() there at call time) -- avoids
# depending on testthat's package-mocking machinery for a plain sourced
# script function. Always restores the original on exit, even on error.
with_mock_fetch <- function(html, code) {
  env <- environment(watch_mapbiomas)
  old <- env$fetch_page
  env$fetch_page <- function(url, timeout_s = 30) html
  on.exit(env$fetch_page <- old, add = TRUE)
  force(code)
}

## ============================================================ ##
## The regression that matters most: strategy 3 alone survives   ##
## a full template rebuild.                                      ##
## ============================================================ ##

test_that("the proximity strategy (3) matches BOTH the June accordion and August table templates", {
  expect_gt(length(mb_strategy_proximity(today_html)), 0)
  expect_gt(length(mb_strategy_proximity(june_html)), 0)
})

test_that("the row-block strategy (2) is specific to today's table markup", {
  expect_gt(length(mb_strategy_row_block(today_html)), 0)
  expect_length(mb_strategy_row_block(june_html), 0) # no <tr class="dl-row"> in the accordion template
})

test_that("the DOI strategy (4) is dormant on today's page but fires on June's", {
  expect_length(mb_strategy_doi(today_html), 0) # today's template cites no DOIs at all
  expect_gt(length(mb_strategy_doi(june_html)), 0)
})

test_that("the filename strategy (1) finds MapBiomas's COL.N convention on both templates", {
  expect_gt(length(mb_strategy_filename(today_html)), 0)
  expect_gt(length(mb_strategy_filename(june_html)), 0)
})

test_that("proximity strategy skips doi.org hrefs (owned exclusively by the DOI strategy)", {
  hits <- mb_strategy_proximity(june_html)
  urls <- vapply(hits, function(h) h$url, character(1))
  expect_false(any(grepl("doi\\.org", urls, ignore.case = TRUE)))
})

## ============================================================ ##
## normalize_link_url()                                          ##
## ============================================================ ##

test_that("normalize_link_url() strips cache-busting params but keeps meaningful ones", {
  u <- normalize_link_url("https://brasil.mapbiomas.org/x.xlsx?ver=6&format=original", site_url)
  expect_equal(u, "https://brasil.mapbiomas.org/x.xlsx?format=original")
})

test_that("normalize_link_url() is idempotent", {
  raw <- "https://Brasil.MapBiomas.org:443/x.xlsx?b=2&ver=1&a=1"
  once <- normalize_link_url(raw, site_url)
  twice <- normalize_link_url(once, site_url)
  expect_equal(once, twice)
})

test_that("normalize_link_url() absolutizes root-relative and protocol-relative hrefs", {
  expect_equal(normalize_link_url("/x.xlsx", site_url), "https://brasil.mapbiomas.org/x.xlsx")
  expect_equal(
    normalize_link_url("//brasil.mapbiomas.org/x.xlsx", site_url),
    "https://brasil.mapbiomas.org/x.xlsx"
  )
})

test_that("normalize_link_url() lowercases scheme+host but leaves the path case alone", {
  u <- normalize_link_url("HTTPS://Brasil.MapBiomas.org/WP-Content/X.xlsx", site_url)
  expect_equal(u, "https://brasil.mapbiomas.org/WP-Content/X.xlsx")
})

## ============================================================ ##
## normalize_label()                                             ##
## ============================================================ ##

test_that("normalize_label() strips tags, decodes entities, and caps length", {
  expect_equal(normalize_label("<b>A</b> &amp; <i>B</i>"), "A & B")
  long <- normalize_label(paste(rep("x", 500), collapse = ""))
  expect_lte(nchar(long), 200)
})

## ============================================================ ##
## watch_mapbiomas() end-to-end                                  ##
## ============================================================ ##

test_that("watch_mapbiomas() produces a well-formed inventory for today's table template", {
  with_mock_fetch(today_html, {
    res <- watch_mapbiomas(empty_inventory)
    expect_gt(nrow(res), 0)
    expect_setequal(names(res), setdiff(INVENTORY_COLS, "first_seen"))
    expect_true(all(res$source == "mapbiomas"))
    expect_true(all(res$page == site_url))
  })
})

test_that("watch_mapbiomas() produces a well-formed inventory for June's accordion template", {
  with_mock_fetch(june_html, {
    res <- watch_mapbiomas(empty_inventory)
    expect_gt(nrow(res), 0)
  })
})

test_that("watch_mapbiomas() stops -- does not write -- when a page yields zero links across all strategies", {
  with_mock_fetch(empty_html, {
    expect_error(watch_mapbiomas(empty_inventory), "ZERO links")
  })
})

test_that("watch_mapbiomas() stops when the link count collapses vs. the previously committed count (shrink guard)", {
  prev <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = paste0("x", 1:10),
    collection = "11", url = paste0("https://x/", 1:10), host = "x",
    strategy = "row_block", first_seen = "2026-01-01"
  )
  with_mock_fetch(today_html, { # only 3 real rows -- well under half of 10
    expect_error(watch_mapbiomas(prev), "dropped from")
  })
})

test_that("watch_mapbiomas() does NOT trip the shrink guard when there's no prior history for the page", {
  with_mock_fetch(today_html, {
    expect_no_error(watch_mapbiomas(empty_inventory))
  })
})

## ============================================================ ##
## carry_first_seen() / diff_inventory() / validate_inventory()  ##
## ============================================================ ##

test_that("carry_first_seen() preserves an existing identity's date and stamps new ones today", {
  old <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = "https://x/a", host = "x", strategy = "row_block", first_seen = "2020-01-01"
  )
  new <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = c("A", "B"), collection = "11",
    url = c("https://x/a", "https://x/b"), host = "x", strategy = "row_block"
  )
  out <- carry_first_seen(old, new, today = as.Date("2026-08-19"))
  expect_equal(out$first_seen[out$label == "A"], "2020-01-01")
  expect_equal(out$first_seen[out$label == "B"], "2026-08-19")
})

test_that("diff_inventory() ignores first_seen when deciding whether a row changed", {
  old <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = "https://x/a", host = "x", strategy = "row_block", first_seen = "2020-01-01"
  )
  new <- old
  new$first_seen <- "2026-08-19" # carried-forward date would never actually change like this, but prove it's ignored anyway
  d <- diff_inventory(old, new)
  expect_equal(d$n, 0)
})

test_that("diff_inventory() reports a real value change as 'changed', not remove+add", {
  old <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = "https://x/a", host = "x", strategy = "row_block", first_seen = "2020-01-01"
  )
  new <- old
  new$url <- "https://x/a-moved"
  d <- diff_inventory(old, new)
  expect_equal(d$n, 1)
  expect_equal(nrow(d$changed), 1)
  expect_equal(nrow(d$added), 0)
  expect_equal(nrow(d$removed), 0)
})

test_that("diff_inventory() reports a collection bump as remove+add (collection is part of identity)", {
  old <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "10",
    url = "https://x/a10", host = "x", strategy = "row_block", first_seen = "2020-01-01"
  )
  new <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = "https://x/a11", host = "x", strategy = "row_block", first_seen = "2026-08-19"
  )
  d <- diff_inventory(old, new)
  expect_equal(nrow(d$added), 1)
  expect_equal(nrow(d$removed), 1)
  expect_equal(nrow(d$changed), 0)
})

test_that("validate_inventory() catches a duplicate identity", {
  dup <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = c("https://x/a", "https://x/a-again"), host = "x",
    strategy = "row_block", first_seen = "2026-08-19"
  )
  errs <- validate_inventory(dup)
  expect_true(any(grepl("duplicate identity", errs)))
})

test_that("validate_inventory() catches an empty url", {
  bad <- tibble::tibble(
    source = "mapbiomas", page = site_url, label = "A", collection = "11",
    url = NA_character_, host = "x", strategy = "row_block", first_seen = "2026-08-19"
  )
  errs <- validate_inventory(bad)
  expect_true(any(grepl("empty url", errs)))
})

test_that("validate_inventory() passes clean output from a real watcher run", {
  with_mock_fetch(today_html, {
    res <- carry_first_seen(empty_inventory, watch_mapbiomas(empty_inventory))
    expect_length(validate_inventory(res), 0)
  })
})

## ============================================================ ##
## Idempotency, end to end                                       ##
## ============================================================ ##

test_that("running the watcher twice against an unchanged page produces zero further diff", {
  with_mock_fetch(today_html, {
    first <- carry_first_seen(empty_inventory, watch_mapbiomas(empty_inventory))
    second <- carry_first_seen(first, watch_mapbiomas(first))
    expect_equal(diff_inventory(first, second)$n, 0)
  })
})
