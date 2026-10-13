# tests/testthat/test-epe-alert-recording.R
#
# Offline test for what actions/scrapers/resolve_epe.R records into the
# shared resolver alert cache (2026-10-12 re-keying, see that file's
# alert_rec() loop): one row per block keyed on the block label, content
# failures only. A page that couldn't be fetched at all records nothing --
# it reaches the GitHub issue through partial_failures/stop() instead.
# curl::curl_fetch_memory is mocked, so no network is touched.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

source(file.path(actions_dir, "scripts", "resolver_alert_cache.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_epe.R"), local = TRUE)

epe_rows <- tibble::tibble(
  survey = "epe", dataset = "national_energy_balance",
  geo_level = NA_character_, year = NA_character_
)
blocks <- c("consumer/industrial_energy_consumption", "national_energy_balance", "energy_state_panel")

# Runs resolve_epe() against a fake curl and returns the alert rows it
# recorded (NULL if it never called alert_rec()), plus the error it threw.
run_epe <- function(fake_fetch) {
  path_var <- "epe_alert_cache_candidate_path"
  if (exists(path_var, envir = .GlobalEnv)) rm(list = path_var, envir = .GlobalEnv)
  on.exit(if (exists(path_var, envir = .GlobalEnv)) rm(list = path_var, envir = .GlobalEnv), add = TRUE)

  err <- tryCatch(
    testthat::with_mocked_bindings(
      resolve_epe(epe_rows),
      curl_fetch_memory = fake_fetch,
      .package = "curl"
    ),
    error = function(e) e
  )
  path <- get0(path_var, envir = .GlobalEnv, ifnotfound = NA_character_)
  list(
    alerts = if (is.na(path)) NULL else read_resolver_alert_cache(path),
    error = if (inherits(err, "error")) conditionMessage(err) else NA_character_
  )
}

test_that("network failures record nothing in the alert cache but still fail the resolver", {
  res <- run_epe(function(url, handle = NULL, ...) stop("Timeout was reached"))
  expect_null(res$alerts)
  # every block failed, so the resolver stops -> resolvers_failed -> issue
  expect_match(res$error, "could not fetch")
})

test_that("content failures record one fail row per block, keyed on the block label", {
  # Pages load, but none of the expected links are on them any more.
  res <- run_epe(function(url, handle = NULL, ...) {
    list(status_code = 200L, content = charToRaw("<html><body>restructured</body></html>"))
  })
  expect_false(is.null(res$alerts))
  expect_setequal(res$alerts$item_key, blocks)
  expect_equal(nrow(res$alerts), length(blocks))
  expect_true(all(res$alerts$verdict == "fail"))
  expect_true(all(grepl("page fetched but", res$alerts$reason, fixed = TRUE)))
  # no reason text leaks into the key any more
  expect_false(any(grepl("\r", res$alerts$item_key, fixed = TRUE)))
})
