# tests/testthat/test-resolver-alert-cache.R
#
# actions/scripts/resolver_alert_cache.R -- the generalized version of
# mapbiomas_resolver_cache.R's new_failed_files() Slack signal, shared by
# resolve_prodes.R and resolve_epe.R. Same anti-noise property, tested the
# same way test-mapbiomas-resolver-cache.R already tests new_failed_files():
# pure functions over synthetic tibbles, no network.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)

source(file.path(actions_dir, "scripts", "resolver_alert_cache.R"), local = TRUE)

empty_cache <- function() read_resolver_alert_cache(file.path(tempdir(), "does_not_exist_alerts.csv"))

## ============================================================ ##
## alert_cache_upsert()                                          ##
## ============================================================ ##

test_that("alert_cache_upsert() appends a brand-new (source, item_key)", {
  cache <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "20260910", dataset = "(all)", verdict = "pass")
  expect_equal(nrow(cache), 1)
  expect_equal(cache$source, "prodes")
  expect_equal(cache$item_key, "20260910")
  expect_equal(cache$verdict, "pass")
})

test_that("alert_cache_upsert() replaces an existing (source, item_key), not duplicates it", {
  cache <- alert_cache_upsert(empty_cache(), source = "epe", item_key = "national_energy_balance", verdict = "pass")
  cache <- alert_cache_upsert(cache, source = "epe", item_key = "national_energy_balance", verdict = "fail", reason = "page 404")
  expect_equal(nrow(cache), 1)
  expect_equal(cache$verdict, "fail")
  expect_equal(cache$reason, "page 404")
})

test_that("alert_cache_upsert() keeps rows for different sources with the same item_key separate", {
  cache <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "x", verdict = "pass")
  cache <- alert_cache_upsert(cache, source = "epe", item_key = "x", verdict = "fail", reason = "unrelated")
  expect_equal(nrow(cache), 2)
  expect_setequal(cache$source, c("prodes", "epe"))
})

## ============================================================ ##
## new_resolver_alerts() -- the Slack-worthy diff                ##
## ============================================================ ##

test_that("new_resolver_alerts(): a re-confirmed known fail is NOT reported (anti-noise)", {
  old <- alert_cache_upsert(empty_cache(), source = "epe", item_key = "energy_state_panel|page 404", verdict = "fail", reason = "page 404")
  new <- alert_cache_upsert(old, source = "epe", item_key = "energy_state_panel|page 404", verdict = "fail", reason = "page 404")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 0)
})

test_that("new_resolver_alerts(): a genuinely new fail IS reported, with its reason", {
  old <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "20260408", verdict = "pass")
  new <- alert_cache_upsert(old, source = "prodes", item_key = "20260910", verdict = "fail", reason = "qml legend mismatch")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 1)
  expect_equal(hits$item_key, "20260910")
  expect_equal(hits$reason, "qml legend mismatch")
})

test_that("new_resolver_alerts(): a new item that PASSES is NOT reported", {
  old <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "20260408", verdict = "pass")
  new <- alert_cache_upsert(old, source = "prodes", item_key = "20260910", verdict = "pass")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 0)
})

test_that("new_resolver_alerts(): a cold/empty committed cache is a bootstrap, not news", {
  old <- empty_cache()
  new <- alert_cache_upsert(old, source = "epe", item_key = "national_energy_balance|broken", verdict = "fail", reason = "broken")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 0)
})

test_that("new_resolver_alerts(): a DIFFERENT new reason on the same block IS reported (not deduped by block alone)", {
  old <- alert_cache_upsert(empty_cache(), source = "epe", item_key = "energy_state_panel|page 404", verdict = "fail", reason = "page 404")
  new <- alert_cache_upsert(old, source = "epe", item_key = "energy_state_panel|columns missing", verdict = "fail", reason = "columns missing")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 1)
  expect_equal(hits$reason, "columns missing")
})

test_that("new_resolver_alerts(): a key that FLIPS from pass to fail IS reported (regression on previously-good content)", {
  # Found live while verifying this feature: a PRODES stamp (or an EPE
  # block) that passed last run and fails THIS run, under the exact same
  # item_key, is a genuine regression -- arguably more alarming than a
  # brand-new key failing for the first time. Keying "fresh" on item_key
  # alone (matching new_failed_files()'s file-identity model too literally)
  # missed this case entirely on the first implementation.
  old <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "20260408", verdict = "pass")
  new <- alert_cache_upsert(old, source = "prodes", item_key = "20260408", verdict = "fail", reason = "qml legend regressed")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 1)
  expect_equal(hits$item_key, "20260408")
  expect_equal(hits$reason, "qml legend regressed")
})

test_that("new_resolver_alerts(): a key that stays fail across 3 generations only alerts once", {
  gen1 <- alert_cache_upsert(empty_cache(), source = "epe", item_key = "energy_state_panel|broken", verdict = "fail", reason = "broken")
  gen2 <- alert_cache_upsert(gen1, source = "epe", item_key = "energy_state_panel|broken", verdict = "fail", reason = "broken")
  gen3 <- alert_cache_upsert(gen2, source = "epe", item_key = "energy_state_panel|broken", verdict = "fail", reason = "broken")

  expect_equal(nrow(new_resolver_alerts(gen1, gen2)), 0)
  expect_equal(nrow(new_resolver_alerts(gen2, gen3)), 0)
})

test_that("new_resolver_alerts(): the same item_key under a DIFFERENT source is not conflated", {
  old <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "x", verdict = "fail", reason = "prodes reason")
  new <- alert_cache_upsert(old, source = "epe", item_key = "x", verdict = "fail", reason = "epe reason")

  hits <- new_resolver_alerts(old, new)
  expect_equal(nrow(hits), 1)
  expect_equal(hits$source, "epe")
})

## ============================================================ ##
## merge_resolver_alert_source()                                 ##
## ============================================================ ##

test_that("merge_resolver_alert_source() replaces one source's rows and leaves others untouched", {
  committed <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "old_stamp", verdict = "pass")
  committed <- alert_cache_upsert(committed, source = "epe", item_key = "national_energy_balance", verdict = "pass")

  prodes_candidate <- alert_cache_upsert(committed, source = "prodes", item_key = "new_stamp", verdict = "fail", reason = "mismatch")
  # prodes's own candidate carries a FULL copy of the committed table
  # (its own fresh row plus epe's untouched baseline row) -- exactly what
  # each resolver actually writes.
  prodes_candidate <- prodes_candidate[prodes_candidate$source == "prodes" | prodes_candidate$source == "epe", ]

  merged <- merge_resolver_alert_source(committed, "prodes", prodes_candidate)

  # prodes accumulates history (old_stamp's pass verdict + new_stamp's fail
  # verdict both persist -- a new release is a NEW item_key, not a
  # replacement of the last one), epe's single row passes through untouched.
  expect_equal(nrow(merged), 3)
  prodes_rows <- merged[merged$source == "prodes", ]
  epe_rows <- merged[merged$source == "epe", ]
  expect_setequal(prodes_rows$item_key, c("old_stamp", "new_stamp"))
  expect_equal(prodes_rows$verdict[prodes_rows$item_key == "new_stamp"], "fail")
  # epe's row is untouched -- came from `committed`, not modified by the
  # prodes-only merge call.
  expect_equal(epe_rows$item_key, "national_energy_balance")
  expect_equal(epe_rows$verdict, "pass")
})

test_that("merging both sources in sequence combines both sources' fresh updates without losing either", {
  committed <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "old_stamp", verdict = "pass")
  committed <- alert_cache_upsert(committed, source = "epe", item_key = "national_energy_balance", verdict = "pass")

  prodes_candidate <- alert_cache_upsert(committed, source = "prodes", item_key = "new_stamp", verdict = "fail", reason = "prodes broke")
  epe_candidate <- alert_cache_upsert(committed, source = "epe", item_key = "energy_state_panel|broke", verdict = "fail", reason = "epe broke too")

  merged <- committed
  merged <- merge_resolver_alert_source(merged, "prodes", prodes_candidate)
  merged <- merge_resolver_alert_source(merged, "epe", epe_candidate)

  hits <- new_resolver_alerts(committed, merged)
  expect_equal(nrow(hits), 2)
  expect_setequal(hits$source, c("prodes", "epe"))
})

## ============================================================ ##
## Round-trip read/write                                         ##
## ============================================================ ##

test_that("write_resolver_alert_cache()/read_resolver_alert_cache() round-trip cleanly", {
  cache <- alert_cache_upsert(empty_cache(), source = "prodes", item_key = "20260910", dataset = "(all)", verdict = "fail", reason = "mismatch")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_resolver_alert_cache(cache, path)

  read_back <- read_resolver_alert_cache(path)
  expect_equal(nrow(read_back), 1)
  expect_equal(read_back$source, "prodes")
  expect_equal(read_back$reason, "mismatch")
})

test_that("read_resolver_alert_cache() returns an empty, correctly-shaped tibble for a missing file", {
  cache <- read_resolver_alert_cache(file.path(tempdir(), "definitely_does_not_exist.csv"))
  expect_equal(nrow(cache), 0)
  expect_equal(names(cache), RESOLVER_ALERT_COLS)
})
