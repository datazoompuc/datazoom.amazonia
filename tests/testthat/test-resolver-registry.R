# tests/testthat/test-resolver-registry.R
#
# actions/scripts/build_manifest.R has no registry file: after sourcing the
# scripts it registers EVERY top-level function named resolve_<x> / watch_<x>
# as the resolver / watcher of source <x> ("Load resolvers + watchers"). So an
# internal helper that merely happens to be named resolve_something becomes a
# resolver of its own. That is what resolve_baci_dictionary() did: it is only
# ever called from resolve_baci(), but the driver also ran it as a resolver
# named "baci_dictionary" on every scheduled run, with no manifest rows to hand
# it -- one wasted request, then "FAILED: resolver returned no rows" in the CI
# log, for a source that does not exist.
#
# These tests ask what the driver would register and check it against the
# manifest's own `resolver` column, so nothing here is a list to update when a
# source is onboarded. The files are read, not run (parse tree only): no network,
# no packages beyond base R, and helper scripts that must never be sourced
# (check_all_loads.R) can be scanned too.
#
# Not checked: that a watcher owns inventory rows. A new watcher legitimately
# starts with none (it scrapes its pages and the driver then writes its first
# rows), so only the naming convention is enforced for watchers.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")
manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not(file.exists(manifest_path), "manifest not present")

# What build_manifest.R's ls() finds after sourcing a file: names bound at the
# top level (`x <- ...`, `x = ...`, `x <<- ...`, with or without backticks), not
# functions defined inside other functions. Takes the file's lines. A definition
# made by assign() or inside an if/local block is not seen -- nor is it a place
# anyone defines a resolver.
top_level_names <- function(lines) {
  exprs <- parse(text = lines, keep.source = FALSE)
  bound <- vapply(exprs, function(e) {
    if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
      (is.symbol(e[[2]]) || is.character(e[[2]]))) {
      as.character(e[[2]])
    } else {
      NA_character_
    }
  }, character(1))
  bound[!is.na(bound)]
}

# The same files build_manifest.R sources from actions/scrapers/ (its two
# list.files() patterns), plus every helper script in actions/scripts/ -- a
# superset of the ones it sources, so a new helper cannot slip through. The
# driver itself is left out: it is not sourced by anything, and its own
# variables (watch_registry, watch_notify, ...) are not registrations.
scraper_files <- list.files(file.path(actions_dir, "scrapers"), pattern = "^(resolve|watch)_.*\\.R$", full.names = TRUE)
script_files <- list.files(file.path(actions_dir, "scripts"), pattern = "\\.R$", full.names = TRUE)
script_files <- script_files[basename(script_files) != "build_manifest.R"]

bindings_of <- function(files) {
  stats::setNames(
    lapply(files, function(f) top_level_names(readLines(f, warn = FALSE, encoding = "UTF-8"))),
    basename(files)
  )
}
scraper_bindings <- bindings_of(scraper_files)
script_bindings <- bindings_of(script_files)

REGISTERED <- "^(resolve|watch)_"

test_that("the scanner reads top-level bindings the way ls() sees them after source()", {
  code <- c(
    "resolve_a <- function(rows) {",
    "  resolve_inner <- function() 1", # inside a function: never registered
    "  NULL",
    "}",
    "watch_b = function(rows) NULL",
    "`resolve_c` <- 1",
    "resolve_d <<- 2",
    "helper_e <- function() NULL",
    "# resolve_commented <- 1",
    "print('resolve_in_a_string')"
  )
  expect_equal(top_level_names(code), c("resolve_a", "watch_b", "resolve_c", "resolve_d", "helper_e"))
  expect_equal(top_level_names(character(0)), character(0))
  # and it is looking at real files, not at nothing
  expect_true(length(scraper_files) >= 10)
  expect_true("resolve_baci" %in% scraper_bindings[["resolve_baci.R"]])
})

test_that("each scraper file defines the one resolver/watcher its name promises, and no helper is named like one", {
  for (file in names(scraper_bindings)) {
    promised <- sub("\\.R$", "", file)
    registrable <- grep(REGISTERED, scraper_bindings[[file]], value = TRUE)

    expect_equal(
      setdiff(registrable, promised), character(0),
      info = paste0(
        file, " defines ", paste(setdiff(registrable, promised), collapse = ", "), ", which build_manifest.R would ",
        "register as a ", if (grepl("^watch_", file)) "watcher" else "resolver", " of its own. Rename the internal ",
        "helper so it does not start with resolve_ or watch_ (this repo's convention: <source>_<what it does>)."
      )
    )
    expect_equal(
      setdiff(promised, registrable), character(0),
      info = paste0(file, " does not define ", promised, "(), so build_manifest.R finds nothing to register for it.")
    )
  }

  for (file in names(script_bindings)) {
    expect_equal(
      grep(REGISTERED, script_bindings[[file]], value = TRUE), character(0),
      info = paste0(
        "actions/scripts/", file, " is sourced by build_manifest.R, which would register any resolve_*/watch_* ",
        "function in it as a resolver/watcher. Name helpers <what it does> instead."
      )
    )
  }
})

test_that("every registered resolver owns manifest rows, and every resolver the manifest names exists", {
  manifest <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")
  owners <- sort(unique(manifest$resolver[!is.na(manifest$resolver)]))

  defined <- unlist(c(scraper_bindings, script_bindings), use.names = FALSE)
  registered <- sort(unique(sub("^resolve_", "", grep("^resolve_", defined, value = TRUE))))

  expect_true(length(owners) >= 10) # the manifest really was read

  # a registered resolver with no row naming it in `resolver` is run with no rows
  # and reported FAILED on every scheduled run (see the header)
  expect_equal(
    setdiff(registered, owners), character(0),
    info = paste0(
      "resolve_<x>() with no manifest row whose `resolver` is <x>: either an internal helper that should not ",
      "be named resolve_*, or a new resolver whose rows have not been given `resolver = <x>` yet."
    )
  )
  # the other way round: rows that name a resolver nobody defined are never refreshed, and nothing says so
  expect_equal(
    setdiff(owners, registered), character(0),
    info = "manifest rows name a `resolver` for which no resolve_<x>() function exists, so they are never refreshed."
  )
})
