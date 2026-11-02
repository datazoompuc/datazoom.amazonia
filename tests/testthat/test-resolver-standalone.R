# tests/testthat/test-resolver-standalone.R
#
# build_manifest.R runs under `Rscript --vanilla` and never loads this package,
# so nothing under actions/ can call the package's own functions. Until
# 2026-11-01 resolve_ips.R and resolve_seeg.R did, inside a tryCatch, so in CI
# their check for a lost year failed silently and never fired. Their tests ran
# with the package loaded, where those calls work, so nothing noticed. The first
# test here fails on any such call; the others prove the two checks work again.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

test_that("nothing the pipeline sources calls a package-only function", {
  pipeline <- c(
    list.files(file.path(actions_dir, "scrapers"), pattern = "\\.R$", full.names = TRUE),
    # check_all_loads.R is left out: it attaches the package on purpose
    setdiff(
      list.files(file.path(actions_dir, "scripts"), pattern = "\\.R$", full.names = TRUE),
      file.path(actions_dir, "scripts", "check_all_loads.R")
    )
  )
  expect_gt(length(pipeline), 15)

  package_only <- c(
    "dataset_meta", "dataset_field", "dataset_url", "datasets_link",
    "parse_years", "effective_table", "link_table"
  )
  # all.names() reads the parsed code, so comments that mention them don't count
  offenders <- unlist(lapply(pipeline, function(f) {
    used <- intersect(all.names(parse(f, keep.source = FALSE)), package_only)
    if (length(used) > 0) paste0(basename(f), ": ", paste(used, collapse = ", "))
  }))
  expect_null(offenders)
})

source(file.path(actions_dir, "scripts", "manifest_validate.R"), local = TRUE)
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_ips.R"), local = TRUE)
source(file.path(actions_dir, "scrapers", "resolve_seeg.R"), local = TRUE)

# Stands in for the alert cache and the ranged reads: the tests below control
# what the workbook contains, and everything else in the resolvers is real.
alert_recorder <- function(source) function(...) invisible(NULL)
workbook_xml <- NULL
pivot_xml <- NULL
zip_remote_entries <- function(url, ...) {
  list(
    "xl/workbook.xml" = "workbook",
    "xl/pivotCache/pivotCacheDefinition1.xml" = "pivot"
  )
}
zip_remote_extract_entry <- function(url, entry, ...) {
  charToRaw(if (identical(entry, "pivot")) pivot_xml else workbook_xml)
}

fake_site <- function(url, handle = NULL, ...) {
  body <- if (grepl("seeg.eco.br", url, fixed = TRUE)) {
    '<a href="https://seeg.eco.br/wp-content/uploads/2025/12/Dados-municipais-13.0.xlsx">x</a>'
  } else if (grepl("main\\.", url)) {
    "painel.ipsamazonia.org.br/uploads/IPS_Amazonia_2023_abc123.xlsx"
  } else {
    '<script src="/static/js/main.abc123.js"></script>'
  }
  list(status_code = 200L, content = charToRaw(body), headers = raw(0), url = url)
}

run_ips <- function(tabs, promised = "2014, 2018, 2021, 2023") {
  workbook_xml <<- paste0('<sheet name="', tabs, '"', collapse = "")
  rows <- tibble::tibble(
    survey = "ips", dataset = c("all", "educ"),
    geo_level = NA_character_, year = NA_character_, available_time = promised
  )
  testthat::with_mocked_bindings(resolve_ips(rows), curl_fetch_memory = fake_site, .package = "curl")
}

run_seeg <- function(years, promised = "1970-2024") {
  workbook_xml <<- '<sheet name="Dados"'
  pivot_xml <<- paste0('<cacheField name="', c(SEEG_EXPECTED_CACHE_FIELDS, years), '"', collapse = "")
  rows <- tibble::tibble(
    survey = "seeg", dataset = c("seeg", "seeg_land"),
    geo_level = NA_character_, year = NA_character_, available_time = promised
  )
  testthat::with_mocked_bindings(resolve_seeg(rows), curl_fetch_memory = fake_site, .package = "curl")
}

test_that("IPS stops when a promised year's tab is gone, and accepts a new one", {
  expect_error(run_ips(c("2014", "2018 ", "2021")), "missing year tab\\(s\\) 2023")

  res <- run_ips(c("2014", "2018 ", "2021", "2023", "2025"))
  expect_equal(unique(res$available_time), "2014, 2018, 2021, 2023, 2025")
})

test_that("SEEG stops when a promised year's field is gone, and accepts a new one", {
  expect_error(run_seeg(1970:2023), "missing year field\\(s\\) 2024")

  res <- run_seeg(1970:2025)
  expect_equal(unique(res$available_time), "1970-2025")
})

test_that("SEEG writes a hole in its years instead of hiding it in a range", {
  res <- run_seeg(c(1970:2000, 2002:2024), promised = "1970-2000, 2002-2024")
  expect_equal(unique(res$available_time), "1970-2000, 2002-2024")
})

test_that("parse_available_time() reads the grammar, and a blank promises nothing", {
  expect_equal(parse_available_time("2005"), 2005L)
  expect_equal(parse_available_time("2001-2003, 2007"), c(2001L, 2002L, 2003L, 2007L))
  expect_equal(parse_available_time(NA_character_), integer(0))
  expect_equal(parse_available_time(""), integer(0))
  expect_equal(parse_available_time(character(0)), integer(0))
})

owned <- function(survey, dataset, geo_level = NA_character_, year = NA_character_) {
  tibble::tibble(survey = survey, dataset = dataset, geo_level = geo_level, year = year)
}

test_that("available_time_not_refreshed() names only owned, non-exempt rows left without a value", {
  rows <- owned("x", c("a", "b"), c("state", NA))

  full <- dplyr::mutate(rows, available_time = "2000-2001")
  expect_length(available_time_not_refreshed(rows, full), 0)

  # left out of the result altogether
  expect_equal(available_time_not_refreshed(rows, full[1, ]), "x/b")
  # returned, but with NA
  partly <- full
  partly$available_time[1] <- NA
  expect_equal(available_time_not_refreshed(rows, partly), "x/a/state")
  # a resolver that never returns the column misses every row
  expect_equal(available_time_not_refreshed(rows, rows), c("x/a/state", "x/b"))

  # a blank key must not match a row with a real one
  wrong_key <- owned("x", "a", NA_character_)
  wrong_key$available_time <- "2000"
  expect_equal(available_time_not_refreshed(owned("x", "a", "state"), wrong_key), "x/a/state")
})

test_that("exempt rows are never reported, and the exemption is that narrow", {
  exempt <- dplyr::bind_rows(
    owned("degrad", "degrad", year = "2007"),
    owned("comex_dic", c("hs_codes", "ncm_codes")),
    owned("aneel", "energy_enterprises_distributed")
  )
  expect_length(available_time_not_refreshed(exempt, exempt), 0)

  expect_equal(
    available_time_not_refreshed(owned("aneel", "energy_generation"), owned("aneel", "energy_generation")),
    "aneel/energy_generation"
  )
})

test_that("every resolver-owned row in the committed manifest has available_time, or is exempt", {
  manifest <- utils::read.csv(
    file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv"),
    colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8"
  )
  blank <- manifest[!is.na(manifest$resolver) & is.na(manifest$available_time), ]
  not_exempt <- blank[!mapply(is_available_time_exempt, blank$survey, blank$dataset), ]
  expect_equal(paste(not_exempt$survey, not_exempt$dataset, sep = "/"), character(0))
})
