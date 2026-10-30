# tests/testthat/test-imazon-resolver.R
#
# Offline test for actions/scrapers/resolve_imazon.R. imazon's two datasets are the CSV
# files behind Imazon's own SAD dashboard (monthly deforestation and degradation alerts
# by municipality); until 2026-10-30 the source was a personal Google Drive file
# (see that script's header). The resolver fetches the dashboard's /periodo endpoint and
# each CSV every run and checks the columns, the values and the years load_imazon()
# relies on. curl is mocked (no network); the CSVs are small stand-ins with the same
# columns.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if(
  !dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
source(file.path(actions_dir, "scrapers", "resolve_imazon.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

DEFORESTATION_URL <- paste0(IMAZON_API, "/desmatamento/municipios.csv")
DEGRADATION_URL <- paste0(IMAZON_API, "/degradacao/municipios.csv")

manifest_rows <- tibble::tibble(
  survey = "imazon", dataset = c("sad_deforestation", "sad_degradation"),
  geo_level = NA_character_, year = NA_character_,
  url = c(DEFORESTATION_URL, DEGRADATION_URL)
)

# ---- stand-ins for what Imazon serves --------------------------------------------------

PERIOD <- '{"ano_min":2008,"ano_max":2026,"mes_max":8}'

# 5 municipalities x 12 months x each year (the last year only up to `last_month`): 1,000+
# rows, never repeated. `years` lets a dataset have gaps, as the degradation file does.
make_csv <- function(alert, years = 2008:2026, last_month = 8, area = 1, mutate = identity, drop = character(0)) {
  grid <- expand.grid(MUNICIPIO = paste("Municipio", 1:5), MES = 1:12, ANO = years, stringsAsFactors = FALSE)
  grid <- grid[!(grid$ANO == max(years) & grid$MES > last_month), ]
  x <- data.frame(
    ALERTA = alert, MES = grid$MES, ANO = grid$ANO, SENSOR = "Sentinel-2", ESTADO = "PA",
    MUNICIPIO = grid$MUNICIPIO, AREAKM2 = area * (1 + (grid$MES %% 3)), stringsAsFactors = FALSE
  )
  x <- mutate(x)
  x <- x[, setdiff(names(x), drop)]
  paste(utils::capture.output(utils::write.csv(x, row.names = FALSE, quote = FALSE)), collapse = "\n")
}

response <- function(body, status = 200L, type = "text/csv; charset=utf-8") {
  list(
    status_code = status,
    headers = charToRaw(paste0("HTTP/1.1 ", status, " X\r\ncontent-type: ", type, "\r\n\r\n")),
    content = if (is.raw(body)) body else charToRaw(body)
  )
}

# `files` maps each URL to a response (or a function that stops, as curl does). Anything
# else is an error: the resolver talks to the dashboard and nobody else.
fake_imazon <- function(deforestation = make_csv("desmatamento"),
                        degradation = make_csv("degradacao", years = c(2008:2024, 2026)),
                        period = response(PERIOD, type = "application/json; charset=utf-8"), seen = NULL) {
  files <- list()
  files[[IMAZON_PERIOD_URL]] <- period
  files[[DEFORESTATION_URL]] <- if (is.character(deforestation)) response(deforestation) else deforestation
  files[[DEGRADATION_URL]] <- if (is.character(degradation)) response(degradation) else degradation
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) {
      seen$urls <- c(seen$urls, url)
      seen$handles <- c(seen$handles, list(handle))
    }
    answer <- files[[url]]
    if (is.null(answer)) stop("the fake world has no such URL: ", url)
    if (is.function(answer)) answer(url) else answer
  }
}
# new_handle() is mocked too, to hand the options straight to the stand-in: a real curl
# handle does not let one read them back (tested in "no request follows a redirect").
with_world <- function(world, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = world, new_handle = function(...) list(...), .package = "curl")
}
run <- function(world = fake_imazon(), rows = manifest_rows) suppressMessages(with_world(world, resolve_imazon(rows)))
error_message <- function(world, rows = manifest_rows) {
  tryCatch(
    {
      run(world, rows)
      "NO ERROR"
    },
    error = function(e) conditionMessage(e)
  )
}
failures_of <- function(world, rows = manifest_rows) attr(run(world, rows), "partial_failures")

# ---- the healthy dashboard -------------------------------------------------------------------

test_that("a healthy dashboard resolves both datasets, with the years each file really has", {
  seen <- new.env()
  out <- run(fake_imazon(seen = seen))

  expect_equal(out$dataset, c("sad_deforestation", "sad_degradation"))
  expect_equal(out$survey, c("imazon", "imazon"))
  expect_equal(out$url, c(DEFORESTATION_URL, DEGRADATION_URL)) # echoed: the ones just verified
  expect_equal(out$available_time, c("2008-2026", "2008-2024, 2026")) # the degradation file has no 2025
  expect_equal(names(out), c("survey", "dataset", "geo_level", "year", "url", "available_time"))
  expect_null(attr(out, "partial_failures"))
  # and what the manifest will say is a string the package's own parser reads back
  expect_equal(parse_years(out$available_time[2]), c(2008:2024, 2026))
  # three requests: the period and the two files, every run
  expect_setequal(seen$urls, c(IMAZON_PERIOD_URL, DEFORESTATION_URL, DEGRADATION_URL))
})

test_that("only the datasets the manifest has are fetched, each row echoing its own keys", {
  rows <- manifest_rows[c(1, 1), ]
  rows$geo_level <- c("municipality", "state")
  seen <- new.env()
  out <- run(fake_imazon(seen = seen), rows)

  expect_equal(out$geo_level, c("municipality", "state"))
  expect_equal(out$available_time, c("2008-2026", "2008-2026"))
  expect_false(DEGRADATION_URL %in% seen$urls)
  expect_equal(sum(seen$urls == DEFORESTATION_URL), 1) # once, however many rows
})

test_that("available_time is written in the grammar parse_years() reads, gaps included", {
  expect_equal(imazon_years_to_available_time(2008:2026), "2008-2026")
  expect_equal(imazon_years_to_available_time(c(2008:2024, 2026)), "2008-2024, 2026")
  expect_equal(imazon_years_to_available_time(c(2026L, 2008L, 2008L, 2010L)), "2008, 2010, 2026") # unsorted, repeated, isolated
  expect_equal(imazon_years_to_available_time(2020L), "2020")
  expect_true(is.na(imazon_years_to_available_time(integer(0))))
  for (years in list(2008:2026, c(2008:2024, 2026), c(2008L, 2010L, 2026L))) {
    expect_equal(parse_years(imazon_years_to_available_time(years)), years)
  }
})

# ---- /periodo ------------------------------------------------------------------------------------

test_that("the period endpoint is parsed, and anything else is refused", {
  ok <- imazon_parse_period(PERIOD)
  expect_equal(c(ok$ano_min, ok$ano_max, ok$mes_max), c(2008L, 2026L, 8L))
  expect_equal(ok$problem, character(0))

  expect_match(imazon_parse_period("<html>Cloudflare</html>")$problem, "not JSON with ano_min", fixed = TRUE)
  expect_match(imazon_parse_period('{"ano_min":2008,"ano_max":2026}')$problem, "not JSON with ano_min", fixed = TRUE)
  expect_match(imazon_parse_period('{"ano_min":2008,"ano_max":2026,"mes_max":13}')$problem, "not a period", fixed = TRUE)
  expect_match(imazon_parse_period('{"ano_min":2027,"ano_max":2026,"mes_max":3}')$problem, "not a period", fixed = TRUE)
})

test_that("a period endpoint that is down, moved or garbled stops the run: nothing can be checked without it", {
  expect_match(error_message(fake_imazon(period = response("oops", status = 500L))), "HTTP 500", fixed = TRUE)
  expect_match(error_message(fake_imazon(period = function(url) stop("Timeout was reached"))), "no HTTP response (Timeout was reached)", fixed = TRUE)
  expect_match(error_message(fake_imazon(period = response("<html></html>", type = "text/html"))), "answered something unexpected", fixed = TRUE)
})

# ---- each dataset's file -----------------------------------------------------------------------------

test_that("the columns load_imazon() reads must be there, and a missing one is named", {
  for (column in c("ANO", "MES", "ESTADO", "MUNICIPIO", "AREAKM2", "ALERTA", "SENSOR")) {
    msg <- failures_of(fake_imazon(deforestation = make_csv("desmatamento", drop = column)))
    expect_match(msg, paste0("columns that load_imazon\\(\\) reads are missing: ", column), info = column)
    expect_match(msg, "^imazon/sad_deforestation: ", info = column)
  }
  # extra columns are fine
  expect_null(attr(run(fake_imazon(deforestation = make_csv("desmatamento", mutate = function(x) transform(x, EXTRA = 1)))), "partial_failures"))
})

test_that("each value the loader assumes is checked, and the reason is in the message", {
  problem <- function(mutate, alert = "desmatamento") {
    failures_of(fake_imazon(deforestation = make_csv(alert, mutate = mutate)))
  }
  expect_match(problem(function(x) { x$MES[1] <- 13L; x }), "MES has values that are not months", fixed = TRUE)
  expect_match(problem(function(x) { x$AREAKM2[1] <- -1; x }), "AREAKM2 has NA, negative or non-numeric", fixed = TRUE)
  expect_match(problem(function(x) { x$AREAKM2[1] <- NA; x }), "AREAKM2 has NA, negative or non-numeric", fixed = TRUE)
  expect_match(problem(function(x) { x$MUNICIPIO[1] <- ""; x }), "MUNICIPIO has blank names", fixed = TRUE)
  expect_match(problem(function(x) { x$ESTADO[1] <- "SP"; x }), "not Legal Amazon states: SP", fixed = TRUE)
  expect_match(problem(function(x) x[c(1, 1:nrow(x)), ]), "repeated", fixed = TRUE)
  # the other dataset's file behind this dataset's URL
  expect_match(problem(identity, alert = "degradacao"), "ALERTA is not always 'desmatamento' (found: degradacao)", fixed = TRUE)
})

test_that("a change of unit would be caught: a year's total in hectares is 100 times the km2", {
  expect_match(failures_of(fake_imazon(deforestation = make_csv("desmatamento", area = 5000))), "a different unit?", fixed = TRUE)
})

test_that("too few rows, or not a CSV at all, is refused", {
  few <- make_csv("desmatamento", years = 2025:2026)
  expect_match(failures_of(fake_imazon(deforestation = few)), "fewer than 1000", fixed = TRUE)
  expect_match(failures_of(fake_imazon(deforestation = "<html><body>Just a moment...</body></html>")), "not a CSV|columns that load_imazon\\(\\) reads are missing")
})

test_that("a feed that has stopped, or runs past the dashboard's own last month, is refused", {
  stale <- make_csv("desmatamento", years = 2008:2023)
  expect_match(failures_of(fake_imazon(deforestation = stale)), "its latest year is 2023, the dashboard's period ends in 2026", fixed = TRUE)
  ahead <- make_csv("desmatamento", last_month = 9)
  expect_match(failures_of(fake_imazon(deforestation = ahead)), "after the dashboard's last month (2026/8)", fixed = TRUE)
  # a feed one year behind is a year with no alerts yet, not a failure
  expect_null(attr(run(fake_imazon(deforestation = make_csv("desmatamento", years = 2008:2025, last_month = 12))), "partial_failures"))
})

# ---- the transport ----------------------------------------------------------------------------------------

test_that("a dataset whose file fails is a PARTIAL failure: the other is still refreshed, and the failure is reported once", {
  out <- run(fake_imazon(degradation = response("gone", status = 404L)))
  expect_equal(out$dataset, "sad_deforestation")
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, "^imazon/sad_degradation: ")
  expect_match(failures, "HTTP 404", fixed = TRUE)
})

test_that("no request follows a redirect: the period endpoint and both files are asked for with followlocation off", {
  seen <- new.env()
  run(fake_imazon(seen = seen))
  expect_equal(seen$urls, c(IMAZON_PERIOD_URL, DEFORESTATION_URL, DEGRADATION_URL))
  for (handle in seen$handles) expect_identical(handle$followlocation, FALSE)
})

test_that("a redirect is not followed, a wrong content-type is refused, a dead connection is reported", {
  expect_match(failures_of(fake_imazon(deforestation = response("", status = 302L))), "HTTP 302", fixed = TRUE)
  expect_match(failures_of(fake_imazon(deforestation = response(make_csv("desmatamento"), type = "text/html"))), "content-type 'text/html', not text/csv", fixed = TRUE)
  expect_match(failures_of(fake_imazon(deforestation = function(url) stop("Could not resolve host"))), "no HTTP response (Could not resolve host)", fixed = TRUE)
})

test_that("when every dataset fails the resolver stops, naming each", {
  msg <- error_message(fake_imazon(deforestation = response("", status = 500L), degradation = response("", status = 500L)))
  expect_match(msg, "every dataset failed", fixed = TRUE)
  expect_match(msg, "imazon/sad_deforestation", fixed = TRUE)
  expect_match(msg, "imazon/sad_degradation", fixed = TRUE)
})

test_that("a row for a dataset the resolver does not know fails that row, not the others", {
  rows <- manifest_rows
  rows$dataset[2] <- "imazon_shp" # the dataset this replaced
  out <- run(fake_imazon(), rows)
  expect_equal(out$dataset, "sad_deforestation")
  expect_match(attr(out, "partial_failures"), "resolve_imazon() only knows sad_deforestation, sad_degradation", fixed = TRUE)
})

test_that("no owned rows stops, and the script defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_match(error_message(fake_imazon(), manifest_rows[0, ]), "no manifest rows tagged resolver == 'imazon'", fixed = TRUE)
  expect_equal(ls(resolver_env, pattern = "^(resolve_|watch_)"), "resolve_imazon")
})

# ---- R/imazon.R and the committed manifest --------------------------------------------------------------------

test_that("the columns the resolver demands include every raw column R/imazon.R reads", {
  imazon_src <- file.path(pkg_root, "R", "imazon.R")
  skip_if_not(file.exists(imazon_src), "R/imazon.R not present")
  code <- readLines(imazon_src, warn = FALSE)
  code <- code[!grepl("^\\s*#", code) & !grepl("^#'", code)]
  for (column in c("ANO", "MES", "ESTADO", "MUNICIPIO", "AREAKM2")) {
    expect_true(any(grepl(paste0("\\b", column, "\\b"), code)), info = paste("R/imazon.R no longer reads", column))
    expect_true(column %in% IMAZON_COLUMNS, info = column)
  }
})

test_that("the committed manifest rows are the resolver's datasets, on Imazon's own dashboard, with no Google Drive left", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")
  rows <- m[m$survey == "imazon", ]

  expect_setequal(rows$dataset, names(IMAZON_DATASETS))
  expect_true(all(rows$resolver == "imazon"))
  expect_equal(rows$url[rows$dataset == "sad_deforestation"], DEFORESTATION_URL)
  expect_equal(rows$url[rows$dataset == "sad_degradation"], DEGRADATION_URL)
  expect_true(all(rows$docs_url == "https://dashboards.imazongeo.org.br/sad/"))
  expect_true(all(rows$available_geo == "municipality"))
  expect_false(anyNA(lapply(rows$available_time, parse_years)))
  expect_equal(min(parse_years(rows$available_time[1])), 2008L)
  expect_false(any(grepl("google", c(rows$url, rows$docs_url), ignore.case = TRUE)))
  expect_false("imazon_shp" %in% m$dataset)
})
