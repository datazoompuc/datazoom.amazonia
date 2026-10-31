# tests/testthat/test-pibmunic-load.R
#
# Offline tests for what load_pibmunic() asks IBGE for and what it calls the result
# (R/pibmunic.R, and sidra_get_table() in R/download.R, tested for itself in
# test-ppm-load.R). Until 2026-10-30 the manifest's sidra_code was the bare table
# number "5938" and load_pibmunic() sent sidra_download() its default
# classific = "all"; sidrar answers that by scraping an IBGE HTML page that is behind
# a bot challenge from some networks, and the function failed there with "Error in
# Download." (country and state, 2021). Table 5938 has no classification, so it is now
# sent with classific = NULL, which sidra_get_table() answers without the page.
# sidra_download() is mocked (no network); there was no test of load_pibmunic() at all.

# ---- what load_pibmunic() asks sidra_download() for -------------------------------

captured <- function(...) {
  seen <- list()
  testthat::local_mocked_bindings(
    sidra_download = function(...) {
      seen[[length(seen) + 1]] <<- list(...)
      stop("captured")
    },
    .package = "datazoom.amazonia"
  )
  suppressWarnings(try(load_pibmunic(...), silent = TRUE))
  seen
}

test_that("the manifest says what these tests assume: one dataset, table 5938, no classification", {
  committed <- link_table()[link_table()$survey == "pibmunic", ]
  expect_equal(nrow(committed), 1)
  expect_equal(committed$dataset, "pibmunic")
  expect_equal(committed$sidra_code, "5938")
})

test_that("load_pibmunic() asks for the whole table with no classification, one call per year", {
  calls <- captured(geo_level = "state", time_period = 2022:2023)
  # the first call stops the function (the stand-in raises), so ask one year at a time too
  first <- calls[[1]]
  expect_equal(first$sidra_code, 5938)
  expect_true("classific" %in% names(first) && is.null(first$classific)) # present, and NULL
  expect_true("category" %in% names(first) && is.null(first$category))
  expect_equal(first$year, "2022")
  expect_equal(first$geo_level, "state")

  for (level in c("country", "state", "municipality")) {
    calls <- captured(geo_level = level, time_period = 2021)
    expect_equal(calls[[1]]$geo_level, level, info = level)
    expect_equal(calls[[1]]$year, "2021", info = level)
    expect_null(calls[[1]]$classific, info = level)
  }
})

test_that("a SIDRA table given as a number is the manifest dataset that reads it: the same request as the dataset's name", {
  by_name <- captured(dataset = "pibmunic", geo_level = "country", time_period = 2021)
  by_number <- captured(dataset = 5938, geo_level = "country", time_period = 2021)
  expect_equal(by_number, by_name)
  expect_null(by_number[[1]]$classific)
  expect_equal(sidra_dataset_of_table("pibmunic", 5938), "pibmunic")
  expect_equal(sidra_dataset_of_table("pibmunic", "5938"), "pibmunic")
  expect_null(sidra_dataset_of_table("pibmunic", 1613)) # PAM's table
})

test_that("a number the manifest does not know keeps the old request (classific = 'all'), and a bad dataset name still stops in check_params()", {
  other <- captured(dataset = 1613, geo_level = "country", time_period = 2021, raw_data = TRUE) # a PAM table
  expect_equal(other[[1]]$sidra_code, 1613)
  expect_equal(other[[1]]$classific, "all")
  expect_equal(other[[1]]$category, "all")

  expect_error(load_pibmunic(dataset = "pibmunic_nonexistent", geo_level = "country", time_period = 2021), "not supported")
})

test_that("time_period is checked against the manifest's available_time: 2023 is in it, 2024 and 2001 are not", {
  testthat::local_mocked_bindings(sidra_download = function(...) stop("captured"), .package = "datazoom.amazonia")
  expect_no_warning(try(load_pibmunic(geo_level = "state", time_period = 2023), silent = TRUE))
  expect_warning(try(load_pibmunic(geo_level = "state", time_period = 2024), silent = TRUE), "time_period must be in 2002-2023", fixed = TRUE)
  expect_warning(try(load_pibmunic(geo_level = "state", time_period = 2001), silent = TRUE), "time_period must be in", fixed = TRUE)
})

# ---- what load_pibmunic() calls the result ----------------------------------------

# sidra_download()'s answer for the table at each level, in sidrar's own column order
# (the pivot below selects variavel:variavel_codigo, so the order matters), with the
# columns IBGE's response carries at that level: checked live 2026-10-30. valor is the
# one numeric column; the names of the variables are the dictionary's (IBGE's own, in
# ASCII).
DIC <- as.data.frame(load_dictionary("pibmunic"))
variable_name <- function(id) DIC$var_pt[match(as.numeric(id), DIC$var_code)]

GEO_COLUMNS <- list(
  country = c("brasil_codigo", "brasil"),
  state = c("unidade_da_federacao_codigo", "unidade_da_federacao"),
  municipality = c("municipio_codigo", "municipio")
)
GEO_CODES <- list(country = "1", state = c("11", "12"), municipality = c("1100015", "1100023"))

fake_value <- function(variable, geo, year) as.numeric(variable) + 10 * as.numeric(geo) %% 1000 + as.numeric(year) / 10000

fake_pibmunic <- function(level = "state", variables = c("37", "496", "571", "6571"), years = "2023", ids = NULL) {
  grid <- expand.grid(geo = GEO_CODES[[level]], year = years, variavel_codigo = variables, stringsAsFactors = FALSE)
  out <- tibble::tibble(
    nivel_territorial_codigo = "3", nivel_territorial = "Unidade da Federação",
    unidade_de_medida_codigo = "40", unidade_de_medida = "Mil Reais",
    valor = mapply(fake_value, grid$variavel_codigo, grid$geo, grid$year)
  )
  out[[GEO_COLUMNS[[level]][1]]] <- grid$geo
  out[[GEO_COLUMNS[[level]][2]]] <- if (level == "country") "Brasil" else paste("Lugar", grid$geo)
  out$ano_codigo <- grid$year
  out$ano <- grid$year
  out$variavel_codigo <- grid$variavel_codigo
  out$variavel <- ifelse(is.na(variable_name(grid$variavel_codigo)), "Variavel nova", variable_name(grid$variavel_codigo))
  out
}

load_fake <- function(level = "state", language = "eng", fake = fake_pibmunic(level), ...) {
  testthat::local_mocked_bindings(sidra_download = function(...) fake, .package = "datazoom.amazonia")
  load_pibmunic(geo_level = level, time_period = 2023, language = language, ...)
}

for (level in c("country", "state", "municipality")) {
  test_that(sprintf("load_pibmunic() at the %s level: one row per place and year, a column per variable, the geography as geo_id", level), {
    out <- load_fake(level)
    codes <- GEO_CODES[[level]]

    expect_equal(nrow(out), length(codes))
    # the state and the municipality are identified by IBGE's code; the country, as it always has
    # been, by the name IBGE gives it ("Brasil"), the only place R/pibmunic.R takes a name for geo_id
    expect_equal(out$geo_id, if (level == "country") "Brasil" else codes)
    expect_equal(out$year, rep("2023", length(codes)))
    expect_equal(names(out)[1:2], c("year", "geo_id"))
    # one column per variable, named after IBGE's own label and ending in its id
    value_cols <- setdiff(names(out), c("year", "geo_id"))
    expect_equal(sub("^.*_v", "", value_cols), c("37", "496", "571", "6571"))
    # the geography's own columns, and the unit columns, are gone
    # (compared by exact name: the variables' own labels mention "Brasil" and "unidade da federacao")
    dropped <- c(
      "brasil", "brasil_codigo", "unidade_da_federacao", "unidade_da_federacao_codigo", "municipio", "municipio_codigo",
      "nivel_territorial", "nivel_territorial_codigo", "unidade_de_medida", "unidade_de_medida_codigo", "ano_codigo", "valor"
    )
    expect_false(any(names(out) %in% dropped))
    # each value arrives under the name of its variable
    for (col in value_cols) {
      id <- sub("^.*_v", "", col)
      expect_equal(out[[col]], vapply(codes, function(g) fake_value(id, g, "2023"), numeric(1)), ignore_attr = TRUE, info = col)
    }
  })
}

test_that("years are rows, and several years are several calls bound together", {
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, year, ...) fake_pibmunic("state", years = year),
    .package = "datazoom.amazonia"
  )
  out <- load_pibmunic(geo_level = "state", time_period = 2021:2023)
  expect_equal(nrow(out), 6)
  expect_equal(sort(unique(out$year)), c("2021", "2022", "2023"))
})

test_that("a value IBGE does not publish leaves an NA where the place has other numbers, and a place with none is dropped", {
  fake <- fake_pibmunic("state", variables = c("37", "496"))
  fake$valor[fake$variavel_codigo == "496" & fake$unidade_da_federacao_codigo == "12"] <- NA
  out <- load_fake("state", fake = fake)
  expect_equal(nrow(out), 2)
  expect_true(is.na(out[out$geo_id == "12", ][[grep("_v496$", names(out))]]))

  fake$valor[fake$unidade_da_federacao_codigo == "12"] <- NA
  expect_equal(load_fake("state", fake = fake)$geo_id, "11")
})

test_that("the Portuguese names", {
  out <- load_fake("state", language = "pt")
  expect_equal(names(out)[1:2], c("ano", "geo_id"))
  expect_false("year" %in% names(out))
})

test_that("raw data is what sidra_download() returned, years bound together, untreated", {
  fake <- fake_pibmunic("state")
  out <- load_fake("state", fake = fake, raw_data = TRUE)
  expect_equal(out, tibble::as_tibble(fake))
  expect_true("variavel_codigo" %in% names(out))
})

# ---- the dictionary labels what load_pibmunic() returns ---------------------------

test_that("every variable column carries its own label, in the language asked for -- 571 and 6571 included", {
  eng <- load_fake("state", language = "eng")
  pt <- load_fake("state", language = "pt")
  for (id in c("37", "496", "571", "6571")) {
    col <- grep(paste0("_v", id, "$"), names(eng), value = TRUE)
    expect_length(col, 1)
    expect_equal(attr(eng[[col]], "label"), DIC$var_eng[match(as.numeric(id), DIC$var_code)], info = id)
    expect_equal(attr(pt[[col]], "label"), DIC$var_pt[match(as.numeric(id), DIC$var_code)], info = id)
  }
  # the two ids that end alike must not share one label (R/pibmunic.R's FRAGILE note: the column
  # of 6571 also matches the pattern of 571, and only the dictionary's order keeps its own label)
  expect_false(identical(
    attr(eng[[grep("_v571$", names(eng))]], "label"),
    attr(eng[[grep("_v6571$", names(eng))]], "label")
  ))
  expect_true(which(DIC$var_code == 571) < which(DIC$var_code == 6571))
  expect_true(which(DIC$var_code == 570) < which(DIC$var_code == 6570))
})

test_that("every variable of the dictionary is labelled when the table returns all of them", {
  out <- load_fake("state", fake = fake_pibmunic("state", variables = as.character(DIC$var_code)))
  value_cols <- setdiff(names(out), c("year", "geo_id"))
  expect_equal(length(value_cols), 46)
  for (col in value_cols) {
    label <- attr(out[[col]], "label")
    expect_true(length(label) == 1 && nzchar(label), info = col)
  }
})

test_that("a variable IBGE adds comes back unlabelled -- not an error -- and the others keep theirs", {
  out <- load_fake("state", fake = fake_pibmunic("state", variables = c("37", "9999")))
  expect_null(attr(out[[grep("_v9999$", names(out))]], "label"))
  expect_equal(attr(out[[grep("_v37$", names(out))]], "label"), "GDP at current prices")
})

# ---- the periodic check covers it -------------------------------------------------

test_that("check_all_loads.R checks pibmunic, in a year the manifest declares and in which every variable is published", {
  script <- file.path(normalizePath(test_path("..", ".."), mustWork = FALSE), "actions", "scripts", "check_all_loads.R")
  skip_if_not(file.exists(script), "actions/ not present (stripped build)")

  exprs <- parse(script, keep.source = FALSE)
  settings <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("datasets_by_fn", "TIME_PERIOD_BY_FN", "TIME_PERIOD")) eval(e, settings)
  }
  expect_equal(settings$datasets_by_fn$load_pibmunic, "pibmunic")
  year <- settings$TIME_PERIOD_BY_FN$load_pibmunic
  expect_true(year %in% parse_years(datasets_link(source = "pibmunic")$available_time))
  # 2021 is the last year with the whole breakdown (value added by activity, taxes): 2022 and 2023
  # carry GDP and its shares only (resolve_pibmunic.R, point 3), so the check keeps to 2021 to
  # exercise the labels of all the variables
  expect_equal(year, 2021)
})
