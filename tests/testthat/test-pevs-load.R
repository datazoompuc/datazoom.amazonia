# tests/testthat/test-pevs-load.R
#
# Offline tests for what load_pevs() asks IBGE for and what it calls the result (R/pevs.R).
# Until 2026-10-31 a PEVS row's sidra_code was a bare table number, so load_pevs() sent
# sidra_download() its default classific = "all"; sidrar answers that by scraping an IBGE
# HTML page that is behind a bot challenge from some networks, and all three datasets failed
# there with "Error in Download.". Now each row says which classification ("289/c193/all").
# The variables are named from the "<dataset>_variables" entries of load_dictionary() (they
# came from two case_when() blocks with no default), and a column is labelled by the exact
# category code at the end of its name (it was matched anywhere in the name, which gave the
# 2025 categories 83410-83419 the labels of 3410-3419). sidra_download() is mocked (no
# network); there was no test of load_pevs() before.

# dataset -> what the manifest says (checked below), and what sidra_download() must
# therefore be asked
PEVS_EXPECTED <- list(
  pevs_forest_crops = list(code = "289/c193/all", table = 289, classific = "c193"),
  pevs_silviculture = list(code = "291/c194/all", table = 291, classific = "c194"),
  pevs_silviculture_area = list(code = "5930/c734/all", table = 5930, classific = "c734")
)

captured <- function(...) {
  seen <- list()
  testthat::local_mocked_bindings(
    sidra_download = function(...) {
      seen[[length(seen) + 1]] <<- list(...)
      stop("captured")
    },
    .package = "datazoom.amazonia"
  )
  suppressWarnings(try(load_pevs(...), silent = TRUE))
  seen
}

test_that("the manifest says what these tests assume: three datasets, each table with its classification spelled out", {
  committed <- link_table()[link_table()$survey == "pevs", ]
  expect_equal(nrow(committed), 3)
  for (dataset in names(PEVS_EXPECTED)) {
    expect_equal(committed$sidra_code[committed$dataset == dataset], PEVS_EXPECTED[[dataset]]$code, info = dataset)
  }
})

# ---- what load_pevs() asks sidra_download() for -----------------------------------

test_that("load_pevs() passes each table's classification, and every category of it, to sidra_download()", {
  for (dataset in names(PEVS_EXPECTED)) {
    calls <- captured(dataset = dataset, geo_level = "state", time_period = 2023)
    want <- PEVS_EXPECTED[[dataset]]
    expect_equal(calls[[1]]$sidra_code, want$table, info = dataset)
    expect_equal(calls[[1]]$classific, want$classific, info = dataset)
    expect_equal(calls[[1]]$category, list("all"), info = dataset)
    expect_equal(calls[[1]]$year, "2023", info = dataset)
    expect_equal(calls[[1]]$geo_level, "state", info = dataset)
  }
})

test_that("a SIDRA table given as a number is the manifest dataset that reads it: the same request as the dataset's name", {
  for (dataset in names(PEVS_EXPECTED)) {
    by_name <- captured(dataset = dataset, geo_level = "country", time_period = 2023)
    by_number <- captured(dataset = PEVS_EXPECTED[[dataset]]$table, geo_level = "country", time_period = 2023)
    expect_equal(by_number, by_name, info = dataset)
    expect_equal(by_number[[1]]$classific, PEVS_EXPECTED[[dataset]]$classific, info = dataset)
    expect_equal(sidra_dataset_of_table("pevs", PEVS_EXPECTED[[dataset]]$table), dataset, info = dataset)
  }
  expect_null(sidra_dataset_of_table("pevs", 1613)) # PAM's table
  expect_null(sidra_dataset_of_table("pevs", 28)) # 28 is not a prefix of 289
})

test_that("a number the manifest does not know keeps the old request (classific = 'all'), and a bad dataset name still stops in check_params()", {
  other <- captured(dataset = 1613, geo_level = "country", time_period = 2023, raw_data = TRUE) # a PAM table
  expect_equal(other[[1]]$sidra_code, 1613)
  expect_equal(other[[1]]$classific, "all")
  expect_equal(other[[1]]$category, "all")

  expect_error(load_pevs(dataset = "pevs_nonexistent", geo_level = "country", time_period = 2023), "not supported")
})

test_that("time_period is checked against the manifest's available_time: 2025 is in it, 2026 and 1985 are not, and the area table starts in 2013", {
  testthat::local_mocked_bindings(sidra_download = function(...) stop("captured"), .package = "datazoom.amazonia")
  expect_no_warning(try(load_pevs("pevs_forest_crops", geo_level = "state", time_period = 2025), silent = TRUE))
  expect_warning(try(load_pevs("pevs_forest_crops", geo_level = "state", time_period = 2026), silent = TRUE), "time_period must be in 1986-2025", fixed = TRUE)
  expect_warning(try(load_pevs("pevs_silviculture", geo_level = "state", time_period = 1985), silent = TRUE), "time_period must be in 1986-2025", fixed = TRUE)
  expect_warning(try(load_pevs("pevs_silviculture_area", geo_level = "state", time_period = 2012), silent = TRUE), "time_period must be in 2013-2025", fixed = TRUE)
})

# ---- what load_pevs() calls the result ----------------------------------------------

# sidra_download()'s answer for each table at each level, in sidrar's own column order
# (the pivot selects the range variavel:tipo_de_produto_codigo once the classification
# columns are renamed, so the order matters; checked live 2026-10-31). The categories include
# the pairs that broke the labelling -- 3410 and 83410, 3419 and 83419 -- and the Total (0).
PEVS_FAKE <- list(
  pevs_forest_crops = list(
    class_col = "tipo_de_produto_extrativo",
    variables = c("144" = "Quantidade produzida na extracao vegetal", "145" = "Valor da producao na extracao vegetal"),
    categories = c("0", "3402", "3410", "83410", "3419", "83419")
  ),
  pevs_silviculture = list(
    class_col = "tipo_de_produto_da_silvicultura",
    variables = c("142" = "Quantidade produzida na silvicultura", "143" = "Valor da producao na silvicultura"),
    categories = c("0", "3455", "33247", "83420")
  ),
  pevs_silviculture_area = list(
    class_col = "especie_florestal",
    variables = c("6549" = "Area total existente em 31/12 dos efetivos da silvicultura"),
    categories = c("0", "39326", "83436")
  )
)
GEO_COLUMNS <- list(
  country = c("brasil_codigo", "brasil"),
  region = c("grande_regiao_codigo", "grande_regiao"),
  state = c("unidade_da_federacao_codigo", "unidade_da_federacao"),
  municipality = c("municipio_codigo", "municipio")
)
GEO_CODES <- list(country = "1", region = c("1", "2"), state = c("11", "12"), municipality = c("1100015", "1100023"))

# the quantity of the Total is not published (".." in IBGE's table: NA after sidrar)
fake_value <- function(variable, category, geo) {
  if (category == "0" && variable %in% c("144", "142")) {
    return(NA_real_)
  }
  as.numeric(variable) + as.numeric(category) / 100000 + as.numeric(geo) / 1e9
}

fake_pevs <- function(dataset, level = "state", variables = NULL, extra_variable = NULL, categories = NULL, year = "2025") {
  spec <- PEVS_FAKE[[dataset]]
  vars <- spec$variables
  if (!is.null(variables)) vars <- vars[names(vars) %in% variables]
  if (!is.null(extra_variable)) vars <- c(vars, stats::setNames("Variavel nova", extra_variable))
  grid <- expand.grid(
    geo = GEO_CODES[[level]], variavel_codigo = names(vars), category = categories %||% spec$categories,
    stringsAsFactors = FALSE
  )
  out <- tibble::tibble(
    nivel_territorial_codigo = "3", nivel_territorial = "Unidade da Federação",
    unidade_de_medida_codigo = "40", unidade_de_medida = "Toneladas",
    valor = mapply(fake_value, grid$variavel_codigo, grid$category, grid$geo)
  )
  out[[GEO_COLUMNS[[level]][1]]] <- grid$geo
  out[[GEO_COLUMNS[[level]][2]]] <- if (level == "country") "Brasil" else paste("Lugar", grid$geo)
  out$ano_codigo <- year
  out$ano <- year
  out$variavel_codigo <- grid$variavel_codigo
  out$variavel <- unname(vars[grid$variavel_codigo])
  out[[paste0(spec$class_col, "_codigo")]] <- grid$category
  out[[spec$class_col]] <- paste("Categoria", grid$category)
  out
}
`%||%` <- function(x, y) if (is.null(x)) y else x

load_fake <- function(dataset, level = "state", language = "eng", fake = fake_pevs(dataset, level), ...) {
  testthat::local_mocked_bindings(sidra_download = function(...) fake, .package = "datazoom.amazonia")
  load_pevs(dataset = dataset, geo_level = level, time_period = 2025, language = language, ...)
}

# the stems load_pevs() gives each dataset's variables, per language
PEVS_STEMS <- list(
  eng = c("144" = "quant", "142" = "quant", "145" = "value", "143" = "value", "6549" = "area"),
  pt = c("144" = "quant", "142" = "quant", "145" = "valor", "143" = "valor", "6549" = "area")
)

for (dataset in names(PEVS_FAKE)) {
  for (language in c("eng", "pt")) {
    test_that(sprintf("load_pevs('%s', language = '%s') names every variable the table reports, with no na_ column", dataset, language), {
      spec <- PEVS_FAKE[[dataset]]
      out <- load_fake(dataset, "state", language)

      expected <- as.vector(outer(
        unname(PEVS_STEMS[[language]][names(spec$variables)]), spec$categories, function(stem, ct) paste0(stem, "_v", ct)
      ))
      # the Total has no quantity: IBGE does not publish it, so there is no such column
      expected <- setdiff(expected, c("quant_v0"))
      expect_setequal(setdiff(names(out), c("geo_id", if (language == "eng") "year" else "ano")), expected)
      expect_false(any(grepl("^na_", names(out))))
      expect_false(anyDuplicated(names(out)) > 0)
      expect_equal(nrow(out), 2)
      expect_equal(out$geo_id, GEO_CODES$state)

      # each value arrives under the name of its variable and category
      for (variable in names(spec$variables)) {
        for (category in spec$categories) {
          col <- paste0(PEVS_STEMS[[language]][[variable]], "_v", category)
          if (!(col %in% names(out))) next
          expect_equal(
            as.numeric(out[[col]]), vapply(GEO_CODES$state, function(g) fake_value(variable, category, g), numeric(1)),
            ignore_attr = TRUE, info = paste(dataset, language, col)
          )
        }
      }
    })
  }
}

for (level in c("country", "region", "state", "municipality")) {
  test_that(sprintf("load_pevs() at the %s level: one row per place, the geography as geo_id, no raw geography columns left", level), {
    out <- load_fake("pevs_silviculture", level)
    codes <- GEO_CODES[[level]]
    # the state and the municipality are identified by IBGE's code; the country and the region by the name
    # IBGE gives them, as they always have been
    expect_equal(out$geo_id, switch(level, country = "Brasil", region = paste("Lugar", codes), codes))
    expect_equal(nrow(out), length(codes))
    expect_equal(names(out)[1:2], c("geo_id", "year"))
    dropped <- c(unlist(GEO_COLUMNS), "nivel_territorial", "nivel_territorial_codigo", "unidade_de_medida", "unidade_de_medida_codigo", "ano_codigo", "valor")
    expect_false(any(names(out) %in% dropped))
  })
}

test_that("the Portuguese names: the year is 'ano', the value 'valor'", {
  out <- load_fake("pevs_forest_crops", language = "pt")
  expect_equal(names(out)[1:2], c("geo_id", "ano"))
  expect_true("valor_v3402" %in% names(out))
  expect_false(any(c("year", "value_v3402") %in% names(out)))
})

test_that("raw data is what sidra_download() returned, untreated", {
  fake <- fake_pevs("pevs_forest_crops")
  out <- load_fake("pevs_forest_crops", fake = fake, raw_data = TRUE)
  expect_equal(out, tibble::as_tibble(fake))
  expect_true("tipo_de_produto_extrativo_codigo" %in% names(out))
})

# ---- an id nobody named ----------------------------------------------------------------

test_that("a variable IBGE adds comes back as v<id>_v<category> with a warning that names it, never as an NA name", {
  fake <- fake_pevs("pevs_silviculture", extra_variable = "9999")
  expect_warning(
    out <- load_fake("pevs_silviculture", fake = fake),
    "IBGE variable(s) with no name in load_pevs(): 9999 (Variavel nova)", fixed = TRUE
  )
  expect_true("v9999_v3455" %in% names(out))
  expect_false(any(grepl("^na_", names(out))))
  # the named variables keep their names
  expect_true(all(c("quant_v3455", "value_v3455") %in% names(out)))
})

test_that("a variable dictionary exists for every dataset, and names every variable the fake tables report", {
  for (dataset in names(PEVS_FAKE)) {
    dic <- load_dictionary(paste0(dataset, "_variables"))
    expect_setequal(format(dic$var_code, scientific = FALSE, trim = TRUE), names(PEVS_FAKE[[dataset]]$variables))
  }
})

# ---- numeric and named calls are one call ----------------------------------------------

test_that("load_pevs(dataset = <table number>) returns exactly what load_pevs(dataset = <name>) does, treated and raw", {
  for (dataset in names(PEVS_EXPECTED)) {
    testthat::local_mocked_bindings(sidra_download = function(...) fake_pevs(dataset), .package = "datazoom.amazonia")
    for (language in c("eng", "pt")) {
      for (raw in c(FALSE, TRUE)) {
        by_name <- load_pevs(dataset = dataset, geo_level = "state", time_period = 2025, language = language, raw_data = raw)
        by_number <- load_pevs(dataset = PEVS_EXPECTED[[dataset]]$table, geo_level = "state", time_period = 2025, language = language, raw_data = raw)
        expect_identical(by_number, by_name, info = paste(dataset, language, raw))
      }
    }
  }
})

# ---- the dictionary labels what load_pevs() returns ---------------------------------------

DIC <- function(dataset) as.data.frame(load_dictionary(dataset))
label_of <- function(out, column) attr(out[[column]], "label")

test_that("every column load_pevs() returns carries the label of ITS category, in the language asked for", {
  for (dataset in names(PEVS_FAKE)) {
    spec <- PEVS_FAKE[[dataset]]
    dic <- DIC(dataset)
    for (language in c("eng", "pt")) {
      out <- load_fake(dataset, "state", language)
      for (column in setdiff(names(out), c("year", "ano", "geo_id"))) {
        category <- sub("^.*_v", "", column)
        if (category == "0") next # the Total is left unlabelled on purpose
        want <- dic[[if (language == "eng") "var_eng" else "var_pt"]][match(as.numeric(category), dic$var_code)]
        expect_equal(label_of(out, column), want, info = paste(dataset, language, column))
      }
    }
  }
})

test_that("the 2025 categories whose ids contain older ids are labelled with their own names, not the older ones' (83410 is not 3410)", {
  out <- load_fake("pevs_forest_crops")
  forest <- DIC("pevs_forest_crops")
  expect_equal(label_of(out, "quant_v3410"), forest$var_eng[forest$var_code == 3410])
  expect_equal(label_of(out, "quant_v83410"), "Baru or Cumaru (almond)")
  expect_equal(label_of(out, "quant_v83419"), "Murumuru")
  expect_equal(label_of(out, "value_v83419"), "Murumuru")
  expect_false(identical(label_of(out, "quant_v83410"), label_of(out, "quant_v3410")))
  expect_false(identical(label_of(out, "quant_v83419"), label_of(out, "quant_v3419")))

  # even a category that has NO row in the dictionary must not borrow the label of the id it contains
  fake <- fake_pevs("pevs_forest_crops", categories = c("3410", "93410"))
  out <- load_fake("pevs_forest_crops", fake = fake)
  expect_equal(label_of(out, "quant_v3410"), forest$var_eng[forest$var_code == 3410])
  expect_null(label_of(out, "quant_v93410"))
})

test_that("every category of the three dictionaries is labelled when the table returns it", {
  for (dataset in names(PEVS_FAKE)) {
    dic <- DIC(dataset)
    ids <- as.character(dic$var_code[dic$var_code != 0])
    out <- load_fake(dataset, "state", "eng", fake = fake_pevs(dataset, categories = ids))
    data_cols <- setdiff(names(out), c("year", "geo_id"))
    data_cols <- data_cols[!grepl("_v0$", data_cols)]
    expect_gte(length(data_cols), length(ids))
    for (col in data_cols) {
      label <- label_of(out, col)
      expect_true(length(label) == 1 && nzchar(label), info = paste(dataset, col))
    }
  }
})

# ---- the periodic check covers every PEVS dataset ------------------------------------------

test_that("check_all_loads.R checks all three PEVS datasets, in a year each of them declares", {
  script <- file.path(normalizePath(test_path("..", ".."), mustWork = FALSE), "actions", "scripts", "check_all_loads.R")
  skip_if_not(file.exists(script), "actions/ not present (stripped build)")

  exprs <- parse(script, keep.source = FALSE)
  settings <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("datasets_by_fn", "TIME_PERIOD_BY_FN", "TIME_PERIOD")) eval(e, settings)
  }
  committed <- link_table()[link_table()$survey == "pevs", ]
  expect_setequal(settings$datasets_by_fn$load_pevs, committed$dataset)

  # no per-function year for pevs: every dataset is checked in the default year, which each declares
  year <- settings$TIME_PERIOD_BY_FN$load_pevs %||% settings$TIME_PERIOD
  for (dataset in committed$dataset) {
    expect_true(year %in% parse_years(committed$available_time[committed$dataset == dataset]), info = dataset)
  }
})
