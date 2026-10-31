# tests/testthat/test-ppm-load.R
#
# Offline tests for what load_ppm() asks IBGE for and what it calls the result
# (R/ppm.R, and sidra_get_table() in R/download.R). Until 2026-10-22 a PPM row's
# sidra_code was a bare table number, so load_ppm() sent sidra_download() its
# default classific = "all"; sidrar answers that by scraping an IBGE HTML page
# that is behind a bot challenge from some networks, and all five datasets
# failed with "Error in Download.". Now the three tables that have a
# classification say which ("74/c80/all") and the two that have none (94, 95)
# are sent with classific = NULL, which sidra_get_table() answers without the
# page. sidra_download() and sidrar::get_sidra() are mocked (no network).

# dataset -> what the manifest says (checked below), and what sidra_download()
# must therefore be asked
PPM_EXPECTED <- list(
  ppm_animal_origin_production = list(code = "74/c80/all", table = 74, classific = "c80"),
  ppm_aquaculture = list(code = "3940/c654/all", table = 3940, classific = "c654"),
  ppm_livestock_inventory = list(code = "3939/c79/all", table = 3939, classific = "c79"),
  ppm_cow_farming = list(code = "94", table = 94, classific = NULL),
  ppm_sheep_farming = list(code = "95", table = 95, classific = NULL)
)

test_that("the manifest says what these tests assume", {
  committed <- link_table()[link_table()$survey == "ppm", ]
  expect_equal(nrow(committed), 5)
  for (dataset in names(PPM_EXPECTED)) {
    expect_equal(committed$sidra_code[committed$dataset == dataset], PPM_EXPECTED[[dataset]]$code, info = dataset)
  }
})

# ---- what load_ppm() asks sidra_download() for -----------------------------------

test_that("load_ppm() passes each table's classification (or none) and the whole table to sidra_download()", {
  for (dataset in names(PPM_EXPECTED)) {
    seen <- NULL
    testthat::local_mocked_bindings(
      sidra_download = function(...) {
        seen <<- list(...)
        stop("captured")
      },
      .package = "datazoom.amazonia"
    )
    expect_error(
      load_ppm(dataset = dataset, geo_level = "state", time_period = 2023),
      "captured"
    )
    want <- PPM_EXPECTED[[dataset]]
    expect_equal(seen$sidra_code, want$table, info = dataset)
    expect_equal(seen$classific, want$classific, info = dataset)
    expect_equal(seen$category, if (is.null(want$classific)) NULL else list("all"), info = dataset)
    expect_equal(seen$year, "2023", info = dataset)
    expect_equal(seen$geo_level, "state", info = dataset)
  }
})

test_that("a SIDRA table given as a number is the manifest dataset that reads it: the same request as the dataset's name", {
  asked <- function(dataset) {
    seen <- NULL
    testthat::local_mocked_bindings(
      sidra_download = function(...) {
        seen <<- list(...)
        stop("captured")
      },
      .package = "datazoom.amazonia"
    )
    expect_error(load_ppm(dataset = dataset, geo_level = "country", time_period = 2023), "captured")
    seen
  }
  for (dataset in names(PPM_EXPECTED)) {
    by_number <- asked(PPM_EXPECTED[[dataset]]$table)
    # 94 and 95 used to be sent with classific = "all" here -- the request that fails
    # where IBGE's table-description page is challenged
    expect_equal(by_number, asked(dataset), info = dataset)
    expect_equal(by_number$classific, PPM_EXPECTED[[dataset]]$classific, info = dataset)
  }
  expect_null(asked(94)$classific)
  expect_null(asked(95)$classific)
})

test_that("a number the manifest does not know keeps the old request (classific = 'all'), and a bad dataset name still stops in check_params()", {
  seen <- NULL
  testthat::local_mocked_bindings(
    sidra_download = function(...) {
      seen <<- list(...)
      stop("captured")
    },
    .package = "datazoom.amazonia"
  )
  expect_error(load_ppm(dataset = 1613, geo_level = "country", time_period = 2023, raw_data = TRUE), "captured") # a PAM table
  expect_equal(seen$sidra_code, 1613)
  expect_equal(seen$classific, "all")
  expect_equal(seen$category, "all")

  expect_error(load_ppm(dataset = "ppm_nonexistent", geo_level = "country", time_period = 2023), "not supported")
})

test_that("sidra_dataset_of_table() finds the dataset by its table, and only when there is exactly one", {
  expect_equal(sidra_dataset_of_table("ppm", 94), "ppm_cow_farming")
  expect_equal(sidra_dataset_of_table("ppm", "3940"), "ppm_aquaculture")
  expect_equal(sidra_dataset_of_table("ppm", 74), "ppm_animal_origin_production")
  expect_null(sidra_dataset_of_table("ppm", 1613)) # PAM's table
  expect_null(sidra_dataset_of_table("ppm", 9)) # 9 is not a prefix of 94
  expect_null(sidra_dataset_of_table("pam", 94))
  # shared by 39 PAM datasets, but only permanent_crops reads the table whole
  # ("1613/c82/all"), and a table number means that one (test-pam-load.R)
  expect_equal(sidra_dataset_of_table("pam", 1613), "permanent_crops")
})

test_that("sidra_code_parts() splits a manifest code into what sidra_download() takes", {
  expect_equal(sidra_code_parts("74/c80/all"), list(table = 74, classific = "c80", category = list("all")))
  expect_equal(sidra_code_parts("3940/c654/79366"), list(table = 3940, classific = "c654", category = list("79366")))
  parts <- sidra_code_parts("94")
  expect_equal(parts$table, 94)
  expect_null(parts$classific)
  expect_null(parts$category)
  expect_equal(names(parts), c("table", "classific", "category")) # NULL entries are kept, not dropped
})

# ---- what load_ppm() calls the result --------------------------------------------

# Brazil-level result of sidra_download() for each dataset, in sidrar's own
# column order (load_ppm()'s ppm_animal_origin_production pivot selects the
# range variavel:tipo_de_produto_de_origem_animal_codigo, so the order matters).
# valor is the one numeric column. The ids are the ones IBGE's metadata declares
# and sidrar returns for each table (checked live 2026-10-22).
PPM_FAKE <- list(
  ppm_livestock_inventory = list(variables = c("105" = "Efetivo dos rebanhos"), classification = "tipo_de_rebanho", categories = c("2670", "2675")),
  ppm_sheep_farming = list(variables = c("108" = "Ovinos tosquiados nos estabelecimentos agropecuários"), classification = NULL, categories = NULL),
  ppm_animal_origin_production = list(variables = c("106" = "Produção de origem animal", "215" = "Valor da produção"), classification = "tipo_de_produto_de_origem_animal", categories = c("0", "2682")),
  ppm_cow_farming = list(variables = c("107" = "Vacas ordenhadas"), classification = NULL, categories = NULL),
  ppm_aquaculture = list(variables = c("4146" = "Produção da aquicultura", "215" = "Valor da produção"), classification = "tipo_de_produto_da_aquicultura", categories = c("0", "32861", "79366"))
)

fake_value <- function(variable, category) as.numeric(variable) %% 1000 + 7 * (match(category, c("0", "2670", "2675", "2682", "32861", "79366", "(none)")))

fake_ppm <- function(dataset) {
  spec <- PPM_FAKE[[dataset]]
  grid <- expand.grid(variavel_codigo = names(spec$variables), category = spec$categories %||% "(none)", stringsAsFactors = FALSE)
  out <- tibble::tibble(
    nivel_territorial_codigo = "1", nivel_territorial = "Brasil",
    unidade_de_medida_codigo = "1", unidade_de_medida = "Cabeças",
    valor = mapply(fake_value, grid$variavel_codigo, grid$category),
    brasil_codigo = "1", brasil = "Brasil",
    ano_codigo = "2023", ano = "2023",
    variavel_codigo = grid$variavel_codigo, variavel = unname(spec$variables[grid$variavel_codigo])
  )
  if (!is.null(spec$classification)) {
    out[[paste0(spec$classification, "_codigo")]] <- grid$category
    out[[spec$classification]] <- paste("Categoria", grid$category)
  }
  out
}

load_fake <- function(dataset, language) {
  testthat::local_mocked_bindings(
    sidra_download = function(...) fake_ppm(dataset),
    .package = "datazoom.amazonia"
  )
  load_ppm(dataset = dataset, geo_level = "country", time_period = 2023, language = language)
}

PPM_LABELS <- list(
  eng = c("105" = "num", "108" = "num", "107" = "num", "106" = "quant", "4146" = "quant", "215" = "value"),
  pt = c("105" = "num", "108" = "num", "107" = "num", "106" = "quant", "4146" = "quant", "215" = "valor")
)

for (dataset in names(PPM_FAKE)) {
  for (language in c("eng", "pt")) {
    test_that(sprintf("load_ppm('%s', language = '%s') names every variable the table reports, with no na_ column", dataset, language), {
      spec <- PPM_FAKE[[dataset]]
      out <- load_fake(dataset, language)

      # sheep and cow farming have no category: the variable id is the suffix
      suffixes <- if (is.null(spec$categories)) names(spec$variables) else spec$categories
      expected <- as.vector(outer(
        unique(unname(PPM_LABELS[[language]][names(spec$variables)])), suffixes, function(l, s) paste0(l, "_v", s)
      ))
      if (!is.null(spec$categories)) {
        # one column per variable x category
        expected <- paste0(rep(unname(PPM_LABELS[[language]][names(spec$variables)]), times = length(spec$categories)), "_v", rep(spec$categories, each = length(spec$variables)))
      }
      expect_setequal(setdiff(names(out), c("geo_id", if (language == "eng") "year" else "ano")), expected)
      expect_false(any(grepl("^na_", names(out))))
      expect_false(anyDuplicated(names(out)) > 0)
      expect_equal(nrow(out), 1)

      # each value arrives under the name of its variable and category
      for (variable in names(spec$variables)) {
        for (category in spec$categories %||% "(none)") {
          suffix <- if (is.null(spec$categories)) variable else category
          col <- paste0(PPM_LABELS[[language]][[variable]], "_v", suffix)
          expect_equal(as.numeric(out[[col]]), fake_value(variable, category), info = paste(dataset, language, col))
        }
      }
    })
  }
}

# ---- sidra_get_table(): the request for a table with no classification -----------

test_that("with a classification (or 'all') sidra_get_table() is sidrar::get_sidra(), argument for argument", {
  seen <- NULL
  testthat::local_mocked_bindings(
    get_sidra = function(...) {
      seen <<- list(...)
      "frame"
    },
    .package = "sidrar"
  )
  expect_equal(
    sidra_get_table(x = 74, geo = "State", period = "2023", classific = "c80", category = list("all")),
    "frame"
  )
  expect_equal(seen, list(x = 74, geo = "State", period = "2023", geo.filter = NULL, classific = "c80", category = list("all")))

  sidra_get_table(x = 3939, geo = "City", period = "2023", geo.filter = list(State = 15), classific = "all", category = "all")
  expect_equal(seen$classific, "all")
  expect_equal(seen$geo.filter, list(State = 15))
  expect_null(seen$api)
})

test_that("with classific = NULL it asks for the path sidrar would have built, through get_sidra(api = )", {
  seen <- NULL
  testthat::local_mocked_bindings(
    get_sidra = function(...) {
      seen <<- list(...)
      "frame"
    },
    .package = "sidrar"
  )
  request <- function(...) {
    sidra_get_table(..., classific = NULL, category = NULL)
    seen$api
  }
  tail <- "/p/2023/v/allxp/f/a/h/y/d/s"

  # every level, as sidrar's own path for it (checked against sidrar on 2026-10-22)
  expect_equal(request(x = 94, geo = "Brazil", period = "2023"), paste0("/t/94/n1/1", tail))
  expect_equal(request(x = 94, geo = "Region", period = "2023"), paste0("/t/94/n2/all", tail))
  expect_equal(request(x = 95, geo = "State", period = "2023"), paste0("/t/95/n3/all", tail))
  expect_equal(request(x = 95, geo = "City", period = "2023"), paste0("/t/95/n6/all", tail))
  # the municipality passes: a state, then the meso- and micro-regions of the states that failed
  expect_equal(request(x = 94, geo = "City", period = "2023", geo.filter = list(State = 15)), paste0("/t/94/n6/in%20n3%2015", tail))
  expect_equal(request(x = 94, geo = "City", period = "2023", geo.filter = list(MesoRegion = 1501)), paste0("/t/94/n6/in%20n8%201501", tail))
  expect_equal(request(x = 94, geo = "City", period = "2023", geo.filter = list(MicroRegion = 15001)), paste0("/t/94/n6/in%20n9%2015001", tail))
  # nothing but api reaches sidrar on this route
  expect_equal(names(seen), "api")
})

test_that("sidra_download() takes the no-classification route for classific = NULL and keeps its other behaviour", {
  seen <- character(0)
  testthat::local_mocked_bindings(
    get_sidra = function(...) {
      args <- list(...)
      seen <<- c(seen, if (is.null(args$api)) "classified route" else args$api)
      data.frame(Valor = 15563445, check.names = FALSE)
    },
    .package = "sidrar"
  )

  out <- suppressMessages(sidra_download(sidra_code = 94, classific = NULL, category = NULL, year = 2023, geo_level = "country"))
  expect_equal(seen, "/t/94/n1/1/p/2023/v/allxp/f/a/h/y/d/s")
  expect_equal(out$valor, 15563445)

  seen <- character(0)
  suppressMessages(sidra_download(sidra_code = 74, classific = "c80", category = list("all"), year = 2023, geo_level = "state"))
  expect_equal(seen, "classified route")

  # a failed request is still "Error in Download." (sidrar's reason is not shown)
  testthat::local_mocked_bindings(get_sidra = function(...) stop("HTTP error 403."), .package = "sidrar")
  expect_error(
    suppressMessages(sidra_download(sidra_code = 94, classific = NULL, category = NULL, year = 2023, geo_level = "state")),
    "Error in Download."
  )
})

# ---- numeric and named calls are one call ---------------------------------------

test_that("load_ppm(dataset = <table number>) returns exactly what load_ppm(dataset = <name>) does, treated and raw", {
  for (dataset in names(PPM_EXPECTED)) {
    testthat::local_mocked_bindings(
      sidra_download = function(...) fake_ppm(dataset),
      .package = "datazoom.amazonia"
    )
    for (language in c("eng", "pt")) {
      for (raw in c(FALSE, TRUE)) {
        by_name <- load_ppm(dataset = dataset, geo_level = "country", time_period = 2023, language = language, raw_data = raw)
        by_number <- load_ppm(dataset = PPM_EXPECTED[[dataset]]$table, geo_level = "country", time_period = 2023, language = language, raw_data = raw)
        expect_identical(by_number, by_name, info = paste(dataset, language, raw))
      }
    }
  }
})

# ---- the dictionary labels what load_ppm() returns -------------------------------

for (dataset in names(PPM_FAKE)) {
  test_that(sprintf("every column load_ppm('%s') returns carries a label, the Total's aside", dataset), {
    out <- load_fake(dataset, "eng")
    data_cols <- setdiff(names(out), c("geo_id", "year"))
    data_cols <- data_cols[!grepl("_v0$", data_cols)] # the Total (code 0) is not labelled by load_ppm() (unlike load_pam())
    expect_gt(length(data_cols), 0)
    for (col in data_cols) {
      label <- attr(out[[col]], "label")
      expect_true(length(label) == 1 && nzchar(label), info = paste(dataset, col))
    }
  })
}

test_that("the aquaculture category IBGE added in 2024 (79366, 'Peixes') comes back labelled", {
  out <- load_fake("ppm_aquaculture", "eng")
  expect_equal(attr(out$quant_v79366, "label"), "Fish")
  expect_equal(attr(out$value_v79366, "label"), "Fish")
  out_pt <- load_fake("ppm_aquaculture", "pt")
  expect_equal(attr(out_pt$quant_v79366, "label"), "Peixes")
})

# ---- the periodic check covers every PPM branch ----------------------------------

test_that("check_all_loads.R checks all five PPM datasets, and the municipality branch of the no-classification route", {
  script <- file.path(normalizePath(test_path("..", ".."), mustWork = FALSE), "actions", "scripts", "check_all_loads.R")
  skip_if_not(file.exists(script), "actions/ not present (stripped build)")

  # evaluate only the two tables of settings, not the script (it loads the package and downloads)
  exprs <- parse(script, keep.source = FALSE)
  settings <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("datasets_by_fn", "GEO_LEVEL_BY_DS", "GEO_LEVEL", "TIME_PERIOD")) eval(e, settings)
  }
  committed <- link_table()[link_table()$survey == "ppm", ]
  expect_setequal(settings$datasets_by_fn$load_ppm, committed$dataset)

  # a table with no classification takes sidra_get_table()'s route; at state level
  # table 95 covers it, and table 94 is checked at municipality level, which is the
  # route's other branch (n6/in n3 <uf>)
  no_classification <- committed$dataset[!grepl("/", committed$sidra_code, fixed = TRUE)]
  expect_setequal(no_classification, c("ppm_cow_farming", "ppm_sheep_farming"))
  level_of <- function(ds) if (is.null(settings$GEO_LEVEL_BY_DS[[ds]])) settings$GEO_LEVEL else settings$GEO_LEVEL_BY_DS[[ds]]
  expect_setequal(vapply(no_classification, level_of, ""), c("state", "municipality"))
  expect_equal(level_of("ppm_cow_farming"), "municipality")
})
