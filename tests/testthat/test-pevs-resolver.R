# tests/testthat/test-pevs-resolver.R
#
# Offline test for actions/scrapers/resolve_pevs.R (added 2026-10-31; see that file's
# header for what was measured live). PEVS's 3 manifest rows are read from IBGE's SIDRA
# API, not from a file: each row's `sidra_code` ("289/c193/all") names a table, its one
# classification and "all of its categories", and what goes stale is available_time, the
# classification a bare or "all" code would silently lose (HTTP 200 with only the "Total"
# slice, or a page behind a bot challenge), the variable ids load_pevs() names, and the
# rows of R/dictionary.R that label the categories. The resolver is a thin layer on
# actions/scripts/sidra_api.R (tested for itself in test-sidra-api.R).
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was measured
# live on 2026-10-31: /metadados is a JSON description with one classification and the
# table's variables (plus a "... - percentual do total geral" twin that sidrar's "allxp"
# never requests); the coverage query is a JSON array whose first element is a header row,
# one row per category and year, "..." where nothing is published, and category 0
# ("Total") has a value but no quantity.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
# the metadata lookup, coverage query and value coding PEVS shares with the other SIDRA
# resolvers, as build_manifest.R sources it before the resolvers
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_pevs.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed rows (3 of them), not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "pevs", ]

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- a fake IBGE ------------------------------------------------------------

# table -> dataset, classification id, the variables it declares (value first), the span
FAKE_TABLES <- list(
  `289` = list(dataset = "pevs_forest_crops", cl = "193", vars = c("144", "145"), span = 1986:2025),
  `291` = list(dataset = "pevs_silviculture", cl = "194", vars = c("142", "143"), span = 1986:2025),
  `5930` = list(dataset = "pevs_silviculture_area", cl = "734", vars = "6549", span = 2013:2025)
)

# Every category the committed dictionary labels, which is what the healthy IBGE offers;
# `extra` are categories IBGE has and the dictionary has not.
fake_categories <- function(table, extra = character(0)) {
  c(as.character(load_dictionary(FAKE_TABLES[[table]]$dataset)$var_code), extra)
}

# Years IBGE publishes, per (table, category). Default: the table's whole span; the
# ids that only exist since 2025 have 2025 alone.
fake_years <- function(table, category) {
  span <- FAKE_TABLES[[table]]$span
  if (category %in% c("83410", "83420", "83436", "99999")) 2025L else span
}

metadata_json <- function(table, extra = character(0), levels = c("N1", "N2", "N3", "N6", "N8", "N9"),
                          id = as.integer(table), classifications = NULL, extra_variables = character(0),
                          drop_variables = character(0), url = paste0("https://sidra.ibge.gov.br/tabela/", table)) {
  spec <- FAKE_TABLES[[table]]
  cats <- fake_categories(table, extra)
  classifications <- classifications %||% list(list(
    id = as.integer(spec$cl), nome = "Classificacao",
    categorias = lapply(cats, function(ct) list(id = as.integer(ct), nome = paste("categoria", ct), unidade = "Toneladas", nivel = 1L))
  ))
  variables <- lapply(setdiff(c(spec$vars, extra_variables), drop_variables), function(v) list(id = as.integer(v), nome = paste("variavel", v), unidade = "Toneladas"))
  # IBGE declares a "... - percentual do total geral" twin (unit "%") for the value variable;
  # sidrar's "allxp" never requests it and load_pevs() does not name it
  percent <- list(list(id = as.integer(paste0("1000", utils::tail(spec$vars, 1))), nome = "variavel - percentual do total geral", unidade = "%"))
  as.character(jsonlite::toJSON(list(
    id = id, nome = paste("Tabela", table), URL = url,
    pesquisa = "Producao da Extracao Vegetal e da Silvicultura",
    periodicidade = list(frequencia = "anual", inicio = min(spec$span), fim = max(spec$span)),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = c(variables, percent),
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

HEADER_ROW <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano",
  D4C = "Produto (Codigo)", D4N = "Produto"
)

# `years` overrides fake_years(): function(table, category) -> integer years. `variable` is
# the SIDRA variable id asked for; category 0 has a value but no quantity.
coverage_json <- function(table, variable, years = fake_years, extra = character(0)) {
  spec <- FAKE_TABLES[[table]]
  rows <- list(HEADER_ROW)
  for (ct in fake_categories(table, extra)) {
    published <- years(table, ct)
    for (y in spec$span) {
      value <- if (y %in% published && !(ct == "0" && variable %in% c("144", "142"))) as.character(1000L + y) else "..."
      rows[[length(rows) + 1]] <- list(
        NC = "1", NN = "Brasil", MC = "1", MN = "Unidade", V = value, D1C = "1", D1N = "Brasil",
        D2C = variable, D2N = "variavel", D3C = as.character(y), D3N = as.character(y),
        D4C = ct, D4N = paste("categoria", ct)
      )
    }
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}

# `override(kind, table, url)` may return a response (or throw like curl does) to replace the
# healthy one; `kind` is "metadata" or "coverage". `seen` collects the requested URLs.
fake_ibge <- function(override = function(kind, table, url) NULL, seen = NULL,
                      metadata = metadata_json, coverage = coverage_json) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("/agregados/[0-9]+/metadados$", url)) {
      table <- sub("^.*/agregados/([0-9]+)/metadados$", "\\1", url)
      return(override("metadata", table, url) %||% resp(200L, metadata(table)))
    }
    if (grepl("/values/t/[0-9]+/n1/all/v/[0-9]+/p/all/c[0-9]+/all$", url)) {
      table <- sub("^.*/values/t/([0-9]+)/.*$", "\\1", url)
      variable <- sub("^.*/v/([0-9]+)/.*$", "\\1", url)
      return(override("coverage", table, url) %||% resp(200L, coverage(table, variable)))
    }
    stop("the fake IBGE has no such URL: ", url)
  }
}
with_ibge <- function(server, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = server, .package = "curl")
}
run <- function(server = fake_ibge(), rows = manifest_rows, dictionary = load_dictionary) {
  with_ibge(server, suppressMessages(resolve_pevs(rows, dictionary = dictionary)))
}
error_message <- function(server, rows = manifest_rows, dictionary = load_dictionary) {
  tryCatch(
    {
      run(server, rows, dictionary)
      "NO ERROR"
    },
    error = function(e) conditionMessage(e)
  )
}
# the committed rows with one of them given another sidra_code
with_code <- function(dataset, code) {
  rows <- manifest_rows
  rows$sidra_code[rows$dataset == dataset] <- code
  rows
}
failures_of <- function(...) attr(run(...), "partial_failures")

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves all 3 rows, with 2 requests per table (6)", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 3)
  expect_setequal(out$dataset, manifest_rows$dataset)
  expect_true(all(out$survey == "pevs"))
  expect_true(all(out$resolver == "pevs"))
  # unkeyed rows stay unkeyed; the values the resolver owns are available_time and docs_url,
  # never sidra_code
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  expect_false("sidra_code" %in% names(out))
  expect_equal(out$available_time[out$dataset == "pevs_forest_crops"], "1986-2025")
  expect_equal(out$available_time[out$dataset == "pevs_silviculture"], "1986-2025")
  expect_equal(out$available_time[out$dataset == "pevs_silviculture_area"], "2013-2025")
  expect_null(attr(out, "partial_failures"))

  expect_length(seen$urls, 6)
  expect_equal(sum(grepl("/metadados$", seen$urls)), 3)
  expect_equal(sum(grepl("/p/all/c[0-9]+/all$", seen$urls)), 3)
})

test_that("available_time is the union of the categories: one that ends early, or starts in 2025, changes nothing", {
  early <- function(table, category) if (category == "3402") 1986:2010 else fake_years(table, category)
  out <- run(fake_ibge(coverage = function(table, variable) coverage_json(table, variable, years = early)))
  expect_equal(out$available_time[out$dataset == "pevs_forest_crops"], "1986-2025")

  # a whole table that stops earlier does move it, and a gap is written in the grammar parse_years() reads
  gap <- function(table, category) setdiff(FAKE_TABLES[[table]]$span, 2000:2001)
  out <- run(fake_ibge(coverage = function(table, variable) coverage_json(table, variable, years = gap)))
  expect_equal(out$available_time[out$dataset == "pevs_silviculture"], "1986-1999, 2002-2025")
  expect_equal(parse_years(out$available_time[out$dataset == "pevs_silviculture"]), setdiff(1986:2025, 2000:2001))

  ended <- function(table, category) 1986:2024
  out <- run(fake_ibge(coverage = function(table, variable) coverage_json(table, variable, years = ended)))
  expect_equal(out$available_time[out$dataset == "pevs_forest_crops"], "1986-2024")
})

test_that("coverage asks for the value of production (145, 143), the area (6549) for the area table -- never the quantity", {
  seen <- new.env()
  run(fake_ibge(seen = seen))
  expect_true(any(grepl("/t/289/n1/all/v/145/p/all/c193/all$", seen$urls)))
  expect_true(any(grepl("/t/291/n1/all/v/143/p/all/c194/all$", seen$urls)))
  expect_true(any(grepl("/t/5930/n1/all/v/6549/p/all/c734/all$", seen$urls)))
  expect_false(any(grepl("/v/(144|142)/", seen$urls)))
})

test_that("a table that declares none of the variables the coverage needs fails its row, not the others", {
  out <- run(fake_ibge(metadata = function(table) metadata_json(table, drop_variables = if (table == "291") c("142", "143") else character(0))))
  expect_setequal(out$dataset, c("pevs_forest_crops", "pevs_silviculture_area"))
  expect_match(attr(out, "partial_failures"), "declares none of the variables 145/143/6549", fixed = TRUE)
})

test_that("docs_url is each table's own page, taken from the metadata only when it is a SIDRA table page; otherwise the cell is left alone", {
  out <- run(fake_ibge())
  expect_equal(out$docs_url[out$dataset == "pevs_forest_crops"], "https://sidra.ibge.gov.br/tabela/289")
  expect_equal(out$docs_url[out$dataset == "pevs_silviculture"], "https://sidra.ibge.gov.br/tabela/291")
  expect_equal(out$docs_url[out$dataset == "pevs_silviculture_area"], "https://sidra.ibge.gov.br/tabela/5930")

  survey_page <- function(table) metadata_json(table, url = "https://sidra.ibge.gov.br/pesquisa/pevs/tabelas")
  out <- run(fake_ibge(metadata = survey_page))
  expect_true(all(is.na(out$docs_url)))
  expect_equal(nrow(out), 3)
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(grep("^(resolve|watch)_", ls(resolver_env), value = TRUE), "resolve_pevs")
})

# ---- the variables ------------------------------------------------------------

test_that("PEVS_NAMED_VARIABLES is the var_code column of each \"<dataset>_variables\" entry load_pevs() names its variables from", {
  expect_setequal(names(PEVS_NAMED_VARIABLES), manifest_rows$dataset)
  for (dataset in names(PEVS_NAMED_VARIABLES)) {
    dic <- load_dictionary(paste0(dataset, "_variables"))
    expect_equal(PEVS_NAMED_VARIABLES[[dataset]], format(dic$var_code, scientific = FALSE, trim = TRUE), info = dataset)
  }
})

test_that("a variable IBGE adds to a table fails that table's row, naming it; the percentage twin never trips the check", {
  out <- run(fake_ibge(metadata = function(table) metadata_json(table, extra_variables = if (table == "291") "9999" else character(0))))
  expect_setequal(out$dataset, c("pevs_forest_crops", "pevs_silviculture_area"))
  msg <- attr(out, "partial_failures")
  expect_length(msg, 1)
  expect_match(msg, "pevs/pevs_silviculture (291/c194/all)", fixed = TRUE)
  expect_match(msg, "variable(s) 9999", fixed = TRUE)
  expect_match(msg, "pevs_silviculture_variables", fixed = TRUE)

  # (the healthy run above declares 1000145, 1000143 and 1006549 and passes with no failure)
  expect_null(attr(run(fake_ibge()), "partial_failures"))
})

# ---- the dictionary -------------------------------------------------------------

test_that("a category IBGE has and the dictionary lacks is a NOTE: the row is still refreshed, and the note says what to add", {
  server <- fake_ibge(metadata = function(table) metadata_json(table, extra = if (table == "289") "99999" else character(0)),
                      coverage = function(table, variable) coverage_json(table, variable, extra = if (table == "289") "99999" else character(0)))
  out <- run(server)
  expect_equal(nrow(out), 3)
  expect_equal(out$available_time[out$dataset == "pevs_forest_crops"], "1986-2025")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "pevs/pevs_forest_crops (289/c193/all)", fixed = TRUE)
  expect_match(notes, "section has no row for IBGE category 99999 (categoria 99999; IBGE unit: Toneladas)", fixed = TRUE)
  expect_match(notes, "come back without a label", fixed = TRUE)
})

test_that("the Total (category 0) is never reported, and the dictionary rows IBGE dropped are not either", {
  dictionary <- function(dataset) {
    d <- load_dictionary(dataset)
    if (dataset == "pevs_silviculture") d[!(as.character(d$var_code) %in% "0"), ] else d
  }
  expect_null(failures_of(fake_ibge(), dictionary = dictionary))

  # a row in the dictionary that IBGE's classification no longer has
  extra_row <- function(dataset) {
    d <- load_dictionary(dataset)
    if (dataset == "pevs_silviculture") rbind(d, d[1, ]) else d
  }
  expect_null(failures_of(fake_ibge(), dictionary = extra_row))
})

test_that("a row for ONE category only needs that category's label: another category's missing row is not its note", {
  server <- fake_ibge(
    metadata = function(table) metadata_json(table, extra = if (table == "289") "99999" else character(0)),
    coverage = function(table, variable) coverage_json(table, variable, extra = if (table == "289") "99999" else character(0))
  )
  # 99999 has no dictionary row, but the row reads only 83410, which has
  expect_null(failures_of(server, with_code("pevs_forest_crops", "289/c193/83410")))
  # the row that reads 99999 is the one that needs it
  notes <- failures_of(server, with_code("pevs_forest_crops", "289/c193/99999"))
  expect_match(notes, "no row for IBGE category 99999", fixed = TRUE)
})

test_that("the dictionary rows of the 30 categories IBGE added for 2025 are there (the ones whose ids contain older ids included)", {
  ids <- function(dataset) as.character(load_dictionary(dataset)$var_code)
  expect_true(all(as.character(83410:83419) %in% ids("pevs_forest_crops")))
  expect_true(all(as.character(83420:83435) %in% ids("pevs_silviculture")))
  expect_true(all(as.character(83436:83439) %in% ids("pevs_silviculture_area")))
  # ... each with a label of its own in both languages, not the label of the older id it contains
  forest <- as.data.frame(load_dictionary("pevs_forest_crops"))
  old <- forest[forest$var_code == 3410, ]
  new <- forest[forest$var_code == 83410, ]
  expect_false(identical(old$var_eng, new$var_eng))
  expect_false(identical(old$var_pt, new$var_pt))
})

test_that("an unreadable dictionary, or one with no section for the dataset, is a note on the rows, not an error", {
  broken <- function(dataset) stop("cannot be read")
  out <- run(fake_ibge(), dictionary = broken)
  expect_equal(nrow(out), 3)
  expect_length(attr(out, "partial_failures"), 3)
  expect_match(attr(out, "partial_failures")[1], "has no section for pevs_", fixed = TRUE)

  # the default argument is a promise: reading the file is what throws, and the resolver forces it itself
  out <- with_ibge(fake_ibge(), suppressMessages(resolve_pevs(manifest_rows, dictionary = stop("R/dictionary.R is gone"))))
  expect_equal(nrow(out), 3)
  expect_match(attr(out, "partial_failures")[1], "R/dictionary.R could not be read (R/dictionary.R is gone)", fixed = TRUE)
})

test_that("pevs_read_dictionary() reads the shipped R/dictionary.R, and says so when it is not there", {
  read <- pevs_read_dictionary(root = pkg_root)
  expect_true(is.function(read))
  for (dataset in c("pevs_forest_crops", "pevs_silviculture", "pevs_silviculture_area", "pevs_forest_crops_variables")) {
    expect_equal(read(dataset), load_dictionary(dataset), info = dataset)
  }
  expect_error(pevs_read_dictionary(root = file.path(tempdir(), "nowhere")), "there is no")
})

# ---- the code's grammar, both ways -------------------------------------------------

test_that("a bare code fails the row, and the message gives the code to write", {
  rows <- with_code("pevs_forest_crops", "289")
  msg <- error_message(fake_ibge(), rows[rows$dataset == "pevs_forest_crops", ])
  expect_match(msg, "names no classification, but table 289 has one (c193)", fixed = TRUE)
  expect_match(msg, "'Total' slice", fixed = TRUE)
  expect_match(msg, "write '289/c193/all'", fixed = TRUE)
})

test_that("classific = 'all' is refused with the id to use, and the code is not rewritten", {
  rows <- with_code("pevs_silviculture", "291/all/all")
  out <- run(fake_ibge(), rows)
  expect_equal(nrow(out), 2) # the other two rows are fine
  msg <- attr(out, "partial_failures")
  expect_match(msg, "uses classific = 'all'", fixed = TRUE)
  expect_match(msg, "write '291/c194/all' instead", fixed = TRUE)
})

test_that("a classification that is not the table's fails the row, naming the table's", {
  msg <- attr(run(fake_ibge(), with_code("pevs_silviculture_area", "5930/c99/all")), "partial_failures")
  expect_match(msg, "classification c99 is not the one table 5930 has (c734)", fixed = TRUE)
})

test_that("a category IBGE no longer lists fails ONLY its row, and says why", {
  msg <- attr(run(fake_ibge(), with_code("pevs_forest_crops", "289/c193/55555")), "partial_failures")
  expect_match(msg, "category 55555 does not exist in classification c193 of table 289", fixed = TRUE)
  expect_match(msg, "HTTP 200", fixed = TRUE)

  # one that exists, alone, resolves to its own span
  out <- run(fake_ibge(), with_code("pevs_forest_crops", "289/c193/83410"))
  expect_equal(out$available_time[out$dataset == "pevs_forest_crops"], "2025")
})

test_that("a category that exists but has no published year at Brazil level fails its row", {
  none <- function(table, category) integer(0)
  msg <- error_message(fake_ibge(coverage = function(table, variable) coverage_json(table, variable, years = none)))
  expect_match(msg, "has no year with a published value at Brazil level", fixed = TRUE)
})

test_that("a sidra_code that is not <table>/c<n>/<id or all> fails its row", {
  for (code in c("289/c193", "abc", "289/c193/all/extra", "289/x193/all")) {
    out <- run(fake_ibge(), with_code("pevs_silviculture", code))
    expect_equal(nrow(out), 2, info = code) # the other two rows are fine
    expect_match(
      attr(out, "partial_failures"), "is not <table>/c<classification>/<category id or all>",
      fixed = TRUE, info = code
    )
  }
})

test_that("a table with no classification, or with more than one, is refused (every PEVS table has exactly one; the coverage query has one path)", {
  none <- function(table) metadata_json(table, classifications = list())
  expect_match(error_message(fake_ibge(metadata = none)), "has no classification, and this resolver expects one", fixed = TRUE)

  two <- function(table) {
    cl <- list(
      list(id = 1L, nome = "um", categorias = list(list(id = 0L, nome = "Total", unidade = NULL, nivel = 1L))),
      list(id = 2L, nome = "dois", categorias = list(list(id = 0L, nome = "Total", unidade = NULL, nivel = 1L)))
    )
    metadata_json(table, classifications = cl)
  }
  expect_match(error_message(fake_ibge(metadata = two)), "has 2 classifications", fixed = TRUE)
})

# ---- the territorial levels --------------------------------------------------------

test_that("an available_geo level the table's metadata does not have fails the row; an unknown word does too; NA is skipped", {
  no_region <- function(table) metadata_json(table, levels = c("N1", "N3", "N6"))
  expect_match(error_message(fake_ibge(metadata = no_region)), "no territorial level N2", fixed = TRUE)

  rows <- manifest_rows
  rows$available_geo[1] <- "country, galaxy"
  expect_match(attr(run(fake_ibge(), rows), "partial_failures"), "'galaxy'", fixed = TRUE)
  rows$available_geo[1] <- NA_character_
  expect_null(attr(run(fake_ibge(), rows), "partial_failures"))
})

# ---- the table itself ----------------------------------------------------------------

test_that("an unreadable table fails ITS row, with the challenge page's own words, and leaves the other tables alone", {
  challenge <- resp(403L, "<html><head><title>Just a moment...</title></head><body>Checking</body></html>")
  out <- run(fake_ibge(override = function(kind, table, url) if (kind == "metadata" && table == "291") challenge else NULL))
  expect_setequal(out$dataset, c("pevs_forest_crops", "pevs_silviculture_area"))
  msg <- attr(run(fake_ibge(override = function(kind, table, url) if (kind == "metadata" && table == "291") challenge else NULL)), "partial_failures")
  expect_length(msg, 1)
  expect_match(msg, "pevs/pevs_silviculture (291/c194/all)", fixed = TRUE)
  expect_match(msg, "HTTP 403", fixed = TRUE)
  expect_match(msg, "Just a moment", fixed = TRUE)
})

test_that("a transport error is a reason, not an exception, and names the URL", {
  out <- run(fake_ibge(override = function(kind, table, url) if (kind == "coverage" && table == "5930") stop("Could not resolve host") else NULL))
  expect_equal(nrow(out), 2)
  msg <- attr(out, "partial_failures")
  expect_match(msg, "Could not resolve host", fixed = TRUE)
  expect_match(msg, "/t/5930/n1/all/v/6549/p/all/c734/all", fixed = TRUE)
})

test_that("each way a table's answer can be wrong is a reason of its own", {
  msg <- function(...) error_message(fake_ibge(...), manifest_rows[1, ])
  expect_match(msg(metadata = function(table) "not json"), "not a SIDRA table description", fixed = TRUE)
  expect_match(msg(metadata = function(table) metadata_json(table, id = 5931L)), "the description is for table 5931, not 289", fixed = TRUE)
  expect_match(msg(coverage = function(table, variable) "<html>Just a moment</html>"), "not a SIDRA value table", fixed = TRUE)
})

test_that("if EVERY row fails the resolver stops, naming the reasons", {
  msg <- error_message(fake_ibge(override = function(kind, table, url) if (kind == "metadata") resp(500L, "boom") else NULL))
  expect_match(msg, "all 3 rows failed verification", fixed = TRUE)
  expect_match(msg, "HTTP 500", fixed = TRUE)
})

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_pevs(manifest_rows[0, ]), "no manifest rows tagged resolver == 'pevs'")
  expect_error(resolve_pevs(NULL), "no manifest rows tagged resolver == 'pevs'")
  keyed <- manifest_rows
  keyed$geo_level[1] <- "state"
  expect_error(resolve_pevs(keyed), "every PEVS row is unkeyed")
  no_code <- manifest_rows
  no_code$sidra_code[2] <- NA_character_
  expect_error(resolve_pevs(no_code), "needs a sidra_code")
  unknown <- manifest_rows
  unknown$dataset[1] <- "pevs_mystery"
  msg <- attr(run(fake_ibge(), unknown), "partial_failures")
  expect_match(msg, "dataset pevs_mystery has no entry in PEVS_NAMED_VARIABLES", fixed = TRUE)
})

# ---- the code's grammar, and the committed rows -----------------------------------------------

test_that("pevs_parse_code reads <table>/c<n>/<category> and a bare table, and rejects everything else", {
  expect_equal(pevs_parse_code("289/c193/all"), list(table = "289", classific = "c193", category = "all"))
  expect_equal(pevs_parse_code("289/c193/83410")$category, "83410")
  expect_equal(pevs_parse_code("289"), list(table = "289", classific = NA_character_, category = NA_character_))
  expect_equal(pevs_parse_code("289/all/all")$classific, "all") # parses, so that it can be refused with a reason
  for (bad in c("", "abc", "289/c193", "289/c193/", "/c193/all", "289/x193/all", NA_character_)) {
    expect_null(pevs_parse_code(bad), info = bad)
  }
  expect_null(pevs_parse_code(NULL))
})

test_that("the package's sidra_code_parts() (what load_pevs() uses) reads every committed code the way this resolver does", {
  for (code in manifest_rows$sidra_code) {
    mine <- pevs_parse_code(code)
    theirs <- sidra_code_parts(code)
    expect_equal(theirs$table, as.numeric(mine$table), info = code)
    expect_equal(theirs$classific, mine$classific, info = code)
    expect_equal(theirs$category, list(mine$category), info = code)
  }
})

test_that("every committed PEVS row is verifiable: owned by the resolver, explicit classification, known geo levels, a table page for docs_url", {
  expect_equal(nrow(manifest_rows), 3)
  expect_true(all(manifest_rows$resolver == "pevs"))
  expect_equal(sort(manifest_rows$sidra_code), sort(c("289/c193/all", "291/c194/all", "5930/c734/all")))
  expect_true(all(is.na(manifest_rows$geo_level)) && all(is.na(manifest_rows$year)))
  for (geo in manifest_rows$available_geo) {
    expect_true(all(trimws(strsplit(geo, ",")[[1]]) %in% names(SIDRA_API_GEO_LEVELS)))
  }
  expect_true(all(grepl(SIDRA_API_TABLE_PAGE, manifest_rows$docs_url)))
  expect_true(all(is.na(manifest_rows$url)))
  expect_setequal(manifest_rows$dataset, names(PEVS_NAMED_VARIABLES))
})

test_that("R/pevs.R hardcodes every table id the manifest uses (its FRAGILE tag says the two must agree)", {
  src <- paste(readLines(file.path(pkg_root, "R", "pevs.R"), warn = FALSE), collapse = "\n")
  for (code in manifest_rows$sidra_code) {
    table <- pevs_parse_code(code)$table
    expect_true(grepl(sprintf("param\\$code == %s\\b", table), src), info = table)
  }
})

test_that("a changed pevs row lists the FRAGILE notes of the three files that treat it (build_manifest.R reads FRAGILITY_SOURCES)", {
  frag_env <- new.env()
  source(file.path(actions_dir, "scripts", "fragility_notes.R"), local = frag_env)
  entries <- Filter(function(e) identical(e$changed_prefix, "pevs"), frag_env$FRAGILITY_SOURCES)
  expect_setequal(vapply(entries, function(e) e$label, ""), c("R/pevs.R", "R/download.R", "R/dictionary.R"))
  for (e in entries) {
    expect_true(file.exists(file.path(pkg_root, e$r_path)), info = e$label)
    expect_gt(length(frag_env$collect_fragility_notes(file.path(pkg_root, e$r_path), e$label)), 0)
  }
})

test_that("the resolver run on the committed rows and a healthy IBGE changes nothing the manifest already says", {
  out <- run(fake_ibge())
  out <- out[match(manifest_rows$dataset, out$dataset), ]
  expect_equal(out$available_time, manifest_rows$available_time)
  expect_equal(out$docs_url, manifest_rows$docs_url)
})
