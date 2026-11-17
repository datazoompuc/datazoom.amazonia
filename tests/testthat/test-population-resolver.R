# tests/testthat/test-population-resolver.R
#
# Offline test for actions/scrapers/resolve_population.R (added 2026-11-12; see that file's
# header for what was measured live). Population's one manifest row is read from IBGE's
# SIDRA API, not from a file: its `sidra_code` is the bare table number "6579" -- the
# estimates, which MEANS "no classification" -- and load_population() asks three other
# tables for the three years the estimates do not have: 793 (Contagem 2007), 1378 (Censo
# 2010, with the Total of its first classification) and 4714 (Censo 2022). What goes stale
# is available_time, the meaning of the bare code, and the map of years to tables, which
# nothing but this resolver notices. The resolver is a thin layer on actions/scripts/
# sidra_api.R (tested for itself in test-sidra-api.R).
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was measured
# live on 2026-11-12: /metadados describes each table, the coverage query is a JSON array
# whose first element is a header row, one row per year (and category), "..." where nothing
# is published.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
# the metadata lookup, coverage query and value coding population shares with the other
# SIDRA resolvers, as build_manifest.R sources it before the resolvers
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_population.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed row, not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "population", ]

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- a fake IBGE ------------------------------------------------------------

ALL_LEVELS <- c("N1", "N2", "N3", "N6", "N8", "N9")

# table -> what IBGE's metadata says of it and which years carry a value for the variable
# the resolver asks about (checked live 2026-11-12)
FAKE_TABLES <- list(
  `6579` = list(variables = "9324", classification = FALSE, years = c(2001:2006, 2008:2009, 2011:2021, 2024:2026), span = 2001:2026),
  `793` = list(variables = "93", classification = FALSE, years = 2007L, span = 2007L),
  `1378` = list(variables = c("93", "1000093"), classification = TRUE, years = 2010L, span = 2010L),
  `4714` = list(variables = c("93", "6318", "614"), classification = FALSE, years = 2022L, span = 2022L)
)

# the four classifications of table 1378, each with its Total (0)
classifications_1378 <- list(
  list(id = 1L, nome = "Situacao do domicilio", categorias = lapply(c(0L, 1L, 2L), function(ct) list(id = ct, nome = paste("c1", ct), unidade = NULL, nivel = 1L))),
  list(id = 2L, nome = "Sexo", categorias = lapply(c(0L, 4L, 5L), function(ct) list(id = ct, nome = paste("c2", ct), unidade = NULL, nivel = 1L))),
  list(id = 287L, nome = "Idade", categorias = lapply(c(0L, 93070L), function(ct) list(id = ct, nome = paste("c287", ct), unidade = NULL, nivel = 1L))),
  list(id = 455L, nome = "Condicao", categorias = lapply(c(0L, 11941L), function(ct) list(id = ct, nome = paste("c455", ct), unidade = NULL, nivel = 1L)))
)

metadata_json <- function(table, variables = NULL, classifications = NULL, levels = NULL, id = as.integer(table),
                          url = paste0("https://sidra.ibge.gov.br/tabela/", table)) {
  spec <- FAKE_TABLES[[table]]
  variables <- variables %||% spec$variables
  classifications <- classifications %||% (if (spec$classification) classifications_1378 else list())
  levels <- levels %||% (if (table == "6579" || table == "4714") c("N1", "N2", "N3", "N6") else ALL_LEVELS)
  as.character(jsonlite::toJSON(list(
    id = id, nome = paste("Tabela", table), URL = url, pesquisa = "Populacao",
    periodicidade = list(frequencia = "anual", inicio = min(spec$span), fim = max(spec$span)),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = lapply(variables, function(v) list(id = as.integer(v), nome = paste("variavel", v), unidade = if (v == "1000093") "%" else "Pessoas")),
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

COVERAGE_HEADER <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano"
)

# Brazil level, every year of the table's span; "..." where nothing is published. A request
# with a classification (D4C) has one row per year of its one category.
coverage_json <- function(table, variable, published = NULL, classified = FALSE) {
  spec <- FAKE_TABLES[[table]]
  published <- published %||% spec$years
  header <- c(COVERAGE_HEADER, if (classified) list(D4C = "Classificacao (Codigo)", D4N = "Classificacao"))
  rows <- list(header)
  for (y in spec$span) {
    cell <- list(
      NC = "1", NN = "Brasil", MC = "1", MN = "Pessoas", V = if (y %in% published) as.character(1000L + y) else "...",
      D1C = "1", D1N = "Brasil", D2C = variable, D2N = "variavel", D3C = as.character(y), D3N = as.character(y)
    )
    if (classified) cell <- c(cell, list(D4C = "0", D4N = "Total"))
    rows[[length(rows) + 1]] <- cell
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}

# `override(kind, table, url)` may return a response (or throw like curl does) to replace
# the healthy one; `kind` is "metadata" or "coverage". `metadata` / `coverage` replace what
# a table answers: function(table) / function(table, variable, classified).
fake_ibge <- function(override = function(kind, table, url) NULL, seen = NULL,
                      metadata = metadata_json,
                      coverage = function(table, variable, classified) coverage_json(table, variable, classified = classified)) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("/agregados/[0-9]+/metadados$", url)) {
      table <- sub("^.*/agregados/([0-9]+)/metadados$", "\\1", url)
      return(override("metadata", table, url) %||% resp(200L, metadata(table)))
    }
    if (grepl("/values/t/[0-9]+/n1/all/v/[0-9]+/p/all(/c[0-9]+/([0-9]+|all))?$", url)) {
      table <- sub("^.*/values/t/([0-9]+)/.*$", "\\1", url)
      variable <- sub("^.*/v/([0-9]+)/.*$", "\\1", url)
      classified <- grepl("/c[0-9]+/([0-9]+|all)$", url)
      return(override("coverage", table, url) %||% resp(200L, coverage(table, variable, classified)))
    }
    stop("the fake IBGE has no such URL: ", url)
  }
}
with_ibge <- function(server, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = server, .package = "curl")
}
run <- function(server = fake_ibge(), rows = manifest_rows) with_ibge(server, suppressMessages(resolve_population(rows)))
error_message <- function(server, rows = manifest_rows) {
  tryCatch(
    {
      run(server, rows)
      "NO ERROR"
    },
    error = function(e) conditionMessage(e)
  )
}
# one table answers differently: `metadata` or `coverage` (the other stays healthy)
with_table <- function(table, metadata = NULL, coverage = NULL, ...) {
  fake_ibge(
    metadata = function(t) if (t == table && !is.null(metadata)) metadata(t) else metadata_json(t),
    coverage = function(t, variable, classified) if (t == table && !is.null(coverage)) coverage(t, variable, classified) else coverage_json(t, variable, classified = classified),
    ...
  )
}
with_code <- function(code) {
  rows <- manifest_rows
  rows$sidra_code <- code
  rows
}

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves the row: 8 requests, available_time the union of the four tables' years, 2023 left as a gap", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 1)
  expect_equal(out$survey, "population")
  expect_equal(out$dataset, "population")
  expect_equal(out$resolver, "population")
  expect_true(is.na(out$geo_level) && is.na(out$year))
  expect_false("sidra_code" %in% names(out))
  expect_equal(out$available_time, "2001-2022, 2024-2026")
  expect_equal(out$docs_url, "https://sidra.ibge.gov.br/tabela/6579")
  expect_null(attr(out, "partial_failures"))

  # two requests per table: metadata and coverage of the resident population (93; 9324 for the estimates)
  expect_length(seen$urls, 8)
  expect_equal(sum(grepl("/metadados$", seen$urls)), 4)
  expect_true(any(grepl("/t/6579/n1/all/v/9324/p/all$", seen$urls)))
  expect_true(any(grepl("/t/793/n1/all/v/93/p/all$", seen$urls)))
  expect_true(any(grepl("/t/1378/n1/all/v/93/p/all/c1/0$", seen$urls)))
  expect_true(any(grepl("/t/4714/n1/all/v/93/p/all$", seen$urls)))

  # the grammar is the one parse_years() reads: 2023 is no year of the series
  expect_false(2023 %in% parse_years(out$available_time))
  expect_true(all(c(2007, 2010, 2022, 2026) %in% parse_years(out$available_time)))
})

test_that("available_time follows the data: a new estimate extends it, one that fills 2023 closes the gap", {
  newer <- function(table, variable, classified) coverage_json(table, variable, published = c(FAKE_TABLES[["6579"]]$years, 2023L))
  out <- run(with_table("6579", coverage = newer))
  expect_equal(out$available_time, "2001-2026")

  fewer <- function(table, variable, classified) coverage_json(table, variable, published = 2001:2020)
  out <- run(with_table("6579", coverage = fewer))
  expect_equal(out$available_time, "2001-2020, 2022") # 2007 and 2010 are inside 2001-2020; 2022 comes from its special table
})

test_that("docs_url is the estimates table's own page, taken from the metadata only when it is a SIDRA table page", {
  survey_page <- function(table) metadata_json(table, url = "https://sidra.ibge.gov.br/pesquisa/estimapop/tabelas")
  out <- run(with_table("6579", metadata = survey_page))
  expect_true(is.na(out$docs_url))
  expect_equal(out$available_time, "2001-2022, 2024-2026")
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(grep("^(resolve|watch)_", ls(resolver_env), value = TRUE), "resolve_population")
})

# ---- the special tables ------------------------------------------------------------

test_that("POPULATION_SPECIAL_TABLES is a copy of the loader's population_special_tables, year by year", {
  loader <- population_special_tables
  expect_setequal(names(POPULATION_SPECIAL_TABLES), names(loader))
  for (year in names(loader)) {
    mine <- POPULATION_SPECIAL_TABLES[[year]]
    theirs <- loader[[year]]
    expect_equal(as.numeric(mine$table), theirs$table, info = year)
    expect_equal(if (is.na(mine$classific)) NULL else mine$classific, theirs$classific, info = year)
    expect_equal(if (is.na(mine$category)) NULL else list(as.numeric(mine$category)), theirs$category, info = year)
  }
  # the three years the estimates table lacks and that the series needs a table for
  expect_setequal(names(POPULATION_SPECIAL_TABLES), c("2007", "2010", "2022"))
})

test_that("each special table is checked: one that cannot be read fails the row and says which table, for which year", {
  challenge <- resp(403L, "<html><head><title>Just a moment...</title></head><body>Checking</body></html>")
  for (case in list(c("793", "2007"), c("1378", "2010"), c("4714", "2022"))) {
    msg <- error_message(fake_ibge(override = function(kind, table, url) if (kind == "metadata" && table == case[1]) challenge else NULL))
    expect_match(msg, sprintf("table %s (the loader's table for %s)", case[1], case[2]), fixed = TRUE, info = case[1])
    expect_match(msg, "HTTP 403", fixed = TRUE, info = case[1])
    expect_match(msg, "Just a moment", fixed = TRUE, info = case[1])
  }
})

test_that("a special table that has no value for its year fails the row, saying which years it has", {
  elsewhere <- function(table, variable, classified) coverage_json(table, variable, published = integer(0), classified = classified)
  for (table in c("793", "1378", "4714")) {
    msg <- error_message(with_table(table, coverage = elsewhere))
    expect_match(msg, sprintf("table %s", table), fixed = TRUE, info = table)
    expect_match(msg, "has no value for", fixed = TRUE, info = table)
  }
})

test_that("a special table that gained a classification fails the row: the loader sends none, and IBGE would answer only the Total", {
  one <- list(list(id = 99L, nome = "Nova", categorias = list(list(id = 0L, nome = "Total", unidade = NULL, nivel = 1L))))
  msg <- error_message(with_table("4714", metadata = function(t) metadata_json(t, classifications = one)))
  expect_match(msg, "table 4714 (the loader's table for 2022)", fixed = TRUE)
  expect_match(msg, "it now has classification(s) c99, but load_population() sends none", fixed = TRUE)
  expect_match(msg, "'Total' slice", fixed = TRUE)
})

test_that("table 1378 must still have the classification the loader asks for, with its Total, and variable 93", {
  without_c1 <- function(t) metadata_json(t, classifications = classifications_1378[-1])
  expect_match(error_message(with_table("1378", metadata = without_c1)), "it has no classification c1 (it has c2, c287, c455)", fixed = TRUE)

  c1_without_total <- classifications_1378
  c1_without_total[[1]]$categorias <- lapply(c(1L, 2L), function(ct) list(id = ct, nome = "x", unidade = NULL, nivel = 1L))
  msg <- error_message(with_table("1378", metadata = function(t) metadata_json(t, classifications = c1_without_total)))
  expect_match(msg, "category 0 does not exist in classification c1", fixed = TRUE)
  expect_match(msg, "HTTP 200", fixed = TRUE)

  expect_match(error_message(with_table("793", metadata = function(t) metadata_json(t, variables = "9999"))), "it does not declare variable 93", fixed = TRUE)
})

test_that("a territorial level the manifest promises and a special table lacks fails the row, naming the table", {
  no_municipality <- function(t) metadata_json(t, levels = c("N1", "N2", "N3"))
  msg <- error_message(with_table("4714", metadata = no_municipality))
  expect_match(msg, "table 4714 (the loader's table for 2022)", fixed = TRUE)
  expect_match(msg, "no territorial level N6", fixed = TRUE)
})

test_that("a table that is not the one the description is for fails with that said", {
  msg <- error_message(with_table("793", metadata = function(t) metadata_json(t, id = 794L)))
  expect_match(msg, "the description is for table 794, not 793", fixed = TRUE)
})

# ---- the variables -----------------------------------------------------------------

test_that("the variables the tables declare are known: 6579 declares 9324, 793 and 1378 93, 4714 93 and its area and density", {
  expect_null(attr(run(fake_ibge()), "partial_failures")) # 6318 and 614 of table 4714, and 1000093 of 1378, are no note
  expect_equal(POPULATION_EXPECTED_VARIABLES[["6579"]], "9324")
  expect_true(all(c("93", "6318", "614") %in% POPULATION_EXPECTED_VARIABLES[["4714"]]))
})

test_that("a variable a table starts to declare is a NOTE: the row is still refreshed, and the note names the table and the variable", {
  out <- run(with_table("6579", metadata = function(t) metadata_json(t, variables = c("9324", "9999"))))
  expect_equal(out$available_time, "2001-2022, 2024-2026")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "population/population (6579)", fixed = TRUE)
  expect_match(notes, "table 6579 now declares variable(s) 9999", fixed = TRUE)

  out <- run(with_table("4714", metadata = function(t) metadata_json(t, variables = c("93", "6318", "614", "7777"))))
  expect_match(attr(out, "partial_failures"), "table 4714 now declares variable(s) 7777", fixed = TRUE)
})

test_that("the estimates table now publishing a year a special table is asked for is a NOTE: both stay, and it says which", {
  both <- function(table, variable, classified) coverage_json(table, variable, published = c(FAKE_TABLES[["6579"]]$years, 2022L))
  out <- run(with_table("6579", coverage = both))
  expect_equal(out$available_time, "2001-2022, 2024-2026")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "table 6579 now has a value for 2022 too", fixed = TRUE)
  expect_match(notes, "still asks table 4714 for it", fixed = TRUE)
})

# ---- the code's meaning, both ways ---------------------------------------------------

test_that("a table that gained a classification fails a bare code, and the message gives the code to write", {
  one <- list(list(id = 99L, nome = "Nova", categorias = list(list(id = 0L, nome = "Total", unidade = NULL, nivel = 1L))))
  msg <- error_message(with_table("6579", metadata = function(t) metadata_json(t, classifications = one), coverage = function(t, v, classified) coverage_json(t, v, classified = classified)))
  expect_match(msg, "table 6579 now has classification c99", fixed = TRUE)
  expect_match(msg, "'Total' slice", fixed = TRUE)
  expect_match(msg, "write '6579/c99/all'", fixed = TRUE)
})

test_that("classific = 'all' is refused with the id to use, once the table has a classification to name", {
  one <- list(list(id = 99L, nome = "Nova", categorias = list(list(id = 0L, nome = "Total", unidade = NULL, nivel = 1L))))
  msg <- error_message(with_table("6579", metadata = function(t) metadata_json(t, classifications = one)), with_code("6579/all/all"))
  expect_match(msg, "uses classific = 'all'", fixed = TRUE)
  expect_match(msg, "write '6579/c99/all' instead", fixed = TRUE)
})

test_that("a code that names a classification the table does not have is refused, with the code to write", {
  msg <- error_message(fake_ibge(), with_code("6579/c99/all"))
  expect_match(msg, "names a classification, but table 6579 has none; write '6579'", fixed = TRUE)
})

test_that("a code for another table is refused: load_population() chooses the other tables around 6579", {
  msg <- error_message(fake_ibge(), with_code("793"))
  expect_match(msg, "sidra_code '793' is table 793", fixed = TRUE)
  expect_match(msg, "around table 6579", fixed = TRUE)
})

test_that("a sidra_code that is not <table> or <table>/c<n>/<id or all> fails its row", {
  for (code in c("6579/all", "6579/c99", "abc", "6579/c99/all/extra")) {
    msg <- error_message(fake_ibge(), with_code(code))
    expect_match(msg, "is not <table> or <table>/c<classification>/<category id or all>", fixed = TRUE, info = code)
  }
})

# ---- the territorial levels -----------------------------------------------------------

test_that("an available_geo level the estimates table lacks fails the row; an unknown word does too; NA is skipped", {
  no_municipality <- function(t) metadata_json(t, levels = c("N1", "N2", "N3"))
  expect_match(error_message(with_table("6579", metadata = no_municipality)), "no territorial level N6", fixed = TRUE)

  rows <- manifest_rows
  rows$available_geo <- "country, galaxy"
  expect_match(error_message(fake_ibge(), rows), "'galaxy'", fixed = TRUE)
  rows$available_geo <- NA_character_
  expect_null(attr(run(fake_ibge(), rows), "partial_failures"))
})

# ---- the estimates table itself -----------------------------------------------------------

test_that("IBGE's API being challenged says so, with the page's own words and the URL", {
  challenge <- resp(403L, "<html><head><title>Just a moment...</title></head><body>Checking</body></html>")
  msg <- error_message(fake_ibge(override = function(kind, table, url) if (table == "6579" && kind == "metadata") challenge else NULL))
  expect_match(msg, "all 1 row(s) failed verification", fixed = TRUE)
  expect_match(msg, "HTTP 403", fixed = TRUE)
  expect_match(msg, "Just a moment", fixed = TRUE)
  expect_match(msg, "agregados/6579/metadados", fixed = TRUE)
})

test_that("a transport error is a reason, not an exception, and names the URL", {
  msg <- error_message(fake_ibge(override = function(kind, table, url) if (kind == "coverage" && table == "6579") stop("Could not resolve host") else NULL))
  expect_match(msg, "Could not resolve host", fixed = TRUE)
  expect_match(msg, "v/9324/p/all", fixed = TRUE)
})

test_that("each way the estimates table's answer can be wrong is a reason of its own", {
  expect_match(error_message(with_table("6579", metadata = function(t) "not json")), "not a SIDRA table description", fixed = TRUE)
  expect_match(error_message(with_table("6579", coverage = function(t, v, classified) "<html>Just a moment</html>")), "not a SIDRA value table", fixed = TRUE)
  none <- function(t, v, classified) coverage_json(t, v, published = integer(0))
  expect_match(error_message(with_table("6579", coverage = none)), "no year with a published value at Brazil level", fixed = TRUE)
})

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_population(manifest_rows[0, ]), "no manifest rows tagged resolver == 'population'")
  expect_error(resolve_population(NULL), "no manifest rows tagged resolver == 'population'")
  keyed <- manifest_rows
  keyed$geo_level <- "state"
  expect_error(resolve_population(keyed), "every population row is unkeyed")
  no_code <- manifest_rows
  no_code$sidra_code <- NA_character_
  expect_error(resolve_population(no_code), "needs a sidra_code")
})

# ---- the code's grammar, and the committed row ------------------------------------------------

test_that("population_parse_code reads <table> and <table>/c<n>/<category> and rejects everything else", {
  expect_equal(population_parse_code("6579"), list(table = "6579", classific = NA_character_, category = NA_character_))
  expect_equal(population_parse_code("74/c80/all"), list(table = "74", classific = "c80", category = "all"))
  expect_equal(population_parse_code("6579/all/all")$classific, "all") # parses, so that it can be refused with a reason
  for (bad in c("", "abc", "6579/c99", "6579/c99/", "/c99/all", "6579/x99/all", NA_character_)) {
    expect_null(population_parse_code(bad), info = bad)
  }
  expect_null(population_parse_code(NULL))
})

test_that("the package's sidra_code_parts() (what load_population() uses) reads the committed code the way this resolver does", {
  mine <- population_parse_code(manifest_rows$sidra_code)
  theirs <- sidra_code_parts(manifest_rows$sidra_code)
  expect_equal(theirs$table, as.numeric(mine$table))
  expect_null(theirs$classific)
  expect_null(theirs$category)
  expect_true(is.na(mine$classific))
})

test_that("the committed row is verifiable: owned by the resolver, the bare estimates table, known geo levels, a table page for docs_url, a gap for 2023", {
  expect_equal(nrow(manifest_rows), 1)
  expect_equal(manifest_rows$resolver, "population")
  expect_equal(manifest_rows$sidra_code, "6579")
  expect_true(is.na(manifest_rows$geo_level) && is.na(manifest_rows$year))
  geo <- trimws(strsplit(manifest_rows$available_geo, ",")[[1]])
  expect_true(all(geo %in% names(SIDRA_API_GEO_LEVELS)))
  expect_match(manifest_rows$docs_url, SIDRA_API_TABLE_PAGE)
  expect_true(is.na(manifest_rows$url))
  years <- parse_years(manifest_rows$available_time)
  expect_false(2023 %in% years)
  expect_true(all(c(2001, 2007, 2010, 2022, 2024) %in% years))
})

test_that("a changed population row lists the FRAGILE notes of the two files that treat it (build_manifest.R reads FRAGILITY_SOURCES)", {
  frag_env <- new.env()
  source(file.path(actions_dir, "scripts", "fragility_notes.R"), local = frag_env)
  entries <- Filter(function(e) identical(e$changed_prefix, "population"), frag_env$FRAGILITY_SOURCES)
  expect_setequal(vapply(entries, function(e) e$label, ""), c("R/population.R", "R/download.R"))
  for (e in entries) {
    expect_true(file.exists(file.path(pkg_root, e$r_path)), info = e$label)
    expect_gt(length(frag_env$collect_fragility_notes(file.path(pkg_root, e$r_path), e$label)), 0)
  }
})

test_that("the resolver run on the committed row and a healthy IBGE changes nothing the manifest already says", {
  out <- run(fake_ibge())
  expect_equal(out$available_time, manifest_rows$available_time)
  expect_equal(out$docs_url, manifest_rows$docs_url)
})
