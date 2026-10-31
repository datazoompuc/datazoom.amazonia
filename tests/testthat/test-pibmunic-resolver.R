# tests/testthat/test-pibmunic-resolver.R
#
# Offline test for actions/scrapers/resolve_pibmunic.R (added 2026-10-30; see that
# file's header for what was measured live). PIB Municipal's one manifest row is
# read from IBGE's SIDRA API, not from a file: its `sidra_code` is the bare table
# number "5938", which MEANS "this table has no classification" (like PPM's tables
# 94 and 95), and what goes stale is available_time and, silently, that meaning --
# for a table that gains a classification a bare code still answers HTTP 200, with
# only the "Total" slice. The resolver is a thin layer on actions/scripts/
# sidra_api.R (tested for itself in test-sidra-api.R).
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was
# measured live on 2026-10-30: /metadados is a JSON description of a table with 46
# variables (7 in thousands of reais, 39 shares with unit "%") and no
# classification; the coverage query is a JSON array whose first element is a header
# row; the variables query lists the variable of each row in its third dimension.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
# the metadata lookup, coverage query and value coding pibmunic shares with the
# other SIDRA resolvers, as build_manifest.R sources it before the resolvers
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_pibmunic.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed row, not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "pibmunic", ]

# ---- a fake IBGE ------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# The 7 variables in thousands of reais; the other 39 are shares, unit "%"
FAKE_VALUE_VARIABLES <- c("37", "543", "498", "513", "517", "6575", "525")

metadata_json <- function(variables = PIBMUNIC_NAMED_VARIABLES, classifications = list(),
                          levels = c("N1", "N2", "N3", "N6", "N8", "N9"), id = 5938L,
                          url = "https://sidra.ibge.gov.br/tabela/5938") {
  variaveis <- lapply(variables, function(v) {
    list(
      id = as.integer(v), nome = paste("variavel", v),
      unidade = if (v %in% FAKE_VALUE_VARIABLES) "Mil Reais" else "%"
    )
  })
  as.character(jsonlite::toJSON(list(
    id = id, nome = "Produto interno bruto - Referencia 2010", URL = url,
    pesquisa = "Produto Interno Bruto dos Municipios",
    periodicidade = list(frequencia = "anual", inicio = 2002L, fim = 2023L),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = variaveis,
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

# One classification, c99, with categories 1 and 2: what the table would look like if
# IBGE gave it one
classification_99 <- list(list(
  id = 99L, nome = "Classificacao",
  categorias = lapply(1:2, function(ct) list(id = ct, nome = paste("categoria", ct), unidade = NULL, nivel = 1L))
))

COVERAGE_HEADER <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano"
)

# Brazil level, every year of the table's span (2002-2023); "..." where nothing is
# published. A table with a classification has one row per category and year (D4C).
coverage_json <- function(variable = "37", published = 2002:2023, classified = FALSE) {
  header <- c(COVERAGE_HEADER, if (classified) list(D4C = "Classificacao (Codigo)", D4N = "Classificacao"))
  rows <- list(header)
  for (ct in if (classified) c("1", "2") else "(none)") {
    for (y in 2002:2023) {
      cell <- list(
        NC = "1", NN = "Brasil", MC = "40", MN = "Mil Reais",
        V = if (y %in% published) as.character(1000L + y) else "...",
        D1C = "1", D1N = "Brasil", D2C = variable, D2N = "variavel", D3C = as.character(y), D3N = as.character(y)
      )
      if (classified) cell <- c(cell, list(D4C = ct, D4N = paste("categoria", ct)))
      rows[[length(rows) + 1]] <- cell
    }
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

# The variables query, with the dimensions in the order IBGE answers it: Brasil, Ano,
# Variavel (the variable is the THIRD dimension here, the second in the coverage query)
variables_json <- function(ids = PIBMUNIC_NAMED_VARIABLES, year = "2023", swap = FALSE) {
  header <- list(
    NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
    MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
    D2C = "Ano (Codigo)", D2N = "Ano", D3C = "Variavel (Codigo)", D3N = "Variavel"
  )
  if (swap) {
    header[c("D2C", "D2N", "D3C", "D3N")] <- list("Variavel (Codigo)", "Variavel", "Ano (Codigo)", "Ano")
  }
  rows <- list(header)
  for (id in ids) {
    cell <- list(
      NC = "1", NN = "Brasil", MC = "40", MN = "Mil Reais", V = "...", D1C = "1", D1N = "Brasil",
      D2C = year, D2N = year, D3C = id, D3N = paste("variavel", id)
    )
    if (swap) cell[c("D2C", "D2N", "D3C", "D3N")] <- list(id, paste("variavel", id), year, year)
    rows[[length(rows) + 1]] <- cell
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}

# `override(kind, url)` may return a response (or throw like curl does) to replace the
# healthy one; `kind` is "metadata", "coverage" or "variables". `seen` collects the
# requested URLs. `coverage_classified` is what the coverage query's /c99/all form
# answers.
fake_ibge <- function(override = function(kind, url) NULL, seen = NULL,
                      metadata = metadata_json(), coverage = coverage_json(),
                      coverage_classified = coverage_json(classified = TRUE), variables = NULL) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("/agregados/5938/metadados$", url)) {
      return(override("metadata", url) %||% resp(200L, metadata))
    }
    if (grepl("/values/t/5938/n1/all/v/37/p/all$", url)) {
      return(override("coverage", url) %||% resp(200L, coverage))
    }
    if (grepl("/values/t/5938/n1/all/v/37/p/all/c99/all$", url)) {
      return(override("coverage", url) %||% resp(200L, coverage_classified))
    }
    if (grepl("/values/t/5938/n1/1/p/[0-9]{4}/v/allxp$", url)) {
      year <- sub("^.*/p/([0-9]{4})/v/allxp$", "\\1", url)
      return(override("variables", url) %||% resp(200L, variables %||% variables_json(year = year)))
    }
    stop("the fake IBGE has no such URL: ", url)
  }
}
with_ibge <- function(server, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = server, .package = "curl")
}
run <- function(server = fake_ibge(), rows = manifest_rows) {
  with_ibge(server, suppressMessages(resolve_pibmunic(rows)))
}
error_message <- function(server, rows = manifest_rows) {
  tryCatch(
    {
      run(server, rows)
      "NO ERROR"
    },
    error = function(e) conditionMessage(e)
  )
}
# a second row on the same table, spelling a classification out
classified_row <- function(code = "5938/c99/all") {
  second <- manifest_rows
  second$dataset <- "pibmunic_classified"
  second$sidra_code <- code
  second
}

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves the row: 3 requests (metadata, coverage of variable 37, the variables of the last year)", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 1)
  expect_equal(out$survey, "pibmunic")
  expect_equal(out$dataset, "pibmunic")
  expect_equal(out$resolver, "pibmunic")
  # an unkeyed row stays unkeyed; the values the resolver owns are available_time and
  # docs_url, never sidra_code
  expect_true(is.na(out$geo_level) && is.na(out$year))
  expect_false("sidra_code" %in% names(out))
  expect_equal(out$available_time, "2002-2023")
  expect_equal(out$docs_url, "https://sidra.ibge.gov.br/tabela/5938")
  expect_null(attr(out, "partial_failures"))

  expect_equal(seen$urls, c(
    "https://servicodados.ibge.gov.br/api/v3/agregados/5938/metadados",
    "https://apisidra.ibge.gov.br/values/t/5938/n1/all/v/37/p/all",
    "https://apisidra.ibge.gov.br/values/t/5938/n1/1/p/2023/v/allxp"
  ))
})

test_that("available_time is the years variable 37 has a value for, gaps included, and the variables are asked for in the last of them", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen, coverage = coverage_json(published = 2002:2020)))
  expect_equal(out$available_time, "2002-2020")
  expect_match(seen$urls[3], "/p/2020/v/allxp$")

  # a year IBGE has not published yet is a row of "..." and does not count
  out <- run(fake_ibge(coverage = coverage_json(published = c(2002:2010, 2012:2023))))
  expect_equal(out$available_time, "2002-2010, 2012-2023")
  # ... and the grammar is the one parse_years() reads
  expect_equal(parse_years(out$available_time), c(2002:2010, 2012:2023))
})

test_that("variable 37 (GDP) decides the years: a table that does not declare it is a failure, not a fallback", {
  without_37 <- metadata_json(variables = setdiff(PIBMUNIC_NAMED_VARIABLES, "37"))
  msg <- error_message(fake_ibge(metadata = without_37))
  expect_match(msg, "declares none of the variables 37", fixed = TRUE)
})

test_that("docs_url is the table's own page, taken from the metadata only when it is a SIDRA table page; otherwise the cell is left alone", {
  expect_equal(run(fake_ibge())$docs_url, "https://sidra.ibge.gov.br/tabela/5938")
  out <- run(fake_ibge(metadata = metadata_json(url = "https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas")))
  expect_true(is.na(out$docs_url))
  expect_equal(out$available_time, "2002-2023")
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(grep("^(resolve|watch)_", ls(resolver_env), value = TRUE), "resolve_pibmunic")
})

# ---- the variables ------------------------------------------------------------

test_that("PIBMUNIC_NAMED_VARIABLES is the var_code column of the \"pibmunic\" entry load_pibmunic() labels its columns from", {
  dic <- load_dictionary("pibmunic")
  expect_equal(PIBMUNIC_NAMED_VARIABLES, as.character(dic$var_code))
  expect_false(anyDuplicated(PIBMUNIC_NAMED_VARIABLES) > 0)
  expect_equal(length(PIBMUNIC_NAMED_VARIABLES), 46)
})

test_that("the share variables (unit '%') are variables of the loader too: the check reads the data, not the metadata's non-percentage list", {
  # sidra_api.R's metadata parser leaves every "%" variable out of `variables` (right for PAM and PPM,
  # where "allxp" never asks for them); here 39 of the 46 are such variables, and the healthy run
  # must still pass with no note
  parsed <- sidra_api_parse_metadata(metadata_json(), "5938")
  expect_equal(length(parsed$variables), 7)
  out <- run(fake_ibge())
  expect_null(attr(out, "partial_failures"))
})

test_that("a variable IBGE adds is a NOTE: the row is still refreshed, and the note names the variable", {
  out <- run(fake_ibge(variables = variables_json(ids = c(PIBMUNIC_NAMED_VARIABLES, "9999"))))
  expect_equal(nrow(out), 1)
  expect_equal(out$available_time, "2002-2023")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "pibmunic/pibmunic (5938)", fixed = TRUE)
  expect_match(notes, "variable(s) 9999", fixed = TRUE)
  expect_match(notes, "unlabelled columns", fixed = TRUE)
})

test_that("a variable the dictionary has and IBGE dropped is not reported", {
  out <- run(fake_ibge(variables = variables_json(ids = setdiff(PIBMUNIC_NAMED_VARIABLES, c("553", "6573")))))
  expect_null(attr(out, "partial_failures"))
})

test_that("when the variables cannot be checked the row is still refreshed, and the note says so (not 'checked and fine')", {
  for (answer in list(
    resp(500L, "Internal Server Error"),
    resp(200L, "<html>Just a moment</html>"),
    resp(200L, "") # empty body
  )) {
    out <- run(fake_ibge(override = function(kind, url) if (kind == "variables") answer else NULL))
    expect_equal(out$available_time, "2002-2023")
    notes <- attr(out, "partial_failures")
    expect_length(notes, 1)
    expect_match(notes, "could not check table 5938's variables against the dictionary", fixed = TRUE)
  }
  out <- run(fake_ibge(override = function(kind, url) if (kind == "variables") stop("Connection reset") else NULL))
  expect_match(attr(out, "partial_failures"), "Connection reset", fixed = TRUE)
})

test_that("pibmunic_parse_variables finds the variable dimension by its header, whichever position it has", {
  ids <- c("37", "553", "6575")
  expect_equal(pibmunic_parse_variables(variables_json(ids))$ids, ids)
  expect_equal(pibmunic_parse_variables(variables_json(ids, swap = TRUE))$ids, ids)
  expect_equal(pibmunic_parse_variables(variables_json(c("37", "37")))$ids, "37") # one id once

  expect_match(pibmunic_parse_variables("not json")$problem, "not a SIDRA value table")
  expect_match(pibmunic_parse_variables("[]")$problem, "not a SIDRA value table")
  # the coverage query carries the variable in its second dimension: found there too
  expect_equal(pibmunic_parse_variables(coverage_json())$ids, "37")
})

test_that("pibmunic_parse_variables refuses a table with no variable dimension, or with codes that are not numbers", {
  no_variable <- as.character(jsonlite::toJSON(list(
    list(V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil", D2C = "Ano (Codigo)", D2N = "Ano"),
    list(V = "1", D1C = "1", D1N = "Brasil", D2C = "2023", D2N = "2023")
  ), auto_unbox = TRUE))
  expect_match(pibmunic_parse_variables(no_variable)$problem, "no single dimension of the response is the variable")

  words <- as.character(jsonlite::toJSON(list(
    list(V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil", D2C = "Variavel (Codigo)", D2N = "Variavel"),
    list(V = "1", D1C = "1", D1N = "Brasil", D2C = "abc", D2N = "abc")
  ), auto_unbox = TRUE))
  expect_match(pibmunic_parse_variables(words)$problem, "not all numbers")
})

# ---- the code's meaning, both ways ---------------------------------------------

test_that("a table that gained a classification fails a bare code, and the message gives the code to write", {
  server <- fake_ibge(metadata = metadata_json(classifications = classification_99))
  msg <- error_message(server)
  expect_match(msg, "table 5938 now has classification c99", fixed = TRUE)
  expect_match(msg, "'Total' slice", fixed = TRUE)
  expect_match(msg, "write '5938/c99/all'", fixed = TRUE)
})

test_that("a code that spells the classification out resolves against a table that has it, with the classified coverage query", {
  seen <- new.env()
  server <- fake_ibge(metadata = metadata_json(classifications = classification_99), seen = seen)
  out <- run(server, classified_row("5938/c99/all"))
  expect_equal(out$available_time, "2002-2023")
  expect_true("https://apisidra.ibge.gov.br/values/t/5938/n1/all/v/37/p/all/c99/all" %in% seen$urls)
  # one category of it
  expect_equal(run(server, classified_row("5938/c99/2"))$available_time, "2002-2023")
})

test_that("a classification the table does not have, 'all', and a category that does not exist each fail the row with their own reason", {
  classified <- fake_ibge(metadata = metadata_json(classifications = classification_99))
  # the row alone: its failure is the resolver's
  reason <- function(code, server) error_message(server, classified_row(code))

  expect_match(reason("5938/c99/all", fake_ibge()), "names a classification, but table 5938 has none; write '5938'", fixed = TRUE)
  expect_match(reason("5938/all/all", classified), "uses classific = 'all'", fixed = TRUE)
  expect_match(reason("5938/all/all", classified), "write '5938/c99/all'", fixed = TRUE)
  expect_match(reason("5938/c77/all", classified), "classification c77 is not one of table 5938's (c99)", fixed = TRUE)
  expect_match(reason("5938/c99/3", classified), "category 3 does not exist in classification c99", fixed = TRUE)
})

test_that("a failing row is reported and left out while the good one is still refreshed", {
  # the table has no classification: the bare row is fine, the classified one is not
  out <- run(fake_ibge(), rbind(manifest_rows, classified_row()))
  expect_equal(out$dataset, "pibmunic")
  expect_equal(attr(out, "partial_failures"), sprintf(
    "pibmunic/pibmunic_classified (5938/c99/all): sidra_code '5938/c99/all' names a classification, but table 5938 has none; write '5938'"
  ))
})

test_that("a sidra_code that is not <table> or <table>/c<n>/<id or all> fails its row", {
  for (code in c("5938/all", "5938/c99", "abc", "5938/c99/all/extra", "5938/")) {
    out <- run(fake_ibge(), rbind(manifest_rows, classified_row(code)))
    expect_match(attr(out, "partial_failures"), "is not <table> or <table>/c<classification>/<category id or all>", fixed = TRUE, info = code)
  }
})

# ---- the territorial levels ------------------------------------------------------

test_that("an available_geo level the table's metadata does not have fails the row; an unknown word does too; NA is skipped", {
  # a level the table lacks takes the committed row down (the only row: the resolver stops)
  no_municipality <- fake_ibge(metadata = metadata_json(levels = c("N1", "N2", "N3")))
  expect_match(error_message(no_municipality), "no territorial level N6", fixed = TRUE)

  # next to the good committed row, a second row on the same table with another available_geo
  geo <- function(available_geo) {
    second <- classified_row("5938")
    second$available_geo <- available_geo
    attr(run(fake_ibge(), rbind(manifest_rows, second)), "partial_failures")
  }
  expect_match(geo("country, galaxy"), "'galaxy'", fixed = TRUE)
  expect_null(geo(NA_character_))
  expect_null(geo("country, state, municipality"))
})

# ---- the table itself ----------------------------------------------------------------

test_that("a table with no published year fails its row", {
  msg <- error_message(fake_ibge(coverage = coverage_json(published = integer(0))))
  expect_match(msg, "has no year with a published value at Brazil level", fixed = TRUE)
})

test_that("IBGE's API being challenged says so, with the page's own words and the URL", {
  challenge <- resp(403L, "<html><head><title>Just a moment...</title></head><body>Checking</body></html>")
  msg <- error_message(fake_ibge(override = function(kind, url) if (kind == "metadata") challenge else NULL))
  expect_match(msg, "all 1 row(s) failed verification", fixed = TRUE)
  expect_match(msg, "HTTP 403", fixed = TRUE)
  expect_match(msg, "Just a moment", fixed = TRUE)
  expect_match(msg, "agregados/5938/metadados", fixed = TRUE)
})

test_that("a transport error is a reason, not an exception, and names the URL", {
  msg <- error_message(fake_ibge(override = function(kind, url) if (kind == "coverage") stop("Could not resolve host") else NULL))
  expect_match(msg, "Could not resolve host", fixed = TRUE)
  expect_match(msg, "v/37/p/all", fixed = TRUE)
})

test_that("each way the table's answer can be wrong is a reason of its own", {
  expect_match(error_message(fake_ibge(metadata = "not json")), "not a SIDRA table description", fixed = TRUE)
  expect_match(error_message(fake_ibge(metadata = metadata_json(id = 5939L))), "the description is for table 5939, not 5938", fixed = TRUE)
  expect_match(error_message(fake_ibge(coverage = "<html>Just a moment</html>")), "not a SIDRA value table", fixed = TRUE)
  two_classifications <- c(classification_99, list(list(id = 98L, nome = "Outra", categorias = list(list(id = 1L, nome = "x", unidade = NULL, nivel = 1L)))))
  expect_match(error_message(fake_ibge(metadata = metadata_json(classifications = two_classifications))), "has 2 classifications", fixed = TRUE)
})

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_pibmunic(manifest_rows[0, ]), "no manifest rows tagged resolver == 'pibmunic'")
  expect_error(resolve_pibmunic(NULL), "no manifest rows tagged resolver == 'pibmunic'")
  keyed <- manifest_rows
  keyed$geo_level <- "state"
  expect_error(resolve_pibmunic(keyed), "every pibmunic row is unkeyed")
  no_code <- manifest_rows
  no_code$sidra_code <- NA_character_
  expect_error(resolve_pibmunic(no_code), "needs a sidra_code")
})

# ---- the code's grammar, and the committed row ---------------------------------------------

test_that("pibmunic_parse_code reads <table> and <table>/c<n>/<category> and rejects everything else", {
  expect_equal(pibmunic_parse_code("5938"), list(table = "5938", classific = NA_character_, category = NA_character_))
  expect_equal(pibmunic_parse_code("74/c80/all"), list(table = "74", classific = "c80", category = "all"))
  expect_equal(pibmunic_parse_code("74/c80/7")$category, "7")
  expect_equal(pibmunic_parse_code("5938/all/all")$classific, "all") # parses, so that it can be refused with a reason
  for (bad in c("", "abc", "5938/c99", "5938/c99/", "/c99/all", "5938/x99/all", NA_character_)) {
    expect_null(pibmunic_parse_code(bad), info = bad)
  }
  expect_null(pibmunic_parse_code(NULL))
})

test_that("the package's sidra_code_parts() (what load_pibmunic() uses) reads the codes the way this resolver does", {
  bare <- sidra_code_parts("5938")
  expect_equal(bare$table, as.numeric(pibmunic_parse_code("5938")$table))
  expect_null(bare$classific)
  expect_null(bare$category)
  classified <- sidra_code_parts("5938/c99/all")
  expect_equal(classified$table, 5938)
  expect_equal(classified$classific, pibmunic_parse_code("5938/c99/all")$classific)
  expect_equal(classified$category, list(pibmunic_parse_code("5938/c99/all")$category))
})

test_that("the committed row is verifiable: owned by the resolver, a bare table code, known geo levels, a table page for docs_url", {
  expect_equal(nrow(manifest_rows), 1)
  expect_equal(manifest_rows$dataset, "pibmunic")
  expect_equal(manifest_rows$resolver, "pibmunic")
  expect_equal(manifest_rows$sidra_code, "5938")
  expect_true(is.na(manifest_rows$geo_level) && is.na(manifest_rows$year))
  geo <- trimws(strsplit(manifest_rows$available_geo, ",")[[1]])
  expect_true(all(geo %in% names(SIDRA_API_GEO_LEVELS)))
  expect_match(manifest_rows$docs_url, SIDRA_API_TABLE_PAGE)
  expect_true(is.na(manifest_rows$url))
})

test_that("a changed pibmunic row lists the FRAGILE notes of the two files that treat it (build_manifest.R reads FRAGILITY_SOURCES)", {
  frag_env <- new.env()
  source(file.path(actions_dir, "scripts", "fragility_notes.R"), local = frag_env)
  entries <- Filter(function(e) identical(e$changed_prefix, "pibmunic"), frag_env$FRAGILITY_SOURCES)
  expect_setequal(vapply(entries, function(e) e$label, ""), c("R/pibmunic.R", "R/download.R"))
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
