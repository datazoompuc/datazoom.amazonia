# tests/testthat/test-cempre-resolver.R
#
# Offline test for actions/scrapers/resolve_cempre.R (added 2026-11-23; see that file's header
# for what was measured live). CEMPRE's one manifest row is read from IBGE's SIDRA API, not
# from a file: its `sidra_code` is "9418/c12762/117897" -- the open table, the CNAE
# classification and the Total -- and load_cempre() asks the closed table 6449 for 2006-2021.
# What goes stale is available_time, the code, the table map and the sector / variable lists
# the loader keeps copies of. The resolver is a thin layer on actions/scripts/sidra_api.R
# (tested for itself in test-sidra-api.R).
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was measured live
# on 2026-11-23: /metadados describes each table (the CNAE with its Total, the 21 sections and
# one division as a stand-in for the other 1,044 categories; four variables and their
# "percentual do total geral" twins), the coverage query is a JSON array whose first element
# is a header row, one row per year and category, "..." where nothing is published.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
# the metadata lookup, coverage query and value coding CEMPRE shares with the other SIDRA
# resolvers, as build_manifest.R sources it before the resolvers
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_cempre.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed row, not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "cempre", ]

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- a fake IBGE ------------------------------------------------------------

ALL_LEVELS <- c("N1", "N2", "N6", "N3")

# table -> which years carry a value for the Total (checked live 2026-11-23)
FAKE_TABLES <- list(
  `6449` = list(years = 2006:2021, name = "Empresas e outras organizacoes ... (CNAE 2.0) - serie encerrada em 2021"),
  `9418` = list(years = 2022:2024, name = "Empresas e outras organizacoes ... (CNAE 2.0)")
)

SECTION_NAMES <- stats::setNames(
  paste(LETTERS[1:21], "Secao"), CEMPRE_SECTORS[-1]
)

# the CNAE of both tables: the Total, the 21 sections, and a division (a digit, not a capital letter)
classification_cnae <- function(sections = SECTION_NAMES, total_name = "Total", extra = list()) {
  cats <- c(
    list(list(id = 117897L, nome = total_name, unidade = NULL, nivel = 0L)),
    lapply(seq_along(sections), function(i) list(id = as.integer(names(sections)[i]), nome = sections[[i]], unidade = NULL, nivel = 1L)),
    list(list(id = 116831L, nome = "01 Agricultura, pecuaria e servicos relacionados", unidade = NULL, nivel = 2L)),
    extra
  )
  list(list(id = 12762L, nome = "Classificacao Nacional de Atividades Economicas (CNAE 2.0)", categorias = cats))
}

# the four variables of both tables, each followed by its "percentual do total geral" twin
VARIABLES <- data.frame(
  id = c(2585L, 1002585L, 707L, 1000707L, 708L, 1000708L, 662L, 1000662L),
  nome = c(
    "Numero de empresas", "Numero de empresas - percentual do total geral", "Pessoal ocupado total",
    "Pessoal ocupado total - percentual do total geral", "Pessoal ocupado assalariado",
    "Pessoal ocupado assalariado - percentual do total geral", "Salarios e outras remuneracoes",
    "Salarios e outras remuneracoes - percentual do total geral"
  ),
  unidade = c("Unidades", "%", "Pessoas", "%", "Pessoas", "%", "Mil Reais", "%"),
  stringsAsFactors = FALSE
)

metadata_json <- function(table, variables = NULL, classifications = NULL, levels = ALL_LEVELS, id = as.integer(table),
                          url = paste0("https://sidra.ibge.gov.br/tabela/", table), name = NULL, span = NULL) {
  spec <- FAKE_TABLES[[table]]
  spec$years <- span %||% spec$years
  variables <- variables %||% VARIABLES
  classifications <- classifications %||% classification_cnae()
  as.character(jsonlite::toJSON(list(
    id = id, nome = name %||% spec$name, URL = url, pesquisa = "Cadastro Central de Empresas",
    periodicidade = list(frequencia = "anual", inicio = min(spec$years), fim = max(spec$years)),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = lapply(seq_len(nrow(variables)), function(i) as.list(variables[i, ])),
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

COVERAGE_HEADER <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano",
  D4C = "Classificacao (Codigo)", D4N = "Classificacao"
)

# Brazil level, one row per year and per category asked for; "..." where nothing is published.
# `published` is the years with a value (for every category); `span` the years the table has
# (default: FAKE_TABLES's).
coverage_json <- function(table, variable, published = NULL, categories = c("117897", names(SECTION_NAMES)), span = NULL) {
  spec <- FAKE_TABLES[[table]]
  spec$years <- span %||% spec$years
  published <- published %||% spec$years
  rows <- list(COVERAGE_HEADER)
  for (category in categories) {
    for (y in spec$years) {
      rows[[length(rows) + 1]] <- list(
        NC = "1", NN = "Brasil", MC = "1", MN = "Unidades", V = if (y %in% published) as.character(1000L + y) else "...",
        D1C = "1", D1N = "Brasil", D2C = variable, D2N = "variavel", D3C = as.character(y), D3N = as.character(y),
        D4C = category, D4N = "x"
      )
    }
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}

# `override(kind, table, url)` may return a response (or throw like curl does) to replace
# the healthy one; `kind` is "metadata" or "coverage". `metadata` / `coverage` replace what
# a table answers: function(table) / function(table, variable).
fake_ibge <- function(override = function(kind, table, url) NULL, seen = NULL,
                      metadata = metadata_json,
                      coverage = function(table, variable) coverage_json(table, variable)) {
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
run <- function(server = fake_ibge(), rows = manifest_rows) with_ibge(server, suppressMessages(resolve_cempre(rows)))
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
# (`span`: the years that table has, for the tables IBGE has extended or shortened)
with_table <- function(table, metadata = NULL, coverage = NULL, span = NULL, ...) {
  fake_ibge(
    metadata = function(t) if (t == table && !is.null(metadata)) metadata(t) else metadata_json(t, span = if (t == table) span),
    coverage = function(t, variable) if (t == table && !is.null(coverage)) coverage(t, variable) else coverage_json(t, variable, span = if (t == table) span),
    ...
  )
}
with_code <- function(code) {
  rows <- manifest_rows
  rows$sidra_code <- code
  rows
}

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves the row: 4 requests, available_time the two tables' years joined, docs_url the open table's page", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 1)
  expect_equal(out$survey, "cempre")
  expect_equal(out$dataset, "cempre")
  expect_equal(out$resolver, "cempre")
  expect_true(is.na(out$geo_level) && is.na(out$year))
  expect_false("sidra_code" %in% names(out))
  expect_equal(out$available_time, "2006-2024")
  expect_equal(out$docs_url, "https://sidra.ibge.gov.br/tabela/9418")
  expect_null(attr(out, "partial_failures"))

  # two requests per table: metadata and coverage of every category of the CNAE, by the
  # number of companies (2585)
  expect_length(seen$urls, 4)
  expect_equal(sum(grepl("/metadados$", seen$urls)), 2)
  expect_true(any(grepl("/agregados/9418/metadados$", seen$urls)))
  expect_true(any(grepl("/agregados/6449/metadados$", seen$urls)))
  expect_true(any(grepl("/t/9418/n1/all/v/2585/p/all/c12762/all$", seen$urls)))
  expect_true(any(grepl("/t/6449/n1/all/v/2585/p/all/c12762/all$", seen$urls)))
})

test_that("available_time follows the data: a new year in the open table extends it, a year it lacks shortens it", {
  out <- run(with_table("9418", span = 2022:2025))
  expect_equal(out$available_time, "2006-2025")

  fewer <- function(table, variable) coverage_json(table, variable, published = 2022:2023)
  out <- run(with_table("9418", coverage = fewer))
  expect_equal(out$available_time, "2006-2023")
})

test_that("docs_url is the open table's own page, taken from the metadata only when it is a SIDRA table page", {
  survey_page <- function(table) metadata_json(table, url = "https://sidra.ibge.gov.br/pesquisa/cempre/tabelas")
  out <- run(with_table("9418", metadata = survey_page))
  expect_true(is.na(out$docs_url))
  expect_equal(out$available_time, "2006-2024")
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(grep("^(resolve|watch)_", ls(resolver_env), value = TRUE), "resolve_cempre")
})

# ---- the copies of the loader's lists ------------------------------------------------------

test_that("CEMPRE_SECTORS, CEMPRE_VARIABLE_INDEX and CEMPRE_CLOSED_TABLES are copies of the loader's own: cempre_sectors, cempre_variable_index, cempre_closed_tables", {
  expect_equal(CEMPRE_SECTORS, cempre_sectors)
  expect_equal(CEMPRE_VARIABLE_INDEX, cempre_variable_index)
  expect_length(CEMPRE_CLOSED_TABLES, length(cempre_closed_tables))
  for (i in seq_along(cempre_closed_tables)) {
    expect_equal(as.numeric(CEMPRE_CLOSED_TABLES[[i]]$table), cempre_closed_tables[[i]]$table)
    expect_equal(CEMPRE_CLOSED_TABLES[[i]]$last_year, cempre_closed_tables[[i]]$last_year)
  }
  expect_equal(CEMPRE_TOTAL, cempre_sectors[1])
})

test_that("the dictionary's 88 id_codes are exactly the 22 sectors times the 4 variables: <sector code>_<k>, which is what the 88-branch case_when() it replaced produced", {
  dic <- load_dictionary("cempre")
  expected <- as.vector(outer(cempre_sectors, unname(cempre_variable_index), paste, sep = "_"))
  expect_length(expected, 88)
  expect_setequal(as.character(dic$id_code), expected)
  expect_false(anyDuplicated(dic$id_code) > 0)
  # the variable of an id_code is the one whose k ends it: the units say so (1 companies, 2 and 3 people, 4 thousand BRL)
  k <- as.integer(sub("^.*_", "", dic$id_code))
  expect_true(all(dic$unit_value[k == 1] == "1K") && all(dic$unit_quantity[k == 1] == "Units"))
  expect_true(all(dic$unit_quantity[k %in% 2:3] == "People"))
  expect_true(all(dic$unit_value[k == 4] == "1K BRL"))
})

# ---- the tables --------------------------------------------------------------------------

test_that("each table is checked: one that cannot be read fails the row and says which", {
  challenge <- resp(403L, "<html><head><title>Just a moment...</title></head><body>Checking</body></html>")
  for (table in c("9418", "6449")) {
    msg <- error_message(fake_ibge(override = function(kind, t, url) if (kind == "metadata" && t == table) challenge else NULL))
    expect_match(msg, "all 1 row(s) failed verification", fixed = TRUE, info = table)
    expect_match(msg, "HTTP 403", fixed = TRUE, info = table)
    expect_match(msg, "Just a moment", fixed = TRUE, info = table)
    expect_match(msg, sprintf("agregados/%s/metadados", table), fixed = TRUE, info = table)
  }
})

test_that("the closed table must still have the last year the loader asks it for, and says which years it has", {
  elsewhere <- function(table, variable) coverage_json(table, variable, published = 2006:2020)
  msg <- error_message(with_table("6449", coverage = elsewhere))
  expect_match(msg, "table 6449 has no value for 2021, the last year load_cempre() asks it for (it has: 2006-2020)", fixed = TRUE)
})

test_that("the open table with no published year fails the row", {
  none <- function(table, variable) coverage_json(table, variable, published = integer(0))
  expect_match(error_message(with_table("9418", coverage = none)), "no year with a published value at Brazil level", fixed = TRUE)
})

test_that("a table that is not the CNAE fails the row: the loader asks for c12762", {
  other <- list(list(id = 99L, nome = "Outra", categorias = list(list(id = 117897L, nome = "Total", unidade = NULL, nivel = 0L))))
  msg <- error_message(with_table("9418", metadata = function(t) metadata_json(t, classifications = other)))
  expect_match(msg, "classification is c99, not c12762 (the CNAE 2.0)", fixed = TRUE)
})

test_that("a sector the table lost fails the row, naming the code: sectors = TRUE downloads it", {
  fewer <- SECTION_NAMES[-3]
  msg <- error_message(with_table("6449", metadata = function(t) metadata_json(t, classifications = classification_cnae(sections = fewer))))
  expect_match(msg, "it has no category 116910, which load_cempre(sectors = TRUE) downloads", fixed = TRUE)
})

test_that("the Total must still be called 'Total', and the sector codes must still be sections", {
  msg <- error_message(with_table("9418", metadata = function(t) metadata_json(t, classifications = classification_cnae(total_name = "Todas"))))
  expect_match(msg, "category 117897, which load_cempre() asks for as the Total, is called 'Todas'", fixed = TRUE)

  renamed <- SECTION_NAMES
  renamed[["116880"]] <- "01 Agora uma divisao"
  msg <- error_message(with_table("9418", metadata = function(t) metadata_json(t, classifications = classification_cnae(sections = renamed))))
  expect_match(msg, "the sector code(s) 116880 are no longer the sections of the classification", fixed = TRUE)
})

test_that("a section IBGE adds is a NOTE: the row is still refreshed, and the note says which and where to add it", {
  extra <- list(list(id = 118000L, nome = "V Nova secao", unidade = NULL, nivel = 1L))
  out <- run(with_table("9418", metadata = function(t) metadata_json(t, classifications = classification_cnae(extra = extra))))
  expect_equal(out$available_time, "2006-2024")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "cempre/cempre (9418/c12762/117897)", fixed = TRUE)
  expect_match(notes, "table 9418 has section(s) 118000 (V Nova secao) that load_cempre(sectors = TRUE) does not download", fixed = TRUE)
  expect_match(notes, "cempre_sectors, CEMPRE_SECTORS and the dictionary", fixed = TRUE)
})

test_that("a territorial level the manifest promises and a table lacks fails the row, naming the table", {
  no_municipality <- function(t) metadata_json(t, levels = c("N1", "N2", "N3"))
  for (table in c("9418", "6449")) {
    msg <- error_message(with_table(table, metadata = no_municipality))
    expect_match(msg, sprintf("table %s's metadata has no territorial level N6", table), fixed = TRUE, info = table)
  }
})

test_that("a table that is not the one the description is for fails with that said", {
  msg <- error_message(with_table("6449", metadata = function(t) metadata_json(t, id = 6450L)))
  expect_match(msg, "the description is for table 6450, not 6449", fixed = TRUE)
})

# ---- the variables ------------------------------------------------------------------------

test_that("the four variables are known, and their percentual-do-total-geral twins are no variable: a healthy IBGE declares eight and notes nothing", {
  expect_null(attr(run(fake_ibge()), "partial_failures"))
  expect_equal(names(CEMPRE_VARIABLE_INDEX), c("2585", "707", "708", "662"))
})

test_that("a variable a table starts to declare FAILS the row: a column of sectors = TRUE would be named _v<id> instead of _<k>", {
  more <- rbind(VARIABLES, data.frame(id = 9999L, nome = "Nova", unidade = "Unidades", stringsAsFactors = FALSE))
  msg <- error_message(with_table("9418", metadata = function(t) metadata_json(t, variables = more)))
  expect_match(msg, "table 9418: it now declares variable(s) 9999 that load_cempre() does not index", fixed = TRUE)
  expect_match(msg, "cempre_variable_index and in CEMPRE_VARIABLE_INDEX", fixed = TRUE)
})

test_that("a new 'percentual do total geral' twin is no new variable (the name decides, not the unit), but any other variable with unit '%' is one", {
  twin <- rbind(VARIABLES, data.frame(id = 1000999L, nome = "Qualquer coisa - percentual do total geral", unidade = "%", stringsAsFactors = FALSE))
  expect_null(attr(run(with_table("9418", metadata = function(t) metadata_json(t, variables = twin))), "partial_failures"))

  share <- rbind(VARIABLES, data.frame(id = 9998L, nome = "Participacao no total", unidade = "%", stringsAsFactors = FALSE))
  expect_match(error_message(with_table("9418", metadata = function(t) metadata_json(t, variables = share))), "declares variable(s) 9998", fixed = TRUE)
})

test_that("a variable a table stops declaring is a NOTE: the row is refreshed, the note says which columns will be missing", {
  fewer <- VARIABLES[!(VARIABLES$id %in% c(662L, 1000662L)), ]
  out <- run(with_table("9418", metadata = function(t) metadata_json(t, variables = fewer)))
  expect_equal(out$available_time, "2006-2024")
  expect_match(attr(out, "partial_failures"), "table 9418 no longer declares variable(s) 662: their columns will be missing", fixed = TRUE)

  # the coverage variable is the first the table declares of the loader's four, 2585 first
  seen <- new.env()
  run(with_table("9418", metadata = function(t) metadata_json(t, variables = VARIABLES[VARIABLES$id != 2585L, ]), seen = seen))
  expect_true(any(grepl("/t/9418/n1/all/v/707/p/all/c12762/all$", seen$urls)))
})

# ---- the series' two tables ----------------------------------------------------------------

test_that("the closed table now publishing a year past its last is a NOTE: the loader still stops it there, and the note says so", {
  out <- run(with_table("6449", span = 2006:2022))
  expect_equal(out$available_time, "2006-2024")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "closed table 6449 now has 2022 too: load_cempre() still asks table 9418 only up to 2021", fixed = TRUE)
})

test_that("the open table now publishing a year of the closed table is a NOTE too: the loader still asks the closed one for it", {
  out <- run(with_table("9418", span = 2021:2024))
  expect_equal(out$available_time, "2006-2024")
  expect_match(attr(out, "partial_failures"), "table 9418 now has 2021 too: load_cempre() still asks the closed table for them", fixed = TRUE)
})

test_that("a gap inside the closed table's span is a NOTE saying which years the loader will get nothing for", {
  gappy <- function(table, variable) coverage_json(table, variable, published = setdiff(2006:2021, 2012))
  out <- run(with_table("6449", coverage = gappy))
  expect_equal(out$available_time, "2006-2011, 2013-2024")
  expect_match(attr(out, "partial_failures"), "closed table 6449 has no value for 2012, years load_cempre() asks it for", fixed = TRUE)
})

test_that("the open table's name saying it is closed is a NOTE: IBGE must have opened a successor, and the note says what to edit", {
  closing <- function(t) metadata_json(t, name = "Empresas ... (CNAE 2.0) - serie encerrada em 2024")
  out <- run(with_table("9418", metadata = closing))
  expect_equal(out$available_time, "2006-2024")
  notes <- attr(out, "partial_failures")
  expect_length(notes, 1)
  expect_match(notes, "table 9418 says it is closed", fixed = TRUE)
  expect_match(notes, "cempre_closed_tables and CEMPRE_CLOSED_TABLES", fixed = TRUE)

  # the closed table's own name says so too, and is no note
  expect_null(attr(run(fake_ibge()), "partial_failures"))
})

# ---- the code's meaning --------------------------------------------------------------------

test_that("a bare table number fails the row, and the message gives the code to write: a request without a classification would return only the Total slice", {
  msg <- error_message(fake_ibge(), with_code("9418"))
  expect_match(msg, "names no classification, but table 9418 has one (c12762)", fixed = TRUE)
  expect_match(msg, "'Total' slice", fixed = TRUE)
  expect_match(msg, "write '9418/c12762/117897'", fixed = TRUE)
})

test_that("classific = 'all' is refused with the id to use: sidrar would scrape an IBGE page behind a bot challenge", {
  msg <- error_message(fake_ibge(), with_code("9418/all/117897"))
  expect_match(msg, "uses classific = 'all'", fixed = TRUE)
  expect_match(msg, "write '9418/c12762/117897' instead", fixed = TRUE)
})

test_that("a code that names another classification, or a category the table does not have, is refused", {
  expect_match(error_message(fake_ibge(), with_code("9418/c99/117897")), "classification c99 is not the CNAE (c12762)", fixed = TRUE)
  msg <- error_message(fake_ibge(), with_code("9418/c12762/999999"))
  expect_match(msg, "category 999999 does not exist in classification c12762 of table 9418", fixed = TRUE)
  expect_match(msg, "HTTP 200", fixed = TRUE)
  # 'all' categories is a valid code (a row for the whole classification)
  expect_equal(run(fake_ibge(), with_code("9418/c12762/all"))$available_time, "2006-2024")
})

test_that("a sidra_code that is not <table>/c<n>/<id or all> fails its row", {
  for (code in c("9418/c12762", "abc", "9418/c12762/117897/extra", "9418/c12762/")) {
    msg <- error_message(fake_ibge(), with_code(code))
    expect_match(msg, "is not <table>/c<classification>/<category id or all>", fixed = TRUE, info = code)
  }
})

test_that("cempre_parse_code reads <table> and <table>/c<n>/<category> and rejects everything else", {
  expect_equal(cempre_parse_code("9418/c12762/117897"), list(table = "9418", classific = "c12762", category = "117897"))
  expect_equal(cempre_parse_code("6449"), list(table = "6449", classific = NA_character_, category = NA_character_))
  expect_equal(cempre_parse_code("9418/all/all")$classific, "all") # parses, so that it can be refused with a reason
  for (bad in c("", "abc", "9418/c12762", "9418/c12762/", "/c12762/all", "9418/x12762/all", NA_character_)) {
    expect_null(cempre_parse_code(bad), info = bad)
  }
  expect_null(cempre_parse_code(NULL))
})

test_that("the package's sidra_code_parts() (what load_cempre() uses) reads the committed code the way this resolver does", {
  mine <- cempre_parse_code(manifest_rows$sidra_code)
  theirs <- sidra_code_parts(manifest_rows$sidra_code)
  expect_equal(theirs$table, as.numeric(mine$table))
  expect_equal(theirs$classific, mine$classific)
  expect_equal(as.character(unlist(theirs$category)), mine$category)
})

# ---- the territorial levels -----------------------------------------------------------------

test_that("an available_geo level a table lacks fails the row; an unknown word does too; NA is skipped", {
  rows <- manifest_rows
  rows$available_geo <- "country, galaxy"
  expect_match(error_message(fake_ibge(), rows), "'galaxy'", fixed = TRUE)
  rows$available_geo <- NA_character_
  expect_null(attr(run(fake_ibge(), rows), "partial_failures"))
})

# ---- the tables' answers ----------------------------------------------------------------------

test_that("a transport error is a reason, not an exception, and names the URL", {
  msg <- error_message(fake_ibge(override = function(kind, table, url) if (kind == "coverage" && table == "9418") stop("Could not resolve host") else NULL))
  expect_match(msg, "Could not resolve host", fixed = TRUE)
  expect_match(msg, "v/2585/p/all/c12762/all", fixed = TRUE)
})

test_that("each way a table's answer can be wrong is a reason of its own", {
  expect_match(error_message(with_table("9418", metadata = function(t) "not json")), "not a SIDRA table description", fixed = TRUE)
  expect_match(error_message(with_table("9418", coverage = function(t, v) "<html>Just a moment</html>")), "not a SIDRA value table", fixed = TRUE)
  two <- c(classification_cnae(), classification_cnae())
  expect_match(error_message(with_table("9418", metadata = function(t) metadata_json(t, classifications = two))), "has 2 classifications", fixed = TRUE)
})

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_cempre(manifest_rows[0, ]), "no manifest rows tagged resolver == 'cempre'")
  expect_error(resolve_cempre(NULL), "no manifest rows tagged resolver == 'cempre'")
  keyed <- manifest_rows
  keyed$geo_level <- "state"
  expect_error(resolve_cempre(keyed), "every cempre row is unkeyed")
  no_code <- manifest_rows
  no_code$sidra_code <- NA_character_
  expect_error(resolve_cempre(no_code), "needs a sidra_code")
})

# ---- the committed row ----------------------------------------------------------------------------

test_that("the committed row is verifiable: owned by the resolver, the open table with its classification and Total, known geo levels, a table page for docs_url, years 2006 to 2024", {
  expect_equal(nrow(manifest_rows), 1)
  expect_equal(manifest_rows$resolver, "cempre")
  expect_equal(manifest_rows$sidra_code, "9418/c12762/117897")
  expect_true(is.na(manifest_rows$geo_level) && is.na(manifest_rows$year))
  geo <- trimws(strsplit(manifest_rows$available_geo, ",")[[1]])
  expect_true(all(geo %in% names(SIDRA_API_GEO_LEVELS)))
  expect_setequal(geo, c("country", "state", "municipality"))
  expect_match(manifest_rows$docs_url, SIDRA_API_TABLE_PAGE)
  expect_equal(manifest_rows$docs_url, "https://sidra.ibge.gov.br/tabela/9418")
  expect_true(is.na(manifest_rows$url))
  expect_equal(parse_years(manifest_rows$available_time), 2006:2024)
})

test_that("a changed cempre row lists the FRAGILE notes of the two files that treat it (build_manifest.R reads FRAGILITY_SOURCES)", {
  frag_env <- new.env()
  source(file.path(actions_dir, "scripts", "fragility_notes.R"), local = frag_env)
  entries <- Filter(function(e) identical(e$changed_prefix, "cempre"), frag_env$FRAGILITY_SOURCES)
  expect_setequal(vapply(entries, function(e) e$label, ""), c("R/cempre.R", "R/download.R"))
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
