# tests/testthat/test-sidra-api.R
#
# Offline test for actions/scripts/sidra_api.R, the metadata lookup, coverage
# query and value coding every SIDRA resolver shares (extracted from
# resolve_pam.R and resolve_ppm.R, which had the same ~150 lines under two
# prefixes). The resolvers' own tests (test-pam-resolver.R, test-ppm-resolver.R)
# exercise it through them; this file tests it for itself, including the
# variants only one of them uses: a table with no classification and the
# fall-back to a table's first variable.
#
# curl::curl_fetch_memory is mocked (no network).

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

source(file.path(actions_dir, "scripts", "sidra_api.R"), local = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- pure helpers -----------------------------------------------------------

test_that("sidra_api_years_to_available_time writes the grammar parse_years() reads, gaps included", {
  cases <- list(
    list(c(2005L), "2005"),
    list(1974:2025, "1974-2025"),
    list(c(1974:1990, 1995:2025), "1974-1990, 1995-2025"),
    list(c(2000L, 2002L), "2000, 2002"),
    list(c(2025L, 1974L, 1974L, 1975L), "1974-1975, 2025"), # unsorted, duplicated
    list(c(1988:2000, 2003L, 2010:2012), "1988-2000, 2003, 2010-2012")
  )
  for (case in cases) {
    expect_equal(sidra_api_years_to_available_time(case[[1]]), case[[2]])
    expect_equal(parse_years(sidra_api_years_to_available_time(case[[1]])), sort(unique(case[[1]])))
  }
  expect_true(is.na(sidra_api_years_to_available_time(integer(0))))
})

HEADER_ROW <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano",
  D4C = "Categoria (Codigo)", D4N = "Categoria"
)

test_that("sidra_api_parse_coverage: '...' and '..' are unpublished; numbers, '-' and 'X' are published; with or without a category", {
  values <- c("123", "-", "X", "...", "..", "0")
  make <- function(classified) {
    rows <- list(if (classified) HEADER_ROW else HEADER_ROW[setdiff(names(HEADER_ROW), c("D4C", "D4N"))])
    for (i in seq_along(values)) {
      rows[[i + 1]] <- c(list(V = values[i], D3C = as.character(2000 + i), D3N = "Ano"), if (classified) list(D4C = "7"))
    }
    rows[[length(rows) + 1]] <- c(list(V = NULL, D3C = "2099", D3N = "Ano"), if (classified) list(D4C = "7")) # null value
    as.character(jsonlite::toJSON(rows, auto_unbox = TRUE, null = "null"))
  }

  with_category <- sidra_api_parse_coverage(make(TRUE), classified = TRUE)
  expect_equal(with_category$problem, character(0))
  expect_equal(with_category$years[["7"]], c(2001L, 2002L, 2003L, 2006L)) # 123, -, X, 0

  without <- sidra_api_parse_coverage(make(FALSE), classified = FALSE)
  expect_equal(without$problem, character(0))
  expect_equal(without$years[["(none)"]], c(2001L, 2002L, 2003L, 2006L))
})

test_that("sidra_api_parse_coverage names what is wrong with a response that is not the value table it asked for", {
  as_json <- function(rows) as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
  cell <- function(year, d3n = "Ano") list(V = "1", D3C = year, D3N = d3n, D4C = "7")
  header <- list(HEADER_ROW)

  expect_match(sidra_api_parse_coverage("<html>Just a moment</html>", TRUE)$problem, "not a SIDRA value table")
  # the header row names what the third dimension is
  month_header <- list(modifyList(HEADER_ROW, list(D3N = "Mes")))
  expect_match(sidra_api_parse_coverage(as_json(c(month_header, list(cell("2001")))), TRUE)$problem, "third dimension is 'Mes', not 'Ano'")
  expect_match(sidra_api_parse_coverage(as_json(header), TRUE)$problem, "no data rows at all")
  expect_match(sidra_api_parse_coverage(as_json(c(header, list(cell("200101")))), TRUE)$problem, "not four-digit years")
})

test_that("sidra_api_snippet shows text, strips markup, and gives up on binary", {
  expect_equal(sidra_api_snippet(raw(0)), "")
  expect_equal(sidra_api_snippet(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x01))), "")
  expect_equal(sidra_api_snippet(charToRaw("<p>Tabela   99999:\nTabela invalida</p>")), "Tabela 99999: Tabela invalida")
  expect_equal(sidra_api_snippet(charToRaw("<style>.x{}</style><script>var a=1;</script>Just a moment...")), "Just a moment...")
  expect_equal(nchar(sidra_api_snippet(charToRaw(strrep("a", 500)), n = 50)), 53)
})

test_that("sidra_api_parse_metadata reads classifications, categories, levels, variables and the table page", {
  meta <- as.character(jsonlite::toJSON(list(
    id = 74L, nome = "x", URL = "https://sidra.ibge.gov.br/tabela/74",
    nivelTerritorial = list(Administrativo = c("N1", "N3", "N6"), Especial = list(), IBGE = list()),
    variaveis = list(
      list(id = 106L, nome = "Producao", unidade = "Vide categorias"),
      list(id = 215L, nome = "Valor da producao", unidade = "Mil Reais"),
      list(id = 1000215L, nome = "Valor da producao - percentual do total geral", unidade = "%")
    ),
    classificacoes = list(list(id = 80L, nome = "Tipo", categorias = list(
      list(id = 0L, nome = "Total", unidade = "Nenhuma"), list(id = 2682L, nome = "Leite", unidade = "Mil litros")
    )))
  ), auto_unbox = TRUE))
  parsed <- sidra_api_parse_metadata(meta, "74")

  expect_equal(parsed$problem, character(0))
  expect_equal(parsed$classifications, list(`80` = c("0", "2682")))
  expect_equal(parsed$category_info[["80"]]$nome, c("Total", "Leite"))
  expect_equal(parsed$category_info[["80"]]$unidade, c("Nenhuma", "Mil litros"))
  expect_equal(parsed$levels, c("N1", "N3", "N6"))
  expect_equal(parsed$variables, c("106", "215")) # the percentage twin is left out
  expect_equal(parsed$variable_info$nome, c("Producao", "Valor da producao"))
  expect_equal(parsed$url, "https://sidra.ibge.gov.br/tabela/74")

  # a table with no classification at all (PPM's 94 and 95)
  none <- sidra_api_parse_metadata(as.character(jsonlite::toJSON(list(
    id = 94L, variaveis = list(list(id = 107L, nome = "Vacas", unidade = "Cabecas")), classificacoes = list()
  ), auto_unbox = TRUE)), "94")
  expect_equal(none$problem, character(0))
  expect_length(none$classifications, 0)
  expect_equal(none$variables, "107")
  expect_true(is.na(none$url))

  expect_match(sidra_api_parse_metadata("{}", "74")$problem, "not a SIDRA table description")
  expect_match(sidra_api_parse_metadata(sub("74", "75", meta, fixed = TRUE), "74")$problem, "the description is for table 75, not 74")
})

test_that("sidra_api_geo_problem and sidra_api_row_years and sidra_api_docs_url", {
  levels <- c("N1", "N3", "N6")
  expect_equal(sidra_api_geo_problem("country, state, municipality", levels, "74"), character(0))
  expect_equal(sidra_api_geo_problem(NA_character_, levels, "74"), character(0))
  expect_match(sidra_api_geo_problem("country, galaxy", levels, "74"), "available_geo names 'galaxy'", fixed = TRUE)
  expect_match(sidra_api_geo_problem("country, region", levels, "74"), "has no territorial level N2", fixed = TRUE)

  tab <- list(years = list(`1` = 2000:2005, `2` = 2003:2010))
  expect_equal(sidra_api_row_years(tab, "all"), 2000:2010)
  expect_equal(sidra_api_row_years(tab, NA_character_), 2000:2010)
  expect_equal(sidra_api_row_years(tab, "2"), 2003:2010)
  expect_null(sidra_api_row_years(tab, "3"))

  expect_equal(sidra_api_docs_url(list(url = "https://sidra.ibge.gov.br/tabela/74")), "https://sidra.ibge.gov.br/tabela/74")
  expect_true(is.na(sidra_api_docs_url(list(url = "https://example.org/x"))))
  expect_true(is.na(sidra_api_docs_url(list(url = NA_character_))))
  expect_true(is.na(sidra_api_docs_url(list(url = "https://sidra.ibge.gov.br/pesquisa/pam/tabelas"))))
})

# ---- sidra_api_check_table against a fake IBGE ------------------------------------

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}
metadata <- function(table, variables, classified = TRUE, classifications = NULL) {
  as.character(jsonlite::toJSON(list(
    id = as.integer(table), URL = paste0("https://sidra.ibge.gov.br/tabela/", table),
    nivelTerritorial = list(Administrativo = c("N1", "N3", "N6")),
    variaveis = lapply(names(variables), function(v) list(id = as.integer(v), nome = paste("v", v), unidade = variables[[v]])),
    classificacoes = classifications %||% if (classified) list(list(id = 81L, nome = "x", categorias = list(list(id = 1L, nome = "a"), list(id = 2L, nome = "b")))) else list()
  ), auto_unbox = TRUE))
}
coverage <- function(classified) {
  rows <- list(if (classified) HEADER_ROW else HEADER_ROW[setdiff(names(HEADER_ROW), c("D4C", "D4N"))])
  for (ct in if (classified) c("1", "2") else "x") for (y in 2000:2003) {
    rows[[length(rows) + 1]] <- c(list(V = if (ct == "2" && y < 2002) "..." else "5", D3C = as.character(y), D3N = "Ano"), if (classified) list(D4C = ct))
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}
# a server answering for one table; `seen` collects the URLs asked
serve <- function(variables, classified = TRUE, classifications = NULL, seen = NULL, meta_override = NULL, cov_override = NULL) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("/metadados$", url)) {
      return(meta_override %||% resp(200L, metadata("77", variables, classified, classifications)))
    }
    cov_override %||% resp(200L, coverage(classified))
  }
}
check <- function(server, ...) {
  testthat::with_mocked_bindings(sidra_api_check_table("77", ...), curl_fetch_memory = server, .package = "curl")
}

test_that("sidra_api_check_table asks the coverage query for the first preferred variable the table declares", {
  seen <- new.env()
  tab <- check(serve(c(`109` = "Hectares", `214` = "Toneladas", `1000214` = "%"), seen = seen), c("215", "214"))

  expect_equal(tab$problem, character(0))
  expect_equal(seen$urls[2], "https://apisidra.ibge.gov.br/values/t/77/n1/all/v/214/p/all/c81/all")
  expect_equal(tab$years, list(`1` = 2000:2003, `2` = 2002:2003))
  expect_equal(tab$variables, c("109", "214"))
  expect_equal(tab$levels, c("N1", "N3", "N6"))
  expect_equal(tab$url, "https://sidra.ibge.gov.br/tabela/77")

  seen <- new.env()
  check(serve(c(`215` = "Mil Reais", `214` = "Toneladas"), seen = seen), c("215", "214"))
  expect_match(seen$urls[2], "/v/215/", fixed = TRUE)
})

test_that("a table with none of the preferred variables fails, or falls back to its first variable when the caller says so", {
  server <- function(seen = NULL) serve(c(`109` = "Hectares", `216` = "Hectares"), seen = seen)

  expect_equal(check(server(), c("215", "214"))$problem, "table 77 declares none of the variables 215/214")

  seen <- new.env()
  tab <- check(server(seen), "215", fall_back_to_first = TRUE)
  expect_equal(tab$problem, character(0))
  expect_match(seen$urls[2], "/v/109/", fixed = TRUE)

  # nothing but a percentage variable: nothing to fall back to
  expect_match(check(serve(c(`1000215` = "%")), "215", fall_back_to_first = TRUE)$problem, "declares no variable to measure coverage with")
})

test_that("a table with no classification is refused unless the caller allows it, and then takes the plain coverage query", {
  server <- function(seen = NULL) serve(c(`107` = "Cabecas"), classified = FALSE, seen = seen)

  expect_equal(check(server(), "107")$problem, "table 77 has no classification, and this resolver expects one")

  seen <- new.env()
  tab <- check(server(seen), "107", allow_no_classification = TRUE)
  expect_equal(tab$problem, character(0))
  expect_equal(seen$urls[2], "https://apisidra.ibge.gov.br/values/t/77/n1/all/v/107/p/all") # no /c<n>/all
  expect_equal(tab$years, list(`(none)` = 2000:2003))
  expect_length(tab$classifications, 0)
})

test_that("a table with two classifications is always refused", {
  two <- list(list(id = 81L, nome = "x", categorias = list(list(id = 1L, nome = "a"))), list(id = 99L, nome = "y", categorias = list(list(id = 1L, nome = "a"))))
  for (allow in c(FALSE, TRUE)) {
    expect_match(
      check(serve(c(`215` = "Mil Reais"), classifications = two), "215", allow_no_classification = allow)$problem,
      "table 77 has 2 classifications (c81, c99); the coverage query here handles at most one",
      fixed = TRUE
    )
  }
})

test_that("each way IBGE can answer wrongly is a returned problem naming the URL or the reason, never an exception", {
  vars <- c(`215` = "Mil Reais")
  cloudflare <- "<html><head><title>Just a moment...</title></head><body>Enable JavaScript</body></html>"

  expect_match(check(serve(vars, meta_override = resp(403L, cloudflare)), "215")$problem, "/agregados/77/metadados answered HTTP 403: Just a moment... Enable JavaScript", fixed = TRUE)
  expect_match(check(serve(vars, cov_override = resp(500L, "Internal Server Error")), "215")$problem, "answered HTTP 500: Internal Server Error", fixed = TRUE)
  expect_match(check(serve(vars, meta_override = resp(200L, "<html>not json</html>")), "215")$problem, "^metadata of table 77: the response is not a SIDRA table description")
  expect_match(check(serve(vars, cov_override = resp(200L, "[]")), "215")$problem, "^coverage of table 77: ")

  throwing <- function(url, handle = NULL, ...) stop("Timeout was reached")
  expect_match(check(throwing, "215")$problem, "could not reach https://servicodados.ibge.gov.br/api/v3/agregados/77/metadados (Timeout was reached)", fixed = TRUE)
})

test_that("a redirect is a problem, not something to follow; and requests identify themselves", {
  redirect <- function(url, handle = NULL, ...) resp(301L, "")
  fetched <- testthat::with_mocked_bindings(sidra_api_fetch("https://example.org/x"), curl_fetch_memory = redirect, .package = "curl")
  expect_equal(fetched$problem, "https://example.org/x answered HTTP 301")

  # the options of the handle every request is made with
  real_new_handle <- curl::new_handle
  options_seen <- NULL
  spy <- function(...) {
    options_seen <<- list(...)
    real_new_handle(...)
  }
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(sidra_api_fetch("https://example.org/x", timeout_s = 7), curl_fetch_memory = redirect, .package = "curl"),
    new_handle = spy, .package = "curl"
  )
  expect_false(options_seen$followlocation)
  expect_equal(options_seen$timeout, 7)
  expect_equal(options_seen$useragent, SIDRA_API_USER_AGENT)
  expect_match(SIDRA_API_USER_AGENT, "github.com/datazoompuc/datazoom.amazonia", fixed = TRUE)
})

test_that("the module defines no resolve_* or watch_* binding, so build_manifest.R never registers it as a resolver", {
  env <- new.env()
  source(file.path(actions_dir, "scripts", "sidra_api.R"), local = env)
  expect_length(ls(env, pattern = "^(resolve_|watch_)"), 0)
  expect_true(all(grepl("^(sidra_api_|SIDRA_API_)", ls(env))))
})
