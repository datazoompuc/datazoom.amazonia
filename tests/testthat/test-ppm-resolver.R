# tests/testthat/test-ppm-resolver.R
#
# Offline test for actions/scrapers/resolve_ppm.R (added 2026-10-22; see that
# file's header for what was measured live). PPM's 5 manifest rows are read from
# IBGE's SIDRA API, one table each: three tables have a classification ("74/c80/all")
# and two have none (a bare "94"), and what goes stale is available_time and, silently, the
# ids -- IBGE answers a query for something that does not exist with HTTP 200 and a
# table of "..." values, and a classified table queried without its classification
# with HTTP 200 and only the "Total" slice.
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was
# measured live on 2026-10-22.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
source(file.path(actions_dir, "scrapers", "resolve_ppm.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed rows (5 of them), not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "ppm", ]

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- a fake IBGE ------------------------------------------------------------

# table -> classification id (NULL: none), its categories, its variables
# (id = unit) and the years each category is published (default: the table's span)
FAKE_TABLES <- list(
  `74` = list(
    cl = "80", categories = c("0", "2682", "2683", "2684", "2685", "2686", "2687"),
    variables = c("106" = "Vide categorias", "215" = "Mil Reais", "1000215" = "%"), span = 1974:2025
  ),
  `3940` = list(
    cl = "654", categories = c("0", "32861", "32887", "79366"),
    variables = c("4146" = "Vide categorias", "215" = "Mil Reais", "1000215" = "%"), span = 2013:2025,
    years = list(`79366` = 2024:2025) # "Peixes": a category that appears in 2024
  ),
  `94` = list(cl = NULL, categories = NULL, variables = c("107" = "Cabecas"), span = 1974:2025),
  `3939` = list(
    cl = "79", categories = c("2670", "2675", "2672", "32794", "32795", "2681", "2677", "32796", "32793", "2680"),
    variables = c("105" = "Cabecas"), span = 1974:2025
  ),
  `95` = list(cl = NULL, categories = NULL, variables = c("108" = "Cabecas"), span = 1974:2025)
)

fake_years <- function(table, category) {
  spec <- FAKE_TABLES[[table]]
  spec$years[[category]] %||% spec$span
}

HEADER_ROW <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano",
  D4C = "Categoria (Codigo)", D4N = "Categoria"
)

metadata_json <- function(table, id = as.integer(table), levels = c("N1", "N2", "N3", "N6", "N8", "N9"),
                          extra_variables = character(0), url = paste0("https://sidra.ibge.gov.br/tabela/", table),
                          classifications = NULL) {
  spec <- FAKE_TABLES[[table]]
  variables <- c(spec$variables, extra_variables)
  classifications <- classifications %||% if (is.null(spec$cl)) {
    list()
  } else {
    list(list(
      id = as.integer(spec$cl), nome = "Classificacao",
      categorias = lapply(spec$categories, function(ct) list(id = as.integer(ct), nome = paste("categoria", ct)))
    ))
  }
  as.character(jsonlite::toJSON(list(
    id = id, nome = paste("Tabela", table), URL = url, pesquisa = "Pesquisa da Pecuaria Municipal",
    periodicidade = list(frequencia = "anual", inicio = min(spec$span), fim = max(spec$span)),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = lapply(seq_along(variables), function(i) list(id = as.integer(names(variables)[i]), nome = paste("variavel", names(variables)[i], if (variables[[i]] == "%") "percentual" else ""), unidade = variables[[i]])),
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

# `variable` is the SIDRA variable id asked for; the "Total" (category 0) has no
# quantity, so only value (215) is published for it.
coverage_json <- function(table, variable, years = fake_years, classified = !is.null(FAKE_TABLES[[table]]$cl)) {
  spec <- FAKE_TABLES[[table]]
  rows <- list(if (classified) HEADER_ROW else HEADER_ROW[setdiff(names(HEADER_ROW), c("D4C", "D4N"))])
  for (ct in spec$categories %||% "(none)") {
    published <- years(table, ct)
    for (y in min(spec$span):max(spec$span)) {
      value <- if (y %in% published && !(ct == "0" && variable != "215")) as.character(1000L + y) else "..."
      row <- list(
        NC = "1", NN = "Brasil", MC = "1", MN = "Unidade", V = value, D1C = "1", D1N = "Brasil",
        D2C = variable, D2N = "variavel", D3C = as.character(y), D3N = as.character(y)
      )
      if (classified) row <- c(row, list(D4C = ct, D4N = paste("categoria", ct)))
      rows[[length(rows) + 1]] <- row
    }
  }
  as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
}

resp <- function(status, body = "") {
  list(status_code = status, headers = raw(0), content = if (is.character(body)) charToRaw(body) else body)
}

# `override(kind, table, url)` may return a response (or throw like curl does)
# to replace the healthy one; `kind` is "metadata" or "coverage". `seen`
# collects the requested URLs.
fake_ibge <- function(override = function(kind, table, url) NULL, seen = NULL,
                      metadata = metadata_json, coverage = coverage_json) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    if (grepl("/agregados/[0-9]+/metadados$", url)) {
      table <- sub("^.*/agregados/([0-9]+)/metadados$", "\\1", url)
      return(override("metadata", table, url) %||% resp(200L, metadata(table)))
    }
    if (grepl("/values/t/[0-9]+/n1/all/v/[0-9]+/p/all(/c[0-9]+/all)?$", url)) {
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
run <- function(server = fake_ibge(), rows = manifest_rows) {
  with_ibge(server, suppressMessages(resolve_ppm(rows)))
}
# the committed rows with one dataset's sidra_code replaced
with_code <- function(dataset, code) {
  rows <- manifest_rows
  rows$sidra_code[rows$dataset == dataset] <- code
  rows
}

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves all 5 rows with 2 requests per table (10)", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 5)
  expect_setequal(out$dataset, manifest_rows$dataset)
  expect_true(all(out$survey == "ppm") && all(out$resolver == "ppm"))
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  # the resolver owns available_time and docs_url, never sidra_code
  expect_equal(names(out), c("survey", "dataset", "geo_level", "year", "available_time", "docs_url", "resolver"))
  expect_null(attr(out, "partial_failures"))

  expect_length(seen$urls, 10)
  expect_equal(sum(grepl("/metadados$", seen$urls)), 5)
  expect_equal(anyDuplicated(seen$urls), 0)
})

test_that("available_time is the union of the table's categories, and docs_url is each row's own table page", {
  out <- run()
  at <- setNames(out$available_time, out$dataset)
  expect_equal(at[["ppm_animal_origin_production"]], "1974-2025")
  expect_equal(at[["ppm_aquaculture"]], "2013-2025") # 79366 starts in 2024; the union starts in 2013
  expect_equal(at[["ppm_cow_farming"]], "1974-2025")
  expect_equal(at[["ppm_livestock_inventory"]], "1974-2025")
  expect_equal(at[["ppm_sheep_farming"]], "1974-2025")

  tables <- sub("/.*", "", manifest_rows$sidra_code[match(out$dataset, manifest_rows$dataset)])
  expect_equal(out$docs_url, paste0("https://sidra.ibge.gov.br/tabela/", tables))
})

test_that("the coverage query is the one measured live: value (215) where declared, else the first real variable; no classification part for 94 and 95", {
  seen <- new.env()
  run(fake_ibge(seen = seen))
  coverage_urls <- grep("/values/t/", seen$urls, value = TRUE)
  url_of <- function(table) grep(sprintf("/t/%s/", table), coverage_urls, value = TRUE)

  # 215 where the table has it -- the "Total" category has no quantity, so 106/4146 would call it never published
  expect_equal(url_of("74"), "https://apisidra.ibge.gov.br/values/t/74/n1/all/v/215/p/all/c80/all")
  expect_equal(url_of("3940"), "https://apisidra.ibge.gov.br/values/t/3940/n1/all/v/215/p/all/c654/all")
  # otherwise the table's first non-percentage variable
  expect_equal(url_of("3939"), "https://apisidra.ibge.gov.br/values/t/3939/n1/all/v/105/p/all/c79/all")
  expect_equal(url_of("94"), "https://apisidra.ibge.gov.br/values/t/94/n1/all/v/107/p/all")
  expect_equal(url_of("95"), "https://apisidra.ibge.gov.br/values/t/95/n1/all/v/108/p/all")
  expect_true(all(grepl("^https://servicodados\\.ibge\\.gov\\.br/api/v3/agregados/[0-9]+/metadados$", grep("metadados", seen$urls, value = TRUE))))
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(ls(resolver_env, pattern = "^(resolve_|watch_)"), "resolve_ppm")
})

# ---- the meaning of a bare code ---------------------------------------------

test_that("a bare code on a table that HAS a classification is refused, naming the code to write", {
  out <- run(rows = with_code("ppm_animal_origin_production", "74"))

  expect_false("ppm_animal_origin_production" %in% out$dataset)
  expect_equal(nrow(out), 4)
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, "^ppm/ppm_animal_origin_production \\(74\\): sidra_code '74' names no classification, but table 74 has one \\(c80\\)")
  expect_match(failures, "return only the 'Total' slice; write '74/c80/all'", fixed = TRUE)
})

test_that("an explicit classification on a table that has NONE is refused, naming the code to write", {
  out <- run(rows = with_code("ppm_cow_farming", "94/c80/all"))

  expect_false("ppm_cow_farming" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "names classification c80, but table 94 has none; write '94'", fixed = TRUE)
})

test_that("a classification that is not the table's, classific = 'all' and a made-up category are refused", {
  out <- run(rows = with_code("ppm_livestock_inventory", "3939/c999/all"))
  expect_false("ppm_livestock_inventory" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "classification c999 is not the one table 3939 has (c79)", fixed = TRUE)

  out <- run(rows = with_code("ppm_livestock_inventory", "3939/all/all"))
  expect_match(attr(out, "partial_failures"), "uses classific = 'all'", fixed = TRUE)
  expect_match(attr(out, "partial_failures"), "write '3939/c79/all' instead", fixed = TRUE)
  expect_false("sidra_code" %in% names(out)) # the resolver never writes sidra_code

  out <- run(rows = with_code("ppm_aquaculture", "3940/c654/999999"))
  expect_false("ppm_aquaculture" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "category 999999 does not exist in classification c654 of table 3940")
  expect_match(attr(out, "partial_failures"), "HTTP 200 and empty values")

  # a real single category is accepted and gets its own span (79366 exists from 2024)
  out <- run(rows = with_code("ppm_aquaculture", "3940/c654/79366"))
  expect_equal(out$available_time[out$dataset == "ppm_aquaculture"], "2024-2025")
})

test_that("a sidra_code that is not <table> or <table>/c<n>/<id or all> fails its row", {
  out <- run(rows = with_code("ppm_sheep_farming", "95-c1"))

  expect_false("ppm_sheep_farming" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "^ppm/ppm_sheep_farming \\(95-c1\\): sidra_code '95-c1' is not <table> or <table>/c<classification>/")
})

# ---- the variable ids load_ppm() names --------------------------------------

test_that("a variable IBGE adds to a table fails that row, naming it; percentage variables and a surplus name do not", {
  out <- run(fake_ibge(metadata = function(table) metadata_json(table, extra_variables = if (table == "74") c("9999" = "Cabecas") else character(0))))

  expect_false("ppm_animal_origin_production" %in% out$dataset)
  expect_equal(nrow(out), 4)
  expect_match(
    attr(out, "partial_failures"),
    "table 74 now declares variable(s) 9999 that load_ppm('ppm_animal_origin_production') does not name",
    fixed = TRUE
  )
  expect_match(attr(out, "partial_failures"), "unnamed na_v<category> columns", fixed = TRUE)

  # 1000215 ("... percentual do total geral", unit %) is declared on 74 and 3940 and never trips the check
  expect_null(attr(run(), "partial_failures"))
})

test_that("a dataset with no entry in PPM_NAMED_VARIABLES fails its row", {
  rows <- manifest_rows
  rows$dataset[rows$dataset == "ppm_cow_farming"] <- "ppm_new_dataset"
  out <- run(rows = rows)

  expect_false("ppm_new_dataset" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "dataset ppm_new_dataset has no entry in PPM_NAMED_VARIABLES")
})

# ---- available_geo ----------------------------------------------------------

test_that("an available_geo level the table's metadata does not have fails the row; an unknown word does too; NA is skipped", {
  out <- run(fake_ibge(metadata = function(table) {
    metadata_json(table, levels = if (table == "95") c("N1", "N2", "N3") else c("N1", "N2", "N3", "N6"))
  }))
  expect_false("ppm_sheep_farming" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "available_geo promises country, state, municipality, but table 95's metadata has no territorial level N6", fixed = TRUE)

  rows <- manifest_rows
  rows$available_geo[rows$dataset == "ppm_cow_farming"] <- "country, galaxy"
  rows$available_geo[rows$dataset == "ppm_sheep_farming"] <- NA_character_
  out <- run(rows = rows)
  expect_false("ppm_cow_farming" %in% out$dataset)
  expect_true("ppm_sheep_farming" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "available_geo names 'galaxy'")
})

# ---- docs_url ---------------------------------------------------------------

test_that("docs_url is taken from the metadata only when it is a SIDRA table page; otherwise the cell is left alone", {
  out <- run(fake_ibge(metadata = function(table) metadata_json(table, url = if (table == "94") "https://example.org/somewhere" else paste0("https://sidra.ibge.gov.br/tabela/", table))))

  expect_equal(nrow(out), 5) # not a failure
  expect_true(is.na(out$docs_url[out$dataset == "ppm_cow_farming"]))
  expect_equal(out$docs_url[out$dataset == "ppm_sheep_farming"], "https://sidra.ibge.gov.br/tabela/95")
  expect_null(attr(out, "partial_failures"))
})

# ---- table-level failures ---------------------------------------------------

test_that("an unreadable table fails its row only, with IBGE's own words; markup is stripped", {
  cloudflare <- paste0(
    "<html><head><title>Just a moment...</title><style>.x{}</style></head>",
    "<body><script>var a=1;</script>Enable JavaScript and cookies to continue</body></html>"
  )
  out <- run(fake_ibge(override = function(kind, table, url) if (kind == "coverage" && table == "94") resp(403L, cloudflare) else NULL))

  expect_equal(nrow(out), 4)
  expect_false("ppm_cow_farming" %in% out$dataset)
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, "^ppm/ppm_cow_farming \\(94\\): ")
  expect_match(failures, "answered HTTP 403: Just a moment... Enable JavaScript and cookies to continue", fixed = TRUE)
  expect_false(grepl("<|var a=1", failures))
})

test_that("a transport error is a reason, not an exception, and names the URL", {
  out <- run(fake_ibge(override = function(kind, table, url) {
    if (kind == "metadata" && table == "3940") stop("Timeout was reached") else NULL
  }))

  expect_false("ppm_aquaculture" %in% out$dataset)
  expect_match(
    attr(out, "partial_failures"),
    "^ppm/ppm_aquaculture \\(3940/c654/all\\): could not reach https://servicodados.ibge.gov.br/api/v3/agregados/3940/metadados \\(Timeout was reached\\)"
  )
})

test_that("each way a table's answer can be wrong is a reason for its row", {
  reason_for <- function(...) {
    out <- run(fake_ibge(...))
    lines <- attr(out, "partial_failures")
    lines[grepl("^ppm/ppm_sheep_farming ", lines)]
  }

  expect_match(
    reason_for(metadata = function(table) if (table == "95") metadata_json(table, id = 94L) else metadata_json(table)),
    "the description is for table 94, not 95"
  )
  expect_match(
    reason_for(override = function(kind, table, url) if (kind == "metadata" && table == "95") resp(200L, "<html><body>Just a moment...</body></html>") else NULL),
    "not a SIDRA table description.*Just a moment"
  )
  # a second classification: the one-path coverage query does not apply
  expect_match(
    reason_for(metadata = function(table) {
      if (table != "95") return(metadata_json(table))
      cl <- function(id) list(id = id, nome = "x", categorias = list(list(id = 1L, nome = "y")))
      metadata_json(table, classifications = list(cl(81L), cl(99L)))
    }),
    "table 95 has 2 classifications \\(c81, c99\\)"
  )
  # the coverage query changed shape: third dimension is no longer the year
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table != "95") return(coverage_json(table, variable))
      sub('"D3N":"Ano"', '"D3N":"Mes"', coverage_json(table, variable), fixed = TRUE)
    }),
    "the third dimension is 'Mes', not 'Ano'"
  )
  # monthly period codes
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table != "95") return(coverage_json(table, variable))
      gsub('"D3C":"20([0-9]{2})"', '"D3C":"20\\100"', coverage_json(table, variable))
    }),
    "not four-digit years"
  )
  # an empty table: header row only
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table == "95") as.character(jsonlite::toJSON(list(HEADER_ROW[setdiff(names(HEADER_ROW), c("D4C", "D4N"))]), auto_unbox = TRUE)) else coverage_json(table, variable)
    }),
    "returned no data rows at all"
  )
  # every value unpublished
  expect_match(
    reason_for(coverage = function(table, variable) {
      coverage_json(table, variable, years = function(t, ct) if (t == "95") integer(0) else fake_years(t, ct))
    }),
    "table 95 resolves, but has no year with a published value at Brazil level"
  )
})

test_that("if EVERY row fails the resolver stops, naming the reasons", {
  down <- fake_ibge(override = function(kind, table, url) resp(500L, "Internal Server Error"))
  expect_error(run(down), "all 5 rows failed verification")
  expect_error(run(down), "ppm/ppm_aquaculture \\(3940/c654/all\\): .*HTTP 500: Internal Server Error")
})

# ---- guards on the input ----------------------------------------------------

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_ppm(manifest_rows[0, ]), "no manifest rows tagged resolver == 'ppm'")
  expect_error(resolve_ppm(NULL), "no manifest rows tagged resolver == 'ppm'")

  keyed <- manifest_rows
  keyed$geo_level[1] <- "state"
  expect_error(resolve_ppm(keyed), "every PPM row is unkeyed")

  no_code <- manifest_rows
  no_code$sidra_code[1] <- NA_character_
  expect_error(resolve_ppm(no_code), "every row needs a sidra_code")
})

# ---- pure helpers, table-driven ---------------------------------------------

test_that("ppm_parse_code reads <table> and <table>/c<n>/<category> and rejects everything else", {
  expect_equal(ppm_parse_code("74/c80/all"), list(table = "74", classific = "c80", category = "all"))
  expect_equal(ppm_parse_code("3940/c654/32861"), list(table = "3940", classific = "c654", category = "32861"))
  expect_equal(ppm_parse_code("94"), list(table = "94", classific = NA_character_, category = NA_character_))
  expect_equal(ppm_parse_code("74/all/all"), list(table = "74", classific = "all", category = "all")) # parses, then refused
  for (bad in c("", "c80", "74/c80", "74/c80/", "74/80/all", "74/c80/x", "74/c80/all/1", " 94", "94 ", "7a")) {
    expect_null(ppm_parse_code(bad), info = bad)
  }
  expect_null(ppm_parse_code(NA_character_))
  expect_null(ppm_parse_code(NULL))
})

test_that("ppm_years_to_available_time writes the grammar parse_years() reads, gaps included", {
  cases <- list(
    list(c(2005L), "2005"),
    list(1974:2025, "1974-2025"),
    list(c(1974:1990, 1995:2025), "1974-1990, 1995-2025"),
    list(c(2000L, 2002L), "2000, 2002"),
    list(c(2025L, 1974L, 1974L, 1975L), "1974-1975, 2025")
  )
  for (case in cases) {
    expect_equal(ppm_years_to_available_time(case[[1]]), case[[2]])
    expect_equal(parse_years(ppm_years_to_available_time(case[[1]])), sort(unique(case[[1]])))
  }
  expect_true(is.na(ppm_years_to_available_time(integer(0))))
})

test_that("ppm_parse_coverage: '...' and '..' are unpublished; numbers, '-' and 'X' are published; with or without a category", {
  values <- c("123", "-", "X", "...", "..", "0")
  make <- function(classified) {
    rows <- list(if (classified) HEADER_ROW else HEADER_ROW[setdiff(names(HEADER_ROW), c("D4C", "D4N"))])
    for (i in seq_along(values)) {
      rows[[i + 1]] <- c(list(V = values[i], D3C = as.character(2000 + i), D3N = "Ano"), if (classified) list(D4C = "7"))
    }
    as.character(jsonlite::toJSON(rows, auto_unbox = TRUE))
  }

  with_category <- ppm_parse_coverage(make(TRUE), classified = TRUE)
  expect_equal(with_category$problem, character(0))
  expect_equal(with_category$years[["7"]], c(2001L, 2002L, 2003L, 2006L)) # 123, -, X, 0

  without <- ppm_parse_coverage(make(FALSE), classified = FALSE)
  expect_equal(without$years[["(none)"]], c(2001L, 2002L, 2003L, 2006L))
})

test_that("ppm_snippet shows text, strips markup, and gives up on binary", {
  expect_equal(ppm_snippet(raw(0)), "")
  expect_equal(ppm_snippet(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x01))), "")
  expect_equal(ppm_snippet(charToRaw("<p>Tabela   99999:\nTabela invalida</p>")), "Tabela 99999: Tabela invalida")
  expect_equal(nchar(ppm_snippet(charToRaw(strrep("a", 500)), n = 50)), 53)
})

# ---- the committed manifest and R/ppm.R -------------------------------------

test_that("every committed PPM row is verifiable: owned by the resolver, one table each, the shape its table needs", {
  expect_equal(nrow(manifest_rows), 5)
  expect_true(all(manifest_rows$resolver == "ppm"))
  parsed <- lapply(manifest_rows$sidra_code, ppm_parse_code)
  expect_true(all(vapply(parsed, Negate(is.null), NA)))
  tables <- vapply(parsed, function(p) p$table, "")
  expect_equal(anyDuplicated(tables), 0) # no table is shared: there is no fan-out to design
  expect_setequal(tables, names(FAKE_TABLES))
  # classific = "all" sends sidrar to an IBGE page behind a bot challenge (resolve_ppm.R, point 2)
  expect_false(any(vapply(parsed, function(p) identical(p$classific, "all"), NA)))
  # bare exactly for the tables that have no classification
  bare <- tables[vapply(parsed, function(p) is.na(p$classific), NA)]
  expect_setequal(bare, c("94", "95"))
  # each row points at its own table's page
  expect_equal(manifest_rows$docs_url, paste0("https://sidra.ibge.gov.br/tabela/", tables))
  geo <- unique(trimws(unlist(strsplit(tolower(manifest_rows$available_geo), ","))))
  expect_true(all(geo %in% names(PPM_GEO_LEVELS)))
  expect_false(anyNA(lapply(manifest_rows$available_time, parse_years)))
})

test_that("PPM_NAMED_VARIABLES is the list of ids R/ppm.R's case_when() blocks name, dataset by dataset", {
  ppm_src <- readLines(file.path(pkg_root, "R", "ppm.R"), warn = FALSE)
  start <- grep("## Translate Variable Names ##", ppm_src, fixed = TRUE)
  end <- grep("## Create Long Format Data ##", ppm_src, fixed = TRUE)
  expect_length(start, 1)
  expect_length(end, 1)
  section <- ppm_src[(start + 1):(end - 1)]
  section <- section[!grepl("^\\s*#", section)]

  block_start <- grep("if \\(param\\$dataset == \"ppm_[a-z_]+\"\\)", section)
  expect_length(block_start, 5)
  found <- list()
  for (k in seq_along(block_start)) {
    block <- section[block_start[k]:(c(block_start, length(section) + 1)[k + 1] - 1)]
    dataset <- sub(".*\"(ppm_[a-z_]+)\".*", "\\1", block[1])
    ids <- regmatches(block, gregexpr("variavel_codigo == \"[0-9]+\"", block))
    found[[dataset]] <- sort(unique(sub(".*\"([0-9]+)\"", "\\1", unlist(ids))))
  }
  expect_setequal(names(found), names(PPM_NAMED_VARIABLES))
  for (dataset in names(found)) {
    expect_equal(found[[dataset]], sort(PPM_NAMED_VARIABLES[[dataset]]), info = dataset)
  }
})

test_that("R/ppm.R hardcodes every table id the manifest uses (its FRAGILE tag says the two must agree)", {
  ppm_src <- readLines(file.path(pkg_root, "R", "ppm.R"), warn = FALSE)
  code_lines <- ppm_src[!grepl("^\\s*#", ppm_src)]
  for (table in sub("/.*", "", manifest_rows$sidra_code)) {
    expect_true(any(grepl(sprintf("param\\$code == %s\\b", table), code_lines)), info = paste("table", table))
  }
})
