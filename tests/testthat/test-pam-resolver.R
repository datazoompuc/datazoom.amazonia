# tests/testthat/test-pam-resolver.R
#
# Offline test for actions/scrapers/resolve_pam.R (added 2026-10-21; see that
# file's header for what was measured live). PAM's 79 manifest rows are read
# from IBGE's SIDRA API, not from a file: each row's `sidra_code`
# ("1613/c82/45981") names a table, a classification and a category, 79 rows
# share 7 tables, and what goes stale is available_time and, silently, the
# ids themselves -- IBGE answers a query for a category that does not exist
# with HTTP 200 and a table of "..." values, so only comparing the manifest
# with the table's metadata notices.
#
# curl::curl_fetch_memory is mocked (no network). The fake IBGE mirrors what was
# measured live on 2026-10-21: /metadados is a JSON description with the
# table's variables, territorial levels and categories; the coverage query
# is a JSON array whose first element is a header row, one row per category and
# year, "..." where nothing is published, and category 0 ("Total") has a value
# of production but no quantity.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("jsonlite")

resolver_env <- new.env()
# the metadata lookup, coverage query and value coding PAM shares with the other
# SIDRA resolvers (tested for itself in test-sidra-api.R), as build_manifest.R
# sources it before the resolvers
source(file.path(actions_dir, "scripts", "sidra_api.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_pam.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

# The real committed rows (79 of them), not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "pam", ]

# ---- a fake IBGE ------------------------------------------------------------

# table -> classification id and the variables it declares. 1612/1613/5457 have
# a value of production (215); the four multi-harvest tables do not.
FAKE_TABLES <- list(
  `1612` = list(cl = "81", vars = c("109", "216", "214", "112", "215")),
  `1613` = list(cl = "82", vars = c("2313", "216", "214", "112", "215")),
  `5457` = list(cl = "782", vars = c("8331", "216", "214", "112", "215")),
  `1002` = list(cl = "81", vars = c("109", "216", "214", "112")),
  `839` = list(cl = "81", vars = c("109", "216", "214", "112")),
  `1000` = list(cl = "81", vars = c("109", "216", "214", "112")),
  `1001` = list(cl = "81", vars = c("109", "216", "214", "112"))
)

parsed_codes <- lapply(manifest_rows$sidra_code, pam_parse_code)
row_table <- vapply(parsed_codes, function(p) p$table, "")
row_category <- vapply(parsed_codes, function(p) p$category, "")

# Every category id the committed rows name for a table, plus the Total (0) for
# the two big tables and a couple of stand-ins where only "all" rows exist.
fake_categories <- function(table) {
  named <- unique(row_category[row_table == table & row_category != "all"])
  if (table %in% c("1612", "1613")) named <- union(named, "0")
  if (length(named) == 0) named <- c("101", "102")
  named
}

# Years IBGE publishes, per (table, category). Default: the table's whole span.
fake_years <- function(table, category) {
  span <- if (table %in% c("1002", "839", "1000", "1001")) 2003:2025 else 1974:2025
  special <- list(
    "1612/40471" = 1974:1987, # alfafa: the series ended in 1987
    "1613/2747" = 1988:2025, # annatto: IBGE publishes from 1988 (the manifest said 1981)
    "1613/45981" = 2015:2025 # acai
  )
  special[[paste0(table, "/", category)]] %||% span
}

HEADER_ROW <- list(
  NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial", MC = "Unidade de Medida (Codigo)",
  MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
  D2C = "Variavel (Codigo)", D2N = "Variavel", D3C = "Ano (Codigo)", D3N = "Ano",
  D4C = "Produto (Codigo)", D4N = "Produto"
)

metadata_json <- function(table, drop_category = character(0), levels = c("N1", "N2", "N3", "N6", "N8", "N9"),
                          id = as.integer(table), classifications = NULL, extra_variables = character(0),
                          url = paste0("https://sidra.ibge.gov.br/tabela/", table)) {
  spec <- FAKE_TABLES[[table]]
  cats <- setdiff(fake_categories(table), drop_category)
  classifications <- classifications %||% list(list(
    id = as.integer(spec$cl), nome = "Produto das lavouras",
    categorias = lapply(cats, function(ct) list(id = as.integer(ct), nome = paste("categoria", ct), unidade = NULL, nivel = 1L))
  ))
  variables <- lapply(c(spec$vars, extra_variables), function(v) list(id = as.integer(v), nome = paste("variavel", v), unidade = "Hectares"))
  # IBGE declares a "... - percentual do total geral" twin (unit "%") for most
  # variables; sidrar's "allxp" never requests them and load_pam() does not name them
  percent <- list(list(id = as.integer(paste0("1000", spec$vars[2])), nome = "variavel - percentual do total geral", unidade = "%"))
  as.character(jsonlite::toJSON(list(
    id = id, nome = paste("Tabela", table), URL = url,
    pesquisa = "Producao Agricola Municipal",
    periodicidade = list(frequencia = "anual", inicio = 1974L, fim = 2025L),
    nivelTerritorial = list(Administrativo = levels, Especial = list(), IBGE = list()),
    variaveis = c(variables, percent),
    classificacoes = classifications
  ), auto_unbox = TRUE, null = "null"))
}

# `years` overrides fake_years(): function(table, category) -> integer years.
# `variable` is the SIDRA variable id asked for; category 0 has no quantity.
coverage_json <- function(table, variable, years = fake_years, zero_dash_from = 2014L) {
  cats <- fake_categories(table)
  rows <- list(HEADER_ROW)
  for (ct in cats) {
    published <- years(table, ct)
    for (y in 1974:2025) {
      value <- if (y %in% published && !(ct == "0" && variable == "214")) {
        # a true zero is "-" in SIDRA (cotton_arboreo after 2013 in real life)
        if (table == "1613" && ct == "2718" && y >= zero_dash_from) "-" else as.character(1000L + y)
      } else {
        "..."
      }
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
run <- function(server = fake_ibge(), rows = manifest_rows) {
  with_ibge(server, suppressMessages(resolve_pam(rows)))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- the healthy path -------------------------------------------------------

test_that("a healthy IBGE resolves all 79 rows, with 2 requests per TABLE (14), not per row", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  expect_equal(nrow(out), 79)
  expect_setequal(out$dataset, manifest_rows$dataset)
  expect_true(all(out$survey == "pam"))
  expect_true(all(out$resolver == "pam"))
  # unkeyed rows stay unkeyed; the values the resolver owns are available_time
  # and docs_url, never sidra_code
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  expect_equal(names(out), c("survey", "dataset", "geo_level", "year", "available_time", "docs_url", "resolver"))
  expect_null(attr(out, "partial_failures"))

  expect_length(seen$urls, 14)
  expect_equal(sum(grepl("/metadados$", seen$urls)), 7)
  expect_setequal(
    sub("^.*/(?:agregados|t)/([0-9]+)[/?].*$", "\\1", seen$urls, perl = TRUE),
    names(FAKE_TABLES)
  )
  expect_equal(anyDuplicated(seen$urls), 0)
})

test_that("available_time is each category's own published span, and whole-table rows get the union", {
  at <- function(out, ds) out$available_time[out$dataset == ds]
  out <- run()

  expect_equal(at(out, "soybean"), "1974-2025")
  expect_equal(at(out, "alfafa"), "1974-1987") # series that ended
  expect_equal(at(out, "annatto_seeds"), "1988-2025") # the manifest said 1981-2023
  expect_equal(at(out, "acai"), "2015-2025")
  # "-" is a published zero: cotton_arboreo keeps its whole span (the old manifest ranges followed this)
  expect_equal(at(out, "cotton_arboreo"), "1974-2025")
  # whole-table rows: the union over the table's categories
  expect_equal(at(out, "all_crops"), "1974-2025")
  expect_equal(at(out, "temporary_crops"), "1974-2025")
  expect_equal(at(out, "permanent_crops"), "1974-2025")
  expect_true(all(c(at(out, "beans"), at(out, "corn"), at(out, "peanut"), at(out, "potato")) == "2003-2025"))
})

test_that("the Total categories resolve: coverage asks for value of production (215), not quantity (214)", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  # category 0 has no quantity, so a coverage query on 214 would call it "never published"
  expect_equal(out$available_time[out$dataset == "temporary_total"], "1974-2025")
  expect_equal(out$available_time[out$dataset == "permanent_total"], "1974-2025")

  coverage_urls <- grep("/values/t/", seen$urls, value = TRUE)
  variable_of <- function(table) sub("^.*/v/([0-9]+)/.*$", "\\1", grep(sprintf("/t/%s/", table), coverage_urls, value = TRUE))
  for (table in c("1612", "1613", "5457")) expect_equal(variable_of(table), "215")
  # the multi-harvest tables declare no value variable: fall back to quantity
  for (table in c("1002", "839", "1000", "1001")) expect_equal(variable_of(table), "214")
  # and the request is the one measured live: Brazil level, every year, every category
  expect_true(all(grepl("^https://apisidra\\.ibge\\.gov\\.br/values/t/[0-9]+/n1/all/v/[0-9]+/p/all/c[0-9]+/all$", coverage_urls)))
  expect_true(all(grepl("^https://servicodados\\.ibge\\.gov\\.br/api/v3/agregados/[0-9]+/metadados$", grep("metadados", seen$urls, value = TRUE))))
})

test_that("docs_url is each table's own page, fanned across every row that reads the table", {
  seen <- new.env()
  out <- run(fake_ibge(seen = seen))

  tables <- row_table[match(out$dataset, manifest_rows$dataset)]
  expect_equal(out$docs_url, paste0("https://sidra.ibge.gov.br/tabela/", tables))
  expect_equal(length(unique(out$docs_url)), 7) # 7 tables for 79 rows ...
  expect_length(seen$urls, 14) # ... and still 2 requests per table, not per row
})

test_that("docs_url is taken from the metadata only when it is a SIDRA table page; otherwise the cell is left alone", {
  out <- run(fake_ibge(metadata = function(table) {
    metadata_json(table, url = if (table == "1613") "https://example.org/somewhere" else paste0("https://sidra.ibge.gov.br/tabela/", table))
  }))

  expect_equal(nrow(out), 79) # not a failure
  expect_true(all(is.na(out$docs_url[out$dataset %in% manifest_rows$dataset[row_table == "1613"]])))
  expect_equal(out$docs_url[out$dataset == "soybean"], "https://sidra.ibge.gov.br/tabela/1612")
  expect_null(attr(out, "partial_failures"))
})

test_that("the resolver defines exactly one resolve_* binding (build_manifest.R registers every one it finds)", {
  expect_equal(ls(resolver_env, pattern = "^(resolve_|watch_)"), "resolve_pam")
})

# ---- the variable ids load_pam() names ----------------------------------------

test_that("a variable IBGE adds to a table fails that table's rows, naming it; percentage variables never trip the check", {
  out <- run(fake_ibge(metadata = function(table) {
    metadata_json(table, extra_variables = if (table == "1613") "9999" else character(0))
  }))

  n_1613 <- sum(row_table == "1613")
  expect_equal(nrow(out), 79 - n_1613)
  expect_false(any(out$dataset %in% manifest_rows$dataset[row_table == "1613"]))
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1) # reported once for the table, not once per crop
  expect_match(failures, sprintf("^pam/table 1613 \\(%d dataset\\(s\\)\\): table 1613 now declares variable\\(s\\) 9999 \\(variavel 9999\\)", n_1613))
  expect_match(failures, "does not name", fixed = TRUE)

  # every table of the healthy fake also declares a percentage variable (unit "%"), and none of them fails
  expect_null(attr(run(), "partial_failures"))
})

test_that("PAM_NAMED_VARIABLES is the list of ids R/pam.R's Translation step names, in both languages", {
  pam_src <- readLines(file.path(pkg_root, "R", "pam.R"), warn = FALSE)
  start <- grep("## Translation", pam_src, fixed = TRUE)
  end <- grep("## Create Long Format Data ##", pam_src, fixed = TRUE)
  expect_length(start, 1)
  expect_length(end, 1)
  section <- pam_src[(start + 1):(end - 1)]
  section <- section[!grepl("^\\s*#", section)]

  per_language <- list(pt = section[grep("language == \"pt\"", section):(grep("language == \"eng\"", section) - 1)],
                       eng = section[grep("language == \"eng\"", section):length(section)])
  for (language in names(per_language)) {
    ids <- regmatches(per_language[[language]], gregexpr("variavel_codigo == \"[0-9]+\"", per_language[[language]]))
    ids <- sort(unique(sub(".*\"([0-9]+)\"", "\\1", unlist(ids))))
    expect_equal(ids, sort(PAM_NAMED_VARIABLES), info = language)
  }
})

# ---- per-row failures -------------------------------------------------------

test_that("a category IBGE no longer lists fails ONLY its row, and says why", {
  # Live, a nonexistent category is not an error: HTTP 200 and a table of "..."
  # values. The fake mirrors it -- the category is gone from the metadata but
  # the coverage query still returns it, all unpublished.
  server <- fake_ibge(
    metadata = function(table) metadata_json(table, drop_category = if (table == "1612") "2713" else character(0)),
    coverage = function(table, variable) {
      coverage_json(table, variable, years = function(t, ct) if (t == "1612" && ct == "2713") integer(0) else fake_years(t, ct))
    }
  )
  out <- run(server)

  expect_equal(nrow(out), 78)
  expect_false("soybean" %in% out$dataset)
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, "^pam/soybean \\(1612/c81/2713\\): category 2713 does not exist in classification c81 of table 1612", perl = TRUE)
  expect_match(failures, "HTTP 200 and empty values")
})

test_that("a category that exists but has no published year at Brazil level fails its row", {
  server <- fake_ibge(coverage = function(table, variable) {
    coverage_json(table, variable, years = function(t, ct) if (t == "1613" && ct == "2744") integer(0) else fake_years(t, ct))
  })
  out <- run(server)

  expect_false("sisal_or_agave" %in% out$dataset)
  expect_equal(nrow(out), 78)
  expect_match(attr(out, "partial_failures"), "^pam/sisal_or_agave \\(1613/c82/2744\\): category 2744 of table 1613 resolves, but has no year with a published value")
})

test_that("a classification that is not the table's fails the row, naming the table's", {
  rows <- manifest_rows
  rows$sidra_code[rows$dataset == "soybean"] <- "1612/c999/2713"
  out <- run(rows = rows)

  expect_false("soybean" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "classification c999 is not one of table 1612's \\(c81\\)")
})

test_that("classific = 'all' is refused with the id to use, and the code is not rewritten", {
  rows <- manifest_rows
  rows$sidra_code[rows$dataset == "temporary_crops"] <- "1612/all/all"
  out <- run(rows = rows)

  expect_false("temporary_crops" %in% out$dataset)
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, "uses classific = 'all'", fixed = TRUE)
  expect_match(failures, "write '1612/c81/all' instead", fixed = TRUE)
  expect_false("sidra_code" %in% names(out)) # the resolver never writes sidra_code
})

test_that("a sidra_code that is not <table>/c<n>/<id or all> fails its row", {
  rows <- manifest_rows
  rows$sidra_code[rows$dataset == "rice"] <- "1612-c81-2692"
  out <- run(rows = rows)

  expect_false("rice" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "^pam/rice \\(1612-c81-2692\\): sidra_code '1612-c81-2692' is not <table>/c<classification>/")
})

test_that("an available_geo level the table's metadata does not have fails the row; an unknown word does too; NA is skipped", {
  # municipality (N6) missing from table 1613's metadata
  out <- run(fake_ibge(metadata = function(table) {
    metadata_json(table, levels = if (table == "1613") c("N1", "N2", "N3") else c("N1", "N2", "N3", "N6"))
  }))
  failed <- setdiff(manifest_rows$dataset, out$dataset)
  expect_setequal(failed, manifest_rows$dataset[row_table == "1613"])
  expect_match(attr(out, "partial_failures")[1], "available_geo promises country, state, municipality, but table 1613's metadata has no territorial level N6", fixed = TRUE)

  rows <- manifest_rows
  rows$available_geo[rows$dataset == "rice"] <- "country, galaxy"
  rows$available_geo[rows$dataset == "wheat"] <- NA_character_
  out <- run(rows = rows)
  expect_false("rice" %in% out$dataset)
  expect_true("wheat" %in% out$dataset)
  expect_match(attr(out, "partial_failures"), "available_geo names 'galaxy'")
})

# ---- table-level failures ---------------------------------------------------

test_that("an unreadable table takes down all of its rows, reported ONCE, and leaves the other tables alone", {
  cloudflare <- paste0(
    "<html><head><title>Just a moment...</title><style>.x{}</style></head>",
    "<body><script>var a=1;</script>Enable JavaScript and cookies to continue</body></html>"
  )
  server <- fake_ibge(override = function(kind, table, url) {
    if (kind == "coverage" && table == "1613") resp(403L, cloudflare) else NULL
  })
  out <- run(server)

  n_1613 <- sum(row_table == "1613")
  expect_equal(nrow(out), 79 - n_1613)
  expect_false(any(out$dataset %in% manifest_rows$dataset[row_table == "1613"]))
  failures <- attr(out, "partial_failures")
  expect_length(failures, 1)
  expect_match(failures, sprintf("^pam/table 1613 \\(%d dataset\\(s\\)\\): ", n_1613))
  expect_match(failures, "answered HTTP 403: Just a moment... Enable JavaScript and cookies to continue", fixed = TRUE)
  expect_false(grepl("<|var a=1", failures)) # markup and script bodies are stripped
})

test_that("a transport error is a reason, not an exception, and names the URL", {
  server <- fake_ibge(override = function(kind, table, url) {
    if (kind == "metadata" && table == "5457") stop("Timeout was reached") else NULL
  })
  out <- run(server)

  expect_false("all_crops" %in% out$dataset)
  expect_equal(nrow(out), 78)
  expect_match(
    attr(out, "partial_failures"),
    "^pam/table 5457 \\(1 dataset\\(s\\)\\): could not reach https://servicodados.ibge.gov.br/api/v3/agregados/5457/metadados \\(Timeout was reached\\)"
  )
})

test_that("each way a table's answer can be wrong is a table-level reason", {
  reason_for <- function(...) {
    server <- fake_ibge(...)
    out <- run(server)
    lines <- attr(out, "partial_failures")
    lines[grepl("^pam/table 839 ", lines)]
  }

  # metadata describes another table
  expect_match(
    reason_for(metadata = function(table) if (table == "839") metadata_json(table, id = 1612L) else metadata_json(table)),
    "the description is for table 1612, not 839"
  )
  # metadata is not JSON (a challenge page that came back with HTTP 200)
  expect_match(
    reason_for(override = function(kind, table, url) if (kind == "metadata" && table == "839") resp(200L, "<html><body>Just a moment...</body></html>") else NULL),
    "not a SIDRA table description.*Just a moment"
  )
  # more than one classification: the one-path coverage query does not apply
  expect_match(
    reason_for(metadata = function(table) {
      if (table != "839") return(metadata_json(table))
      cl <- function(id) list(id = id, nome = "x", categorias = list(list(id = 1L, nome = "y")))
      metadata_json(table, classifications = list(cl(81L), cl(99L)))
    }),
    "table 839 has 2 classifications \\(c81, c99\\)"
  )
  # the table declares neither value (215) nor quantity (214)
  expect_match(
    reason_for(metadata = function(table) {
      if (table != "839") return(metadata_json(table))
      sub('"id":214', '"id":9999', sub('"id":215', '"id":9998', metadata_json(table)), fixed = TRUE)
    }),
    "table 839 declares none of the variables 215/214"
  )
  # the coverage query changed shape: third dimension is no longer the year
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table != "839") return(coverage_json(table, variable))
      sub('"D3N":"Ano"', '"D3N":"Mes"', coverage_json(table, variable), fixed = TRUE)
    }),
    "the third dimension is 'Mes', not 'Ano'"
  )
  # monthly period codes
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table != "839") return(coverage_json(table, variable))
      gsub('"D3C":"20([0-9]{2})"', '"D3C":"20\\100"', coverage_json(table, variable))
    }),
    "not four-digit years"
  )
  # an empty table: header row only
  expect_match(
    reason_for(coverage = function(table, variable) {
      if (table == "839") as.character(jsonlite::toJSON(list(HEADER_ROW), auto_unbox = TRUE)) else coverage_json(table, variable)
    }),
    "returned no data rows at all"
  )
})

test_that("if EVERY row fails the resolver stops, naming the reasons", {
  down <- fake_ibge(override = function(kind, table, url) resp(500L, "Internal Server Error"))
  expect_error(run(down), "all 79 rows failed verification")
  expect_error(run(down), "pam/table 1612 \\(35 dataset\\(s\\)\\): .*HTTP 500: Internal Server Error")
})

# ---- guards on the input ----------------------------------------------------

test_that("the resolver refuses input it does not understand", {
  expect_error(resolve_pam(manifest_rows[0, ]), "no manifest rows tagged resolver == 'pam'")
  expect_error(resolve_pam(NULL), "no manifest rows tagged resolver == 'pam'")

  keyed <- manifest_rows
  keyed$geo_level[1] <- "state"
  expect_error(resolve_pam(keyed), "every PAM row is unkeyed")

  no_code <- manifest_rows
  no_code$sidra_code[1] <- NA_character_
  expect_error(resolve_pam(no_code), "every row needs a sidra_code")
})

# ---- pure helpers, table-driven ---------------------------------------------

test_that("pam_parse_code reads <table>/c<n>/<category> and rejects everything else", {
  expect_equal(pam_parse_code("1613/c82/45981"), list(table = "1613", classific = "c82", category = "45981"))
  expect_equal(pam_parse_code("5457/c782/all"), list(table = "5457", classific = "c782", category = "all"))
  # the old form parses (so it can be refused with a useful message)
  expect_equal(pam_parse_code("5457/all/all"), list(table = "5457", classific = "all", category = "all"))
  for (bad in c("", "1613", "1613/c82", "1613/c82/", "1613/82/2747", "1613/c82/x", "c82/2747", "1613/c82/2747/1", " 1613/c82/2747")) {
    expect_null(pam_parse_code(bad), info = bad)
  }
  expect_null(pam_parse_code(NA_character_))
  expect_null(pam_parse_code(NULL))
})

test_that("the package's sidra_code_parts() (what load_pam() uses) reads every code the way this resolver does", {
  for (code in manifest_rows$sidra_code) {
    ours <- sidra_code_parts(code)
    theirs <- pam_parse_code(code)
    expect_equal(as.character(ours$table), theirs$table, info = code)
    expect_equal(ours$classific, theirs$classific, info = code)
    expect_equal(ours$category, list(theirs$category), info = code)
  }
})

# (the years grammar, the value coding of the coverage query and the response
# excerpt are shared with the other SIDRA resolvers and tested in test-sidra-api.R)

# ---- the committed manifest and R/pam.R ------------------------------------------

test_that("every committed PAM row is verifiable: owned by the resolver, explicit classification, known geo levels", {
  expect_equal(nrow(manifest_rows), 79)
  expect_true(all(manifest_rows$resolver == "pam"))
  expect_true(all(vapply(parsed_codes, Negate(is.null), NA)))
  # classific = "all" sends sidrar to an IBGE page behind a bot challenge (resolve_pam.R, point 5)
  expect_false(any(vapply(parsed_codes, function(p) p$classific == "all", NA)))
  expect_setequal(row_table, names(FAKE_TABLES))
  geo <- unique(trimws(unlist(strsplit(tolower(manifest_rows$available_geo), ","))))
  expect_true(all(geo %in% names(SIDRA_API_GEO_LEVELS)))
  expect_false(anyNA(lapply(manifest_rows$available_time, parse_years)))
  # each row points at its own table's page (the survey page is bot-challenged)
  expect_equal(manifest_rows$docs_url, paste0("https://sidra.ibge.gov.br/tabela/", row_table))
})

test_that("R/pam.R hardcodes every table id the manifest uses (its FRAGILE tags say the two must agree)", {
  pam_src <- readLines(file.path(pkg_root, "R", "pam.R"), warn = FALSE)
  for (table in unique(row_table)) {
    expect_true(any(grepl(sprintf("\\b%s\\b", table), pam_src) & !grepl("^\\s*#", pam_src)), info = paste("table", table))
  }
})
