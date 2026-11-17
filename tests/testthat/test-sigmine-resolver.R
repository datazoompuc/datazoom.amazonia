# tests/testthat/test-sigmine-resolver.R
#
# Offline unit tests for actions/scrapers/resolve_sigmine.R.
# Mirrors ANM's IIS directory listing HTML and real .dbf payloads (built via
# foreign::write.dbf()) while mocking HTTP and ranged-GET zip helpers.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")
skip_if_not_installed("foreign")

resolver_env <- new.env(parent = globalenv())
source(file.path(actions_dir, "scripts", "zip_remote_listing.R"), local = resolver_env)
source(file.path(actions_dir, "scrapers", "resolve_sigmine.R"), local = resolver_env)

sigmine_rows <- tibble::tibble(
  survey = c("sigmine", "sigmine"),
  dataset = c("sigmine_active", "sigmine_inactive"),
  geo_level = c(NA_character_, NA_character_),
  year = c(NA_character_, NA_character_)
)

FAKE_DIR_HTML <- paste0(
  '<html><head><title>dadosabertos.anm.gov.br - /SIGMINE/PROCESSOS_MINERARIOS/</title></head><body>',
  '<pre><A HREF="/SIGMINE/">[To Parent Directory]</A><br>',
  '11/16/2026 11:11 PM 127834880 <A HREF="/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip">BRASIL.zip</A><br>',
  '11/16/2026 11:12 PM 150800162 <A HREF="/SIGMINE/PROCESSOS_MINERARIOS/PROCESSOS_INATIVOS.zip">PROCESSOS_INATIVOS.zip</A><br>',
  '</pre></body></html>'
)

raw_dbf_bytes <- function(df) {
  p <- tempfile(fileext = ".dbf")
  on.exit(unlink(p), add = TRUE)
  foreign::write.dbf(df, p)
  readBin(p, what = "raw", n = file.info(p)$size)
}

active_dbf_df <- function(years = c(1934L, 2026L)) {
  n <- length(years)
  data.frame(
    PROCESSO = rep("1/2020", n),
    NUMERO = rep(1L, n),
    ANO = years,
    AREA_HA = rep(2.0, n),
    ID = rep("1", n),
    FASE = rep("AUTORIZACAO", n),
    ULT_EVENTO = rep("EVENTO", n),
    NOME = rep("EMPRESA", n),
    SUBS = rep("OURO", n),
    USO = rep("INDUSTRIAL", n),
    UF = rep("PA", n),
    DSProcesso = rep("000.001/2020", n),
    stringsAsFactors = FALSE
  )
}

inactive_dbf_df <- function(years = c(1935L, 2026L)) {
  n <- length(years)
  data.frame(
    PROCESSO = rep("2/1935", n),
    NUMERO = rep(2L, n),
    ANO = years,
    AREA_HA = rep(1.5, n),
    ID = rep("2", n),
    DSProcesso = rep("000.002/1935", n),
    stringsAsFactors = FALSE
  )
}

run_resolver <- function(rows = sigmine_rows,
                         dir_status = 200L,
                         dir_html = FAKE_DIR_HTML,
                         entries_fn = NULL,
                         extract_fn = NULL) {
  if (is.null(entries_fn)) {
    entries_fn <- function(url, ...) {
      if (grepl("BRASIL\\.zip$", url)) {
        list("BRASIL.shp" = list(), "BRASIL.dbf" = list(name = "BRASIL.dbf"))
      } else {
        list("PROCESSOS_INATIVOS.shp" = list(), "PROCESSOS_INATIVOS.dbf" = list(name = "PROCESSOS_INATIVOS.dbf"))
      }
    }
  }
  if (is.null(extract_fn)) {
    extract_fn <- function(url, entry, ...) {
      if (grepl("BRASIL\\.zip$", url)) {
        raw_dbf_bytes(active_dbf_df())
      } else {
        raw_dbf_bytes(inactive_dbf_df())
      }
    }
  }

  old_entries <- resolver_env$zip_remote_entries
  old_extract <- resolver_env$zip_remote_extract_entry
  on.exit({
    resolver_env$zip_remote_entries <- old_entries
    resolver_env$zip_remote_extract_entry <- old_extract
  }, add = TRUE)
  resolver_env$zip_remote_entries <- entries_fn
  resolver_env$zip_remote_extract_entry <- extract_fn

  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle = NULL, ...) {
      list(status_code = dir_status, content = charToRaw(dir_html))
    },
    .package = "curl"
  )

  resolver_env$resolve_sigmine(rows)
}

test_that("resolve_sigmine stops when rows are missing, empty, or unmapped", {
  expect_error(resolver_env$resolve_sigmine(), "no manifest rows tagged")
  expect_error(resolver_env$resolve_sigmine(tibble::tibble()), "no manifest rows tagged")

  bad <- tibble::tibble(
    survey = "sigmine", dataset = "sigmine_unknown",
    geo_level = NA_character_, year = NA_character_
  )
  expect_error(resolver_env$resolve_sigmine(bad), "unmapped SIGMINE dataset")
})

test_that("sigmine_parse_dir_links resolves root-relative, relative, and absolute hrefs", {
  links <- resolver_env$sigmine_parse_dir_links(
    '<A HREF="/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip">A</A><a href="PROCESSOS_INATIVOS.zip">B</a><a href="https://example.org/X.zip">C</a>'
  )
  expect_equal(
    links,
    c(
      "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip",
      "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/PROCESSOS_INATIVOS.zip",
      "https://example.org/X.zip"
    )
  )
})

test_that("resolve_sigmine resolves both datasets, verifies DBF schema and years, and omits version", {
  res <- run_resolver()

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L)
  expect_equal(res$dataset, c("sigmine_active", "sigmine_inactive"))
  expect_equal(
    res$url,
    c(
      "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip",
      "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/PROCESSOS_INATIVOS.zip"
    )
  )
  expect_equal(res$docs_url, c("https://dadosabertos.anm.gov.br/", "https://dadosabertos.anm.gov.br/"))
  expect_equal(res$available_time, c("1934-2026", "1935-2026"))
  expect_equal(res$resolver, c("sigmine", "sigmine"))
  expect_false("version" %in% names(res))
  expect_null(attr(res, "partial_failures"))
})

test_that("resolve_sigmine stops when the directory listing fails or lists no zips", {
  expect_error(run_resolver(dir_status = 503L), "HTTP 503")
  expect_error(run_resolver(dir_html = "<html><body>empty</body></html>"), "no .zip links")
})

test_that("one dataset failing produces partial_failures and preserves the healthy row", {
  # Case 1: missing from directory listing
  html_only_active <- sub('<A HREF="/SIGMINE/PROCESSOS_MINERARIOS/PROCESSOS_INATIVOS.zip">[^<]+</A>', "", FAKE_DIR_HTML)
  res1 <- run_resolver(dir_html = html_only_active)
  expect_equal(res1$dataset, "sigmine_active")
  expect_length(attr(res1, "partial_failures"), 1L)
  expect_match(attr(res1, "partial_failures")[1L], "sigmine_inactive:.*PROCESSOS_INATIVOS\\.zip")

  # Case 2: missing .shp inside zip for active
  res2 <- run_resolver(entries_fn = function(url, ...) {
    if (grepl("BRASIL\\.zip$", url)) {
      list("BRASIL.dbf" = list(name = "BRASIL.dbf"))
    } else {
      list("PROCESSOS_INATIVOS.shp" = list(), "PROCESSOS_INATIVOS.dbf" = list(name = "PROCESSOS_INATIVOS.dbf"))
    }
  })
  expect_equal(res2$dataset, "sigmine_inactive")
  expect_match(attr(res2, "partial_failures")[1L], "sigmine_active:.*expected 1 \\.shp")

  # Case 3: broken positional nome:uf slice for active
  res3 <- run_resolver(extract_fn = function(url, entry, ...) {
    if (grepl("BRASIL\\.zip$", url)) {
      df <- active_dbf_df()
      df <- df[, c("PROCESSO", "NUMERO", "ANO", "AREA_HA", "ID", "ULT_EVENTO", "NOME", "FASE", "SUBS", "USO", "UF", "DSProcesso")]
      raw_dbf_bytes(df)
    } else {
      raw_dbf_bytes(inactive_dbf_df())
    }
  })
  expect_equal(res3$dataset, "sigmine_inactive")
  expect_match(attr(res3, "partial_failures")[1L], "positional slice nome:uf")
})

test_that("resolve_sigmine stops when all datasets fail", {
  expect_error(
    run_resolver(entries_fn = function(url, ...) list("readme.txt" = list())),
    "all datasets failed"
  )
})
