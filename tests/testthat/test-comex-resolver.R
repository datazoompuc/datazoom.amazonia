# tests/testthat/test-comex-resolver.R
#
# Offline test for actions/scrapers/resolve_comex.R (added 2026-10-17; see
# that file's header for the live investigation every assumption below comes
# from). COMEX publishes one bulk CSV per dataset per year; the resolver
# probes the candidate years with ONE ranged GET each and keeps available_time
# and the host current. The server's behaviors that shaped it, mocked here:
#   * a file that does not exist still answers 200 -- `text/plain`, a 1420-byte
#     Joomla index.php -- so a status code (or a HEAD) proves nothing and only
#     `text/csv` means "file";
#   * the old host's certificate expired and both hosts send an incomplete TLS
#     chain, so the resolver must stop() with a message that names the cause,
#     never weaken verification or guess a range;
#   * the loader (R/comex.R) reads specific raw columns, so a header missing
#     one must stop the resolver before it publishes that year.
#
# curl::curl_fetch_memory is mocked (no network). The fake world mirrors what
# was measured live on 2026-10-17: files for 1997-2026 in every dataset, the
# import-by-NCM files carrying two extra columns (VL_FRETE, VL_SEGURO).

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_comex.R"), local = TRUE)

comex_datasets <- c("export_mun", "export_prod", "import_mun", "import_prod")
comex_rows <- tibble::tibble(
  survey = "comex", dataset = comex_datasets,
  geo_level = NA_character_, year = NA_character_
)
# The two dictionaries load_trade_dic() reads: manifest survey "comex_dic".
comex_dictionaries <- c("hs_codes", "ncm_codes")
all_rows <- tibble::tibble(
  survey = c(rep("comex", 4), rep("comex_dic", 2)), dataset = c(comex_datasets, comex_dictionaries),
  geo_level = NA_character_, year = NA_character_
)
dictionary_rows <- all_rows[all_rows$survey == "comex_dic", ]

# The header line the real dictionaries start with (read live 2026-10-19).
dictionary_header_line <- function(dataset, drop = character(0)) {
  cols <- switch(dataset,
    hs_codes = c(
      "CO_SH6", "NO_SH6_POR", "NO_SH6_ESP", "NO_SH6_ING", "CO_SH4", "NO_SH4_POR", "NO_SH4_ESP",
      "NO_SH4_ING", "CO_SH2", "NO_SH2_POR", "NO_SH2_ESP", "NO_SH2_ING", "CO_NCM_SECROM",
      "NO_SEC_POR", "NO_SEC_ESP", "NO_SEC_ING"
    ),
    ncm_codes = c(
      "CO_NCM", "CO_UNID", "CO_SH6", "CO_PPE", "CO_PPI", "CO_FAT_AGREG", "CO_CUCI_ITEM",
      "CO_CGCE_N3", "CO_SIIT", "CO_ISIC_CLASSE", "CO_EXP_SUBSET", "NO_NCM_POR", "NO_NCM_ESP", "NO_NCM_ING"
    )
  )
  paste0('"', setdiff(cols, drop), '"', collapse = ";")
}

# The header line each dataset's real files start with (measured live).
header_line <- function(dataset, drop = character(0)) {
  cols <- switch(dataset,
    export_mun = , import_mun = COMEX_MUN_COLUMNS,
    export_prod = COMEX_NCM_COLUMNS,
    import_prod = c(COMEX_NCM_COLUMNS, "VL_FRETE", "VL_SEGURO")
  )
  paste0('"', setdiff(cols, drop), '"', collapse = ";")
}

csv_response <- function(dataset, drop = character(0)) {
  list(
    status_code = 206L,
    headers = charToRaw("HTTP/1.1 206 Partial Content\r\ncontent-type: text/csv\r\ncontent-range: bytes 0-4095/60077097\r\n\r\n"),
    content = charToRaw(paste0(header_line(dataset, drop), '\n"2025";"07";"27011200"\n'))
  )
}
# What the server answers for a path that does not exist: 200, text/plain.
placeholder_response <- function() {
  list(
    status_code = 200L,
    headers = charToRaw("HTTP/1.1 200 OK\r\ncontent-type: text/plain; charset=UTF-8\r\ncontent-length: 1420\r\n\r\n"),
    content = charToRaw("<?php\n/**\n * @package    Joomla.Site\n */\ndefine('_JEXEC', 1);\n")
  )
}
status_response <- function(status, ctype = "text/html") {
  list(
    status_code = status,
    headers = charToRaw(paste0("HTTP/1.1 ", status, " X\r\ncontent-type: ", ctype, "\r\n\r\n")),
    content = charToRaw("<html>nope</html>")
  )
}

# A fake COMEX server. `years` = which years exist per dataset; `drop` =
# columns missing from a given dataset's files (optionally only some years);
# `dictionaries` = which of the two dictionaries exist (a missing one answers
# with the placeholder, like any wrong path); `override` = function(dataset,
# year) -> a response, or NULL to fall through (year is NA for a dictionary).
fake_comex <- function(years = NULL, drop = list(), override = function(dataset, year) NULL, seen = NULL,
                       dictionaries = comex_dictionaries) {
  if (is.null(years)) {
    years <- stats::setNames(rep(list(1997:2026), length(comex_datasets)), comex_datasets)
  }
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    d <- regmatches(url, regexec("/balanca/bd/tabelas/(NCM_SH|NCM)\\.csv$", url))[[1]]
    if (length(d) > 0) {
      dataset <- c(NCM_SH = "hs_codes", NCM = "ncm_codes")[[d[2]]]
      o <- override(dataset, NA_integer_)
      if (!is.null(o)) {
        return(o)
      }
      if (!dataset %in% dictionaries) {
        return(placeholder_response())
      }
      return(list(
        status_code = 206L,
        headers = charToRaw("HTTP/1.1 206 Partial Content\r\ncontent-type: text/csv\r\ncontent-range: bytes 0-4095/3356175\r\n\r\n"),
        content = charToRaw(paste0(dictionary_header_line(dataset, drop[[dataset]]), '\n"49011000";"10"\n'))
      ))
    }
    m <- regmatches(url, regexec("/comexstat-bd/(mun|ncm)/(EXP|IMP)_([0-9]{4})(_MUN)?\\.csv$", url))[[1]]
    if (length(m) == 0) {
      return(placeholder_response())
    }
    dataset <- paste0(
      c(EXP = "export", IMP = "import")[[m[3]]], "_",
      c(mun = "mun", ncm = "prod")[[m[2]]]
    )
    year <- as.integer(m[4])
    o <- override(dataset, year)
    if (!is.null(o)) {
      return(o)
    }
    if (year %in% years[[dataset]]) csv_response(dataset, drop[[dataset]]) else placeholder_response()
  }
}

run <- function(fetch, rows = comex_rows) {
  testthat::with_mocked_bindings(
    resolve_comex(rows),
    curl_fetch_memory = fetch, .package = "curl"
  )
}
error_message <- function(fetch, rows = comex_rows) {
  tryCatch(run(fetch, rows), error = function(e) conditionMessage(e))
}

test_that("every dataset gets its own verified range and the `$year$` URL on the current host", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen))

  expect_named(out, c("survey", "dataset", "geo_level", "year", "url", "docs_url", "available_time"))
  expect_equal(out$dataset, comex_datasets)
  expect_equal(out$available_time, rep("1997-2026", 4))
  expect_equal(out$url, c(
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/mun/EXP_$year$_MUN.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_$year$.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/mun/IMP_$year$_MUN.csv",
    "https://balanca.mdic.gov.br/balanca/bd/comexstat-bd/ncm/IMP_$year$.csv"
  ))
  expect_true(all(out$docs_url == COMEX_DOCS_URL))
  # blank-key rows only, never a dataset = NA row and no monthly `version`
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  expect_false("version" %in% names(out))

  # nothing was probed anywhere but the current host, and `ncmv2/` never is
  expect_true(all(startsWith(seen$urls, COMEX_HOST)))
  expect_false(any(grepl("ncmv2", seen$urls, fixed = TRUE)))
})

test_that("a 200 text/plain placeholder is a missing file, not a year (the server's catch-all)", {
  years <- stats::setNames(rep(list(1997:2025), 4), comex_datasets) # 2026 and everything else: placeholder
  out <- run(fake_comex(years))
  expect_equal(out$available_time, rep("1997-2025", 4))
})

test_that("ranges are per dataset, not shared", {
  years <- list(
    export_mun = 1997:2026, export_prod = 1997:2026,
    import_mun = 1997:2025, import_prod = 1998:2025
  )
  out <- run(fake_comex(years))
  expect_equal(stats::setNames(out$available_time, out$dataset), c(
    export_mun = "1997-2026", export_prod = "1997-2026",
    import_mun = "1997-2025", import_prod = "1998-2025"
  ))
})

test_that("only the manifest rows it owns are returned, and only their files are probed", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen), rows = comex_rows[comex_rows$dataset == "import_mun", ])
  expect_equal(out$dataset, "import_mun")
  expect_true(all(grepl("/mun/IMP_", seen$urls, fixed = TRUE)))
})

test_that("no real file at all stops, naming the dataset and the path probed", {
  msg <- error_message(fake_comex(stats::setNames(rep(list(integer(0)), 4), comex_datasets)))
  expect_match(msg, "no real CSV (text/csv) found for 'export_mun'", fixed = TRUE)
  expect_match(msg, "mun/EXP_$year$_MUN.csv", fixed = TRUE)
})

test_that("a hole inside the range stops: available_time cannot express a gap", {
  years <- stats::setNames(rep(list(setdiff(1997:2026, 2010L)), 4), comex_datasets)
  msg <- error_message(fake_comex(years))
  expect_match(msg, "except year(s) 2010", fixed = TRUE)
})

test_that("a header missing a column R/comex.R reads stops, naming the column and the year", {
  msg <- error_message(fake_comex(drop = list(import_mun = "CO_MUN")))
  expect_match(msg, "'import_mun' files no longer carry the columns R/comex.R reads", fixed = TRUE)
  expect_match(msg, "missing CO_MUN", fixed = TRUE)

  # only recent years broken: still caught, because every year is checked
  recent_only <- function(dataset, year) {
    if (dataset == "export_prod" && year >= 2025) csv_response(dataset, drop = "KG_LIQUIDO") else NULL
  }
  msg <- error_message(fake_comex(override = recent_only))
  expect_match(msg, "2025: missing KG_LIQUIDO", fixed = TRUE)
})

test_that("extra columns are fine (import-by-NCM files carry VL_FRETE and VL_SEGURO)", {
  expect_equal(header_line("import_prod"), paste0('"', c(COMEX_NCM_COLUMNS, "VL_FRETE", "VL_SEGURO"), '"', collapse = ";"))
  expect_equal(run(fake_comex())$dataset, comex_datasets)
})

test_that("a status that is neither a CSV nor a clean 404 stops instead of being read as 'absent'", {
  msg <- error_message(fake_comex(override = function(dataset, year) if (year == 2026) status_response(403L) else NULL))
  expect_match(msg, "unexpected HTTP 403", fixed = TRUE)

  # a 302 is the host saying the layout moved: neither "file" nor "absent".
  # (That the resolver's curl handle does not FOLLOW redirects is a handle
  # option, invisible to this mock -- not covered here; see the resolver.)
  msg <- error_message(fake_comex(override = function(dataset, year) if (year == 1995) status_response(302L) else NULL))
  expect_match(msg, "unexpected HTTP 302", fixed = TRUE)

  # a clean 404 (should the server ever stop faking 200) is simply absent
  not_found <- function(dataset, year) if (year > 2025 || year < 1997) status_response(404L) else NULL
  expect_equal(run(fake_comex(override = not_found))$available_time, rep("1997-2025", 4))
})

test_that("an expired certificate stops with the cause named", {
  expired <- function(url, handle = NULL, ...) {
    stop("schannel: next InitializeSecurityContext failed: SEC_E_CERT_EXPIRED (0x80090328) - The received certificate has expired.")
  }
  msg <- error_message(expired)
  expect_match(msg, "could not reach https://balanca.mdic.gov.br/", fixed = TRUE)
  expect_match(msg, "TLS certificate has expired", fixed = TRUE)
  expect_match(msg, "curl said: schannel", fixed = TRUE)
})

test_that("an unverifiable chain (OpenSSL, intermediate not sent) stops and says what to add", {
  chain <- function(url, handle = NULL, ...) {
    stop("SSL peer certificate or SSH remote key was not OK: [balanca.mdic.gov.br] SSL certificate problem: unable to get local issuer certificate")
  }
  msg <- error_message(chain)
  expect_match(msg, "cannot verify the server's TLS certificate chain", fixed = TRUE)
  expect_match(msg, "AC SERPRO AR46 OV TLS CA 2025", fixed = TRUE)
  expect_match(msg, "never turn verification off", fixed = TRUE)
})

test_that("any other transport failure is reported as no HTTP response, with curl's text", {
  msg <- error_message(function(url, handle = NULL, ...) stop("Timeout was reached"))
  expect_match(msg, "no HTTP response", fixed = TRUE)
  expect_match(msg, "curl said: Timeout was reached", fixed = TRUE)
})

test_that("a dataset this resolver does not know, or no rows, stops", {
  rows <- tibble::tibble(survey = "comex", dataset = "export_sh6", geo_level = NA_character_, year = NA_character_)
  expect_match(error_message(fake_comex(), rows), "only knows export_mun, import_mun, export_prod, import_prod, hs_codes, ncm_codes", fixed = TRUE)
  expect_match(error_message(fake_comex(), comex_rows[0, ]), "no manifest rows tagged resolver == 'comex'", fixed = TRUE)
})

test_that("the two dictionaries are verified and emitted under survey comex_dic, with no available_time", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen), rows = all_rows)

  # one output row per input row, same order, each under its own survey
  expect_equal(out$survey, all_rows$survey)
  expect_equal(out$dataset, all_rows$dataset)
  expect_equal(out$url[5:6], c(
    "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv",
    "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM.csv"
  ))
  expect_equal(out$docs_url, rep(COMEX_DOCS_URL, 6))
  # the dictionaries have no years: NA = "leave the cell alone"; the yearly rows are unchanged
  expect_true(all(is.na(out$available_time[5:6])))
  expect_equal(out$available_time[1:4], rep("1997-2026", 4))
  expect_equal(out$url[1:4], run(fake_comex(), rows = comex_rows)$url)
  # both were really probed, on the current host only
  expect_true(all(c(
    "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM_SH.csv",
    "https://balanca.mdic.gov.br/balanca/bd/tabelas/NCM.csv"
  ) %in% seen$urls))
  expect_true(all(startsWith(seen$urls, COMEX_HOST)))
})

test_that("dictionary rows alone are resolved without probing any yearly file", {
  seen <- new.env()
  out <- run(fake_comex(seen = seen), rows = dictionary_rows)
  expect_equal(out$dataset, comex_dictionaries)
  expect_length(seen$urls, 2)
  expect_false(any(grepl("comexstat-bd", seen$urls, fixed = TRUE)))
})

test_that("a dictionary that is not a real CSV (the server's 200 text/plain placeholder) stops", {
  msg <- error_message(fake_comex(dictionaries = "ncm_codes"), rows = dictionary_rows)
  expect_match(msg, "no real CSV (text/csv) found for dictionary 'hs_codes'", fixed = TRUE)
  expect_match(msg, "tabelas/NCM_SH.csv", fixed = TRUE)
  expect_match(msg, "text/plain placeholder", fixed = TRUE)
})

test_that("a dictionary missing a column load_trade_dic() selects stops, naming it", {
  msg <- error_message(fake_comex(drop = list(hs_codes = "NO_SH4_ING")), rows = dictionary_rows)
  expect_match(msg, "dictionary 'hs_codes'", fixed = TRUE)
  expect_match(msg, "missing NO_SH4_ING", fixed = TRUE)

  msg <- error_message(fake_comex(drop = list(ncm_codes = c("CO_NCM", "NO_NCM_POR"))), rows = dictionary_rows)
  expect_match(msg, "dictionary 'ncm_codes'", fixed = TRUE)
  expect_match(msg, "missing CO_NCM, NO_NCM_POR", fixed = TRUE)

  # columns the loader does not read are free to come and go (NCM_SH.csv has 16, NCM.csv 14)
  expect_equal(run(fake_comex(drop = list(hs_codes = "NO_SEC_ING")), rows = dictionary_rows)$dataset, comex_dictionaries)
})

test_that("a row under the wrong survey is refused: a dictionary under 'comex' would pass as a dataset of load_br_trade()", {
  wrong <- all_rows[all_rows$dataset %in% c("export_mun", "hs_codes"), ]
  wrong$survey <- c("comex_dic", "comex") # swapped
  msg <- error_message(fake_comex(), rows = wrong)
  expect_match(msg, "under the wrong survey", fixed = TRUE)
  expect_match(msg, "comex_dic/export_mun", fixed = TRUE)
  expect_match(msg, "comex/hs_codes", fixed = TRUE)
})

test_that("header parsing tolerates a BOM, quotes, case and garbage bytes", {
  expect_equal(
    comex_header_columns(charToRaw(paste0(intToUtf8(0xFEFF), "\"co_ano\";\"CO_MES\";SH4\r\n\"2025\";\"01\"\n"))),
    c("CO_ANO", "CO_MES", "SH4")
  )
  expect_equal(comex_header_columns(raw(0)), character(0))
  expect_no_error(comex_header_columns(as.raw(c(0xe9, 0xff, 0x3b, 0x00, 0x41, 0x0a))))
})

test_that("the columns the resolver demands are the ones R/comex.R actually reads", {
  comex_src <- file.path(pkg_root, "R", "comex.R")
  skip_if_not(file.exists(comex_src), "R/comex.R not present")
  src <- tolower(paste(readLines(comex_src, warn = FALSE), collapse = "\n"))
  for (dataset in comex_datasets) {
    for (col in COMEX_DATASETS[[dataset]]$required) {
      expect_true(
        grepl(paste0("\\b", tolower(col), "\\b"), src),
        info = paste(dataset, col, "is required by resolve_comex.R but R/comex.R never mentions it")
      )
    }
  }
  # the dictionaries' columns, which load_trade_dic()'s callers select
  for (dataset in comex_dictionaries) {
    for (col in COMEX_DICTIONARIES[[dataset]]$required) {
      expect_true(
        grepl(paste0("\\b", tolower(col), "\\b"), src),
        info = paste(dataset, col, "is required by resolve_comex.R but R/comex.R never mentions it")
      )
    }
  }
})

test_that("the manifest is the single source of truth for the dictionary URLs: R/comex.R hard-codes none", {
  # Until 2026-10-19 load_trade_dic() built NCM.csv / NCM_SH.csv URLs from a
  # literal no resolver touched. When the old host's certificate expired
  # (2026-10-03) the manifest could be fixed for every user at once, but that
  # literal reached users only with a new package version. They are manifest
  # rows now (survey "comex_dic"); this keeps a URL from creeping back in.
  comex_src <- file.path(pkg_root, "R", "comex.R")
  skip_if_not(file.exists(comex_src), "R/comex.R not present")

  # (a) statically: no URL at all in the code (comments may still cite examples)
  code <- readLines(comex_src, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("https?://", code)), info = "R/comex.R's code must not contain a URL literal")
  expect_true(any(grepl('dataset_url(source = "comex_dic"', code, fixed = TRUE)))

  skip_if_not(exists("load_trade_dic", mode = "function"), "package code not loaded")

  # (b) behaviorally: what load_trade_dic() reads is exactly what the manifest
  # lookup returns, for each dictionary -- a made-up manifest drives it.
  seen <- character(0)
  # load_trade_dic() downloads to a temp file and reads it from there, so the
  # URL it was given shows up in download.file(), not in read_delim().
  download_spy <- function(url, destfile, ...) {
    seen <<- c(seen, url)
    invisible(0L)
  }
  read_spy <- function(file, ...) {
    tibble::tibble(co_sh4 = "0101", no_sh4_por = "a", no_sh4_ing = "b")
  }
  lookup <- function(source, dataset, ...) paste0("manifest://", source, "/", dataset)
  load_with <- function(type, dataset_url_fn = lookup) {
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        testthat::with_mocked_bindings(load_trade_dic(type), read_delim = read_spy, .package = "readr"),
        download.file = download_spy, .package = "utils"
      ),
      dataset_url = dataset_url_fn, .package = "datazoom.amazonia"
    )
  }
  suppressMessages({
    load_with("hs")
    load_with("ncm")
  })
  expect_equal(seen, c("manifest://comex_dic/hs_codes", "manifest://comex_dic/ncm_codes"))

  # (c) and the committed manifest supplies them (setup.R forces the local snapshot)
  expect_equal(dataset_url(source = "comex_dic", dataset = "hs_codes"), paste0(COMEX_BD, "tabelas/NCM_SH.csv"))
  expect_equal(dataset_url(source = "comex_dic", dataset = "ncm_codes"), paste0(COMEX_BD, "tabelas/NCM.csv"))

  # (d) a manifest without the row, or an unknown type, fails with a clear
  # message instead of handing NA / an undefined variable to read_delim()
  expect_error(
    load_with("hs", function(source, dataset, ...) NA_character_),
    "No download URL found for source 'comex_dic', dataset 'hs_codes'",
    fixed = TRUE
  )
  expect_error(load_with("cuci"), "unknown dictionary type 'cuci'", fixed = TRUE)
})

test_that("the committed manifest rows agree with what the resolver owns and emits", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  all_m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")
  m <- all_m[all_m$survey == "comex", ]
  expect_setequal(m$dataset, comex_datasets)
  expect_true(all(m$resolver == "comex"))
  expect_true(all(startsWith(m$url, COMEX_BASE)))
  expect_true(all(grepl("$year$", m$url, fixed = TRUE)))
  expect_true(all(m$docs_url == COMEX_DOCS_URL))
  expect_true(all(grepl("^1997-[0-9]{4}$", m$available_time)))

  # the two dictionaries: own survey, same resolver, plain URL on the same host, no years
  d <- all_m[all_m$survey == COMEX_DICTIONARY_SURVEY, ]
  expect_setequal(d$dataset, comex_dictionaries)
  expect_true(all(d$resolver == "comex"))
  expect_true(all(d$docs_url == COMEX_DOCS_URL))
  expect_true(all(is.na(d$available_time) & is.na(d$geo_level) & is.na(d$year)))
  expect_equal(
    stats::setNames(d$url, d$dataset)[comex_dictionaries],
    stats::setNames(paste0(COMEX_BD, vapply(COMEX_DICTIONARIES[comex_dictionaries], function(s) s$path, "")), comex_dictionaries)
  )
})

test_that("a dictionary is not a dataset of load_br_trade(): that is why it has its own survey", {
  skip_if_not(exists("check_params", mode = "function"), "package code not loaded")
  # check_params() validates a dataset against EVERY row of its survey. Under
  # "comex" these two would be accepted and then crash inside load_br_trade();
  # under "comex_dic" the validation rejects them up front.
  expect_error(
    check_params(list(source = "comex", dataset = "hs_codes", time_period = 2020)),
    "Dataset hs_codes not supported"
  )
  expect_no_error(check_params(list(source = "comex", dataset = "export_mun", time_period = 2020)))
})

test_that("the CI wiring that lets a Linux runner verify balanca.mdic.gov.br is in place and never weakens TLS", {
  # The host omits its intermediate CA (resolve_comex.R's header, point 2), so
  # every workflow that downloads from it needs the composite action below.
  # Nothing fails loudly if someone drops the step -- COMEX just goes red on
  # Linux again -- hence this guard. .github/ is .Rbuildignore'd: skipped there.
  skip_if_not_installed("yaml")
  action_path <- file.path(pkg_root, ".github", "actions", "trust-serpro-intermediate", "action.yml")
  skip_if_not(file.exists(action_path), ".github/ not present (stripped build)")

  action <- yaml::read_yaml(action_path)
  expect_equal(action$runs$using, "composite")
  scripts <- vapply(action$runs$steps, function(s) {
    expect_equal(s$shell, "bash") # every composite `run` step must name its shell
    s$run
  }, character(1))
  script <- paste(scripts, collapse = "\n")
  # the safety properties, in the script itself
  expect_match(script, "AC SERPRO AR46 OV TLS CA 2025", fixed = TRUE) # refuses any other certificate
  expect_match(script, "openssl verify -CAfile /etc/ssl/certs/ca-certificates.crt", fixed = TRUE) # must chain to a root already trusted
  expect_false(grepl("--insecure|curl[^\n]* -k |ssl_verifypeer|verify=False|NODE_TLS_REJECT_UNAUTHORIZED|GIT_SSL_NO_VERIFY", script))

  step_ref <- "./.github/actions/trust-serpro-intermediate"
  wired <- list(
    "update-manifest.yaml" = list(job = "refresh-manifest", before = "Run build_manifest.R", after = "Install resolver + package dependencies"),
    "check-load-br-trade.yaml" = list(job = "check", before = "Run checks for load_br_trade", after = "r-lib/actions/setup-r-dependencies@v2"),
    "check-dz-amazonia.yaml" = list(job = "check-downloads", before = "Run checks for one function", after = "Install package dependencies")
  )
  for (wf_name in names(wired)) {
    wf <- yaml::read_yaml(file.path(pkg_root, ".github", "workflows", wf_name))
    job <- wf$jobs[[wired[[wf_name]]$job]]
    expect_equal(job$`runs-on`, "ubuntu-latest", info = wf_name) # the step is Linux-only (sudo, update-ca-certificates)
    steps <- job$steps
    label <- function(s) {
      if (!is.null(s$name)) s$name else if (!is.null(s$uses)) s$uses else ""
    }
    labels <- vapply(steps, label, character(1))
    i_use <- which(vapply(steps, function(s) identical(s$uses, step_ref), logical(1)))
    expect_length(i_use, 1)
    expect_true(isTRUE(steps[[i_use]]$`continue-on-error`), info = wf_name) # a failure must not take down the whole job
    expect_gt(i_use, which(labels == wired[[wf_name]]$after)[1]) # after the dependencies are installed ...
    expect_lt(i_use, which(labels == wired[[wf_name]]$before)[1]) # ... before the R script that downloads
    text_of <- function(x) if (is.null(x)) "" else x
    expect_lt(which(vapply(steps, function(s) grepl("actions/checkout", text_of(s$uses)), logical(1)))[1], i_use) # a local action needs the checkout first
    # no leftover inline copy of the script next to the action
    expect_false(any(vapply(steps, function(s) grepl("acserproar46ovtlsca2025", text_of(s$run)), logical(1))), info = wf_name)
  }
  # only the load_br_trade job of the matrix workflow gets it: the other 21 are untouched
  matrix_step <- yaml::read_yaml(file.path(pkg_root, ".github", "workflows", "check-dz-amazonia.yaml"))$jobs[["check-downloads"]]$steps
  matrix_step <- Filter(function(s) identical(s$uses, step_ref), matrix_step)[[1]]
  expect_equal(matrix_step$`if`, "matrix.fn == 'load_br_trade'")
})
