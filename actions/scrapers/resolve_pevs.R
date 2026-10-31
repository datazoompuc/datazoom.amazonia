# actions/scrapers/resolve_pevs.R
#
# PEVS (Producao da Extracao Vegetal e da Silvicultura, IBGE) is read LIVE from
# IBGE's SIDRA API, like PAM and PPM: there is no file and no `url`. Each of the 3
# manifest rows is one SIDRA table, and R/pevs.R's load_pevs() hands the row's
# `sidra_code` to sidra_download(). A thin per-source layer on
# actions/scripts/sidra_api.R (the metadata lookup, the coverage query and the value
# coding every SIDRA resolver shares), the way resolve_pam.R is; what is here is
# PEVS's own: the grammar of its sidra_code, which variable ids load_pevs() names,
# and what its dictionary labels.
#
# SAME MODEL AS resolve_pam.R, resolve_ppm.R: a VERIFICATION resolver. Nothing needs
# discovering (the table ids are fixed), but IBGE's metadata says whether the manifest
# still describes the table, and its data says how far each series reaches. So every
# run VERIFIES each row's sidra_code against the table's metadata and REFRESHES
# available_time from the table's own data (and keeps docs_url pointed at the table's
# own page).
#
# VERIFIED LIVE (2026-10-31):
#
#   1. THE TABLES. https://servicodados.ibge.gov.br/api/v3/agregados/<table>/metadados
#      answers all three (HTTP 200, no bot challenge); all are annual, offer N1, N2,
#      N3, N6, N8 and N9 (so available_geo "country, state, municipality, region"
#      holds), share no table with any other survey, and have ONE classification each:
#         289   "Quantidade produzida e valor da producao na extracao vegetal"
#               c193 "Tipo de produto extrativo", 64 categories; variables 144
#               (quantity) and 145 (value)
#         291   "... na silvicultura", c194 "Tipo de produto da silvicultura", 38
#               categories; variables 142 (quantity) and 143 (value)
#         5930  "Area total existente em 31/12 dos efetivos da silvicultura"
#               c734 "Especie florestal", 8 categories; variable 6549 (area)
#      (each also declares a "... - percentual do total geral" variable, 1000145,
#      1000143, 1006549, which sidrar's "allxp" never requests; the data confirms it:
#      the all-variables query returns exactly 144/145, 142/143 and 6549.)
#
#   2. THE sidra_code SHAPE IS "<table>/c<classification>/all". The manifest said just
#      "289", "291", "5930", and load_pevs() passed that to sidra_download() with its
#      default classific = "all". For classific = "all" sidrar::get_sidra() first
#      scrapes an IBGE HTML page (apisidra.ibge.gov.br/desctabapi.aspx?c=<table>) to
#      learn the classification ids, and that page answers HTTP 403 behind a Cloudflare
#      bot challenge ("Just a moment...") from the machine this was investigated on:
#      12 tries (4 per table, state level, 2023) all failed with a bare "Error in
#      Download."; with the classification spelled out ("289/c193/all", the same
#      explicit form PAM and PPM use, and the identical request: sidrar builds it
#      without the page) the same 12 calls all returned data. The challenge is not a
#      property of a table: probed with curl, the page answered HTTP 200 now and then
#      for each of 289, 291, 5930, 5938 and 74 and HTTP 403 most of the time (4 of 15
#      requests passed), so on such a network load_pevs() failed mostly but not always.
#      This resolver refuses classific = "all" and a bare code (for a table that has a
#      classification a bare code would still answer HTTP 200, with only the "Total"
#      slice), and says which code to write.
#
#   3. WHAT WENT STALE. available_time: the manifest said 1986-2023 (2013-2023 for
#      5930); IBGE publishes through 2025 for all three, with no gap in the union of
#      the categories (1986-2025, 1986-2025, 2013-2025). The convention is
#      resolve_pam.R's: a year counts as covered when its value is neither "..." (not
#      available) nor ".." (not applicable); "-" (zero) and "X" (suppressed) count. The
#      variable that decides it is value of production (145, 143) where the table has
#      one -- the "Total" category has no quantity -- and otherwise (5930) the area.
#      The datasets read the whole table, so the union of the categories is the answer.
#      Categories do differ: 289 has 41 categories for 1986-2025, 9 for 1986-2024, 10
#      published only for 2025, and a few others; 291 has 16 published only for 2025.
#
#   4. THE case_when() GAP load_pam() HAD. load_pevs() named its variables with two
#      case_when() blocks with no default (142/144 "quant", 143/145 "value", 6549
#      "area"); the three tables report exactly those, so no column was NA yet, but an
#      id IBGE added would have ended up in a column called na_v<category>. They are
#      now "<dataset>_variables" entries of load_dictionary() (R/dictionary.R), as
#      for PPM, and an id missing there becomes v<id> and warns. This resolver
#      re-checks the declared variables on every run (PEVS_NAMED_VARIABLES, kept equal
#      to those entries by tests/testthat/test-pevs-resolver.R) and reports an id the
#      dictionary does not name as a failure of that row, before it can reach a user.
#
#   5. THE DICTIONARY, AND WHAT IT HAD WRONG. R/dictionary.R's three PEVS sections label
#      the columns load_pevs() returns: a column is named by its category id and
#      labelled by matching that id against var_code. Compared with the live
#      classifications on 2026-10-31 they lacked 30 categories IBGE added for 2025
#      (ids 83410-83439: 10 in table 289, 16 in 291, 4 in 5930 -- the new species in
#      silviculture, new fruits and nuts in extraction). Worse, load_pevs() matched
#      the id ANYWHERE in the column's name, and 83410-83419 contain the codes
#      3410-3419: the 2025 columns of table 289 came back labelled with the WRONG
#      products -- quant_v83410 (Baru ou Cumaru) as "Umbu (fruit)", quant_v83419
#      (Murumuru) as "Hevea (liquid latex)" -- checked live. The 30 rows are added, the
#      match is anchored on the end of the name (_v<id>$), and because the same gap
#      can open again with no change anywhere in this repository, every run repeats the
#      comparison from the committed source of R/dictionary.R (pevs_read_dictionary():
#      the file is self-contained, and build_manifest.R never loads the package) and
#      reports a missing row. It is a NOTE, not a failure of the row: the row's
#      available_time is still refreshed, but the note travels with the other partial
#      failures (exit 11, an issue), because it needs a person to write the IBGE name
#      and an English gloss. Not checked: names, units and translations of existing rows.
#
#   6. TIMING. 3 tables, 2 requests each: 6 requests a run.
#
#   7. docs_url. The manifest pointed all three rows at
#      https://sidra.ibge.gov.br/pesquisa/pevs/tabelas/brasil/2019, a page that (like the
#      survey's /pesquisa/pevs/tabelas) answers HTTP 403 behind the same challenge, so
#      nothing could verify it, and a year-specific view at that.
#      https://sidra.ibge.gov.br/tabela/<table> -- the `URL` the metadata itself reports
#      -- answers HTTP 200 for all three. docs_url is per row: its own table page,
#      refreshed from the metadata on every run.
#
# DIFFERENCES FROM resolve_ppm.R
#   * Built on sidra_api.R (resolve_ppm.R, merged before sidra_api.R existed, still
#     carries its own copy of that code): sidra_api_check_table(), sidra_api_geo_problem(),
#     sidra_api_row_years(), sidra_api_docs_url().
#   * Every PEVS table has a classification, so a code that spells none out is always a
#     failure, never "a table with no classification".
#   * No fan-out: each row is its own table, so every failure is reported per row.
#   * Failure model: the same as PAM's and PPM's. A row that fails is left alone and
#     reported through `partial_failures` (exit 11 when other rows changed); only "every
#     row failed" -- IBGE's API moved or challenged -- stops the resolver.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage; that sidrar still
# parses IBGE's response and labels its columns as R/pevs.R expects (tagged "# FRAGILE:"
# there, as are its hardcoded table ids).
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_* (or watch_*)
# as a resolver, so everything else in this file is pevs_* or PEVS_*.

# The variable that decides whether a year is "published", in order of preference: the
# first one a table declares is used (point 3 of the header). 145 and 143 are the value
# of production of tables 289 and 291, 6549 the area of table 5930; none of the three
# tables declares more than one of them.
PEVS_COVERAGE_VARIABLES <- c("145", "143", "6549")

# dataset -> the IBGE variable ids load_pevs() names for it: the var_code column of its
# "<dataset>_variables" entry of load_dictionary() (R/dictionary.R). A copy (this script
# runs without the package): tests/testthat/test-pevs-resolver.R fails if the two ever
# differ.
PEVS_NAMED_VARIABLES <- list(
  pevs_forest_crops = c("144", "145"),
  pevs_silviculture = c("142", "143"),
  pevs_silviculture_area = "6549"
)

# "289/c193/all" -> list(table = "289", classific = "c193", category = "all"); "289" ->
# list(table = "289", classific = NA, category = NA); NULL when it is neither. A
# classific of "all" parses (so it can be refused with a message).
pevs_parse_code <- function(code) {
  if (is.null(code) || length(code) != 1 || is.na(code)) {
    return(NULL)
  }
  m <- regmatches(code, regexec("^([0-9]+)(?:/(c[0-9]+|all)/([0-9]+|all))?$", code))[[1]]
  if (length(m) != 4) {
    return(NULL)
  }
  list(
    table = m[2],
    classific = if (nzchar(m[3])) m[3] else NA_character_,
    category = if (nzchar(m[4])) m[4] else NA_character_
  )
}

# The shipped load_dictionary() (R/dictionary.R), read from the source file:
# build_manifest.R never loads the package, and the function is self-contained. `root` is
# the repository root, build_manifest.R's `repo_root`. Throws when the file is missing or
# does not define load_dictionary().
pevs_read_dictionary <- function(root = get0("repo_root", envir = globalenv(), ifnotfound = ".")) {
  path <- file.path(root, "R", "dictionary.R")
  if (!file.exists(path)) {
    stop("there is no ", path)
  }
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  if (!is.function(env$load_dictionary)) {
    stop(path, " does not define load_dictionary()")
  }
  env$load_dictionary
}

# What the dictionary lacks for one dataset (point 5): one message per dataset,
# character(0) when nothing. load_pevs() names a column by the category id of the table's
# classification and labels it by matching that id against the dictionary's var_code; code
# 0 is the Total, which load_pevs() leaves out of the labelling on purpose. `dictionary` is
# load_dictionary() or the error that reading it threw.
pevs_dictionary_gaps <- function(dataset, tab, dictionary, category = NA_character_) {
  if (inherits(dictionary, "error")) {
    return(sprintf("R/dictionary.R could not be read (%s), so its %s section was not checked", conditionMessage(dictionary), dataset))
  }
  wanted <- tab$category_info[[1]]
  wanted <- wanted[wanted$id != "0", , drop = FALSE]
  if (!is.na(category) && category != "all") {
    wanted <- wanted[wanted$id == category, , drop = FALSE] # a single-category row only needs its own label
  }

  dic <- tryCatch(dictionary(dataset), error = function(e) e)
  if (inherits(dic, "error") || !is.data.frame(dic)) {
    return(sprintf("R/dictionary.R has no section for %s (load_dictionary() fails for it), so none of its columns is labelled", dataset))
  }
  gaps <- wanted[!(wanted$id %in% as.character(dic$var_code)), , drop = FALSE]
  if (nrow(gaps) == 0) {
    return(character(0))
  }
  sprintf(
    paste(
      "R/dictionary.R's %s section has no row for IBGE category %s: its columns come back without a label.",
      "Add one (var_code, var_pt = IBGE's name, var_eng = an English gloss in the section's style, the units)"
    ),
    dataset,
    paste(sprintf("%s (%s; IBGE unit: %s)", gaps$id, gaps$nome, gaps$unidade), collapse = ", ")
  )
}

# Checks ONE table: its metadata, then its coverage (sidra_api.R). Returns list(problem,
# classifications, category_info, levels, variables, years, url, ...) with `problem`
# character(0) when both worked. Never throws.
pevs_check_table <- function(table) {
  sidra_api_check_table(table, PEVS_COVERAGE_VARIABLES)
}

# Checks ONE manifest row against its (already checked) table. Returns list(problem =
# character(0), available_time = "1986-2025", docs_url = <the table's page or NA>, note =
# <character(0) or what R/dictionary.R lacks>) or list(problem = "<reason>"). A `problem`
# leaves the row out of the refresh; a `note` does not (the row is right, something else in
# the repository needs a follow-up).
pevs_check_row <- function(code, dataset, available_geo, checked, dictionary) {
  parsed <- pevs_parse_code(code)
  if (is.null(parsed)) {
    return(list(problem = sprintf(
      "sidra_code '%s' is not <table>/c<classification>/<category id or all>", code
    )))
  }
  tab <- checked[[parsed$table]]
  if (length(tab$problem) > 0) {
    return(list(problem = tab$problem))
  }
  row_fail <- function(msg) list(problem = msg)

  # exactly one classification: sidra_api_check_table() has already refused a table with
  # none ("has no classification, and this resolver expects one") or with several
  cl_ids <- names(tab$classifications)
  if (is.na(parsed$classific)) {
    return(row_fail(sprintf(
      paste(
        "sidra_code '%s' names no classification, but table %s has one (c%s): a bare code would return only",
        "the 'Total' slice; write '%s/c%s/all'"
      ),
      code, parsed$table, cl_ids, parsed$table, cl_ids
    )))
  }
  if (parsed$classific == "all") {
    return(row_fail(sprintf(
      paste(
        "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is",
        "behind a bot challenge (see this file's header, point 2); write '%s/c%s/%s' instead"
      ),
      code, parsed$table, cl_ids, parsed$category
    )))
  }
  if (sub("^c", "", parsed$classific) != cl_ids) {
    return(row_fail(sprintf(
      "classification %s is not the one table %s has (c%s)", parsed$classific, parsed$table, cl_ids
    )))
  }
  if (parsed$category != "all" && !(parsed$category %in% tab$classifications[[cl_ids]])) {
    return(row_fail(sprintf(
      paste(
        "category %s does not exist in classification %s of table %s (IBGE answers a query for a",
        "nonexistent category with HTTP 200 and empty values, so this check is the only thing that notices)"
      ),
      parsed$category, parsed$classific, parsed$table
    )))
  }

  named <- PEVS_NAMED_VARIABLES[[dataset]]
  if (is.null(named)) {
    return(row_fail(sprintf(
      "dataset %s has no entry in PEVS_NAMED_VARIABLES -- add the variable ids its %s_variables entry of R/dictionary.R names",
      dataset, dataset
    )))
  }
  unnamed <- setdiff(tab$variables, named)
  if (length(unnamed) > 0) {
    return(row_fail(sprintf(
      paste(
        "table %s now declares variable(s) %s that load_pevs('%s') does not name (it would return them as",
        "unnamed v<id>_v<category> columns, with a warning); name them in the %s_variables entry of R/dictionary.R and in PEVS_NAMED_VARIABLES"
      ),
      parsed$table, paste(unnamed, collapse = ", "), dataset, dataset
    )))
  }

  geo_problem <- sidra_api_geo_problem(available_geo, tab$levels, parsed$table)
  if (length(geo_problem) > 0) {
    return(row_fail(geo_problem))
  }

  years <- sidra_api_row_years(tab, parsed$category)
  if (length(years) == 0) {
    return(row_fail(sprintf(
      "category %s of table %s resolves, but has no year with a published value at Brazil level",
      parsed$category, parsed$table
    )))
  }

  list(
    problem = character(0),
    available_time = sidra_api_years_to_available_time(years),
    docs_url = sidra_api_docs_url(tab),
    note = pevs_dictionary_gaps(dataset, tab, dictionary, parsed$category)
  )
}

resolve_pevs <- function(rows, dictionary = pevs_read_dictionary()) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_pevs(): no manifest rows tagged resolver == 'pevs' -- ",
      "cannot tell which SIDRA tables to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_pevs() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_pevs(): every PEVS row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_pevs(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  row_table <- vapply(
    rows$sidra_code,
    function(code) {
      p <- pevs_parse_code(code)
      if (is.null(p)) NA_character_ else p$table
    },
    character(1),
    USE.NAMES = FALSE
  )
  tables <- unique(row_table[!is.na(row_table)])
  checked <- stats::setNames(lapply(tables, pevs_check_table), tables)
  for (t in tables) {
    if (length(checked[[t]]$problem) == 0) {
      all_years <- sort(unique(unlist(checked[[t]]$years, use.names = FALSE)))
      message("resolve_pevs(): table ", t, " ok -- ", length(checked[[t]]$years), " categories published, ", sidra_api_years_to_available_time(all_years))
    }
  }

  # forced once here, so a dictionary that cannot be read is a note on every row rather
  # than an error out of the middle of the loop
  dictionary <- tryCatch(dictionary, error = function(e) e)

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  notes <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    res <- pevs_check_row(rows$sidra_code[i], rows$dataset[i], rows$available_geo[i], checked, dictionary)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
    } else {
      available_time[i] <- res$available_time
      docs_url[i] <- res$docs_url
      if (length(res$note) > 0) notes[i] <- res$note
    }
  }

  failed <- !is.na(problems)
  # a note is reported like a failure (exit 11, an issue) but does not hold back the row's
  # refresh: the row is right, a file in the repository needs a follow-up
  failure_lines <- c(
    sprintf("pevs/%s (%s): %s", rows$dataset[failed], rows$sidra_code[failed], problems[failed]),
    sprintf("pevs/%s (%s): %s", rows$dataset[!is.na(notes)], rows$sidra_code[!is.na(notes)], notes[!is.na(notes)])
  )

  if (all(failed)) {
    stop(
      "resolve_pevs(): all ", n, " rows failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines[seq_len(sum(failed))], collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the manifest
  # already says about it.
  ok <- !failed
  out <- tibble::tibble(
    survey = "pevs", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "pevs"
  )
  if (length(failure_lines) > 0) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
