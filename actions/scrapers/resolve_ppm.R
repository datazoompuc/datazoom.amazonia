# actions/scrapers/resolve_ppm.R
#
# PPM (Pesquisa da Pecuaria Municipal, IBGE) is read LIVE from IBGE's SIDRA
# API, like PAM: there is no file and no `url`. Each of the 5 manifest rows is
# one SIDRA table, and R/ppm.R's load_ppm() hands the row's `sidra_code` to
# sidra_download(). This is the second SIDRA resolver; it adapts
# resolve_pam.R's (on branch update-pam when this was written) rather than
# copying it, and says below where the two differ.
#
# SAME MODEL AS resolve_pam.R: a VERIFICATION resolver. Nothing needs
# discovering (the table ids are fixed), but IBGE's metadata can say whether the
# manifest still describes the table, and its data says how far each series
# reaches. So every run VERIFIES each row's sidra_code against the table's
# metadata and REFRESHES available_time from the table's own data (and keeps
# docs_url pointed at the table's own page, see point 7).
#
# VERIFIED LIVE (2026-10-22):
#
#   1. THE MECHANISM TRANSFERS. https://servicodados.ibge.gov.br/api/v3/agregados/
#      <table>/metadados answers all five tables (74, 3940, 94, 3939, 95) with
#      the same JSON resolve_pam.R reads, and https://apisidra.ibge.gov.br/
#      values/t/<table>/n1/all/v/<var>/p/all[/c<classification>/all] returns every
#      year (and category) at Brazil level in one call, with "..." / ".." for
#      what is not published. All five are annual, offer N1, N2, N3, N6, N8 and
#      N9 (so available_geo "country, state, municipality" holds), and do not
#      share a table with each other or with any other survey.
#
#   2. THE sidra_code SHAPE IS NOT PAM'S, AND IT IS NOT ONE SHAPE.
#         74    "Producao de origem animal"      classification c80   (7 categories)
#         3940  "Producao da aquicultura"        classification c654  (26)
#         3939  "Efetivo dos rebanhos"           classification c79   (10)
#         94    "Vacas ordenhadas"               none
#         95    "Ovinos tosquiados"              none
#      The manifest used to say just "74", "3940", ... for all five, and
#      load_ppm() passed that to sidra_download() with its defaults
#      (classific = "all"). For classific = "all" sidrar::get_sidra() first
#      scrapes an IBGE HTML page to learn the classification ids, and that page
#      answers HTTP 403 behind a Cloudflare bot challenge from the machine this
#      was investigated on -- even for 94 and 95, where there is nothing to
#      find. load_ppm() failed for ALL FIVE datasets there ("Error in
#      Download.", country and state, 2023); PAM failed for 7 of its 79 rows.
#      The manifest now says "74/c80/all", "3940/c654/all", "3939/c79/all" for
#      the three classified tables (the same explicit form PAM uses; sidrar
#      builds the identical request without the page) and stays "94" / "95" for
#      the two that have no classification, which load_ppm() now sends through
#      sidra_download()'s no-classification route (R/download.R,
#      sidra_get_table()). A bare code therefore MEANS "this table has no
#      classification", and this resolver checks it: a bare code on a table
#      that has one would silently return only the "Total" slice (74: 2 rows
#      instead of 7, HTTP 200, checked live).
#
#   3. WHAT GOES STALE. available_time: the manifest said 1974-2023 (aquaculture
#      2013-2023); IBGE publishes through 2025 for all five, with no gaps. The
#      convention is resolve_pam.R's: a year counts as covered when its value is
#      not "..." (not available) or ".." (not applicable); "-" (zero) and "X"
#      (suppressed) count. The variable that decides it is value of production
#      (215) where the table has one -- 74 and 3940: the "Total" category has no
#      quantity (106 and 4146 print ".." for it) -- and otherwise the table's first
#      non-percentage variable (105, 107, 108). Aquaculture's category 79366
#      ("Peixes") is published only from 2024; as these datasets read the whole
#      table, the union of the categories is the answer.
#
#   4. THE case_when() GAP load_pam() HAD DOES NOT EXIST HERE. load_ppm() names
#      (the "<dataset>_variables" entries of load_dictionary(), R/dictionary.R)
#      105 (3939), 108 (95), 107 (94), 106 and 215 (74), 4146 and 215 (3940). The
#      metadata declares exactly those (plus "... - percentual do total geral"
#      variables, 1000215, which sidrar's "allxp" never requests) and the data
#      returns exactly those. This resolver re-checks it on every run
#      (PPM_NAMED_VARIABLES, kept equal to those entries by
#      tests/testthat/test-ppm-resolver.R): a variable IBGE adds to a table is
#      reported as a failure of that row, before it can reach a user as an
#      unnamed na_v<category> column.
#
#   5. A NONEXISTENT CATEGORY IS NOT AN ERROR (resolve_pam.R, point 4): HTTP 200,
#      "..." values, empty name. The category check below is the only thing that
#      notices a renumbered id; with "<table>/c<n>/all" rows there is no category
#      id to go stale, so it is the classification id that is checked.
#
#   6. TIMING. 5 tables, 2 requests each: 10 requests, ~0.5 MB a run.
#
#   7. docs_url. The manifest pointed all five rows at
#      https://sidra.ibge.gov.br/pesquisa/ppm/tabelas/brasil/2021. That page, and
#      the survey's /pesquisa/ppm/tabelas, answer HTTP 403 behind the same
#      Cloudflare challenge (so nothing here can verify them) and the first is a
#      year-specific view. https://sidra.ibge.gov.br/tabela/<table> -- the `URL`
#      the metadata itself reports -- answers HTTP 200 for all five. With one
#      table per row, docs_url is per row: each row's own table page, refreshed
#      from the metadata on every run.
#
#   8. THE DICTIONARY. R/dictionary.R's PPM section labels the columns load_ppm()
#      returns: a column is named by its category id (by the variable id for 94
#      and 95) and labelled by matching that id against var_code. Compared with
#      the live classifications of tables 3939, 74 and 3940 and the variables of
#      94 and 95 on 2026-10-22, it had exactly one gap in the five sections:
#      category 79366 ("Peixes", Quilogramas) of table 3940, which IBGE
#      publishes from 2024 as "-" (zero) for every state so far -- load_ppm()
#      drops non-numbers, so no column was affected yet; the first number IBGE
#      gives it would have come back unlabelled. The row was added. Because that
#      can happen again with no change anywhere in this repository, and the
#      classification is already in hand, every run repeats the comparison from
#      the committed source of R/dictionary.R (ppm_read_dictionary(): the file is
#      self-contained, and build_manifest.R never loads the package) and reports
#      a missing row. It is a NOTE, not a failure of the row: the row's
#      available_time is still refreshed, but the note travels with the other
#      partial failures (exit 11, an issue), because it needs a person to write
#      the IBGE name and an English gloss. Not checked: names, units and
#      translations of existing rows (nothing reads the units; a few are rough).
#
# BUILT ON sidra_api.R (2026-11-23). This file used to carry its own copy of the metadata
# lookup, the coverage query and the value coding (ppm_fetch(), ppm_parse_metadata(),
# ppm_parse_coverage(), ppm_snippet(), ppm_years_to_available_time() and the constants
# behind them): resolve_pam.R's code before it was extracted into actions/scripts/
# sidra_api.R, which resolve_pam.R, resolve_pibmunic.R, resolve_pevs.R and
# resolve_population.R use. The extraction was done for PAM only, so the two copies
# stayed. It is gone: ppm_check_table() is sidra_api_check_table() with the options
# PPM needs, and the levels, years and docs_url of a row come from sidra_api.R's
# sidra_api_geo_problem(), sidra_api_row_years() and sidra_api_docs_url(). Behaviour is
# the same (checked on the live tables when this was done); what is left
# here is PPM's own: the grammar of its sidra_code, the variable ids load_ppm() names,
# the dictionary check and the row logic.
#
# DIFFERENCES FROM resolve_pam.R
#   * sidra_code: bare "<table>" or "<table>/c<n>/<category|all>" (PAM: always
#     the latter). Whether the classification exists is verified both ways.
#   * No fan-out. PAM's 79 rows share 7 tables, so a failed table is reported
#     once for all its rows; here each row is its own table. The code still
#     groups by table (a sixth row on a shared table would just work), but every
#     failure is reported per row.
#   * The coverage query has no /c<n>/all part for a table without a
#     classification; the first of 215 / the table's first variable.
#   * Also checks the variable ids (point 4), returns docs_url (point 7) and
#     compares R/dictionary.R with the classification (point 8; the extra
#     argument `dictionary` is only there so tests can pass a fake).
#   * Failure model: the same as PAM's, deliberately. With 5 rows a stop() on any
#     failure would be tempting, but then one flaky request, or one retired
#     table, would throw away the four good refreshes of that month's PR and
#     show up as a failed resolver. A row that fails is left alone and reported
#     through `partial_failures` (exit 11 when other rows changed); only "every
#     row failed" -- IBGE's API moved or challenged -- stops the resolver.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage; that sidrar
# still parses IBGE's response; or R/ppm.R's hardcoded table ids and column
# references (tagged "# FRAGILE:" there).
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_* (or
# watch_*) as a resolver, so everything else in this file is ppm_* or PPM_*.

# The variable that decides whether a year is "published": this one where the
# table declares it, otherwise its first variable (point 3; sidra_api_check_table()'s
# fall_back_to_first).
PPM_PREFERRED_VARIABLE <- "215"

# dataset -> the IBGE variable ids load_ppm() names for it: the var_code column of
# its "<dataset>_variables" entry of load_dictionary() (R/dictionary.R). A copy
# (this script runs without the package): tests/testthat/test-ppm-resolver.R
# fails if the two ever differ.
PPM_NAMED_VARIABLES <- list(
  ppm_livestock_inventory = "105",
  ppm_sheep_farming = "108",
  ppm_animal_origin_production = c("106", "215"),
  ppm_cow_farming = "107",
  ppm_aquaculture = c("4146", "215")
)

# "74/c80/all" -> list(table = "74", classific = "c80", category = "all");
# "94" -> list(table = "94", classific = NA, category = NA); NULL when it is
# neither. A classific of "all" parses (so it can be refused with a message).
ppm_parse_code <- function(code) {
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

# Checks ONE table: its metadata, then its coverage (sidra_api.R: the value of production
# (215) decides the years where the table declares it, else its first variable; a table
# without a classification is fine, and then the coverage query has no /c<n>/all part).
# Returns list(problem, classifications, category_info, levels, variables, variable_info,
# url, years) with `problem` character(0) when both worked. Never throws.
ppm_check_table <- function(table) {
  sidra_api_check_table(table, PPM_PREFERRED_VARIABLE, fall_back_to_first = TRUE, allow_no_classification = TRUE)
}

# The shipped load_dictionary() (R/dictionary.R), read from the source file:
# build_manifest.R never loads the package, and the function is self-contained.
# `root` is the repository root, build_manifest.R's `repo_root`. Throws when the
# file is missing or does not define load_dictionary().
ppm_read_dictionary <- function(root = get0("repo_root", envir = globalenv(), ifnotfound = ".")) {
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

# What the dictionary lacks for one dataset (point 8): one message per dataset,
# character(0) when nothing. load_ppm() names a column by the category id of
# its classification, or by the variable id for a table without one, and labels
# it by matching that id against the dictionary's var_code; code 0 is the Total,
# which load_ppm() leaves out of the labelling on purpose. `dictionary` is
# load_dictionary() or the error that reading it threw.
ppm_dictionary_gaps <- function(dataset, tab, dictionary, category = NA_character_) {
  if (inherits(dictionary, "error")) {
    return(sprintf("R/dictionary.R could not be read (%s), so its %s section was not checked", conditionMessage(dictionary), dataset))
  }
  classified <- length(tab$classifications) > 0
  wanted <- if (classified) tab$category_info[[1]] else tab$variable_info
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
      "R/dictionary.R's %s section has no row for IBGE %s %s: its columns come back without a label.",
      "Add one (var_code, var_pt = IBGE's name, var_eng = an English gloss in the section's style, the units)"
    ),
    dataset, if (classified) "category" else "variable",
    paste(sprintf("%s (%s; IBGE unit: %s)", gaps$id, gaps$nome, gaps$unidade), collapse = ", ")
  )
}

# Checks ONE manifest row against its (already checked) table. Returns
# list(problem = character(0), available_time = "1974-2025", docs_url = <page or
# NA>, note = <character(0) or what R/dictionary.R lacks>) or list(problem =
# "<reason>"). A `problem` leaves the row out of the refresh; a `note` does not
# (the row is right, something else in the repository needs a follow-up).
ppm_check_row <- function(code, dataset, available_geo, checked, dictionary) {
  parsed <- ppm_parse_code(code)
  if (is.null(parsed)) {
    return(list(problem = sprintf(
      "sidra_code '%s' is not <table> or <table>/c<classification>/<category id or all>", code
    )))
  }
  tab <- checked[[parsed$table]]
  if (length(tab$problem) > 0) {
    return(list(problem = tab$problem))
  }
  row_fail <- function(msg) list(problem = msg)

  cl_ids <- names(tab$classifications)
  if (is.na(parsed$classific)) {
    if (length(cl_ids) > 0) {
      return(row_fail(sprintf(
        paste(
          "sidra_code '%s' names no classification, but table %s has one (c%s): a bare code would return only",
          "the 'Total' slice; write '%s/c%s/all'"
        ),
        code, parsed$table, cl_ids[1], parsed$table, cl_ids[1]
      )))
    }
  } else {
    if (parsed$classific == "all") {
      return(row_fail(sprintf(
        paste(
          "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is",
          "behind a bot challenge (see this file's header, point 2); write '%s' instead"
        ),
        code, if (length(cl_ids) > 0) sprintf("%s/c%s/%s", parsed$table, cl_ids[1], parsed$category) else parsed$table
      )))
    }
    if (length(cl_ids) == 0) {
      return(row_fail(sprintf(
        "sidra_code '%s' names classification %s, but table %s has none; write '%s'",
        code, parsed$classific, parsed$table, parsed$table
      )))
    }
    if (!(sub("^c", "", parsed$classific) %in% cl_ids)) {
      return(row_fail(sprintf(
        "classification %s is not the one table %s has (c%s)", parsed$classific, parsed$table, cl_ids[1]
      )))
    }
    if (parsed$category != "all" && !(parsed$category %in% tab$classifications[[sub("^c", "", parsed$classific)]])) {
      return(row_fail(sprintf(
        paste(
          "category %s does not exist in classification %s of table %s (IBGE answers a query for a",
          "nonexistent category with HTTP 200 and empty values, so this check is the only thing that notices)"
        ),
        parsed$category, parsed$classific, parsed$table
      )))
    }
  }

  named <- PPM_NAMED_VARIABLES[[dataset]]
  if (is.null(named)) {
    return(row_fail(sprintf("dataset %s has no entry in PPM_NAMED_VARIABLES -- add the variable ids its %s_variables entry of R/dictionary.R names", dataset, dataset)))
  }
  unnamed <- setdiff(tab$variables, named)
  if (length(unnamed) > 0) {
    return(row_fail(sprintf(
      paste(
        "table %s now declares variable(s) %s that load_ppm('%s') does not name (it would return them as",
        "unnamed v<id>_v<category> columns, with a warning); name them in the %s_variables entry of R/dictionary.R and in PPM_NAMED_VARIABLES"
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
    return(row_fail(sprintf("table %s resolves, but has no year with a published value at Brazil level", parsed$table)))
  }

  list(
    problem = character(0),
    available_time = sidra_api_years_to_available_time(years),
    docs_url = sidra_api_docs_url(tab),
    note = ppm_dictionary_gaps(dataset, tab, dictionary, parsed$category)
  )
}

resolve_ppm <- function(rows, dictionary = ppm_read_dictionary()) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_ppm(): no manifest rows tagged resolver == 'ppm' -- ",
      "cannot tell which SIDRA tables to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_ppm() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_ppm(): every PPM row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_ppm(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  row_table <- vapply(
    rows$sidra_code,
    function(code) {
      p <- ppm_parse_code(code)
      if (is.null(p)) NA_character_ else p$table
    },
    character(1),
    USE.NAMES = FALSE
  )
  tables <- unique(row_table[!is.na(row_table)])
  checked <- stats::setNames(lapply(tables, ppm_check_table), tables)
  for (t in tables) {
    if (length(checked[[t]]$problem) == 0) {
      all_years <- sort(unique(unlist(checked[[t]]$years, use.names = FALSE)))
      message("resolve_ppm(): table ", t, " ok -- ", length(checked[[t]]$classifications), " classification(s), ", sidra_api_years_to_available_time(all_years))
    }
  }

  # forced once here, so a dictionary that cannot be read is a note on every row
  # rather than an error out of the middle of the loop
  dictionary <- tryCatch(dictionary, error = function(e) e)

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  notes <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    res <- ppm_check_row(rows$sidra_code[i], rows$dataset[i], rows$available_geo[i], checked, dictionary)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
    } else {
      available_time[i] <- res$available_time
      docs_url[i] <- res$docs_url
      if (length(res$note) > 0) notes[i] <- res$note
    }
  }

  failed <- !is.na(problems)
  # a note is reported like a failure (exit 11, an issue) but does not hold back
  # the row's refresh: the row is right, a file in the repository needs a follow-up
  failure_lines <- c(
    sprintf("ppm/%s (%s): %s", rows$dataset[failed], rows$sidra_code[failed], problems[failed]),
    sprintf("ppm/%s (%s): %s", rows$dataset[!is.na(notes)], rows$sidra_code[!is.na(notes)], notes[!is.na(notes)])
  )

  if (all(failed)) {
    stop(
      "resolve_ppm(): all ", n, " rows failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines[seq_len(sum(failed))], collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the
  # manifest already says about it (see the failure model above).
  ok <- !failed
  out <- tibble::tibble(
    survey = "ppm", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "ppm"
  )
  if (length(failure_lines) > 0) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
