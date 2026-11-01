# actions/scrapers/resolve_pibmunic.R
#
# PIB Municipal (Produto Interno Bruto dos Municipios, IBGE) is read LIVE from
# IBGE's SIDRA API, like PAM and PPM: there is no file and no `url`. The one
# manifest row is one SIDRA table, 5938, and R/pibmunic.R's load_pibmunic() hands
# its `sidra_code` to sidra_download(). This is a thin per-source layer on
# actions/scripts/sidra_api.R (the metadata lookup, the coverage query and the
# value coding every SIDRA resolver shares), the way resolve_pam.R is; what is
# here is pibmunic's own: the grammar of its sidra_code, which variable ids
# load_pibmunic() names, what its one row means.
#
# SAME MODEL AS resolve_pam.R and resolve_ppm.R: a VERIFICATION resolver. Nothing
# needs discovering (the table id is fixed), but IBGE's metadata says whether the
# manifest still describes the table, and its data says how far the series
# reaches. So every run VERIFIES the row's sidra_code against the table's
# metadata and REFRESHES available_time from the table's own data (and keeps
# docs_url pointed at the table's own page).
#
# VERIFIED LIVE (2026-10-30):
#
#   1. THE TABLE. https://servicodados.ibge.gov.br/api/v3/agregados/5938/metadados
#      (HTTP 200, no bot challenge): "Produto interno bruto a precos correntes,
#      impostos, liquidos de subsidios, sobre produtos a precos correntes e valor
#      adicionado bruto a precos correntes total e por atividade economica, e
#      respectivas participacoes - Referencia 2010"; annual; territorial levels N1,
#      N2, N3, N6, N8 and N9 (so available_geo "country, state, municipality"
#      holds); 46 variables; and NO CLASSIFICATION. It shares no table with any
#      other survey.
#
#   2. THE sidra_code IS A BARE TABLE NUMBER, AND THAT MEANS "NO CLASSIFICATION" --
#      the same convention as PPM's tables 94 and 95. Until 2026-10-30 that bare
#      "5938" was sent by load_pibmunic() to sidra_download() with its default
#      classific = "all", and for classific = "all" sidrar::get_sidra() first
#      scrapes an IBGE HTML page (apisidra.ibge.gov.br/desctabapi.aspx?c=5938) to
#      learn the classification ids -- even for a table with nothing to find. That
#      page answers HTTP 403 behind a Cloudflare bot challenge ("Just a moment...")
#      from the machine this was investigated on, with both User-Agents tried (a
#      browser's and curl's), and load_pibmunic() failed there with a bare "Error in
#      Download." at country and state level (2021). The PAM investigation (2026-10-21)
#      recorded the CI job check-load-pibmunic as green August to October 2026 (not
#      re-checked here), so the failure depends on the network. Sent with no classification instead (R/download.R,
#      sidra_get_table(), written for PPM) the same table answered at all three
#      levels for 2023: 46 variables for Brazil, 1,242 rows for the states, 256,220
#      rows (5,570 municipalities, ~2 minutes) for the municipalities. A bare code
#      is therefore only right while the table has no classification, and this
#      resolver checks it BOTH ways: for a table that did get one, a bare code would
#      still answer HTTP 200 but with only the "Total" slice (74: 2 rows instead of
#      7, checked for PPM). A code that spells a classification out ("5938/c<n>/all")
#      is accepted when the table has that classification, which is how a
#      maintainer would fix the row if IBGE ever added one; "all" is refused.
#
#   3. WHAT WENT STALE. available_time: the manifest said 2002-2021 (and
#      check_all_loads.R "max disponivel no SIDRA e 2021"); the metadata says
#      2002-2023 and the data for the Brazil level agrees, a published value in
#      every one of the 22 years. The variable that decides it is 37, GDP at
#      current prices -- the table's own headline variable, so a table that stopped
#      declaring it is a failure, not a fallback. The convention is the one in
#      sidra_api.R: a year counts as covered when its value is neither "..." (not
#      available) nor ".." (not applicable).
#
#      "2002-2023" IS NOT "ALL 46 VARIABLES FOR EVERY YEAR". Counting the variables
#      with a number: 18 of 46 at Brazil level in each of 2018-2021 but only 2 in
#      2022 and in 2023 (variable 37 and 496, GDP and its share of Brazil's GDP); at
#      state level 25 variables in 2021 and 3 in 2022 and 2023 (37, 496, 530); for
#      Rondonia's municipalities 41 in 2021 and 6 in 2022 and 2023 (37, 496, 497, 530,
#      552, 553). So, as of 2026-10-30, SIDRA has GDP and its shares for 2022 and
#      2023 and the rest -- taxes, value added in total and by activity, and their
#      shares -- up to 2021. IBGE lists no other table for it (the survey has table
#      5938, the Gini tables 5939 and 599, and table 21, the discontinued reference-2002
#      series). available_time follows the headline variable, as PAM's and PPM's do;
#      the caveat is in NEWS.md and the vignette. It is not checked per variable
#      (a variable going from published to not, or back, would not be noticed).
#
#   4. THE VARIABLES. load_pibmunic() takes whatever columns sidrar returns and
#      labels each one by matching its variable id against the var_code column of
#      the "pibmunic" entry of load_dictionary() (R/dictionary.R). The data query
#      load_pibmunic() makes (every variable, "allxp") returns 46 variables for
#      the table and the dictionary has those same 46: 7 values in thousands of
#      reais (37, 543, 498, 513, 517, 6575, 525) and 39 shares ("Participacao ...",
#      unit "%"), which sidra_api.R's metadata parser leaves out of `variables`
#      because it drops every "%" -- so the data, not the metadata, is what is
#      compared here. A variable IBGE adds comes back as an unlabelled column
#      (nothing breaks, unlike PAM's old na_v<crop> columns), so it is a NOTE: the
#      row's available_time is still refreshed, and the note travels with the other
#      partial failures (exit 11, an issue) because a person has to write the IBGE
#      name and an English gloss. A variable the dictionary has and IBGE dropped is
#      harmless and not reported.
#
#   5. docs_url. The manifest pointed at https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas,
#      the survey page, which answers HTTP 403 behind the same Cloudflare challenge
#      (so nothing could verify it). https://sidra.ibge.gov.br/tabela/5938 -- the
#      `URL` the metadata itself reports -- answers HTTP 200, and is refreshed from
#      the metadata on every run.
#
#   6. TIMING. 3 requests a run (metadata, coverage, variables): ~25 KB.
#
# DIFFERENCES FROM resolve_ppm.R
#   * Built on sidra_api.R (resolve_ppm.R, merged before sidra_api.R existed, still
#     carries its own copy of that code): sidra_api_check_table() with
#     allow_no_classification, sidra_api_geo_problem(), sidra_api_row_years(),
#     sidra_api_docs_url().
#   * One row, one table: no fan-out and no table-level/row-level split in the
#     failure lines.
#   * The variable check reads the DATA (point 4), because here the percentage
#     variables are real variables of the loader's output.
#   * It does not read R/dictionary.R: there is no per-category dictionary to
#     compare, only the 46 variable ids (PIBMUNIC_NAMED_VARIABLES, kept equal to the
#     dictionary entry by tests/testthat/test-pibmunic-resolver.R).
#   * Failure model: the same as PAM's and PPM's. A row that fails is left alone and
#     reported through `partial_failures`; with a single row, "every row failed"
#     (IBGE's API moved or challenged) is the only failure there is, and it stops the
#     resolver.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage; that sidrar still
# parses IBGE's response and labels its columns as R/pibmunic.R expects (tagged
# "# FRAGILE:" there); that IBGE has not published a NEW table for a later reference
# year (5938 is "Referencia 2010"; a successor would need a new row, not a refresh).
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_* (or
# watch_*) as a resolver, so everything else in this file is pibmunic_* or
# PIBMUNIC_*.

# The variable that decides whether a year is "published" (point 3): GDP at current
# prices.
PIBMUNIC_COVERAGE_VARIABLES <- "37"

# The IBGE variable ids load_pibmunic() labels: the var_code column of the
# "pibmunic" entry of load_dictionary() (R/dictionary.R), in its order. A copy (this
# script runs without the package): tests/testthat/test-pibmunic-resolver.R fails if
# the two ever differ.
PIBMUNIC_NAMED_VARIABLES <- c(
  "37", "553", "552", "497", "530", "496", "543", "571", "570", "545", "551", "544",
  "498", "555", "554", "500", "546", "499", "513", "516", "557", "556", "515", "547",
  "514", "517", "520", "559", "558", "519", "548", "518", "6575", "6574", "6571",
  "6570", "6572", "6569", "6573", "525", "528", "563", "562", "527", "550", "526"
)

# Brazil level, one year, every variable load_pibmunic() asks for ("allxp").
# sprintf(url, table, year).
PIBMUNIC_VARIABLES_URL <- "https://apisidra.ibge.gov.br/values/t/%s/n1/1/p/%s/v/allxp"

# "5938" -> list(table = "5938", classific = NA, category = NA); "74/c80/all" ->
# list(table = "74", classific = "c80", category = "all"); NULL when it is neither.
# A classific of "all" parses (so it can be refused with a message).
pibmunic_parse_code <- function(code) {
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

# Pure: the variable ids a SIDRA value table (the variables query) holds. The
# dimension that carries them is found by its header row -- the first element of
# IBGE's array names every column -- not by position, which depends on the request.
# Returns list(problem, ids).
pibmunic_parse_variables <- function(text) {
  fail <- function(msg) list(problem = msg, ids = character(0))

  d <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (!is.data.frame(d) || nrow(d) < 2) {
    return(fail("the response is not a SIDRA value table (a JSON array with a header row and data rows)"))
  }
  name_columns <- grep("^D[0-9]+N$", names(d), value = TRUE)
  is_variable <- vapply(name_columns, function(col) grepl("^Vari", as.character(d[[col]][1])), logical(1))
  if (sum(is_variable) != 1) {
    return(fail("no single dimension of the response is the variable (its header row has none, or several, starting with 'Vari')"))
  }
  ids <- as.character(d[[sub("N$", "C", name_columns[is_variable])]][-1])
  if (!all(grepl("^[0-9]+$", ids))) {
    return(fail("the variable codes are not all numbers"))
  }
  list(problem = character(0), ids = unique(ids))
}

# Checks ONE table: its metadata and coverage (sidra_api.R), then the variables
# its data returns. Returns what sidra_api_check_table() does plus `variable_note`:
# a sentence when the variables could not be checked, or when the table has some
# that load_pibmunic() does not label (character(0) otherwise). Never throws; a
# failed variable check is a note, not a failure of the table.
pibmunic_check_table <- function(table) {
  tab <- sidra_api_check_table(table, PIBMUNIC_COVERAGE_VARIABLES, allow_no_classification = TRUE)
  if (length(tab$problem) > 0) {
    return(tab)
  }

  tab$variable_note <- character(0)
  years <- sort(unique(unlist(tab$years, use.names = FALSE)))
  if (length(years) == 0) {
    return(tab) # the row check reports that no year is published
  }
  got <- sidra_api_fetch(sprintf(PIBMUNIC_VARIABLES_URL, table, max(years)))
  if (length(got$problem) > 0) {
    tab$variable_note <- sprintf("could not check table %s's variables against the dictionary: %s", table, got$problem)
    return(tab)
  }
  parsed <- pibmunic_parse_variables(got$text)
  if (length(parsed$problem) > 0) {
    tab$variable_note <- sprintf("could not check table %s's variables against the dictionary: %s", table, parsed$problem)
    return(tab)
  }
  unnamed <- setdiff(parsed$ids, PIBMUNIC_NAMED_VARIABLES)
  if (length(unnamed) > 0) {
    tab$variable_note <- sprintf(
      paste(
        "table %s now returns variable(s) %s that load_pibmunic() does not label (they would come back as",
        "unlabelled columns); add them to the pibmunic entry of R/dictionary.R and to PIBMUNIC_NAMED_VARIABLES"
      ),
      table, paste(unnamed, collapse = ", ")
    )
  }
  tab
}

# Checks ONE manifest row against its (already checked) table. Returns
# list(problem = character(0), available_time = "2002-2023", docs_url = <the
# table's page or NA>, note = <the table's variable note>) or list(problem =
# "<reason>").
pibmunic_check_row <- function(code, available_geo, checked) {
  parsed <- pibmunic_parse_code(code)
  if (is.null(parsed)) {
    return(list(problem = sprintf("sidra_code '%s' is not <table> or <table>/c<classification>/<category id or all>", code)))
  }
  tab <- checked[[parsed$table]]
  if (length(tab$problem) > 0) {
    return(list(problem = tab$problem))
  }

  cl_ids <- names(tab$classifications)
  if (is.na(parsed$classific)) {
    if (length(cl_ids) > 0) {
      return(list(problem = sprintf(
        paste(
          "table %s now has classification c%s, but sidra_code '%s' says it has none: load_pibmunic() would send no",
          "classification, and IBGE answers that with HTTP 200 and only the 'Total' slice; write '%s/c%s/all'"
        ),
        parsed$table, cl_ids[1], code, parsed$table, cl_ids[1]
      )))
    }
  } else {
    if (length(cl_ids) == 0) {
      return(list(problem = sprintf(
        "sidra_code '%s' names a classification, but table %s has none; write '%s'", code, parsed$table, parsed$table
      )))
    }
    if (parsed$classific == "all") {
      return(list(problem = sprintf(
        paste(
          "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is",
          "behind a bot challenge (see this file's header, point 2); write '%s/c%s/%s' instead"
        ),
        code, parsed$table, cl_ids[1], parsed$category
      )))
    }
    cl_id <- sub("^c", "", parsed$classific)
    if (!(cl_id %in% cl_ids)) {
      return(list(problem = sprintf(
        "classification %s is not one of table %s's (%s)", parsed$classific, parsed$table, paste0("c", cl_ids, collapse = ", ")
      )))
    }
    if (parsed$category != "all" && !(parsed$category %in% tab$classifications[[cl_id]])) {
      return(list(problem = sprintf(
        paste(
          "category %s does not exist in classification %s of table %s (IBGE answers a query for a",
          "nonexistent category with HTTP 200 and empty values, so this check is the only thing that notices)"
        ),
        parsed$category, parsed$classific, parsed$table
      )))
    }
  }

  geo_problem <- sidra_api_geo_problem(available_geo, tab$levels, parsed$table)
  if (length(geo_problem) > 0) {
    return(list(problem = geo_problem))
  }

  years <- sidra_api_row_years(tab, parsed$category)
  if (length(years) == 0) {
    return(list(problem = sprintf("table %s resolves, but has no year with a published value at Brazil level", parsed$table)))
  }

  list(
    problem = character(0),
    available_time = sidra_api_years_to_available_time(years),
    docs_url = sidra_api_docs_url(tab),
    note = tab$variable_note
  )
}

resolve_pibmunic <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_pibmunic(): no manifest rows tagged resolver == 'pibmunic' -- ",
      "cannot tell which SIDRA code to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_pibmunic() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_pibmunic(): every pibmunic row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_pibmunic(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  row_table <- vapply(
    rows$sidra_code,
    function(code) {
      p <- pibmunic_parse_code(code)
      if (is.null(p)) NA_character_ else p$table
    },
    character(1)
  )
  tables <- unique(stats::na.omit(row_table))
  checked <- stats::setNames(lapply(tables, pibmunic_check_table), tables)
  for (t in tables) {
    if (length(checked[[t]]$problem) == 0) {
      all_years <- sort(unique(unlist(checked[[t]]$years, use.names = FALSE)))
      message("resolve_pibmunic(): table ", t, " ok -- ", sidra_api_years_to_available_time(all_years))
    }
  }

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  notes <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    res <- pibmunic_check_row(rows$sidra_code[i], rows$available_geo[i], checked)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
    } else {
      available_time[i] <- res$available_time
      docs_url[i] <- res$docs_url
      if (length(res$note) > 0) notes[i] <- res$note
    }
  }

  failed <- !is.na(problems)
  failure_lines <- c(
    sprintf("pibmunic/%s (%s): %s", rows$dataset[failed], rows$sidra_code[failed], problems[failed]),
    sprintf("pibmunic/%s (%s): %s", rows$dataset[!is.na(notes)], rows$sidra_code[!is.na(notes)], notes[!is.na(notes)])
  )

  if (all(failed)) {
    stop(
      "resolve_pibmunic(): all ", n, " row(s) failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines, collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the
  # manifest already says about it.
  ok <- !failed
  out <- tibble::tibble(
    survey = "pibmunic", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "pibmunic"
  )
  if (length(failure_lines) > 0) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
