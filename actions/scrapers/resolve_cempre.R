# actions/scrapers/resolve_cempre.R
#
# CEMPRE (Cadastro Central de Empresas, IBGE) is read LIVE from IBGE's SIDRA API, like PAM,
# PPM, PEVS, PIB municipal and population: there is no file and no `url`. The manifest has one
# row for it, SIDRA table 9418 with its classification and the Total ("9418/c12762/117897"),
# and R/cempre.R's load_cempre() hands the row's `sidra_code` to sidra_download() -- except for
# the years of a table IBGE has closed, which it asks that table for. A thin per-source layer
# on actions/scripts/sidra_api.R (the metadata lookup, the coverage query and the value coding
# every SIDRA resolver shares), like resolve_population.R; what is here is CEMPRE's own: the two
# tables, the sectors, and the variables a column's name is built from.
#
# SAME MODEL AS resolve_pam.R, resolve_population.R: a VERIFICATION resolver. Nothing needs
# discovering, but IBGE's metadata says whether the manifest and the loader still describe the
# tables, and the data says which years each has. So every run VERIFIES the row's sidra_code
# and the closed table, and REFRESHES available_time from the data of both (and docs_url from
# the metadata of the open one).
#
# VERIFIED LIVE (2026-11-23):
#
#   1. THE SERIES IS IN TWO TABLES. https://servicodados.ibge.gov.br/api/v3/agregados/<table>/
#      metadados (HTTP 200, no bot challenge) and the data API say:
#         6449  "Empresas e outras organizacoes, pessoal ocupado total, pessoal ocupado
#               assalariado, salarios e outras remuneracoes, por secao, divisao, grupo e classe
#               da classificacao de atividades (CNAE 2.0) -- serie encerrada em 2021", annual
#               2006-2021, classification c12762, variables 2585, 707, 708 and 662;
#         9418  the same name without "serie encerrada", annual 2022-2024, the same
#               classification, the same 1,067 categories with the same ids and names, the same
#               four variables, the same levels (N1, N2, N3, N6).
#      Every category has a value for every year of its table. There is no 2025 yet. The
#      manifest said 2006-2020 and load_cempre() knew only 6449: load_cempre(time_period =
#      2022) stopped with "Error in Download." at country and state level, and returned an
#      EMPTY table at municipality level; 2021 worked but warned that it was out of range.
#      The two do not join up: Brazil's number of companies is 5,748,599 in 2021 (6449) and
#      9,431,239 in 2022 (9418), persons employed 55.3 and 62.7 million. Documented in
#      load_cempre()'s help; nothing a resolver can do about it.
#
#   2. THE MANIFEST'S sidra_code NAMES THE OPEN TABLE, THE CODE MAPS THE CLOSED ONE'S YEARS.
#      As for population (resolve_population.R, point 2): one dataset, one row, the year ->
#      table map in R/cempre.R (cempre_closed_tables; a copy here, CEMPRE_CLOSED_TABLES, kept
#      equal by tests/testthat/test-cempre-resolver.R) -- not a row per table (the user would
#      have to know which table has which year) and not rows keyed by year (one per year). The
#      row is the OPEN table because that is the one that grows: when IBGE publishes 2025 the
#      resolver extends available_time and nothing else changes. When IBGE closes 9418 too, its
#      name will say "serie encerrada em ..." (6449's does): this resolver notes it, and the
#      maintainer points the row at the successor and adds 9418 to cempre_closed_tables.
#
#   3. THE sidra_code IS "<table>/c12762/<category>", AND NO CLOUDFLARE BUG. Until 2026-11-23
#      the manifest said just "6449" while load_cempre() hardcoded the classification
#      ("C12762") and the categories in its calls. Unlike PAM, PPM, PEVS, PIB municipal and
#      population, whose loaders passed sidra_download() its default classific = "all" (so
#      sidrar scraped IBGE's table-description page, behind a Cloudflare bot challenge), every
#      sidra_download() call of load_cempre() names a classification and a category, and
#      sidrar never needs the page: checked live from the machine where the others failed,
#      load_cempre() answered at country, state and municipality level for 2006, 2020 and 2021.
#      The manifest now says what the loader sends, for the reason the other resolvers say it:
#      a bare code on a table that has a classification would mean "none", and a request
#      without one answers HTTP 200 with only the "Total" slice. The category in the code is
#      what sectors = FALSE asks for, the Total (117897); sectors = TRUE asks for each of the
#      22 codes of CEMPRE_SECTORS.
#
#   4. THE 88-BRANCH case_when() WAS A PRODUCT, NOT A RELATION. Parsed from R/cempre.R and
#      crossed: every one of the 88 branches (22 sector codes x 4 variables) gives the column
#      the id_code "<sector code>_<k>", with k = 1 for variable 2585, 2 for 707 ("Pessoal
#      ocupado total"), 3 for 708 ("Pessoal ocupado assalariado") and 4 for 662; the sector
#      part is the sector's own code in every branch, and the dictionary's 88 id_codes are
#      the same 88 strings (labels per sector, units per variable). So the sector is the
#      identity and the variable a four-entry lookup, which is all load_cempre() now holds
#      (cempre_variable_index, copied here as CEMPRE_VARIABLE_INDEX). The variables are the
#      four each table declares apart from the "... - percentual do total geral" twins (1002585,
#      1000707, 1000708, 1000662: "allxp" never returns them). A fifth variable would have got
#      an NA in the old case_when(); it now gets "v<id>" and a warning, and this resolver
#      refuses it before a user sees it.
#
#   5. THE SECTORS. The 22 codes load_cempre() downloads one by one with sectors = TRUE are the
#      Total (117897) and the 21 sections of the CNAE, A to U (116830 ... 117892): the whole
#      first level of the classification, in both tables. They are checked here: all in the
#      table, the Total named "Total", and the sections -- the categories whose name is a
#      capital letter and a space, as against the divisions' digits -- exactly those 21. A
#      section IBGE adds is a NOTE (the sector-by-sector download would miss it).
#
#   6. THE LABELS. The dictionary labels a column by its id_code, matched against the end of the
#      column's name (R/cempre.R; it was matched anywhere until 2026-11-23, which cannot misfire
#      here -- the 88 ids are six digits, an underscore and a digit, none inside another -- but
#      did for PEVS). With sectors = FALSE there is no id_code and no label: the columns are
#      the variables' names alone (unchanged).
#
#   7. docs_url. The manifest pointed at https://sidra.ibge.gov.br/pesquisa/cempre/tabelas, the
#      survey page, which answers HTTP 403 behind the same challenge. https://sidra.ibge.gov.br/
#      tabela/9418 -- the `URL` the metadata itself reports -- answers HTTP 200, and is refreshed
#      from the metadata on every run.
#
#   8. TIMING. Two tables, two requests each: 4 requests a run (the coverage query asks every
#      one of the 1,067 categories, ~1 MB each).
#
# FAILURE MODEL. The same as PAM's and population's: a row that fails is left alone and reported
# through `partial_failures`; with the single row of this source, "it failed" is the only
# failure there is, and it stops the resolver. The closed table failing its check fails the row
# too: the loader depends on it for 2006-2021, and leaving those years out of available_time
# because one request flaked would be a wrong refresh.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage; that sidrar still parses
# IBGE's response and labels its columns as R/cempre.R expects (tagged "# FRAGILE:" there); that
# a table IBGE opens next exists -- the only sign is a table name that says "encerrada".
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_* (or watch_*) as a
# resolver, so everything else in this file is cempre_* or CEMPRE_*.

# The classification of both tables: the CNAE 2.0.
CEMPRE_CLASSIFICATION <- "12762"

# The category id of the Total, and the 22 codes sectors = TRUE asks for (the Total and the 21
# sections A to U): a copy of cempre_sectors in R/cempre.R (this script runs without the
# package): tests/testthat/test-cempre-resolver.R fails if the two ever differ.
CEMPRE_TOTAL <- "117897"
CEMPRE_SECTORS <- c(
  "117897", "116830", "116880", "116910", "117296", "117307", "117329", "117363", "117484",
  "117543", "117555", "117608", "117666", "117673", "117714", "117774", "117788", "117810",
  "117838", "117861", "117888", "117892"
)

# variable id -> the k of a column's "<sector code>_<k>" suffix: a copy of
# cempre_variable_index in R/cempre.R, kept equal by the same test. Which year of a table is
# "published" is decided by these, in this order: the first one the table declares.
CEMPRE_VARIABLE_INDEX <- c(`2585` = 1L, `707` = 2L, `708` = 3L, `662` = 4L)

# The tables IBGE has closed and the last year each has: a copy of cempre_closed_tables in
# R/cempre.R, kept equal by the same test.
CEMPRE_CLOSED_TABLES <- list(list(table = "6449", last_year = 2021L))

# "9418/c12762/117897" -> list(table = "9418", classific = "c12762", category = "117897");
# "6449" -> list(table = "6449", classific = NA, category = NA); NULL when it is neither. A
# classific of "all" parses (so it can be refused with a message).
cempre_parse_code <- function(code) {
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

# Checks ONE table (the open one or a closed one): its metadata and coverage (sidra_api.R), then
# what load_cempre() needs of it -- the CNAE classification, the 22 sector codes, the Total, the
# four variables -- and a name that says whether it is closed. Returns what
# sidra_api_check_table() does plus `notes`; `problem` is character(0) when the table is as the
# loader expects. Never throws.
cempre_check_table <- function(table) {
  tab <- sidra_api_check_table(table, names(CEMPRE_VARIABLE_INDEX))
  if (length(tab$problem) > 0) {
    return(tab)
  }
  fail <- function(msg) list(problem = sprintf("table %s: %s", table, msg))

  if (!identical(names(tab$classifications), CEMPRE_CLASSIFICATION)) {
    return(fail(sprintf("its classification is c%s, not c%s (the CNAE 2.0) that load_cempre() asks for", paste(names(tab$classifications), collapse = ", c"), CEMPRE_CLASSIFICATION)))
  }
  categories <- tab$category_info[[CEMPRE_CLASSIFICATION]]
  missing_sectors <- setdiff(CEMPRE_SECTORS, categories$id)
  if (length(missing_sectors) > 0) {
    return(fail(sprintf("it has no category %s, which load_cempre(sectors = TRUE) downloads", paste(missing_sectors, collapse = ", "))))
  }
  total_name <- categories$nome[categories$id == CEMPRE_TOTAL]
  if (!identical(total_name, "Total")) {
    return(fail(sprintf("category %s, which load_cempre() asks for as the Total, is called '%s'", CEMPRE_TOTAL, total_name)))
  }
  sections <- categories$id[grepl("^[A-Z] ", categories$nome)]
  not_sections <- setdiff(CEMPRE_SECTORS, c(CEMPRE_TOTAL, sections))
  if (length(not_sections) > 0) {
    return(fail(sprintf(
      "the sector code(s) %s are no longer the sections of the classification (a capital letter and a name)",
      paste(not_sections, collapse = ", ")
    )))
  }
  unexpected <- setdiff(tab$variables, names(CEMPRE_VARIABLE_INDEX))
  if (length(unexpected) > 0) {
    return(fail(sprintf(
      paste(
        "it now declares variable(s) %s that load_cempre() does not index (a column of sectors = TRUE would end in",
        "_v<id> instead of _<k>, with a warning); give them a k in cempre_variable_index and in CEMPRE_VARIABLE_INDEX"
      ),
      paste(unexpected, collapse = ", ")
    )))
  }

  notes <- character(0)
  new_sections <- setdiff(sections, CEMPRE_SECTORS)
  if (length(new_sections) > 0) {
    notes <- c(notes, sprintf(
      "table %s has section(s) %s that load_cempre(sectors = TRUE) does not download (add them to cempre_sectors, CEMPRE_SECTORS and the dictionary)",
      table, paste(sprintf("%s (%s)", new_sections, categories$nome[match(new_sections, categories$id)]), collapse = ", ")
    ))
  }
  lost <- setdiff(names(CEMPRE_VARIABLE_INDEX), tab$variables)
  if (length(lost) > 0) {
    notes <- c(notes, sprintf("table %s no longer declares variable(s) %s: their columns will be missing", table, paste(lost, collapse = ", ")))
  }
  tab$notes <- notes
  tab
}

# Checks the manifest row against its (already checked) tables. Returns list(problem =
# character(0), available_time = "2006-2024", docs_url = <the open table's page or NA>, note =
# <character(0) or what is worth a person's look>) or list(problem = "<reason>").
cempre_check_row <- function(code, available_geo, open, closed) {
  parsed <- cempre_parse_code(code)
  if (is.null(parsed)) {
    return(list(problem = sprintf("sidra_code '%s' is not <table>/c<classification>/<category id or all>", code)))
  }
  if (length(open$problem) > 0) {
    return(list(problem = open$problem))
  }
  want <- sprintf("c%s", CEMPRE_CLASSIFICATION)
  if (is.na(parsed$classific)) {
    return(list(problem = sprintf(
      paste(
        "sidra_code '%s' names no classification, but table %s has one (%s): a bare code would return only the 'Total' slice;",
        "write '%s/%s/%s'"
      ),
      code, parsed$table, want, parsed$table, want, CEMPRE_TOTAL
    )))
  }
  if (parsed$classific == "all") {
    return(list(problem = sprintf(
      paste(
        "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is behind a bot challenge",
        "(see this file's header, point 3); write '%s/%s/%s' instead"
      ),
      code, parsed$table, want, parsed$category
    )))
  }
  if (parsed$classific != want) {
    return(list(problem = sprintf("classification %s is not the CNAE (%s) that load_cempre() asks for", parsed$classific, want)))
  }
  if (parsed$category != "all" && !(parsed$category %in% open$classifications[[CEMPRE_CLASSIFICATION]])) {
    return(list(problem = sprintf(
      paste(
        "category %s does not exist in classification %s of table %s (IBGE answers a query for a nonexistent category with",
        "HTTP 200 and empty values, so this check is the only thing that notices)"
      ),
      parsed$category, parsed$classific, parsed$table
    )))
  }

  for (tab in c(list(open), closed)) {
    if (length(tab$problem) > 0) {
      return(list(problem = tab$problem))
    }
  }
  geo_problems <- unlist(lapply(c(list(open), closed), function(tab) sidra_api_geo_problem(available_geo, tab$levels, tab$table)))
  if (length(geo_problems) > 0) {
    return(list(problem = geo_problems[1]))
  }

  open_years <- sidra_api_row_years(open, parsed$category)
  if (length(open_years) == 0) {
    return(list(problem = sprintf("table %s resolves, but has no year with a published value at Brazil level", parsed$table)))
  }

  served <- integer(0)
  last_closed <- -Inf
  notes <- open$notes
  for (i in seq_along(closed)) {
    spec <- CEMPRE_CLOSED_TABLES[[i]]
    years <- sidra_api_row_years(closed[[i]], parsed$category) # the same category the loader asks both tables for
    if (!(spec$last_year %in% years)) {
      return(list(problem = sprintf(
        "table %s has no value for %d, the last year load_cempre() asks it for (it has: %s)",
        spec$table, spec$last_year, sidra_api_years_to_available_time(years)
      )))
    }
    inside <- years[years <= spec$last_year]
    served <- c(served, inside)
    last_closed <- max(last_closed, spec$last_year)
    notes <- c(notes, closed[[i]]$notes)
    if (any(years > spec$last_year)) {
      notes <- c(notes, sprintf(
        "closed table %s now has %s too: load_cempre() still asks table %s only up to %d; decide which to keep (cempre_closed_tables and CEMPRE_CLOSED_TABLES)",
        spec$table, paste(years[years > spec$last_year], collapse = ", "), parsed$table, spec$last_year
      ))
    }
    gaps <- setdiff(seq(min(inside), spec$last_year), inside)
    if (length(gaps) > 0) {
      notes <- c(notes, sprintf("closed table %s has no value for %s, years load_cempre() asks it for", spec$table, paste(gaps, collapse = ", ")))
    }
  }
  served <- c(served, open_years[open_years > last_closed])

  if (any(open_years <= last_closed)) {
    notes <- c(notes, sprintf(
      "table %s now has %s too: load_cempre() still asks the closed table for them",
      parsed$table, paste(open_years[open_years <= last_closed], collapse = ", ")
    ))
  }
  if (!is.na(open$name) && grepl("encerrad", open$name, ignore.case = TRUE)) {
    notes <- c(notes, sprintf(
      "table %s says it is closed ('%s'): IBGE must have a successor (pesquisa CL, Cadastro Central de Empresas); point the row at it and add table %s to cempre_closed_tables and CEMPRE_CLOSED_TABLES",
      parsed$table, open$name, parsed$table
    ))
  }

  list(
    problem = character(0),
    available_time = sidra_api_years_to_available_time(served),
    docs_url = sidra_api_docs_url(open),
    note = notes
  )
}

resolve_cempre <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_cempre(): no manifest rows tagged resolver == 'cempre' -- ",
      "cannot tell which SIDRA code to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_cempre() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_cempre(): every cempre row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_cempre(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  # the tables are checked once, whatever the rows (one row today): the open one is the first
  # row's, the closed ones are the loader's
  first <- cempre_parse_code(rows$sidra_code[1])
  open <- if (is.null(first)) list(problem = sprintf("sidra_code '%s' is not <table>/c<classification>/<category id or all>", rows$sidra_code[1])) else cempre_check_table(first$table)
  if (length(open$problem) == 0) open$table <- first$table
  closed <- lapply(CEMPRE_CLOSED_TABLES, function(spec) {
    tab <- cempre_check_table(spec$table)
    tab$table <- spec$table
    tab
  })
  if (length(open$problem) == 0) {
    message("resolve_cempre(): table ", open$table, " ok -- ", sidra_api_years_to_available_time(sidra_api_row_years(open, CEMPRE_TOTAL)))
  }
  for (i in seq_along(closed)) {
    if (length(closed[[i]]$problem) == 0) {
      message("resolve_cempre(): closed table ", closed[[i]]$table, " ok -- ", sidra_api_years_to_available_time(sidra_api_row_years(closed[[i]], CEMPRE_TOTAL)))
    }
  }

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  notes <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    res <- cempre_check_row(rows$sidra_code[i], rows$available_geo[i], open, closed)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
    } else {
      available_time[i] <- res$available_time
      docs_url[i] <- res$docs_url
      if (length(res$note) > 0) notes[i] <- paste(res$note, collapse = "; ")
    }
  }

  failed <- !is.na(problems)
  failure_lines <- c(
    sprintf("cempre/%s (%s): %s", rows$dataset[failed], rows$sidra_code[failed], problems[failed]),
    sprintf("cempre/%s (%s): %s", rows$dataset[!is.na(notes)], rows$sidra_code[!is.na(notes)], notes[!is.na(notes)])
  )

  if (all(failed)) {
    stop(
      "resolve_cempre(): all ", n, " row(s) failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines, collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the manifest already
  # says about it.
  ok <- !failed
  out <- tibble::tibble(
    survey = "cempre", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "cempre"
  )
  if (length(failure_lines) > 0) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
