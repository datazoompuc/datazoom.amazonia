# actions/scrapers/resolve_population.R
#
# Population (Estimativas de Populacao, IBGE) is read LIVE from IBGE's SIDRA API, like
# PAM, PPM, PEVS and PIB municipal: there is no file and no `url`. The manifest has one
# row for it, SIDRA table 6579, and R/population.R's load_population() hands its
# `sidra_code` to sidra_download() -- except for three years, which the estimates table
# does not have and which load_population() asks other tables for. A thin per-source
# layer on actions/scripts/sidra_api.R (the metadata lookup, the coverage query and the
# value coding every SIDRA resolver shares), the way resolve_pibmunic.R is; what is here
# is population's own: the four tables, and what each must be.
#
# SAME MODEL AS resolve_pam.R, resolve_pibmunic.R: a VERIFICATION resolver. Nothing needs
# discovering (the table ids are fixed), but IBGE's metadata says whether the manifest
# and the loader still describe the tables, and the data says which years each has. So
# every run VERIFIES the row's sidra_code and the tables of the three special years, and
# REFRESHES available_time from the data of all four (and docs_url from the metadata of
# 6579).
#
# VERIFIED LIVE (2026-11-12):
#
#   1. FOUR TABLES MAKE ONE SERIES. https://servicodados.ibge.gov.br/api/v3/agregados/
#      <table>/metadados (HTTP 200, no bot challenge) and the data API say:
#         6579  "Populacao residente estimada" (Estimativas de Populacao), annual
#               2001-2026, one variable (9324), NO classification, levels N1, N2, N3, N6.
#               Values for 2001-2006, 2008-2009, 2011-2021 and 2024-2026: none for 2007,
#               2010, 2022 or 2023.
#         793   "Populacao residente" (Contagem da Populacao), 2007 only; variable 93, no
#               classification; N1 N2 N3 N6 N8 N9.
#         1378  Censo Demografico, 2010 only; variable 93 and FOUR classifications: c1
#               Situacao do domicilio, c2 Sexo, c287 Idade, c455 Condicao no domicilio.
#         4714  "Populacao Residente, Area territorial e Densidade demografica" (Censo
#               2022), 2022 only; variables 93 (resident population), 6318 (area), 614
#               (density); no classification; N1 N2 N3 N6.
#      Each year of the series is therefore in exactly one table; 2023 is in none (IBGE
#      published no estimate between the 2022 Census and the 2024 estimates). The tables
#      agree on what matters: 183,987,291 people in Brazil in 2007 (793), 190,755,799 in
#      2010 (1378), 203,080,756 in 2022 (4714), 213,317,639 estimated in 2021 (6579).
#
#   2. ONE MANIFEST ROW, THE YEAR -> TABLE MAP IN CODE. The manifest row stays
#      "population,population" with sidra_code 6579, and load_population() picks the table
#      of 2007, 2010 and 2022 itself (population_special_tables, R/population.R; until
#      2026-11-12 an `if` in the shared sidra_download(), and only for 2007 and 2010).
#      The alternatives, and why not:
#        * one dataset per table: the user would have to know which table holds which
#          year, and load_population(time_period = 2005:2012) would have to become three
#          calls -- a breaking change for no gain. The tables are not different datasets,
#          they are the same variable in the years the estimates table skips.
#        * rows of ONE dataset keyed by year (the manifest's own mechanism for "this
#          dataset differs by year", as DEGRAD's yearly rows): every key needs a row, so
#          25 rows (one per year with data), 22 of them saying 6579, each owned by this
#          resolver.
#        * a survey of its own for the three tables (like comex_dic next to comex): it
#          would show three more entries in datasets_link(), and only to move into the
#          manifest a map that changes once a decade and has to be pinned by tests either
#          way. censoagro's rows are no precedent: each is a different table of different
#          variables used in every year, not the same variable in different tables by year.
#      So the map is code, and the resolver checks it: this file keeps a copy
#      (POPULATION_SPECIAL_TABLES) that tests/testthat/test-population-resolver.R holds
#      equal to the loader's.
#
#   3. THE sidra_code IS A BARE TABLE NUMBER, AND THAT MEANS "NO CLASSIFICATION" -- as for
#      PPM's 94 and 95 and pibmunic's 5938. Until 2026-11-12 load_population() sent 6579
#      (and 793) to sidra_download() with its default classific = "all", and for
#      classific = "all" sidrar::get_sidra() first scrapes an IBGE HTML page
#      (apisidra.ibge.gov.br/desctabapi.aspx?c=<table>) to learn the classification ids,
#      even for a table with none. That page answers HTTP 403 behind a Cloudflare bot
#      challenge from the machine this was investigated on (9 of 9 requests, tables 6579,
#      793 and 4714), and load_population() failed there with a bare "Error in Download."
#      for every year tried except 2010 (country level: 2007, 2015, 2022, 2023, 2025;
#      state level: 2015) -- 2010 worked only because its special case spelled out
#      c1 = Total, so sidrar never needed the page. Sent with no classification instead
#      (R/download.R, sidra_get_table()), 6579, 793 and 4714 answer at all three levels:
#      country 1 row, state 27, municipality 5,571 (6579, 2024), 5,564 (793) and 5,570
#      (4714). This resolver checks the meaning both ways: for a table that gained a
#      classification a bare code would still answer HTTP 200, with only the "Total"
#      slice.
#
#   4. WHAT WENT STALE, AND WHAT WAS MISSING. available_time: the manifest said 2001-2021;
#      6579 has published 2024, 2025 and 2026 since, and the 2022 Census has its own table
#      (4714) that load_population() did not know: load_population(time_period = 2022)
#      warned "Option time_period must be in 2001-2021" and then stopped with "Error in
#      Download.", since 6579 has no 2022. available_time is now the union of the years
#      each table has a value for -- "2001-2022, 2024-2026" -- written in the grammar
#      parse_years() reads, gap included, so check_params() warns for 2023 and not for the
#      rest. The convention is sidra_api.R's: a year counts as covered when its value is
#      neither "..." nor "..".
#
#   5. THE VARIABLES. load_population() renames two ids (recode(), English only): 93, the
#      resident population of 2007, 2010 and 2022, and 9324, the estimate of every other
#      year. 6579 declares only 9324, 793 and 1378 only 93 (1378's 1000093 is a "percentual
#      do total geral" that "allxp" never requests), 4714 93, 6318 and 614: the area and the
#      density, which load_population() leaves out of its 2022 rows (they would be columns
#      of 2022 alone, passed through under IBGE's own names, unlike 2007 and 2010). Any
#      other variable a table starts to declare would pass through the same way, unrenamed
#      but not lost, so it is a NOTE here, not a failure.
#
#   6. docs_url. The manifest pointed at https://sidra.ibge.gov.br/pesquisa/estimapop/
#      tabelas, the survey page, which answers HTTP 403 behind the same challenge.
#      https://sidra.ibge.gov.br/tabela/6579 -- the `URL` the metadata itself reports --
#      answers HTTP 200, and is refreshed from the metadata on every run.
#
#   7. TIMING. Four tables, two requests each: 8 requests a run.
#
# FAILURE MODEL. The same as PAM's and pibmunic's: a row that fails is left alone and
# reported through `partial_failures`; with the single row of this source, "it failed" is
# the only failure there is, and it stops the resolver. A special table that cannot be
# checked fails the row too -- the loader depends on it for its year, and leaving the
# year out of available_time because one request flaked would be a wrong refresh.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage of each table; that
# sidrar still parses IBGE's response and labels its columns as R/population.R expects
# (tagged "# FRAGILE:" there); that IBGE has not published a table for a census year the
# map does not know. Nothing here can find such a table (2022's was missing from the map
# until 2026-11-12 and was found by hand); the one sign is a gap in available_time, as 2023
# has, for a year inside the series that no table covers.
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_* (or
# watch_*) as a resolver, so everything else in this file is population_* or POPULATION_*.

POPULATION_MAIN_TABLE <- "6579"

# The variable that decides whether a year of the estimates table is "published" (point 1).
POPULATION_COVERAGE_VARIABLE <- "9324"

# The resident population of the censuses and the count (point 1): the variable every
# special table is asked for.
POPULATION_VARIABLE <- "93"

# year -> the table load_population() asks for it, and with which classification: a copy
# of population_special_tables in R/population.R (this script runs without the package):
# tests/testthat/test-population-resolver.R fails if the two ever differ. classific and
# category are NA for a table with no classification.
POPULATION_SPECIAL_TABLES <- list(
  `2007` = list(table = "793", classific = NA_character_, category = NA_character_),
  `2010` = list(table = "1378", classific = "c1", category = "0"),
  `2022` = list(table = "4714", classific = NA_character_, category = NA_character_)
)

# table -> the variable ids it is known to declare (point 5); one beyond them is a note.
POPULATION_EXPECTED_VARIABLES <- list(
  `6579` = "9324",
  `793` = "93",
  `1378` = "93",
  `4714` = c("93", "6318", "614")
)

# Brazil level, every year, one variable, ONE classification and one of its categories.
# sprintf(url, table, variable, classification id, category).
POPULATION_COVERAGE_URL_CATEGORY <- "https://apisidra.ibge.gov.br/values/t/%s/n1/all/v/%s/p/all/c%s/%s"

# "6579" -> list(table = "6579", classific = NA, category = NA); "74/c80/all" ->
# list(table = "74", classific = "c80", category = "all"); NULL when it is neither.
# A classific of "all" parses (so it can be refused with a message).
population_parse_code <- function(code) {
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

# The note for the variables a table declares that load_population() does not know, or
# character(0).
population_variable_note <- function(table, declared) {
  unexpected <- setdiff(declared, POPULATION_EXPECTED_VARIABLES[[table]])
  if (length(unexpected) == 0) {
    return(character(0))
  }
  sprintf(
    paste(
      "table %s now declares variable(s) %s that load_population() does not know (a variable it does not rename",
      "comes back under IBGE's own name, and table 4714's rows are cut to variable 93); decide what to do with",
      "them and add them to POPULATION_EXPECTED_VARIABLES"
    ),
    table, paste(unexpected, collapse = ", ")
  )
}

# Checks the table of the manifest's code (6579): its metadata and coverage (sidra_api.R),
# then its variables. Returns what sidra_api_check_table() does plus `note`. Never throws.
population_check_main <- function(table) {
  tab <- sidra_api_check_table(table, POPULATION_COVERAGE_VARIABLE, allow_no_classification = TRUE)
  if (length(tab$problem) > 0) {
    return(tab)
  }
  tab$note <- population_variable_note(table, tab$variables)
  tab
}

# Checks ONE special table for ITS year: that its metadata is the table's, that it has the
# classification the loader's request assumes (none, or c1 with category 0), that it
# declares variable 93 and the territorial levels the manifest promises, and that variable
# 93 has a value for `year`. Returns list(problem, years, note): `problem` character(0)
# when all of that holds, `years` the years with a value. Never throws.
population_check_special <- function(year, spec, available_geo) {
  fail <- function(msg) list(problem = sprintf("table %s (the loader's table for %s): %s", spec$table, year, msg))

  meta_got <- sidra_api_fetch(sprintf(SIDRA_API_METADATA_URL, spec$table))
  if (length(meta_got$problem) > 0) {
    return(fail(meta_got$problem))
  }
  meta <- sidra_api_parse_metadata(meta_got$text, spec$table)
  if (length(meta$problem) > 0) {
    return(fail(meta$problem))
  }

  cl_ids <- names(meta$classifications)
  if (is.na(spec$classific)) {
    if (length(cl_ids) > 0) {
      return(fail(sprintf(
        paste(
          "it now has classification(s) %s, but load_population() sends none: IBGE answers that with HTTP 200 and",
          "only the 'Total' slice of each; name them in population_special_tables and POPULATION_SPECIAL_TABLES"
        ),
        paste0("c", cl_ids, collapse = ", ")
      )))
    }
  } else {
    cl_id <- sub("^c", "", spec$classific)
    if (!(cl_id %in% cl_ids)) {
      return(fail(sprintf(
        "it has no classification %s (it has %s), which load_population() asks for", spec$classific, paste0("c", cl_ids, collapse = ", ")
      )))
    }
    if (!(spec$category %in% meta$classifications[[cl_id]])) {
      return(fail(sprintf(
        paste(
          "category %s does not exist in classification %s (IBGE answers a query for a nonexistent category with",
          "HTTP 200 and empty values, so this check is the only thing that notices)"
        ),
        spec$category, spec$classific
      )))
    }
  }
  if (!(POPULATION_VARIABLE %in% meta$variables)) {
    return(fail(sprintf("it does not declare variable %s, the resident population", POPULATION_VARIABLE)))
  }
  geo_problem <- sidra_api_geo_problem(available_geo, meta$levels, spec$table)
  if (length(geo_problem) > 0) {
    return(fail(geo_problem))
  }

  url <- if (is.na(spec$classific)) {
    sprintf(SIDRA_API_COVERAGE_URL_PLAIN, spec$table, POPULATION_VARIABLE)
  } else {
    sprintf(POPULATION_COVERAGE_URL_CATEGORY, spec$table, POPULATION_VARIABLE, sub("^c", "", spec$classific), spec$category)
  }
  cov_got <- sidra_api_fetch(url)
  if (length(cov_got$problem) > 0) {
    return(fail(cov_got$problem))
  }
  cov <- sidra_api_parse_coverage(cov_got$text, classified = !is.na(spec$classific))
  if (length(cov$problem) > 0) {
    return(fail(sprintf("coverage: %s", cov$problem)))
  }
  years <- sort(unique(unlist(cov$years, use.names = FALSE)))
  if (!(as.integer(year) %in% years)) {
    return(fail(sprintf("it has no value for %s at Brazil level (it has: %s)", year, sidra_api_years_to_available_time(years))))
  }

  list(problem = character(0), years = years, note = population_variable_note(spec$table, meta$variables))
}

# Checks the manifest row against its (already checked) tables. Returns list(problem =
# character(0), available_time = "2001-2022, 2024-2026", docs_url = <6579's page or NA>,
# note = <character(0) or what is worth a person's look>) or list(problem = "<reason>").
population_check_row <- function(code, available_geo, main, specials) {
  parsed <- population_parse_code(code)
  if (is.null(parsed)) {
    return(list(problem = sprintf("sidra_code '%s' is not <table> or <table>/c<classification>/<category id or all>", code)))
  }
  if (parsed$table != POPULATION_MAIN_TABLE) {
    return(list(problem = sprintf(
      "sidra_code '%s' is table %s; load_population() chooses the tables of 2007, 2010 and 2022 around table %s, the estimates",
      code, parsed$table, POPULATION_MAIN_TABLE
    )))
  }
  if (length(main$problem) > 0) {
    return(list(problem = main$problem))
  }

  cl_ids <- names(main$classifications)
  if (is.na(parsed$classific)) {
    if (length(cl_ids) > 0) {
      return(list(problem = sprintf(
        paste(
          "table %s now has classification c%s, but sidra_code '%s' says it has none: load_population() would send no",
          "classification, and IBGE answers that with HTTP 200 and only the 'Total' slice; write '%s/c%s/all'"
        ),
        parsed$table, cl_ids[1], code, parsed$table, cl_ids[1]
      )))
    }
  } else if (length(cl_ids) == 0) {
    return(list(problem = sprintf(
      "sidra_code '%s' names a classification, but table %s has none; write '%s'", code, parsed$table, parsed$table
    )))
  } else if (parsed$classific == "all") {
    return(list(problem = sprintf(
      paste(
        "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is",
        "behind a bot challenge (see this file's header, point 3); write '%s/c%s/%s' instead"
      ),
      code, parsed$table, cl_ids[1], parsed$category
    )))
  }

  geo_problem <- sidra_api_geo_problem(available_geo, main$levels, parsed$table)
  if (length(geo_problem) > 0) {
    return(list(problem = geo_problem))
  }

  for (year in names(specials)) {
    if (length(specials[[year]]$problem) > 0) {
      return(list(problem = specials[[year]]$problem))
    }
  }

  estimate_years <- sidra_api_row_years(main, NA_character_)
  if (length(estimate_years) == 0) {
    return(list(problem = sprintf("table %s resolves, but has no year with a published value at Brazil level", parsed$table)))
  }

  notes <- c(main$note, unlist(lapply(specials, function(s) s$note), use.names = FALSE))
  overlap <- intersect(as.integer(names(specials)), estimate_years)
  if (length(overlap) > 0) {
    notes <- c(notes, sprintf(
      paste(
        "table %s now has a value for %s too: load_population() still asks table %s for it; compare the two and decide",
        "which to keep (population_special_tables and POPULATION_SPECIAL_TABLES)"
      ),
      POPULATION_MAIN_TABLE, paste(overlap, collapse = ", "),
      paste(vapply(overlap, function(y) POPULATION_SPECIAL_TABLES[[as.character(y)]]$table, ""), collapse = ", ")
    ))
  }

  list(
    problem = character(0),
    available_time = sidra_api_years_to_available_time(c(estimate_years, as.integer(names(specials)))),
    docs_url = sidra_api_docs_url(main),
    note = notes
  )
}

resolve_population <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_population(): no manifest rows tagged resolver == 'population' -- ",
      "cannot tell which SIDRA code to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_population() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_population(): every population row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_population(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  # the four tables are checked once, whatever the rows (one row today); the special ones
  # against the first row's available_geo, which is the promise they have to keep
  main <- population_check_main(POPULATION_MAIN_TABLE)
  geo <- rows$available_geo[1]
  specials <- stats::setNames(
    lapply(names(POPULATION_SPECIAL_TABLES), function(year) population_check_special(year, POPULATION_SPECIAL_TABLES[[year]], geo)),
    names(POPULATION_SPECIAL_TABLES)
  )
  if (length(main$problem) == 0) {
    message(
      "resolve_population(): table ", POPULATION_MAIN_TABLE, " ok -- ",
      sidra_api_years_to_available_time(sidra_api_row_years(main, NA_character_))
    )
  }
  for (year in names(specials)) {
    if (length(specials[[year]]$problem) == 0) {
      message("resolve_population(): table ", POPULATION_SPECIAL_TABLES[[year]]$table, " ok -- ", year)
    }
  }

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  notes <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    res <- population_check_row(rows$sidra_code[i], rows$available_geo[i], main, specials)
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
    sprintf("population/%s (%s): %s", rows$dataset[failed], rows$sidra_code[failed], problems[failed]),
    sprintf("population/%s (%s): %s", rows$dataset[!is.na(notes)], rows$sidra_code[!is.na(notes)], notes[!is.na(notes)])
  )

  if (all(failed)) {
    stop(
      "resolve_population(): all ", n, " row(s) failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines, collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the manifest
  # already says about it.
  ok <- !failed
  out <- tibble::tibble(
    survey = "population", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "population"
  )
  if (length(failure_lines) > 0) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
