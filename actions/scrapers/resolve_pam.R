# actions/scrapers/resolve_pam.R
#
# PAM (Producao Agricola Municipal, IBGE) is read LIVE from IBGE's SIDRA API:
# there is no file and no `url`. Each of the 79 manifest rows names a SIDRA
# table, a classification and a category in `sidra_code` ("1613/c82/45981" =
# table 1613, classification c82 "Produto das lavouras permanentes", category
# 45981 "Acai"; "5457/c782/all" = every category of the table), and
# R/pam.R's load_pam() hands the three parts to sidrar::get_sidra() through
# R/download.R's sidra_download(). This was the FIRST resolver for a
# SIDRA-based source; the part every SIDRA resolver needs -- the metadata
# lookup, the coverage query and the value coding -- now lives in
# actions/scripts/sidra_api.R (point 9), and what is left here is PAM's own:
# the grammar of its sidra_code, which variable ids load_pam() names, what its
# rows mean. See "REUSE" at the end of this header.
#
# WHY THIS IS A VERIFICATION RESOLVER (resolve_terraclimate.R's model), NOT A
# DISCOVERY ONE. Nothing needs discovering -- the table ids are fixed and
# the crop list is the manifest's own. What goes stale is (1) `available_time`
# (the manifest said 1974-2023 for most rows; IBGE has published 2024 and 2025
# since) and (2) the sidra_code itself, if IBGE ever renumbers a category or
# retires a table, and (3) nothing tells anyone when (2) happens -- see point 4.
# So this resolver VERIFIES every sidra_code against IBGE's own description of
# the table and REFRESHES one field, available_time, from IBGE's own data.
#
# VERIFIED LIVE (2026-10-21):
#
#   1. THE METADATA SOURCE. https://servicodados.ibge.gov.br/api/v3/agregados/
#      <table>/metadados is a documented JSON API (IBGE "Servico de dados"),
#      HTTP 200 with no bot challenge. It gives the table's name, its
#      frequency and first/last period, its territorial levels (N1 Brazil, N2
#      region, N3 state, N6 municipality, N8/N9 meso/micro-region), its
#      variables, and every classification with every category id and name.
#      sidrar::info_sidra() is NOT a usable alternative: it scrapes the HTML
#      page https://apisidra.ibge.gov.br/desctabapi.aspx?c=<table>, which
#      answers HTTP 403 with a Cloudflare managed challenge ("Just a moment...")
#      from the machine this was investigated on, for every User-Agent tried.
#
#   2. WHAT THE METADATA CANNOT SAY. The period it reports is the TABLE's
#      (1974-2025 for 1612, 1613 and 5457; 2003-2025 for 839, 1000, 1001 and
#      1002), not each category's: acai exists only from 2015, annatto only
#      from 1988, alfafa only until 1987. Per-category coverage needs data. One
#      request per table gets all of it:
#         https://apisidra.ibge.gov.br/values/t/<table>/n1/all/v/<var>/p/all/
#         c<classification>/all
#      i.e. Brazil level, every year, every category (0.4-1.3 MB for the three
#      big tables, ~25 KB for the four small ones; 7 requests, ~2.7 MB a run).
#      A year IBGE does not publish for a category is still a ROW, with the
#      value "..." (not available) or ".." (not applicable); a true zero is "-"
#      and a suppressed value "X". A year counts as covered when its value is
#      not "..." or ".." -- the convention the manifest's old ranges followed
#      (cotton_arboreo has only "-" after 2013 and the manifest listed it
#      through 2023). Against it, 4 of the 79 old ranges were exactly right
#      (alfafa, cashew, forage_cane, rubber_liquid_latex: series that ended in
#      1987), 74 differed only in the end year (2023, two editions behind: 66
#      now end in 2025 and 8 -- india_tea, jute_fiber, linen_seeds, pea, quince,
#      ramie_fiber, rye, tung -- in 2024), and one, annatto_seeds, started in
#      1981 when IBGE publishes from 1988.
#
#   3. WHICH VARIABLE. The "Total" categories (1612/c81/0 and 1613/c82/0) have
#      no quantity (tonnes of crops of different units cannot be added), so
#      v/214 reports them as never published. Value of production (215) exists
#      for every category; area (216) agrees with it; checked over all 8,996
#      category-years of tables 1612, 1613 and 5457: the three variables
#      disagree ONLY on the Total's quantity. The four multi-harvest tables
#      (839, 1000, 1001, 1002) have no value variable, so 214 is used there.
#      PAM_COVERAGE_VARIABLES lists the preference order; the first one the
#      table declares in its metadata is used.
#
#   4. WHAT A BAD CODE DOES. A category id that does not exist in the table is
#      NOT an error: IBGE answers HTTP 200 and a normal-looking table whose
#      value is "..." everywhere and whose category NAME is empty, so
#      sidrar::get_sidra() hands back five NA rows and load_pam() hands back
#      nothing, with no message. Only a classification that does not belong
#      to the table (HTTP 400 "Parametro C999 (Classificacao) incompativel com
#      a tabela") or a table that does not exist (HTTP 400 "Tabela 99999:
#      Tabela invalida") is rejected -- and sidra_download() turns even those
#      into a bare "Error in Download." (it throws away the reason sidrar
#      captured). A year with no data is the same silent shape: HTTP 200, "..."
#      values. So the only thing that notices an id IBGE renumbered is
#      comparing the manifest's ids to the metadata's, which is exactly what
#      this resolver does.
#
#   5. THE `classific = "all"` FORM IS NOT USED. Seven rows used to say
#      "<table>/all/all" (all_crops, temporary_crops, permanent_crops, beans,
#      corn, peanut, potato). For classific = "all" (also sidra_download()'s
#      default) sidrar::get_sidra() first scrapes the same Cloudflare-
#      challenged HTML page as point 1 to learn the classification ids, and
#      when that fails the user gets "Error in Download." -- load_pam(
#      "all_crops") did exactly that on the investigating machine, while the
#      single-crop rows (which name the classification) worked. The explicit
#      form "<table>/c<classification>/all" builds the identical API request
#      (".../c782/all") without the page. The 7 rows were rewritten to it; the
#      resolver verifies the classification id like any other and REFUSES a
#      "<table>/all/<x>" row with a message naming the id to use, rather than
#      rewriting sidra_code itself: the manifest, not the resolver, says which
#      classification a dataset reads.
#
#   6. TERRITORIAL LEVELS. All seven tables declare N1, N2, N3, N6, N8, N9, so
#      the manifest's available_geo ("country, state, municipality") is
#      honoured by IBGE. (N2/region is also served, and R/pam.R has a branch
#      for it, but check_params() rejects it because the manifest does not list
#      it and the documentation does not offer it -- left as is.) The resolver
#      checks that every level a row's available_geo names exists in the
#      table's metadata; it does not rewrite available_geo.
#
#   7. docs_url. The manifest pointed all 79 rows at the survey page,
#      https://sidra.ibge.gov.br/pesquisa/pam/tabelas. Checked 2026-10-22, that
#      page (and /pesquisa/pam/tabelas/brasil/2023) answers HTTP 403 behind the
#      same Cloudflare managed challenge as the HTML table page of point 1, so
#      nothing here can verify it. https://sidra.ibge.gov.br/tabela/<table> --
#      the `URL` the metadata itself reports -- answers HTTP 200 for all seven
#      tables, with the table's number in its <title>. docs_url is therefore each
#      row's table page, fanned across the rows that read the table (7 pages for
#      79 rows, from the 7 metadata responses already fetched, no extra
#      request), refreshed on every run and left alone when the metadata
#      reports anything but a table page.
#
#   8. VARIABLE IDS. load_pam() names seven IBGE variable ids (the "pam_variables"
#      entry of load_dictionary(), R/dictionary.R) -- 112, 214, 215, 216, 109,
#      8331 and 2313 -- and the seven tables declare
#      exactly those (plus "... - percentual do total geral" twins, unit "%",
#      which sidrar's "allxp" never requests; 2313 is table 1613's "area
#      destinada a colheita", the one that used to come back as an unnamed
#      na_v<crop> column). PAM_NAMED_VARIABLES is a copy of that entry's ids,
#      kept equal to it by tests/testthat/test-pam-resolver.R, and every run
#      checks that each table still declares nothing else: a variable IBGE adds
#      is reported, naming it and IBGE's name for it, before it reaches a user as
#      a v<id>_v<crop> column with a warning. It fails the TABLE (all its rows are
#      left alone and reported once), the way resolve_ppm.R treats the same
#      check. R/pam.R, unlike R/ppm.R, has that v<id> fallback, so a note that
#      did not hold back the refresh would also do; strict was kept so the two
#      resolvers behave alike.
#
#   9. SHARED CODE. The ~150 lines this file and resolve_ppm.R had in common
#      (the GET, the response excerpt, the metadata and coverage parsers, the
#      years grammar, the territorial-level check) are in
#      actions/scripts/sidra_api.R, sourced by build_manifest.R next to
#      zip_remote_listing.R, the other helper resolvers share. This file calls it
#      for the PAM tables with PAM_COVERAGE_VARIABLES and keeps its own
#      pam_parse_code(), pam_check_table(), pam_check_row() and resolve_pam().
#
# WHAT EACH RUN DOES (2 requests per distinct TABLE, 14 for the whole source,
# never per row -- 79 rows share 7 tables):
#   a. GET the table's metadata; require the table id, the row's classification
#      and category ids, the territorial levels the row advertises, and that
#      no variable is declared that load_pam() does not name (point 8).
#   b. GET the coverage query of point 2 and compute, per category, the years
#      with a published value. A category with none at all is a failure (it
#      resolves, but a user would get nothing). available_time is that set of
#      years ("1974-2025", or "1974-1990, 1995-2025" if there are gaps -- the
#      grammar parse_years() reads); for a row that reads a whole table
#      ("<table>/c<n>/all") it is the union over its categories. docs_url is the
#      table's page (point 7).
#
# FAILURE SEMANTICS. A row that fails is left alone (absent from the result) and
# reported through `partial_failures`, so one retired crop does not stop the
# other 78 from refreshing; build_manifest.R turns that into exit code 11. A
# TABLE that cannot be checked (HTTP error, not JSON, no data) takes down all
# of its rows, and is reported once ("pam/table 1613 (39 dataset(s)): ...")
# instead of 39 times. If EVERY row fails, the resolver stops: that is the
# signature of a systemic change (IBGE moved or challenged its API), not of a
# flake. There is deliberately no retry: nothing else in actions/scrapers/ has
# one, and a flaky request becomes a partial failure the next monthly run
# clears.
#
# WHAT THIS DOES NOT CHECK: municipality- or state-level coverage (a crop can be
# published for Brazil and absent in a given state; PAM leaves those cells
# as "-"/"..." and load_pam() drops them), that sidrar still parses IBGE's
# response, or R/pam.R's hardcoded table ids and column renames (those are
# tagged "# FRAGILE:" there). It does not discover new crops either: a category
# IBGE adds to a table needs a manifest row (and a dictionary entry in
# R/dictionary.R). On 2026-10-21 tables 1612 and 1613 had 15 categories with
# no dataset of their own (1612: abobora 83374, alface 83375, canola 83384,
# cenoura 83381, chuchu 83379, gergelim 83383, inhame 83382, milho verde
# 83377, morango 83378, pimentao 83376, repolho 83380; 1613: acerola 83385,
# cupuacu 83386, graviola 83387, palmito 90001). They are still returned by
# "temporary_crops", "permanent_crops" and "all_crops", which read whole
# tables; this resolver does not report them.
#
# REUSE (PEVS, PIB municipal, CEMPRE, ...; resolve_ppm.R already follows it). The
# shape that carries over to any SIDRA source, most of it now in sidra_api.R:
# (1) one metadata GET and one coverage GET per distinct
# TABLE, never per row; (2) rows are checked against the metadata's
# classification/category ids, because the data API accepts a made-up category
# silently (point 4); (3) available_time comes from the table's own data, not
# from metadata (point 2); (4) a row says "<table>/c<n>/<category>" with the
# classification spelled out (point 5); (5) per-row partial failures, whole
# resolver stops only when nothing could be checked. What is PAM-specific and
# would change: the classification is a crop list (other surveys have several
# classifications per table, so a row's sidra_code needs several cNNN/<id>
# pairs and the coverage query a path per classification), the variable
# preference order, and the geo-level wording.
#
# Naming: build_manifest.R registers EVERY top-level binding called resolve_*
# (or watch_*) as a resolver, so everything else in this file is pam_* or
# PAM_*.

# The variable that decides whether a year is "published", in order of
# preference: the first one a table declares is used (point 3 of the header).
# The four multi-harvest tables have no 215, so they fall to 214; a table that
# declares neither is a failure (sidra_api_check_table()'s fall_back_to_first
# stays FALSE: these two ids are known to be enough for PAM's 7 tables).
PAM_COVERAGE_VARIABLES <- c("215", "214")

# The IBGE variable ids load_pam() names: the "pam_variables" entry of
# load_dictionary() (R/dictionary.R), in both languages. 112 yield, 214
# quantity, 215 value, 216 harvested area, 109 planted area, 8331
# planted-or-destined-to-harvest area (table 5457) and 2313 destined-to-harvest
# area (table 1613). A copy of that entry's var_code column (this script runs
# without the package): tests/testthat/test-pam-resolver.R fails if the two
# ever differ.
PAM_NAMED_VARIABLES <- c("112", "214", "215", "216", "109", "8331", "2313")

# "1613/c82/45981" -> list(table = "1613", classific = "c82", category =
# "45981"); NULL when it is not <digits>/c<digits>|all/<digits>|all. A
# classific of "all" is a valid parse (the caller explains why it is refused).
pam_parse_code <- function(code) {
  if (is.null(code) || length(code) != 1 || is.na(code)) {
    return(NULL)
  }
  m <- regmatches(code, regexec("^([0-9]+)/(c[0-9]+|all)/([0-9]+|all)$", code))[[1]]
  if (length(m) != 4) {
    return(NULL)
  }
  list(table = m[2], classific = m[3], category = m[4])
}

# Checks ONE table: its metadata, then its coverage (sidra_api.R), then that
# every non-percentage variable it declares is one load_pam() names. Returns
# list(problem, classifications, levels, years, ...) with `problem`
# character(0) when all worked. Never throws.
pam_check_table <- function(table) {
  tab <- sidra_api_check_table(table, PAM_COVERAGE_VARIABLES)
  if (length(tab$problem) > 0) {
    return(tab)
  }
  unnamed <- tab$variable_info[!(tab$variable_info$id %in% PAM_NAMED_VARIABLES), , drop = FALSE]
  if (nrow(unnamed) > 0) {
    return(list(problem = sprintf(
      paste(
        "table %s now declares variable(s) %s that load_pam() does not name (it would return them as",
        "unnamed v<id>_v<crop> columns, with a warning); name them in the pam_variables entry of R/dictionary.R and in PAM_NAMED_VARIABLES"
      ),
      table, paste0(unnamed$id, " (", unnamed$nome, ")", collapse = ", ")
    )))
  }
  tab
}

# Checks ONE manifest row against its (already checked) table. Returns
# list(problem = character(0), available_time = "1974-2025", docs_url = <the
# table's page or NA>) or list(problem = "<reason>", table_level = <TRUE when
# the whole table failed>).
pam_check_row <- function(code, available_geo, checked) {
  parsed <- pam_parse_code(code)
  if (is.null(parsed)) {
    return(list(
      problem = sprintf("sidra_code '%s' is not <table>/c<classification>/<category id or all>", code),
      table_level = FALSE
    ))
  }
  tab <- checked[[parsed$table]]
  if (length(tab$problem) > 0) {
    return(list(problem = tab$problem, table_level = TRUE))
  }
  row_fail <- function(msg) list(problem = msg, table_level = FALSE)

  cl_ids <- names(tab$classifications)
  if (parsed$classific == "all") {
    return(row_fail(sprintf(
      paste(
        "sidra_code '%s' uses classific = 'all', which makes sidrar scrape an IBGE HTML page that is",
        "behind a bot challenge (see this file's header, point 5); write '%s/c%s/%s' instead"
      ),
      code, parsed$table, cl_ids[1], parsed$category
    )))
  }
  if (!(sub("^c", "", parsed$classific) %in% cl_ids)) {
    return(row_fail(sprintf(
      "classification %s is not one of table %s's (%s)", parsed$classific, parsed$table, paste0("c", cl_ids, collapse = ", ")
    )))
  }
  cl_id <- sub("^c", "", parsed$classific)
  categories <- tab$classifications[[cl_id]]

  if (parsed$category != "all" && !(parsed$category %in% categories)) {
    return(row_fail(sprintf(
      paste(
        "category %s does not exist in classification %s of table %s (IBGE answers a query for a",
        "nonexistent category with HTTP 200 and empty values, so this check is the only thing that notices)"
      ),
      parsed$category, parsed$classific, parsed$table
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
    docs_url = sidra_api_docs_url(tab)
  )
}

resolve_pam <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_pam(): no manifest rows tagged resolver == 'pam' -- ",
      "cannot tell which SIDRA codes to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_pam() needs the 'curl' and 'jsonlite' packages (CI-only; not package Imports).")
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_pam(): every PAM row is unkeyed (one row per dataset) -- found a row with a geo_level or a year. ",
      "This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$sidra_code) | rows$sidra_code == "")) {
    stop("resolve_pam(): every row needs a sidra_code to verify -- this resolver doesn't invent one.")
  }

  tables <- unique(stats::na.omit(vapply(
    rows$sidra_code,
    function(code) {
      p <- pam_parse_code(code)
      if (is.null(p)) NA_character_ else p$table
    },
    character(1)
  )))
  checked <- stats::setNames(lapply(tables, pam_check_table), tables)
  for (t in tables) {
    if (length(checked[[t]]$problem) == 0) {
      all_years <- sort(unique(unlist(checked[[t]]$years, use.names = FALSE)))
      message("resolve_pam(): table ", t, " ok -- ", length(checked[[t]]$years), " categories published, ", sidra_api_years_to_available_time(all_years))
    }
  }

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  docs_url <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  table_level <- rep(FALSE, n)
  for (i in seq_len(n)) {
    res <- pam_check_row(rows$sidra_code[i], rows$available_geo[i], checked)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
      table_level[i] <- isTRUE(res$table_level)
    } else {
      available_time[i] <- res$available_time
      docs_url[i] <- res$docs_url
    }
  }

  failed <- !is.na(problems)
  row_table <- vapply(
    rows$sidra_code,
    function(code) {
      p <- pam_parse_code(code)
      if (is.null(p)) NA_character_ else p$table
    },
    character(1)
  )
  failure_lines <- c(
    vapply(
      unique(row_table[failed & table_level]),
      function(t) {
        sprintf(
          "pam/table %s (%d dataset(s)): %s", t, sum(failed & table_level & row_table %in% t),
          problems[failed & table_level & row_table %in% t][1]
        )
      },
      character(1), USE.NAMES = FALSE
    ),
    sprintf("pam/%s (%s): %s", rows$dataset[failed & !table_level], rows$sidra_code[failed & !table_level], problems[failed & !table_level])
  )

  if (all(failed)) {
    stop(
      "resolve_pam(): all ", n, " rows failed verification -- a systemic change, not a flake. The usual cause is ",
      "IBGE's API (metadata at servicodados.ibge.gov.br, values at apisidra.ibge.gov.br) being unreachable, ",
      "challenged or restructured. Reasons:\n- ",
      paste(failure_lines, collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a row that failed keeps whatever the
  # manifest already says about it (see FAILURE SEMANTICS).
  ok <- !failed
  out <- tibble::tibble(
    survey = "pam", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    available_time = available_time[ok], docs_url = docs_url[ok], resolver = "pam"
  )
  if (any(failed)) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
