# actions/scrapers/resolve_pam.R
#
# PAM (Producao Agricola Municipal, IBGE) is read LIVE from IBGE's SIDRA API:
# there is no file and no `url`. Each of the 79 manifest rows names a SIDRA
# table, a classification and a category in `sidra_code` ("1613/c82/45981" =
# table 1613, classification c82 "Produto das lavouras permanentes", category
# 45981 "Acai"; "5457/c782/all" = every category of the table), and
# R/pam.R's load_pam() hands the three parts to sidrar::get_sidra() through
# R/download.R's sidra_download(). This is the FIRST resolver for a
# SIDRA-based source (PPM, PEVS, PIB municipal and the rest are next), so it
# is written to be copied: see "REUSE" at the end of this header.
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
# WHAT EACH RUN DOES (2 requests per distinct TABLE, 14 for the whole source,
# never per row -- 79 rows share 7 tables):
#   a. GET the table's metadata; require the table id, the row's classification
#      and category ids, and the territorial levels the row advertises.
#   b. GET the coverage query of point 2 and compute, per category, the years
#      with a published value. A category with none at all is a failure (it
#      resolves, but a user would get nothing). available_time is that set of
#      years ("1974-2025", or "1974-1990, 1995-2025" if there are gaps -- the
#      grammar parse_years() reads); for a row that reads a whole table
#      ("<table>/c<n>/all") it is the union over its categories.
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
# REUSE (PPM, PEVS, PIB municipal, CEMPRE, ...). The shape that carries over to
# any SIDRA source: (1) one metadata GET and one coverage GET per distinct
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

PAM_METADATA_URL <- "https://servicodados.ibge.gov.br/api/v3/agregados/%s/metadados"

# Brazil level, every year, every category of one classification, one variable.
# sprintf(PAM_COVERAGE_URL, table, variable, classification id) -- the
# classification id is the bare number ("81"), the path wants "c81".
PAM_COVERAGE_URL <- "https://apisidra.ibge.gov.br/values/t/%s/n1/all/v/%s/p/all/c%s/all"

# The variable that decides whether a year is "published", in order of
# preference: the first one a table declares is used (point 3 of the header).
PAM_COVERAGE_VARIABLES <- c("215", "214")

# What SIDRA prints instead of a number when there is none: "..." not
# available, ".." not applicable. Everything else ("-" zero, "X" suppressed,
# a number) is a published value.
PAM_UNPUBLISHED <- c("...", "..")

# available_geo word -> the IBGE territorial level that must exist in the
# table's metadata.
PAM_GEO_LEVELS <- c(country = "N1", region = "N2", state = "N3", municipality = "N6")

PAM_USER_AGENT <- "datazoom.amazonia-manifest-resolver (+https://github.com/datazoompuc/datazoom.amazonia)"

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

# Pure: sorted integer years -> the manifest's available_time grammar
# ("2005", "1974-2025", "1974-1990, 1995-2025"), or NA when there are none.
pam_years_to_available_time <- function(years) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0) {
    return(NA_character_)
  }
  starts <- c(1L, which(diff(years) > 1L) + 1L)
  ends <- c(starts[-1] - 1L, length(years))
  ranges <- ifelse(
    years[starts] == years[ends],
    as.character(years[starts]),
    sprintf("%d-%d", years[starts], years[ends])
  )
  paste(ranges, collapse = ", ")
}

# A short, single-line excerpt of the text of a response body, or "" when it
# is empty or not text -- IBGE explains its 400s in one plain sentence, and a
# Cloudflare challenge is an HTML page whose text ("Just a moment...") is what
# matters, not its markup.
pam_snippet <- function(content, n = 160) {
  if (length(content) == 0) {
    return("")
  }
  head_bytes <- content[seq_len(min(length(content), 4000))]
  if (any(head_bytes == as.raw(0))) {
    return("")
  }
  txt <- rawToChar(head_bytes)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) {
    return("")
  }
  txt <- gsub("(?s)<(script|style)[^>]*>.*?</\\1>", " ", txt, perl = TRUE)
  txt <- gsub("<[^>]*>", " ", txt)
  txt <- trimws(gsub("[[:space:]]+", " ", txt))
  if (nchar(txt) > n) txt <- paste0(substr(txt, 1, n), "...")
  txt
}

# One GET, redirects not followed (a redirect means the API moved -- worth a
# loud failure, not a silent follow). Returns list(problem = character(0),
# text = <body>) or list(problem = "<reason>"). Never throws: "could not
# check" is a reason, never conflated with "checked and fine".
pam_fetch <- function(url, timeout_s = 120) {
  h <- curl::new_handle(timeout = timeout_s, followlocation = FALSE, useragent = PAM_USER_AGENT)
  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(problem = sprintf("could not reach %s (%s)", url, conditionMessage(resp))))
  }
  if (resp$status_code != 200L) {
    snippet <- pam_snippet(resp$content)
    return(list(problem = sprintf(
      "%s answered HTTP %d%s", url, resp$status_code,
      if (nzchar(snippet)) paste0(": ", snippet) else ""
    )))
  }
  text <- rawToChar(resp$content)
  Encoding(text) <- "UTF-8"
  list(problem = character(0), text = text)
}

# Pure: parses one table's /metadados JSON. Returns list(problem, classifications,
# levels, variables); `problem` is character(0) when the description is usable.
# classifications is a named list: classification id ("81") -> its category ids.
pam_parse_metadata <- function(text, table) {
  fail <- function(msg) list(problem = msg)

  meta <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(meta) || is.null(meta$id) || is.null(meta$classificacoes)) {
    snippet <- pam_snippet(charToRaw(enc2utf8(text)), n = 80)
    return(fail(sprintf(
      "the response is not a SIDRA table description (no id/classificacoes)%s",
      if (nzchar(snippet)) paste0("; it starts with: ", snippet) else ""
    )))
  }
  if (!identical(as.character(meta$id), as.character(table))) {
    return(fail(sprintf("the description is for table %s, not %s", meta$id, table)))
  }

  classifications <- lapply(meta$classificacoes, function(cl) {
    vapply(cl$categorias, function(ct) as.character(ct$id), character(1))
  })
  names(classifications) <- vapply(meta$classificacoes, function(cl) as.character(cl$id), character(1))

  list(
    problem = character(0),
    classifications = classifications,
    levels = as.character(unlist(meta$nivelTerritorial, use.names = FALSE)),
    variables = vapply(meta$variaveis, function(v) as.character(v$id), character(1))
  )
}

# Pure: parses the coverage query. Returns list(problem, years) where years is
# a named list: category id -> sorted integer years with a published value
# (categories with none are absent). The first element of IBGE's array is a
# header row naming each column, which is how the layout is checked.
pam_parse_coverage <- function(text) {
  fail <- function(msg) list(problem = msg)

  d <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (!is.data.frame(d) || !all(c("V", "D3C", "D3N", "D4C") %in% names(d))) {
    return(fail("the response is not a SIDRA value table (a JSON array with V, D3C, D3N and D4C)"))
  }
  if (!identical(d$D3N[1], "Ano")) {
    return(fail(sprintf("the third dimension is '%s', not 'Ano' -- the layout of the request changed", d$D3N[1])))
  }
  d <- d[-1, , drop = FALSE]
  if (nrow(d) == 0) {
    return(fail("the table returned no data rows at all"))
  }
  if (!all(grepl("^[0-9]{4}$", d$D3C))) {
    return(fail("the period codes are not four-digit years (the table is not annual)"))
  }
  published <- !is.na(d$V) & !(d$V %in% PAM_UNPUBLISHED)
  years <- lapply(split(as.integer(d$D3C[published]), d$D4C[published]), function(y) sort(unique(y)))
  list(problem = character(0), years = years)
}

# Checks ONE table: its metadata, then its coverage. Returns list(problem,
# classifications, levels, years) with `problem` character(0) when both worked.
# Never throws.
pam_check_table <- function(table) {
  meta_got <- pam_fetch(sprintf(PAM_METADATA_URL, table))
  if (length(meta_got$problem) > 0) {
    return(list(problem = meta_got$problem))
  }
  meta <- pam_parse_metadata(meta_got$text, table)
  if (length(meta$problem) > 0) {
    return(list(problem = sprintf("metadata of table %s: %s", table, meta$problem)))
  }
  if (length(meta$classifications) != 1) {
    return(list(problem = sprintf(
      "table %s has %d classifications (%s); the coverage query here handles exactly one (see REUSE in this file's header)",
      table, length(meta$classifications), paste0("c", names(meta$classifications), collapse = ", ")
    )))
  }
  variable <- PAM_COVERAGE_VARIABLES[PAM_COVERAGE_VARIABLES %in% meta$variables][1]
  if (is.na(variable)) {
    return(list(problem = sprintf(
      "table %s declares none of the variables %s", table, paste(PAM_COVERAGE_VARIABLES, collapse = "/")
    )))
  }

  cov_got <- pam_fetch(sprintf(PAM_COVERAGE_URL, table, variable, names(meta$classifications)))
  if (length(cov_got$problem) > 0) {
    return(list(problem = cov_got$problem))
  }
  cov <- pam_parse_coverage(cov_got$text)
  if (length(cov$problem) > 0) {
    return(list(problem = sprintf("coverage of table %s: %s", table, cov$problem)))
  }

  list(
    problem = character(0),
    classifications = meta$classifications,
    levels = meta$levels,
    years = cov$years
  )
}

# Checks ONE manifest row against its (already checked) table. Returns
# list(problem = character(0), available_time = "1974-2025") or list(problem =
# "<reason>", table_level = <TRUE when the whole table failed>).
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

  if (!is.na(available_geo)) {
    wanted <- trimws(strsplit(tolower(available_geo), ",")[[1]])
    unknown <- setdiff(wanted, names(PAM_GEO_LEVELS))
    if (length(unknown) > 0) {
      return(row_fail(sprintf("available_geo names '%s', which has no IBGE territorial level on record in PAM_GEO_LEVELS", paste(unknown, collapse = "', '"))))
    }
    missing_levels <- setdiff(PAM_GEO_LEVELS[wanted], tab$levels)
    if (length(missing_levels) > 0) {
      return(row_fail(sprintf(
        "available_geo promises %s, but table %s's metadata has no territorial level %s",
        available_geo, parsed$table, paste(missing_levels, collapse = ", ")
      )))
    }
  }

  years <- if (parsed$category == "all") {
    sort(unique(unlist(tab$years, use.names = FALSE)))
  } else {
    tab$years[[parsed$category]]
  }
  if (length(years) == 0) {
    return(row_fail(sprintf(
      "category %s of table %s resolves, but has no year with a published value at Brazil level",
      parsed$category, parsed$table
    )))
  }

  list(problem = character(0), available_time = pam_years_to_available_time(years))
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
      message("resolve_pam(): table ", t, " ok -- ", length(checked[[t]]$years), " categories published, ", pam_years_to_available_time(all_years))
    }
  }

  n <- nrow(rows)
  available_time <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)
  table_level <- rep(FALSE, n)
  for (i in seq_len(n)) {
    res <- pam_check_row(rows$sidra_code[i], rows$available_geo[i], checked)
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
      table_level[i] <- isTRUE(res$table_level)
    } else {
      available_time[i] <- res$available_time
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
    available_time = available_time[ok], resolver = "pam"
  )
  if (any(failed)) {
    attr(out, "partial_failures") <- failure_lines
  }
  out
}
