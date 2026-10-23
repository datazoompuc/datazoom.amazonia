# actions/scripts/sidra_api.R
#
# What every SIDRA resolver (resolve_pam.R, resolve_ppm.R, and the ones for PEVS,
# PIB municipal, CEMPRE, ...) needs from IBGE's two APIs, in one place: ask a
# table's metadata what it is (its classification, categories, variables and
# territorial levels), and ask its data how far each series reaches. Each
# resolver keeps what is its own -- the grammar of its sidra_code, which
# variable ids its load_*() names, what its rows mean -- and calls this for the
# rest. It was extracted from the two resolvers once there were two (they had
# ~150 lines in common, identical but for a prefix), so a fix here reaches both.
#
# WHERE IT LIVES AND HOW IT IS CONSUMED, after actions/scripts/zip_remote_listing.R,
# the existing precedent for a helper shared by resolvers: it sits in
# actions/scripts/ -- NOT in actions/scrapers/, where build_manifest.R registers
# every resolve_*/watch_* binding of every file it finds -- and build_manifest.R
# source()s it once, next to zip_remote_listing.R, before it sources the
# resolvers, so they find these functions in the global environment. A resolver
# never source()s it itself; a resolver's test does (tests/testthat/test-pam-
# resolver.R, test-sidra-api.R), the way test-zip-remote-listing.R does.
#
# Like zip_remote_listing.R it is split into a FETCH layer (sidra_api_fetch(),
# the only code that touches curl) and PURE parse layers (text in, structures
# out), and every genuine failure is a returned `problem`, never an empty
# result: "could not check" must never be conflated with "checked and fine".
#
# What the resolvers' headers verified live about these APIs (2026-10-21/22):
#   * https://servicodados.ibge.gov.br/api/v3/agregados/<table>/metadados is a
#     documented JSON API with no bot challenge. It gives the table's frequency
#     and period, its territorial levels (N1 Brazil, N2 region, N3 state, N6
#     municipality, N8/N9 meso/micro-region), its variables (with a unit: "%" for
#     the "... - percentual do total geral" ones, which sidrar's "allxp" never
#     requests), and each classification with every category's id, name and unit.
#     It reports only the TABLE's period.
#   * https://apisidra.ibge.gov.br/values/t/<table>/n1/all/v/<var>/p/all[/c<cl>/all]
#     returns every year (and category) at Brazil level in one call. A year IBGE
#     does not publish is still a ROW, with the value "..." (not available) or
#     ".." (not applicable); "-" is a true zero and "X" a suppressed value. A
#     year counts as covered when its value is neither "..." nor "..".
#   * sidrar::info_sidra() and sidrar's classific = "all" scrape an HTML page
#     (apisidra.ibge.gov.br/desctabapi.aspx) that is behind a Cloudflare bot
#     challenge from some networks; nothing here touches it.
#   * IBGE answers a query for a category that does not exist with HTTP 200 and
#     "..." values, so a resolver must compare ids with the metadata itself.
#
# Naming: everything is sidra_api_* / SIDRA_API_*; none starts with resolve_ or
# watch_, and none collides with the package's own sidra_download() and friends.

SIDRA_API_METADATA_URL <- "https://servicodados.ibge.gov.br/api/v3/agregados/%s/metadados"

# Brazil level, every year, one variable; with every category of the table's one
# classification when it has one. sprintf(url, table, variable[, classification
# id]) -- the classification id is the bare number ("81"), the path wants "c81".
SIDRA_API_COVERAGE_URL_CLASSIFIED <- "https://apisidra.ibge.gov.br/values/t/%s/n1/all/v/%s/p/all/c%s/all"
SIDRA_API_COVERAGE_URL_PLAIN <- "https://apisidra.ibge.gov.br/values/t/%s/n1/all/v/%s/p/all"

# What SIDRA prints instead of a number when there is none. Everything else ("-"
# zero, "X" suppressed, a number) is a published value.
SIDRA_API_UNPUBLISHED <- c("...", "..")

# available_geo word -> the IBGE territorial level that must exist in the table's
# metadata.
SIDRA_API_GEO_LEVELS <- c(country = "N1", region = "N2", state = "N3", municipality = "N6")

# The page of a table on sidra.ibge.gov.br, the `URL` the metadata reports. It
# answers HTTP 200 (the survey pages, /pesquisa/<survey>/tabelas, are bot-
# challenged), so it is what docs_url points at.
SIDRA_API_TABLE_PAGE <- "^https://sidra\\.ibge\\.gov\\.br/tabela/[0-9]+$"

SIDRA_API_USER_AGENT <- "datazoom.amazonia-manifest-resolver (+https://github.com/datazoompuc/datazoom.amazonia)"

# Pure: sorted integer years -> the manifest's available_time grammar
# ("2005", "1974-2025", "1974-1990, 1995-2025"), or NA when there are none.
sidra_api_years_to_available_time <- function(years) {
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

# A short, single-line excerpt of the text of a response body, or "" when it is
# empty or not text -- IBGE explains its 400s in one plain sentence, and a
# Cloudflare challenge is an HTML page whose text ("Just a moment...") is what
# matters, not its markup.
sidra_api_snippet <- function(content, n = 160) {
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
# loud failure, not a silent follow). Returns list(problem = character(0), text =
# <body>) or list(problem = "<reason>"). Never throws.
sidra_api_fetch <- function(url, timeout_s = 120) {
  h <- curl::new_handle(timeout = timeout_s, followlocation = FALSE, useragent = SIDRA_API_USER_AGENT)
  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(problem = sprintf("could not reach %s (%s)", url, conditionMessage(resp))))
  }
  if (resp$status_code != 200L) {
    snippet <- sidra_api_snippet(resp$content)
    return(list(problem = sprintf(
      "%s answered HTTP %d%s", url, resp$status_code,
      if (nzchar(snippet)) paste0(": ", snippet) else ""
    )))
  }
  text <- rawToChar(resp$content)
  Encoding(text) <- "UTF-8"
  list(problem = character(0), text = text)
}

# Pure: parses one table's /metadados JSON. Returns list(problem,
# classifications, category_info, levels, variables, variable_info, url);
# `problem` is character(0) when the description is usable. classifications is a
# named list (classification id "80" -> its category ids), EMPTY for a table
# without one; category_info has, per classification, a data frame of id, IBGE's
# name and unit; variables are the ids of the non-percentage variables, which is
# what sidrar's "allxp" returns, and variable_info their id, name and unit.
sidra_api_parse_metadata <- function(text, table) {
  fail <- function(msg) list(problem = msg)

  meta <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(meta) || is.null(meta$id) || is.null(meta$variaveis)) {
    snippet <- sidra_api_snippet(charToRaw(enc2utf8(text)), n = 80)
    return(fail(sprintf(
      "the response is not a SIDRA table description (no id/variaveis)%s",
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

  describe <- function(items) {
    data.frame(
      id = vapply(items, function(z) as.character(z$id), character(1)),
      nome = vapply(items, function(z) if (is.null(z$nome)) NA_character_ else as.character(z$nome), character(1)),
      unidade = vapply(items, function(z) if (is.null(z$unidade)) NA_character_ else as.character(z$unidade), character(1)),
      stringsAsFactors = FALSE
    )
  }
  category_info <- lapply(meta$classificacoes, function(cl) describe(cl$categorias))
  names(category_info) <- names(classifications)

  is_percent <- vapply(
    meta$variaveis,
    function(v) identical(v$unidade, "%") || grepl("percentual", if (is.null(v$nome)) "" else v$nome, ignore.case = TRUE),
    logical(1)
  )
  variable_info <- describe(meta$variaveis)[!is_percent, , drop = FALSE]

  list(
    problem = character(0),
    classifications = classifications,
    category_info = category_info,
    levels = as.character(unlist(meta$nivelTerritorial, use.names = FALSE)),
    variables = variable_info$id,
    variable_info = variable_info,
    url = if (is.character(meta$URL) && length(meta$URL) == 1) meta$URL else NA_character_
  )
}

# Pure: parses the coverage query. Returns list(problem, years) where years is a
# named list: category id ("(none)" for a table without a classification) ->
# sorted integer years with a published value (categories with none are absent).
# The first element of IBGE's array is a header row naming each column, which is
# how the layout is checked.
sidra_api_parse_coverage <- function(text, classified) {
  fail <- function(msg) list(problem = msg)

  d <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  needed <- c("V", "D3C", "D3N", if (classified) "D4C")
  if (!is.data.frame(d) || !all(needed %in% names(d))) {
    return(fail(sprintf("the response is not a SIDRA value table (a JSON array with %s)", paste(needed, collapse = ", "))))
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
  category <- if (classified) d$D4C else rep("(none)", nrow(d))
  published <- !is.na(d$V) & !(d$V %in% SIDRA_API_UNPUBLISHED)
  years <- lapply(split(as.integer(d$D3C[published]), category[published]), function(y) sort(unique(y)))
  list(problem = character(0), years = years)
}

# Checks ONE table: its metadata, then its coverage. Never throws. Returns
# list(problem, classifications, category_info, levels, variables, variable_info,
# url, years) with `problem` character(0) when both worked.
#
#   preferred_variables   the variable that decides whether a year is published,
#                         in order of preference: the first one the table declares
#   fall_back_to_first    use the table's first non-percentage variable when none
#                         of the preferred is declared (PPM) instead of failing
#                         (PAM, whose two preferred ids are known to be enough)
#   allow_no_classification  a table with no classification is fine (PPM's 94 and
#                         95): the coverage query then has no /c<n>/all part
#
# A table with more than one classification is always a problem: the coverage
# query handles one path.
sidra_api_check_table <- function(table, preferred_variables, fall_back_to_first = FALSE, allow_no_classification = FALSE) {
  meta_got <- sidra_api_fetch(sprintf(SIDRA_API_METADATA_URL, table))
  if (length(meta_got$problem) > 0) {
    return(list(problem = meta_got$problem))
  }
  meta <- sidra_api_parse_metadata(meta_got$text, table)
  if (length(meta$problem) > 0) {
    return(list(problem = sprintf("metadata of table %s: %s", table, meta$problem)))
  }
  n_classifications <- length(meta$classifications)
  if (n_classifications > 1) {
    return(list(problem = sprintf(
      "table %s has %d classifications (%s); the coverage query here handles at most one",
      table, n_classifications, paste0("c", names(meta$classifications), collapse = ", ")
    )))
  }
  if (n_classifications == 0 && !allow_no_classification) {
    return(list(problem = sprintf("table %s has no classification, and this resolver expects one", table)))
  }

  variable <- preferred_variables[preferred_variables %in% meta$variables][1]
  if (is.na(variable)) {
    if (!fall_back_to_first) {
      return(list(problem = sprintf(
        "table %s declares none of the variables %s", table, paste(preferred_variables, collapse = "/")
      )))
    }
    if (length(meta$variables) == 0) {
      return(list(problem = sprintf("table %s declares no variable to measure coverage with", table)))
    }
    variable <- meta$variables[1]
  }

  classified <- n_classifications == 1
  cov_url <- if (classified) {
    sprintf(SIDRA_API_COVERAGE_URL_CLASSIFIED, table, variable, names(meta$classifications))
  } else {
    sprintf(SIDRA_API_COVERAGE_URL_PLAIN, table, variable)
  }
  cov_got <- sidra_api_fetch(cov_url)
  if (length(cov_got$problem) > 0) {
    return(list(problem = cov_got$problem))
  }
  cov <- sidra_api_parse_coverage(cov_got$text, classified)
  if (length(cov$problem) > 0) {
    return(list(problem = sprintf("coverage of table %s: %s", table, cov$problem)))
  }

  list(
    problem = character(0),
    classifications = meta$classifications,
    category_info = meta$category_info,
    levels = meta$levels,
    variables = meta$variables,
    variable_info = meta$variable_info,
    url = meta$url,
    years = cov$years
  )
}

# Pure: what is wrong with the territorial levels a row's available_geo promises,
# given the levels the table's metadata has. character(0) when nothing, or when
# available_geo is NA (any level goes).
sidra_api_geo_problem <- function(available_geo, levels, table) {
  if (is.na(available_geo)) {
    return(character(0))
  }
  wanted <- trimws(strsplit(tolower(available_geo), ",")[[1]])
  unknown <- setdiff(wanted, names(SIDRA_API_GEO_LEVELS))
  if (length(unknown) > 0) {
    return(sprintf(
      "available_geo names '%s', which has no IBGE territorial level on record in SIDRA_API_GEO_LEVELS",
      paste(unknown, collapse = "', '")
    ))
  }
  missing_levels <- setdiff(SIDRA_API_GEO_LEVELS[wanted], levels)
  if (length(missing_levels) > 0) {
    return(sprintf(
      "available_geo promises %s, but table %s's metadata has no territorial level %s",
      available_geo, table, paste(missing_levels, collapse = ", ")
    ))
  }
  character(0)
}

# Pure: the table's own page, for docs_url, when the metadata reports one of the
# expected form; NA (leave the cell alone) otherwise.
sidra_api_docs_url <- function(tab) {
  if (!is.na(tab$url) && grepl(SIDRA_API_TABLE_PAGE, tab$url)) tab$url else NA_character_
}

# Pure: the years a row reads out of a checked table -- every category's union
# for a whole-table row (category "all", or NA for a table without
# classification), the one category's otherwise.
sidra_api_row_years <- function(tab, category) {
  if (is.na(category) || category == "all") {
    sort(unique(unlist(tab$years, use.names = FALSE)))
  } else {
    tab$years[[category]]
  }
}
