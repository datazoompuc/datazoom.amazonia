# actions/scrapers/resolve_imazon.R
#
# Imazon's SAD (Sistema de Alerta de Desmatamento), the monthly forest-loss
# monitoring of the Amazon, published on Imazon's own dashboard:
#   https://dashboards.imazongeo.org.br/sad/
# load_imazon() reads two files from it, one per dataset:
#   sad_deforestation  .../sad/api/sad/desmatamento/municipios.csv
#   sad_degradation    .../sad/api/sad/degradacao/municipios.csv
# CSV: ALERTA, MES, ANO, SENSOR, ESTADO, MUNICIPIO, AREAKM2 -- one row per month,
# municipality (name and state, no IBGE code) and satellite sensor, area in square
# kilometers, 2008 to the latest month. Deforestation: ~36,000 rows, 1.9 MB;
# degradation (fire, selective logging): ~7,200 rows, 0.4 MB, with whole years
# missing (nothing for 2025 on 2026-10-30). Behind them, the dashboard has a
# /periodo endpoint: {"ano_min":2008,"ano_max":2026,"mes_max":8}.
#
# WHERE THIS CAME FROM (2026-10-30). load_imazon() used to read "imazon.rds" from a
# contributor's personal Google Drive: a Datazoom conversion of a 2020 shapefile
# ("Fronteiras_Amazonia_2020", column Front_2020, municipalities scored 0 to 3).
# The first onboarding of this source (commit 8130f34) could only verify that file,
# because no official copy was found. Looking again, on the portal (imazongeo.org.br)
# and behind its dashboards: nothing Imazon publishes is that product.
#   - SAD (dashboards.imazongeo.org.br/sad): deforestation and degradation alerts, by
#     municipality among others, CSV/GeoJSON/shapefile. USED HERE.
#   - Ameaca e Pressao (/ap): deforestation on and around protected areas; its unit is
#     the Indigenous Land or conservation unit, not the municipality; no 0-3 score.
#   - PrevisIA (previsia.org.br): a raster, one GeoTIFF a year, of the predicted
#     probability of deforestation in the next 12 months (values 0 to 0.26 on a ~1 km
#     grid); no municipal table, and a continuous probability, not a class.
#   - FloreSer: secondary vegetation (regrowth) by municipality and year, built on
#     MapBiomas land cover: a different subject.
#   - Simex: selective logging monitoring (infographics, a dashboard, a map); no
#     municipal table found.
# So this is a DIFFERENT PRODUCT, not a new source for the same one: monthly forest
# loss by municipality instead of a 2020 snapshot score. See NEWS.md.
#
# WHY THESE TWO FILES AND NOT THE S3 BUCKET. The dashboard also publishes, on
# imazongeo3-web.s3.sa-east-1.amazonaws.com/dashboard/sad/, alert-level CSV and
# GeoJSON files (591,619 deforestation alerts, 25 MB). Their municipality field is
# blank for 99.4% of the area: it is the dashboard's server that assigns alerts to
# municipalities. The /api/sad CSVs are that assignment, already summed by month,
# municipality and sensor, and their yearly totals equal the S3 files' to the
# square kilometer in every year 2008-2026 (checked 2026-10-30) -- i.e. nothing is
# dropped. The cost: they are the
# dashboard's own data feed, not a documented API, so Imazon can change them without
# notice. That is what this resolver watches.
#
# WHAT IS CHECKED, EVERY RUN, for each dataset the manifest has (resolve_imazon() never
# trusts a file because it answered):
#   1. /periodo answers JSON with ano_min <= ano_max and a month 1-12;
#   2. the CSV answers 200 and is text/csv, without a redirect;
#   3. its header has the columns load_imazon() reads (ANO, MES, ESTADO, MUNICIPIO,
#      AREAKM2, plus ALERTA, SENSOR), and it has rows (>= 1,000);
#   4. its values are what the loader assumes: ALERTA is this dataset's own word, month
#      1-12, state one of the nine Legal Amazon ones, a municipality name, area a
#      number >= 0, no NA, no row repeated; the yearly totals are below 100,000 km2
#      (a change of unit to hectares would be 100 times that);
#   5. its latest year is the dashboard's ano_max, or the year before (a feed that has
#      stopped for more than a year is stale), and not after mes_max.
# It emits `available_time` from the years the file really has -- the manifest's was
# typed by hand, and the degradation file has gaps ("2008-2024, 2026"). `url` is
# echoed; nothing else is owned. A dataset that fails is a partial failure: the other
# one is still refreshed.
#
# NOT checked here: that the municipality names match the IBGE table the loader joins
# them with (that needs the package; R/imazon.R warns when some do not).
#
# Needs curl and jsonlite (CI-only, like the other resolvers).

IMAZON_API <- "https://dashboards.imazongeo.org.br/sad/api/sad"
IMAZON_PERIOD_URL <- paste0(IMAZON_API, "/periodo")

# dataset -> the word its ALERTA column carries. A copy of what R/imazon.R reads:
# tests/testthat/test-imazon-resolver.R fails if they differ.
IMAZON_DATASETS <- list(
  sad_deforestation = "desmatamento",
  sad_degradation = "degradacao"
)
IMAZON_COLUMNS <- c("ALERTA", "MES", "ANO", "SENSOR", "ESTADO", "MUNICIPIO", "AREAKM2")
IMAZON_STATES <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
IMAZON_MIN_ROWS <- 1000L
IMAZON_MAX_YEAR_KM2 <- 100000

# 2008, 2009, 2010, 2012 -> "2008-2010, 2012": the grammar parse_years() reads.
imazon_years_to_available_time <- function(years) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    return(NA_character_)
  }
  ends <- c(which(diff(years) != 1L), length(years))
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  paste(mapply(function(s, e) if (s == e) as.character(years[s]) else paste(years[s], years[e], sep = "-"), starts, ends), collapse = ", ")
}

# /periodo's JSON -> list(problem, ano_min, ano_max, mes_max).
imazon_parse_period <- function(text) {
  p <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  fail <- function(why) list(problem = why, ano_min = NA_integer_, ano_max = NA_integer_, mes_max = NA_integer_)
  if (!is.list(p) || !all(c("ano_min", "ano_max", "mes_max") %in% names(p))) {
    return(fail("it is not JSON with ano_min, ano_max and mes_max"))
  }
  v <- suppressWarnings(as.integer(unlist(p[c("ano_min", "ano_max", "mes_max")])))
  if (anyNA(v) || v[1] > v[2] || !v[3] %in% 1:12) {
    return(fail(paste0("its values are not a period: ", paste(names(p), unlist(p), sep = "=", collapse = ", "))))
  }
  list(problem = character(0), ano_min = v[1], ano_max = v[2], mes_max = v[3])
}

# What is wrong with a dataset's CSV (text), as sentences (character(0): nothing),
# and the years it has. Pure: no network.
imazon_check_csv <- function(text, alert, period) {
  x <- tryCatch(utils::read.csv(text = text, stringsAsFactors = FALSE, na.strings = c("NA", "")), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 2) {
    return(list(problem = "it is not a CSV", years = integer(0), rows = 0L, last_month = NA_integer_))
  }
  missing <- setdiff(IMAZON_COLUMNS, names(x))
  if (length(missing) > 0) {
    return(list(problem = sprintf("columns that load_imazon() reads are missing: %s", paste(missing, collapse = ", ")), years = integer(0), rows = nrow(x), last_month = NA_integer_))
  }

  problems <- character(0)
  if (nrow(x) < IMAZON_MIN_ROWS) {
    problems <- c(problems, sprintf("it has %d rows (fewer than %d)", nrow(x), IMAZON_MIN_ROWS))
  }
  area <- suppressWarnings(as.numeric(x$AREAKM2))
  month <- suppressWarnings(as.integer(x$MES))
  year <- suppressWarnings(as.integer(x$ANO))
  if (anyNA(area) || any(area < 0)) problems <- c(problems, "AREAKM2 has NA, negative or non-numeric values")
  if (anyNA(month) || !all(month %in% 1:12)) problems <- c(problems, "MES has values that are not months 1-12")
  if (anyNA(year)) problems <- c(problems, "ANO has non-numeric values")
  if (!all(x$ALERTA %in% alert)) problems <- c(problems, sprintf("ALERTA is not always '%s' (found: %s)", alert, paste(utils::head(setdiff(unique(x$ALERTA), alert), 3), collapse = ", ")))
  if (anyNA(x$MUNICIPIO) || !all(nzchar(trimws(x$MUNICIPIO)))) problems <- c(problems, "MUNICIPIO has blank names")
  odd_states <- setdiff(unique(x$ESTADO), IMAZON_STATES)
  if (length(odd_states) > 0) problems <- c(problems, sprintf("ESTADO has values that are not Legal Amazon states: %s", paste(utils::head(odd_states, 5), collapse = ", ")))
  if (anyDuplicated(x[c("ALERTA", "MES", "ANO", "SENSOR", "ESTADO", "MUNICIPIO")]) > 0) problems <- c(problems, "some month/municipality/sensor rows are repeated")

  if (!anyNA(year) && !anyNA(area)) {
    yearly <- tapply(area, year, sum)
    if (any(yearly > IMAZON_MAX_YEAR_KM2)) {
      problems <- c(problems, sprintf("a year's total is %s km2 (more than %s: a different unit?)", format(round(max(yearly)), big.mark = ","), format(IMAZON_MAX_YEAR_KM2, big.mark = ",")))
    }
    if (!is.na(period$ano_max)) {
      if (max(year) > period$ano_max || max(year) < period$ano_max - 1L) {
        problems <- c(problems, sprintf("its latest year is %d, the dashboard's period ends in %d", max(year), period$ano_max))
      }
      if (max(year) == period$ano_max && !anyNA(month) && max(month[year == max(year)]) > period$mes_max) {
        problems <- c(problems, sprintf("it has %d/%d, after the dashboard's last month (%d/%d)", max(year), max(month[year == max(year)]), period$ano_max, period$mes_max))
      }
    }
  }

  list(
    problem = problems, years = sort(unique(year[!is.na(year)])), rows = nrow(x),
    last_month = if (anyNA(year) || anyNA(month) || length(year) == 0L) NA_integer_ else max(month[year == max(year)])
  )
}

resolve_imazon <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_imazon(): no manifest rows tagged resolver == 'imazon' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_imazon() needs the 'curl' package (CI-only; not a package Import).")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("resolve_imazon() needs the 'jsonlite' package (CI-only; not a package Import).")
  }

  # A redirect is not followed: Imazon moving these files would answer with one.
  fetch <- function(url) {
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 120, connecttimeout = 20, followlocation = FALSE)),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      return(list(error = paste0("no HTTP response (", conditionMessage(resp), ")")))
    }
    headers <- tryCatch(curl::parse_headers_list(resp$headers), error = function(e) list())
    type <- headers[["content-type"]]
    list(status = resp$status_code, type = if (is.null(type)) "" else type, body = resp$content)
  }
  text_of <- function(raw) {
    txt <- rawToChar(raw[raw != as.raw(0)])
    Encoding(txt) <- "UTF-8"
    txt
  }

  resp <- fetch(IMAZON_PERIOD_URL)
  if (!is.null(resp$error) || resp$status != 200L) {
    stop("resolve_imazon(): ", IMAZON_PERIOD_URL, " -- ", if (!is.null(resp$error)) resp$error else paste0("HTTP ", resp$status), ". The dashboard's data feed may have moved.")
  }
  period <- imazon_parse_period(text_of(resp$body))
  if (length(period$problem) > 0) {
    stop("resolve_imazon(): ", IMAZON_PERIOD_URL, " answered something unexpected: ", period$problem, ".")
  }

  results <- list()
  failures <- character(0)
  for (dataset in unique(rows$dataset)) {
    fail <- function(why) failures <<- c(failures, sprintf("imazon/%s: %s", dataset, why))
    alert <- IMAZON_DATASETS[[dataset]]
    if (is.null(alert)) {
      fail(sprintf("resolve_imazon() only knows %s", paste(names(IMAZON_DATASETS), collapse = ", ")))
      next
    }
    urls <- unique(rows$url[rows$dataset == dataset])
    if (length(urls) != 1L || is.na(urls)) {
      fail("its manifest rows do not agree on one url")
      next
    }

    resp <- fetch(urls)
    if (!is.null(resp$error)) {
      fail(paste0(urls, " -- ", resp$error))
      next
    }
    if (resp$status != 200L) {
      fail(sprintf("%s -- HTTP %s (a redirect or an error: the file may have moved)", urls, resp$status))
      next
    }
    if (!grepl("^text/csv", resp$type, ignore.case = TRUE)) {
      fail(sprintf("%s -- content-type '%s', not text/csv", urls, resp$type))
      next
    }
    check <- imazon_check_csv(text_of(resp$body), alert, period)
    if (length(check$problem) > 0) {
      fail(sprintf("%s no longer has the shape load_imazon() relies on -- %s", urls, paste(check$problem, collapse = "; ")))
      next
    }

    message(sprintf(
      "resolve_imazon(): %s ok -- %d rows, %s, latest month %d/%d",
      dataset, check$rows, imazon_years_to_available_time(check$years), max(check$years), check$last_month
    ))
    results[[dataset]] <- imazon_years_to_available_time(check$years)
  }

  if (length(results) == 0) {
    stop("resolve_imazon(): every dataset failed -- ", paste(failures, collapse = " | "))
  }

  # One output row per owned row of a dataset that checked out, echoing its own keys.
  ok <- rows$dataset %in% names(results)
  out <- tibble::tibble(
    survey = rows$survey[ok],
    dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok],
    year = rows$year[ok],
    url = rows$url[ok],
    available_time = unname(unlist(results[rows$dataset[ok]]))
  )
  if (length(failures) > 0) {
    attr(out, "partial_failures") <- failures
  }
  out
}
