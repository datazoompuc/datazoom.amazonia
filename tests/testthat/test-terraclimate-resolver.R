# tests/testthat/test-terraclimate-resolver.R
#
# Offline test for actions/scrapers/resolve_terraclimate.R (added 2026-10-19;
# see that file's header for what was measured live). TerraClimate's 14
# manifest rows share one THREDDS NCSS endpoint, and the thing that goes stale
# is the file name (it embeds the series' start year -- 1958 under v1.0, 1950
# under v1.1) and the coverage in available_time. The server answers HTTP 200
# with an EMPTY BODY for any name it does not have, so every check here is
# about what the response CONTAINS, not its status.
#
# curl::curl_fetch_memory is mocked (no network). The fake server mirrors what
# was measured live on 2026-10-19: dataset.xml is a <gridDataset> with a
# <TimeSpan> and a time axis of 912 monthly steps (1950-01 .. 2025-12); a
# subset request returns a ~5.9 KB classic netCDF file ("CDF\001").

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("curl")

source(file.path(actions_dir, "scrapers", "resolve_terraclimate.R"), local = TRUE)

# ---- a fake THREDDS server --------------------------------------------------

BASE <- "http://thredds.northwestknowledge.net:8080/thredds/ncss"

months_between <- function(begin, end) {
  (as.integer(substr(end, 1, 4)) - as.integer(substr(begin, 1, 4))) * 12L +
    (as.integer(substr(end, 6, 7)) - as.integer(substr(begin, 6, 7))) + 1L
}

# The parts of THREDDS' dataset.xml the resolver reads, in the real layout.
meta_xml <- function(code, begin = "1950-01-01T00:00:00Z", end = "2025-12-01T00:00:00Z",
                     steps = months_between(begin, end), grid = code) {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<gridDataset location="/thredds/ncss/x.nc" path="path">\n',
    '  <axis name="time" shape="', steps, '" type="double" axisType="Time">\n',
    '    <attribute name="units" value="days since 1900-01-01 00:00:00" />\n',
    "  </axis>\n",
    '  <gridSet name="time lat lon">\n',
    '    <grid name="', grid, '" desc="air_temperature" shape="time lat lon" type="short">\n',
    "    </grid>\n",
    "  </gridSet>\n",
    "  <TimeSpan>\n",
    "    <begin>", begin, "</begin>\n",
    "    <end>", end, "</end>\n",
    "  </TimeSpan>\n",
    "</gridDataset>\n"
  )
}

resp <- function(status, body = raw(0), type = NULL, extra = character(0)) {
  # A real response's header block is its status line, one line per header,
  # and exactly ONE blank line: curl::parse_headers_list() reads the LAST
  # block, so a stray second blank line would hide every header.
  header_lines <- c(
    paste0("HTTP/1.1 ", status, " X"),
    if (!is.null(type)) paste0("content-type: ", type),
    extra
  )
  list(
    status_code = status,
    headers = charToRaw(paste0(paste0(header_lines, "\r\n", collapse = ""), "\r\n")),
    content = if (is.character(body)) charToRaw(body) else body
  )
}
xml_response <- function(code, ...) resp(200L, meta_xml(code, ...), "application/xml;charset=UTF-8")
netcdf_response <- function() resp(200L, c(charToRaw("CDF"), as.raw(1), raw(5000)), "application/x-netcdf")
# What this server really answers for a name it does not have: 200, no body,
# no Content-Type.
empty_200 <- function() resp(200L, raw(0), NULL)

# TERRACLIMATE_CODES is the resolver's own table; code -> dataset name.
code_of_url <- function(url) {
  sub("^.*/agg_terraclimate_([A-Za-z]+)_1950_CurrentYear_GLOBE\\.nc.*$", "\\1", url)
}

# `meta` and `probe` are function(code, url) returning a response (or throwing
# like curl does); `seen` is an environment collecting the URLs requested.
fake_server <- function(meta = function(code, url) xml_response(code),
                        probe = function(code, url) netcdf_response(),
                        seen = NULL) {
  function(url, handle = NULL, ...) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    code <- code_of_url(url)
    if (grepl("/dataset\\.xml$", url)) {
      return(meta(code, url))
    }
    if (grepl("_GLOBE\\.nc\\?", url)) {
      return(probe(code, url))
    }
    stop("the fake server has no such URL: ", url)
  }
}
with_server <- function(server, expr) {
  testthat::with_mocked_bindings(expr, curl_fetch_memory = server, .package = "curl")
}
run <- function(server, rows = manifest_rows) {
  with_server(server, suppressMessages(resolve_terraclimate(rows)))
}

# The real committed rows (14 of them), not a hand-made copy.
manifest_rows <- link_table()[link_table()$survey == "terraclimate", ]

# ---- the healthy path -------------------------------------------------------

test_that("a healthy server resolves all 14 variables to the complete years it serves", {
  seen <- new.env()
  out <- run(fake_server(seen = seen))

  expect_equal(nrow(out), 14)
  expect_setequal(out$dataset, names(TERRACLIMATE_CODES))
  expect_true(all(out$survey == "terraclimate"))
  expect_true(all(out$resolver == "terraclimate"))
  expect_true(all(out$available_time == "1950-2025"))
  # the url is echoed back unchanged: the resolver verifies it, it doesn't choose it
  expect_equal(out$url, manifest_rows$url[match(out$dataset, manifest_rows$dataset)])
  # unkeyed rows stay unkeyed
  expect_true(all(is.na(out$geo_level)) && all(is.na(out$year)))
  # columns it doesn't own are absent ("leave the cell alone"), not blanked
  expect_false(any(c("docs_url", "available_geo", "version", "archive_file") %in% names(out)))
  expect_null(attr(out, "partial_failures"))

  # exactly two small requests per variable, metadata first
  expect_length(seen$urls, 28)
  expect_equal(sum(grepl("/dataset\\.xml$", seen$urls)), 14)
})

test_that("the probe asks for a real one-month subset at December of the last complete year", {
  seen <- new.env()
  run(fake_server(seen = seen))
  probes <- grep("_GLOBE\\.nc\\?", seen$urls, value = TRUE)

  expect_length(probes, 14)
  expect_true(all(grepl("&time_start=2025-12-01T00%3A00%3A00Z&time_end=2025-12-01T00%3A00%3A00Z&", probes, fixed = TRUE)))
  expect_true(all(grepl("&accept=netcdf$", probes)))
  # the variable code travels in the file name AND in &var=, case included
  pdsi <- grep("_PDSI_", probes, value = TRUE)
  expect_length(pdsi, 1)
  expect_match(pdsi, "&var=PDSI&", fixed = TRUE)
})

test_that("an annual update that adds a year moves available_time", {
  out <- run(fake_server(meta = function(code, url) xml_response(code, end = "2026-12-01T00:00:00Z")))
  expect_true(all(out$available_time == "1950-2026"))
})

test_that("a final year that is only partly published is not advertised", {
  # the server would clamp the request silently and hand back 3 months of 2026
  seen <- new.env()
  out <- run(fake_server(meta = function(code, url) xml_response(code, end = "2026-03-01T00:00:00Z"), seen = seen))
  expect_true(all(out$available_time == "1950-2025"))
  expect_true(all(grepl("time_end=2025-12-01", grep("_GLOBE\\.nc\\?", seen$urls, value = TRUE), fixed = TRUE)))
})

test_that("variables may disagree: each row gets its own coverage", {
  out <- run(fake_server(meta = function(code, url) {
    xml_response(code, end = if (code == "ppt") "2026-12-01T00:00:00Z" else "2025-12-01T00:00:00Z")
  }))
  expect_equal(out$available_time[out$dataset == "precipitation"], "1950-2026")
  expect_true(all(out$available_time[out$dataset != "precipitation"] == "1950-2025"))
})

# ---- a variable that fails is partial; all failing is systemic --------------

test_that("an empty HTTP 200 for one variable's metadata is a partial failure, the other 13 still refresh", {
  seen <- new.env()
  out <- run(fake_server(
    meta = function(code, url) if (code == "ws") empty_200() else xml_response(code),
    seen = seen
  ))

  expect_equal(nrow(out), 13)
  expect_false("wind_speed" %in% out$dataset)
  pf <- attr(out, "partial_failures")
  expect_length(pf, 1)
  expect_match(pf, "^terraclimate/wind_speed: ")
  expect_match(pf, "empty dataset description")
  # no point probing a dataset that does not exist
  expect_false(any(grepl("_ws_.*\\?", seen$urls)))
})

test_that("metadata that looks fine but a subset request that returns an empty 200 is still a failure", {
  out <- run(fake_server(probe = function(code, url) if (code == "q") empty_200() else netcdf_response()))
  expect_equal(nrow(out), 13)
  pf <- attr(out, "partial_failures")
  expect_match(pf, "^terraclimate/runoff: a real subset request for 2025-12 failed")
  expect_match(pf, "0 byte")
})

test_that("a 400 from the subset service carries the server's own explanation", {
  msg <- "Requested time range 2030-01-01T00:00:00Z - 2030-12-01T00:00:00Z does not intersect actual time range 1950-01-01T00:00:00Z - 2025-12-01T00:00:00Z"
  out <- run(fake_server(probe = function(code, url) if (code == "tmax") resp(400L, msg) else netcdf_response()))
  expect_match(attr(out, "partial_failures"), "HTTP 400 instead of 200: Requested time range 2030", fixed = TRUE)
})

test_that("an HTML page served with HTTP 200 is not mistaken for netCDF", {
  html <- paste0("<html><body>", strrep("Service unavailable. ", 80), "</body></html>")
  out <- run(fake_server(probe = function(code, url) if (code == "vap") resp(200L, html, "text/html") else netcdf_response()))
  pf <- attr(out, "partial_failures")
  expect_match(pf, "does not start with a netCDF signature")
  expect_match(pf, "text/html")
  # the text of the page, not its markup
  expect_match(pf, "it starts with: Service unavailable. Service unavailable.", fixed = TRUE)
  expect_false(grepl("<html>", pf, fixed = TRUE))
})

test_that("a Tomcat 404 page is reported by its text, not its markup", {
  # byte-for-byte shape of what the real server answered for a path it does not
  # have (captured live 2026-10-19, 627 bytes)
  page <- paste0(
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">\n  <head>\n',
    '    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\n',
    "    <title>TDS - Error report</title>\n",
    '    <link rel="stylesheet" href="/thredds/tds.css" type="text/css"/>\n  </head>\n  <body>\n',
    "    <h1>HTTP Status 404 - Not Found</h1>\n    <HR size=\"1\" noshade=\"noshade\">\n",
    "    <p><b>Status</b> 404 - Not Found</p>\n    <HR size=\"1\" noshade=\"noshade\">\n",
    "    <h3>THREDDS Data Server Version 4.6\n      -- <a href='http://x'>Documentation</a></h3>\n  </body>\n</html>\n"
  )
  out <- run(fake_server(meta = function(code, url) if (code == "ws") resp(404L, page, "text/html;charset=UTF-8") else xml_response(code)))
  pf <- attr(out, "partial_failures")
  expect_match(pf, "answered HTTP 404: TDS - Error report HTTP Status 404 - Not Found Status 404 - Not Found", fixed = TRUE)
  expect_false(grepl("<", sub("^[^:]*: http[^ ]* ", "", pf)))
})

test_that("a redirect is reported as a moved host, not followed", {
  out <- run(fake_server(meta = function(code, url) {
    if (code == "swe") resp(301L, "", "text/html", "Location: https://thredds.nkn.uidaho.edu/thredds/ncss/x") else xml_response(code)
  }))
  pf <- attr(out, "partial_failures")
  expect_match(pf, "^terraclimate/snow_water_equivalent: ")
  expect_match(pf, "redirects \\(HTTP 301\\) to https://thredds.nkn.uidaho.edu/thredds/ncss/x")
})

test_that("a transport failure names the URL it could not reach", {
  out <- run(fake_server(meta = function(code, url) if (code == "soil") stop("Timeout was reached") else xml_response(code)))
  pf <- attr(out, "partial_failures")
  expect_match(pf, "^terraclimate/soil_moisture: could not reach http://thredds.northwestknowledge.net:8080/thredds/ncss/agg_terraclimate_soil_")
  expect_match(pf, "Timeout was reached", fixed = TRUE)
})

test_that("every variable failing stops the resolver, pointing at the file-name pattern", {
  # this is what the 1958 -> 1950 rename looked like: the old name answers 200 with nothing
  expect_error(
    run(fake_server(meta = function(code, url) empty_200())),
    "all 14 variables failed verification.*file-name pattern.*agg_terraclimate_<code>_1950_CurrentYear_GLOBE.nc"
  )
  err <- tryCatch(run(fake_server(meta = function(code, url) empty_200())), error = function(e) conditionMessage(e))
  # every dataset is listed with its own reason
  for (ds in names(TERRACLIMATE_CODES)) expect_match(err, paste0("- ", ds, ": "), fixed = TRUE)
})

# ---- pure functions ---------------------------------------------------------

test_that("terraclimate_parse_metadata() accepts the real layout and reads the span", {
  p <- terraclimate_parse_metadata(meta_xml("tmax"), "tmax")
  expect_identical(p$problem, character(0))
  expect_equal(p$begin, "1950-01-01T00:00:00Z")
  expect_equal(p$end, "2025-12-01T00:00:00Z")
  expect_equal(p$steps, 912L)
})

test_that("terraclimate_parse_metadata() rejects what is not the expected dataset", {
  problem <- function(xml, code = "tmax") terraclimate_parse_metadata(xml, code)$problem

  expect_match(problem(""), "empty dataset description")
  expect_match(problem("   \n"), "empty dataset description")
  expect_match(problem("<html><body>Not found</body></html>"), "not a THREDDS <gridDataset>")
  # the code is case-sensitive: the file for PDSI does not declare "pdsi"
  expect_match(problem(meta_xml("PDSI"), "pdsi"), "does not declare a grid variable named 'pdsi'")
  expect_match(problem(meta_xml("tmin"), "tmax"), "named 'tmax'")
  expect_match(problem(sub("(?s)<TimeSpan>.*</TimeSpan>", "", meta_xml("tmax"), perl = TRUE)), "no <TimeSpan>")
  expect_match(problem(meta_xml("tmax", begin = "soon", end = "later", steps = 1)), "cannot read the <TimeSpan>")
  expect_match(problem(sub('<axis name="time" shape="912"', '<axis name="x"', meta_xml("tmax"))), "no time axis")
  # a sparse / yearly / climatology axis is not the monthly series load_climate() assumes
  expect_match(problem(meta_xml("tmax", steps = 12)), "has 12 step\\(s\\), but a monthly series from 1950-01-01T00:00:00Z to 2025-12-01T00:00:00Z has 912")
})

test_that("terraclimate_complete_years() keeps whole calendar years only", {
  expect_equal(terraclimate_complete_years("1950-01-01T00:00:00Z", "2025-12-01T00:00:00Z"), c(first = 1950L, last = 2025L))
  expect_equal(terraclimate_complete_years("1950-01-01T00:00:00Z", "2026-03-01T00:00:00Z"), c(first = 1950L, last = 2025L))
  expect_equal(terraclimate_complete_years("1950-02-01T00:00:00Z", "2025-12-01T00:00:00Z"), c(first = 1951L, last = 2025L))
  expect_equal(terraclimate_complete_years("2025-01-01T00:00:00Z", "2025-12-01T00:00:00Z"), c(first = 2025L, last = 2025L))
  expect_identical(terraclimate_complete_years("2025-03-01T00:00:00Z", "2025-09-01T00:00:00Z"), integer(0))
})

test_that("a span with no complete calendar year is a failure for that variable", {
  expect_error(
    run(fake_server(meta = function(code, url) xml_response(code, begin = "2025-03-01T00:00:00Z", end = "2025-09-01T00:00:00Z"))),
    "contains no complete calendar year"
  )
})

test_that("terraclimate_netcdf_problem() accepts classic netCDF and netCDF-4, nothing else", {
  classic1 <- c(charToRaw("CDF"), as.raw(1), raw(2000))
  classic2 <- c(charToRaw("CDF"), as.raw(2), raw(2000))
  hdf5 <- c(as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a)), raw(2000))
  expect_identical(terraclimate_netcdf_problem(200L, "application/x-netcdf", classic1), character(0))
  expect_identical(terraclimate_netcdf_problem(200L, "application/x-netcdf", classic2), character(0))
  expect_identical(terraclimate_netcdf_problem(200L, "application/x-netcdf4", hdf5), character(0))
  # the Content-Type is advisory: the signature decides
  expect_identical(terraclimate_netcdf_problem(200L, NA_character_, classic1), character(0))

  expect_match(terraclimate_netcdf_problem(200L, "application/x-netcdf", c(charToRaw("CDF"), as.raw(3), raw(2000))), "netCDF signature")
  expect_match(terraclimate_netcdf_problem(200L, "application/zip", c(charToRaw("PK"), raw(2000))), "netCDF signature")
  expect_match(terraclimate_netcdf_problem(200L, NA_character_, raw(0)), "0 byte")
  expect_match(terraclimate_netcdf_problem(200L, "application/x-netcdf", classic1[1:500]), "500 byte")
  expect_match(terraclimate_netcdf_problem(500L, "text/html", raw(0)), "HTTP 500 instead of 200")
  expect_length(terraclimate_netcdf_problem(NA_integer_, NA_character_, raw(0)), 1)
})

test_that("terraclimate_snippet() returns single-line printable text only", {
  expect_equal(terraclimate_snippet(charToRaw("a\n  b\tc")), "a b c")
  expect_equal(terraclimate_snippet(raw(0)), "")
  expect_equal(terraclimate_snippet(c(charToRaw("CDF"), as.raw(1), raw(20))), "") # binary
  expect_equal(terraclimate_snippet(charToRaw(strrep("x", 500)), n = 160), paste0(strrep("x", 160), "..."))
  # markup goes, including style/script bodies; the text stays
  expect_equal(
    terraclimate_snippet(charToRaw("<html><style>p { color: red }</style><h1>Not   Found</h1></html>")),
    "Not Found"
  )
})

# ---- guard clauses fire before any request ----------------------------------

test_that("the resolver refuses rows it cannot verify, before touching the network", {
  never <- fake_server(meta = function(code, url) stop("the network must not be touched"))
  one <- manifest_rows[1, ]

  expect_error(with_server(never, resolve_terraclimate(NULL)), "no manifest rows")
  expect_error(with_server(never, resolve_terraclimate(manifest_rows[0, ])), "no manifest rows")
  expect_error(with_server(never, resolve_terraclimate(dplyr::mutate(one, dataset = "snowfall"))), "no variable code on record for dataset\\(s\\) snowfall")
  expect_error(with_server(never, resolve_terraclimate(dplyr::mutate(one, year = "2020"))), "unkeyed")
  expect_error(with_server(never, resolve_terraclimate(dplyr::mutate(one, geo_level = "state"))), "unkeyed")
  expect_error(with_server(never, resolve_terraclimate(dplyr::mutate(one, url = NA_character_))), "existing url")
})

test_that("a trailing slash on the manifest url does not change the requests", {
  seen <- new.env()
  rows <- dplyr::mutate(manifest_rows, url = paste0(url, "/"))
  run(fake_server(seen = seen), rows = rows)
  expect_false(any(grepl("ncss//", seen$urls, fixed = TRUE)))
})

# ---- the resolver's copies of the package's facts agree with the package ----

# Finds `name <- <rhs>` anywhere inside an expression and returns the rhs.
find_assignment <- function(expr, name) {
  if (!is.call(expr)) {
    return(NULL)
  }
  if (identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name(name))) {
    return(expr[[3]])
  }
  for (i in seq_along(expr)) {
    # an empty argument (x[, 1]) is a missing-symbol element: skip it
    part <- tryCatch(expr[[i]], error = function(e) NULL)
    found <- if (is.call(part)) find_assignment(part, name) else NULL
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

test_that("TERRACLIMATE_CODES is exactly R/climate.R's dataset_names", {
  parsed <- parse(file.path(pkg_root, "R", "climate.R"), keep.source = FALSE)
  rhs <- NULL
  for (e in parsed) {
    if (is.null(rhs)) rhs <- find_assignment(e, "dataset_names")
  }
  expect_false(is.null(rhs))
  expect_identical(eval(rhs), TERRACLIMATE_CODES)
})

test_that("the file-name pattern and the query keys match what R/download.R builds", {
  lines <- readLines(file.path(pkg_root, "R", "download.R"), warn = FALSE)
  # the request is built right after the `filename <- paste0(` line, whose first
  # argument is the file-name prefix on a line of its own (anchoring on the
  # `if (source == "terraclimate")` line would also match perform_download()'s)
  start <- grep('^\\s*"agg_terraclimate_",?\\s*$', lines)[1] - 1
  block <- paste(lines[start:(start + 40)], collapse = "\n")

  expect_match(block, paste0('"', TERRACLIMATE_FILE_PREFIX, '"'), fixed = TRUE)
  expect_match(block, paste0('"', TERRACLIMATE_FILE_SUFFIX, '"'), fixed = TRUE)

  keys <- function(x) unlist(regmatches(x, gregexpr("(?<=[?&])[A-Za-z_]+(?==)", x, perl = TRUE)))
  # literal pieces of the request in download.R: "&var=", "&south=", ..., "?" -- the parameter names, in order
  pkg_keys <- keys(paste(regmatches(block, gregexpr('"[?&][^"]*"', block))[[1]], collapse = ""))
  probe_keys <- keys(terraclimate_probe_url(BASE, "tmax", 2025))
  expect_identical(probe_keys, pkg_keys)
  expect_gte(length(probe_keys), 11)
})

test_that("terraclimate_filename() builds the same name for every code the package knows", {
  expect_equal(terraclimate_filename("tmax"), "agg_terraclimate_tmax_1950_CurrentYear_GLOBE.nc")
  expect_equal(terraclimate_filename("PDSI"), "agg_terraclimate_PDSI_1950_CurrentYear_GLOBE.nc")
})

# ---- who the server's logs see ----------------------------------------------

test_that("both the resolver and the package's own download say who they are", {
  # Measured against a localhost listener (2026-10-19): the resolver sent its
  # own string, but perform_download()'s external curl sent a bare "curl/8.x".
  expect_match(TERRACLIMATE_USER_AGENT, "^datazoom\\.amazonia-manifest-resolver \\(\\+https://github\\.com/datazoompuc/datazoom\\.amazonia\\)$")
  expect_match(DATAZOOM_USER_AGENT, "^datazoom\\.amazonia R package \\(\\+https://github\\.com/datazoompuc/datazoom\\.amazonia\\)$")
  # different on purpose: the weekly check is distinguishable from users' downloads
  expect_false(identical(DATAZOOM_USER_AGENT, TERRACLIMATE_USER_AGENT))
})

test_that("perform_download() hands curl the TerraClimate User-Agent as ONE quoted argument, and puts the option back", {
  seen <- new.env()
  fake_download_file <- function(url, destfile, method = "auto", quiet = FALSE, ...) {
    seen$extra <- getOption("download.file.extra")
    seen$method <- method
    0L
  }
  run_download <- function(source) {
    testthat::with_mocked_bindings(
      perform_download(
        path = "http://example.invalid/x.nc", temp = tempfile(),
        download_method = "curl", source = source, quiet = TRUE
      ),
      download.file = fake_download_file, .package = "utils"
    )
  }

  # A previous value is set on purpose: perform_download()'s local guard
  # (options()[c(...)]) cannot put back an option that was UNSET to begin with
  # (it returns an NA-named NULL), which is harmless in practice because
  # external_download()'s own guard restores it -- but it is not what this test
  # is about.
  withr::local_options(download.file.extra = "--previous")
  run_download("terraclimate")
  expect_identical(seen$method, "curl")
  # shQuote() matters: unquoted, curl got only "datazoom.amazonia" and then tried
  # to resolve "R" and "package" as hostnames (measured)
  expect_identical(seen$extra, paste("--user-agent", shQuote(DATAZOOM_USER_AGENT)))
  expect_identical(getOption("download.file.extra"), "--previous")

  # no other curl-method source is touched
  run_download("baci")
  expect_identical(seen$extra, "--ssl-no-revoke")
  run_download("mapbiomas")
  expect_identical(seen$extra, "--previous")
  expect_identical(getOption("download.file.extra"), "--previous")
})

# ---- the committed manifest agrees with the resolver ------------------------

test_that("the 14 committed rows are what the resolver owns and expects", {
  expect_equal(nrow(manifest_rows), 14)
  expect_setequal(manifest_rows$dataset, names(TERRACLIMATE_CODES))
  expect_true(all(manifest_rows$resolver == "terraclimate"))
  # one shared base endpoint: the host the Climatology Lab documents (see the resolver's header)
  expect_equal(unique(manifest_rows$url), BASE)
  expect_equal(unique(manifest_rows$docs_url), "https://www.climatologylab.org/terraclimate.html")
  expect_true(all(grepl("^[0-9]{4}-[0-9]{4}$", manifest_rows$available_time)))
  expect_equal(unique(manifest_rows$available_geo), "municipality")
})

test_that("resolve_terraclimate.R registers exactly one resolver in build_manifest.R's registry", {
  # build_manifest.R treats EVERY top-level binding named resolve_* as a
  # resolver (and watch_* as a watcher), so this file's helpers must not use
  # those prefixes. (ls() inside test_that() would list the test block's own
  # frame, so look at the environment the file was source()'d into.)
  src_env <- environment(resolve_terraclimate)
  expect_identical(grep("^resolve_", ls(src_env), value = TRUE), "resolve_terraclimate")
  expect_identical(grep("^watch_", ls(src_env), value = TRUE), character(0))
})

# ---- what a user sees: the manifest's coverage drives load_climate()'s warning ----

test_that("check_params() accepts every year TerraClimate v1.1 serves and still warns outside them", {
  param <- function(year) list(source = "terraclimate", dataset = "max_temperature", time_period = year)

  # these used to warn ("Option time_period must be in 1958-2022")
  expect_warning(check_params(param(2023:2025)), NA)
  expect_warning(check_params(param(1950:1957)), NA)
  expect_warning(check_params(param(1950:2025)), NA)
  # and a year the server does not have still does (it would be an HTTP 400 there)
  expect_warning(check_params(param(2026)), "must be in 1950-2025")
  expect_warning(check_params(param(1949)), "must be in 1950-2025")
})
