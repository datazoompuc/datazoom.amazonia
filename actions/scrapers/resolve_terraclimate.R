# actions/scrapers/resolve_terraclimate.R
#
# TerraClimate is one THREDDS server (University of Idaho's Northwest
# Knowledge Network) and 14 manifest rows, one per climate variable, that all
# carry the same `url`: the base of its NetCDF Subset Service,
# http://thredds.northwestknowledge.net:8080/thredds/ncss. That URL is NOT a
# download link on its own (a bare GET answers HTTP 400 "No variables
# requested"); R/download.R builds the real request from it at call time:
#   <url>/agg_terraclimate_<code>_1950_CurrentYear_GLOBE.nc?&var=<code>&south=..
# where <code> is the variable's short code (tmax, tmin, ws, ..., PDSI).
#
# WHY THIS IS A VERIFICATION RESOLVER (resolve_degrad.R's model), NOT A
# DISCOVERY ONE (resolve_deter.R's). There is nothing to discover: the 14
# endpoints are fixed and the file name is derived by a rule, so DETER's
# "find the link in an API response, with a keyword fallback" machinery has
# no job here. What can go stale is exactly what went stale before: (1) the
# FILE NAME, which embeds the start year of the series, and (2) the coverage
# in `available_time`, because the file is literally called
# "..._CurrentYear_GLOBE.nc" and keeps growing. So, unlike DEGRAD (a frozen
# product, pure verification), this resolver also REFRESHES one field,
# available_time, from the server's own metadata on every run.
#
# VERIFIED LIVE (2026-10-19):
#
#   1. The data is TerraClimate v1.1 (global attribute `version = V1.1`, ERA5
#      anomalies on WorldClim climatologies), 1950-01 to 2025-12 -- 912
#      monthly steps -- identical for all 14 variables. The manifest said
#      "1958-2022" (the retired v1.0 span, last edited 2023): wrong at both
#      ends. The Climatology Lab's page (climatologylab.org/terraclimate.html,
#      "Updated 15 February 2026") says v1.1 replaced v1.0 in February 2025
#      and that the legacy v1.0 files "have been migrated to an ftp server".
#
#   2. THE SERVER ANSWERS HTTP 200 WITH AN EMPTY BODY (no Content-Type) for
#      any dataset name it does not have: the retired v1.0 name
#      agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc, an unknown code, a
#      wrong-case code (pdsi for PDSI), a path that does not exist at all.
#      This is how the 1958 -> 1950 rename showed up: download.file() reported
#      success and left a 0-byte file, and the user got "[rast] cannot open
#      this file as a SpatRaster" (R/download.R's file name was 1958 until
#      commit 5284ac8, 2026-08-08; the v1.2.0 release still has it). A status
#      code therefore proves nothing on this server -- every check below reads
#      the BODY.
#
#   3. Other responses, for reference: a valid file with an unknown variable
#      is HTTP 400 text/plain ("Variable: nope is not contained in the
#      requested dataset"); a time range entirely outside the data is HTTP 400
#      naming the real range ("... does not intersect actual time range
#      1950-01-01T00:00:00Z - 2025-12-01T00:00:00Z"); a range that merely
#      STRADDLES the end is clamped silently and answered HTTP 200 with
#      whatever exists. (R/climate.R always asks for the last month of the
#      final year, so asking for a year that is only partly there succeeds
#      with fewer than 12 months -- see "complete years" below.)
#
#   4. What each run does, per variable (2 small requests, ~17 KB together;
#      28 requests and ~240 KB per run for the whole source):
#        a. GET <url>/<file>/dataset.xml -- THREDDS' own machine-readable
#           description of the dataset. It must be a <gridDataset> that
#           declares a <grid name="<code>">, with a <TimeSpan> and a time
#           axis whose step count equals the number of months between the
#           span's ends (so a sparse/yearly/climatology axis is caught).
#        b. GET a real NCSS subset -- one month, a 0.1 degree box in
#           Rondonia, built exactly as R/download.R builds a user's request --
#           at the last month of the last COMPLETE year, and require a real
#           netCDF back (classic "CDF\001/\002", or the HDF5 signature of a
#           netCDF-4 file) of plausible size. This proves the file name
#           pattern, the variable code and the claimed last year all work
#           end to end, with the server answering the way it answers a user.
#
#   COMPLETE YEARS ONLY. available_time lists calendar years for which all 12
#   months exist: a span ending 2026-03 yields "...-2025", not "...-2026",
#   because load_climate() asks for January-December of each year and the
#   server would silently hand back three months (point 3). The first year
#   follows the same rule (a span starting 1950-02 starts at 1951). Today's
#   span ends on 2025-12-01, so today's value is "1950-2025".
#
#   HOST. thredds.northwestknowledge.net:8080, tds-proxy.nkn.uidaho.edu and
#   thredds.nkn.uidaho.edu are three names for ONE server (same IP,
#   129.101.202.140; a byte comparison of the same subset differs only in a
#   3-byte generation timestamp). The two nkn.uidaho.edu names speak HTTPS on
#   443 with a complete certificate chain (valid to 2027-01-09); the first is
#   plain HTTP on port 8080. The manifest keeps the first because it is the
#   one the Climatology Lab documents: its page, its "best practices" page
#   and its own example NCSS script (terraclimate_ncss_subset.sh, whose
#   request is byte-for-byte the one R/download.R builds) all use it. The
#   resolver verifies whatever `url` the manifest says, so switching hosts is
#   a one-cell manifest change that this resolver then checks. If this host
#   ever stops answering, try https://tds-proxy.nkn.uidaho.edu/thredds/ncss.
#
#   ROBOTS.TXT. The server's robots.txt is the stock THREDDS one:
#   "Disallow: /thredds/ncss/" (and dodsC, fileServer, ...) for every agent,
#   meant to keep crawlers off data services. The lab's own guidance
#   (climate.northwestknowledge.net/TERRACLIMATE/TERRACLIMATE_GRIDMET_
#   guidance.php) names NCSS "a good option for a small rectangular subset"
#   and ships scripts for it, which is exactly what load_climate() does for a
#   user. This resolver is the same API use at a much smaller scale -- not a
#   crawl -- and identifies itself in its User-Agent. If the lab ever objects,
#   `build_manifest.R --skip terraclimate` takes it out of the run.
#
# WHAT THIS DOES NOT CHECK: the values (units, scale_factor, fill values), that
# terra can read the file, the two bounding boxes in R/climate.R, or the 1950
# in the file name beyond "the server serves that exact name". It does not
# discover new variables either -- a 15th variable needs a manifest row, an
# entry in TERRACLIMATE_CODES and an entry in R/climate.R's dataset_names
# (tests/testthat/test-terraclimate-resolver.R fails until all three agree).
#
# FAILURE SEMANTICS. A variable that fails verification is left alone
# (its row is simply absent from the result: "leave the cell alone") and
# reported through the `partial_failures` attribute, so one flaky variable
# does not stop the other 13 from refreshing; build_manifest.R turns that
# into exit code 11. If EVERY variable fails, the resolver stops: that is the
# signature of a systemic change (file-name pattern, host), not of a flake.
#
# Naming: build_manifest.R registers EVERY top-level binding called
# resolve_* as a resolver, so everything else in this file is terraclimate_*
# or TERRACLIMATE_*.

# dataset -> short code, as in R/climate.R's dataset_names (the codes are
# case-sensitive: PDSI, not pdsi). tests/testthat/test-terraclimate-resolver.R
# fails if the two tables ever differ.
TERRACLIMATE_CODES <- c(
  max_temperature = "tmax",
  min_temperature = "tmin",
  wind_speed = "ws",
  vapor_pressure_deficit = "vpd",
  vapor_pressure = "vap",
  snow_water_equivalent = "swe",
  shortwave_radiation_flux = "srad",
  soil_moisture = "soil",
  runoff = "q",
  precipitation = "ppt",
  potential_evaporation = "pet",
  climatic_water_deficit = "def",
  water_evaporation = "aet",
  palmer_drought_severity_index = "PDSI"
)

# The file-name pattern R/download.R builds: agg_terraclimate_<code>_1950_
# CurrentYear_GLOBE.nc. The "1950" is the start of the series and changed once
# already (1958 under v1.0) -- it is the single most likely thing to break.
TERRACLIMATE_FILE_PREFIX <- "agg_terraclimate_"
TERRACLIMATE_FILE_SUFFIX <- "_1950_CurrentYear_GLOBE.nc"

# The probe's box: 0.1 x 0.1 degrees of forest in Rondonia, i.e. 2 x 2 cells
# of the 1/24 degree grid, ~5.9 KB of netCDF for one month.
TERRACLIMATE_PROBE_BOX <- c(south = "-10", north = "-9.9", west = "-60", east = "-59.9")

# A netCDF file with its global attributes alone is ~4 KB (measured), so
# anything smaller than this is not one.
TERRACLIMATE_MIN_NETCDF_BYTES <- 1000

TERRACLIMATE_USER_AGENT <- "datazoom.amazonia-manifest-resolver (+https://github.com/datazoompuc/datazoom.amazonia)"

terraclimate_filename <- function(code) {
  paste0(TERRACLIMATE_FILE_PREFIX, code, TERRACLIMATE_FILE_SUFFIX)
}

# First element of a possibly-NULL header value, as a string or NA.
terraclimate_first <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
}

# A short, single-line excerpt of the TEXT of a response body, or "" when it is
# empty or binary -- the server explains its 400s in plain text ("Variable:
# nope is not contained in the requested dataset"; "Requested time range ...
# does not intersect actual time range ..."), and its 404s as a Tomcat HTML
# page whose text ("HTTP Status 404 - Not Found") is what matters, not its
# markup. That text is the most useful thing a failure message can carry.
terraclimate_snippet <- function(content, n = 160) {
  if (length(content) == 0) {
    return("")
  }
  head_bytes <- content[seq_len(min(length(content), 4000))]
  # Plain ASCII text only (tab, newline, CR, space..~), checked on the raw
  # bytes: a regex class like [^[:print:][:space:]] misfires on R for Windows
  # (it flags ordinary ASCII), so this must not depend on the locale.
  if (!all(head_bytes %in% as.raw(c(9, 10, 13, 32:126)))) {
    return("")
  }
  txt <- rawToChar(head_bytes)
  txt <- gsub("(?s)<(script|style)[^>]*>.*?</\\1>", " ", txt, perl = TRUE)
  txt <- gsub("<[^>]*>", " ", txt)
  txt <- trimws(gsub("[[:space:]]+", " ", txt))
  if (nchar(txt) > n) txt <- paste0(substr(txt, 1, n), "...")
  txt
}

# One GET, no redirects followed (a redirect means the host moved -- worth a
# loud failure, not a silent follow). Throws, naming the URL, on any transport
# failure: "could not check" must never be conflated with "checked and fine".
terraclimate_get <- function(url, timeout_s = 60) {
  h <- curl::new_handle(timeout = timeout_s, followlocation = FALSE, useragent = TERRACLIMATE_USER_AGENT)
  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) stop("could not reach ", url, " (", conditionMessage(e), ")", call. = FALSE)
  )
  headers <- curl::parse_headers_list(resp$headers)
  list(
    status = resp$status_code,
    content_type = terraclimate_first(headers[["content-type"]]),
    location = terraclimate_first(headers[["location"]]),
    content = resp$content
  )
}

# Pure (no network): reads THREDDS' dataset.xml for one variable. Returns
# list(problem, begin, end, steps); `problem` is character(0) when the
# description is the expected one, otherwise ONE reason string.
terraclimate_parse_metadata <- function(xml, code) {
  fail <- function(msg) list(problem = msg, begin = NA_character_, end = NA_character_, steps = NA_integer_)

  if (is.null(xml) || length(xml) != 1 || is.na(xml) || !nzchar(trimws(xml))) {
    return(fail(paste(
      "empty dataset description (this server answers HTTP 200 with an empty body",
      "for a dataset name it does not have)"
    )))
  }
  if (!grepl("<gridDataset", xml, fixed = TRUE)) {
    return(fail("the response is not a THREDDS <gridDataset> description"))
  }
  if (!grepl(sprintf('<grid name="%s"', code), xml, fixed = TRUE)) {
    return(fail(sprintf("the dataset does not declare a grid variable named '%s'", code)))
  }

  span <- regmatches(xml, regexec("<TimeSpan>\\s*<begin>([^<]+)</begin>\\s*<end>([^<]+)</end>\\s*</TimeSpan>", xml))[[1]]
  if (length(span) != 3) {
    return(fail("the description carries no <TimeSpan> with a <begin> and an <end>"))
  }
  begin <- trimws(span[2])
  end <- trimws(span[3])
  date_shape <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T"
  year_month <- function(x) as.integer(c(substr(x, 1, 4), substr(x, 6, 7)))
  if (!grepl(date_shape, begin) || !grepl(date_shape, end)) {
    return(fail(sprintf("cannot read the <TimeSpan> ('%s' to '%s') as dates", begin, end)))
  }
  b <- year_month(begin)
  e <- year_month(end)

  steps_txt <- regmatches(xml, regexec('<axis name="time" shape="([0-9]+)"', xml))[[1]]
  if (length(steps_txt) != 2) {
    return(fail("the description declares no time axis"))
  }
  steps <- as.integer(steps_txt[2])
  expected <- (e[1] - b[1]) * 12L + (e[2] - b[2]) + 1L
  if (is.na(steps) || steps != expected) {
    return(fail(sprintf(
      "the time axis has %s step(s), but a monthly series from %s to %s has %d",
      steps_txt[2], begin, end, expected
    )))
  }

  list(problem = character(0), begin = begin, end = end, steps = steps)
}

# Pure: the calendar years a span covers in full, as c(first = , last = ), or
# integer(0) when it contains none. See "COMPLETE YEARS ONLY" above.
terraclimate_complete_years <- function(begin, end) {
  b_year <- as.integer(substr(begin, 1, 4))
  b_month <- as.integer(substr(begin, 6, 7))
  e_year <- as.integer(substr(end, 1, 4))
  e_month <- as.integer(substr(end, 6, 7))
  first <- if (b_month == 1L) b_year else b_year + 1L
  last <- if (e_month == 12L) e_year else e_year - 1L
  if (last < first) {
    return(integer(0))
  }
  c(first = first, last = last)
}

# The request R/download.R builds for a user, narrowed to one month (December
# of `year`) and the probe box. Same parameter names, same order.
terraclimate_probe_url <- function(base, code, year) {
  stamp <- sprintf("%d-12-01T00%%3A00%%3A00Z", year)
  paste0(
    sub("/+$", "", base), "/", terraclimate_filename(code), "?",
    "&var=", code,
    "&south=", TERRACLIMATE_PROBE_BOX[["south"]],
    "&north=", TERRACLIMATE_PROBE_BOX[["north"]],
    "&west=", TERRACLIMATE_PROBE_BOX[["west"]],
    "&east=", TERRACLIMATE_PROBE_BOX[["east"]],
    "&horizStride=1",
    "&time_start=", stamp,
    "&time_end=", stamp,
    "&timeStride=1",
    "&disableProjSubset=on&addLatLon=true&accept=netcdf"
  )
}

# Pure verdict on the probe's response: character(0) when it is a real netCDF
# file, otherwise ONE short reason. Reads the body -- see point 2 above.
terraclimate_netcdf_problem <- function(status, content_type, content, min_bytes = TERRACLIMATE_MIN_NETCDF_BYTES) {
  if (length(status) != 1 || is.na(status) || status != 200) {
    snippet <- terraclimate_snippet(content)
    return(sprintf(
      "HTTP %s instead of 200%s",
      if (length(status) == 1) status else "NA",
      if (nzchar(snippet)) paste0(": ", snippet) else ""
    ))
  }
  if (length(content) < min_bytes) {
    return(sprintf(
      paste(
        "the response body is %d byte(s), too small for a netCDF file (this server",
        "answers HTTP 200 with an empty body for a dataset name or variable it does not have)"
      ),
      length(content)
    ))
  }
  classic <- identical(content[1:3], charToRaw("CDF")) && content[4] %in% as.raw(1:2)
  hdf5 <- identical(content[1:8], as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a)))
  if (!classic && !hdf5) {
    snippet <- terraclimate_snippet(content, n = 60)
    return(sprintf(
      "the body (Content-Type '%s') does not start with a netCDF signature (\"CDF\" + 1 or 2, or the HDF5 one of a netCDF-4 file)%s",
      if (length(content_type) == 1 && !is.na(content_type)) content_type else "missing",
      if (nzchar(snippet)) paste0("; it starts with: ", snippet) else ""
    ))
  }
  character(0)
}

# Verifies ONE variable. Returns list(problem = character(0), available_time =
# "1950-2025") or list(problem = "<reason>"). Never throws.
terraclimate_check_variable <- function(base, code) {
  base <- sub("/+$", "", base)

  meta_url <- paste0(base, "/", terraclimate_filename(code), "/dataset.xml")
  meta <- tryCatch(terraclimate_get(meta_url), error = function(e) e)
  if (inherits(meta, "error")) {
    return(list(problem = conditionMessage(meta)))
  }
  if (meta$status %in% c(301L, 302L, 303L, 307L, 308L)) {
    return(list(problem = sprintf(
      "%s redirects (HTTP %d) to %s -- the host or path moved", meta_url, meta$status,
      if (is.na(meta$location)) "an unknown location" else meta$location
    )))
  }
  if (meta$status != 200L) {
    snippet <- terraclimate_snippet(meta$content)
    return(list(problem = sprintf(
      "%s answered HTTP %d%s", meta_url, meta$status, if (nzchar(snippet)) paste0(": ", snippet) else ""
    )))
  }
  xml <- tryCatch(rawToChar(meta$content), error = function(e) "")
  parsed <- terraclimate_parse_metadata(xml, code)
  if (length(parsed$problem) > 0) {
    return(list(problem = paste0(meta_url, ": ", parsed$problem)))
  }

  years <- terraclimate_complete_years(parsed$begin, parsed$end)
  if (length(years) == 0) {
    return(list(problem = sprintf(
      "%s: the span %s to %s contains no complete calendar year", meta_url, parsed$begin, parsed$end
    )))
  }

  probe_url <- terraclimate_probe_url(base, code, years[["last"]])
  probe <- tryCatch(terraclimate_get(probe_url), error = function(e) e)
  if (inherits(probe, "error")) {
    return(list(problem = conditionMessage(probe)))
  }
  if (probe$status %in% c(301L, 302L, 303L, 307L, 308L)) {
    return(list(problem = sprintf(
      "%s redirects (HTTP %d) to %s -- the host or path moved", probe_url, probe$status,
      if (is.na(probe$location)) "an unknown location" else probe$location
    )))
  }
  reason <- terraclimate_netcdf_problem(probe$status, probe$content_type, probe$content)
  if (length(reason) > 0) {
    return(list(problem = paste0("a real subset request for ", years[["last"]], "-12 failed (", probe_url, "): ", reason)))
  }

  list(
    problem = character(0),
    available_time = if (years[["first"]] == years[["last"]]) {
      as.character(years[["last"]])
    } else {
      sprintf("%d-%d", years[["first"]], years[["last"]])
    }
  )
}

resolve_terraclimate <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_terraclimate(): no manifest rows tagged resolver == 'terraclimate' -- ",
      "cannot tell which variables to verify. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_terraclimate() needs the 'curl' package (CI-only; not a package Import).")
  }
  unknown <- setdiff(rows$dataset, names(TERRACLIMATE_CODES))
  if (length(unknown) > 0) {
    stop(
      "resolve_terraclimate(): no variable code on record for dataset(s) ",
      paste(unknown, collapse = ", "),
      " -- add them to TERRACLIMATE_CODES above (and to dataset_names in R/climate.R), ",
      "after confirming the code live in the lab's variable guide."
    )
  }
  if (any(!is.na(rows$geo_level) | !is.na(rows$year))) {
    stop(
      "resolve_terraclimate(): every TerraClimate row is unkeyed (one row per variable) -- ",
      "found a row with a geo_level or a year. This resolver doesn't know how to handle it."
    )
  }
  if (any(is.na(rows$url) | rows$url == "")) {
    stop("resolve_terraclimate(): every row needs an existing url to verify -- this resolver doesn't invent one.")
  }

  available_time <- rep(NA_character_, nrow(rows))
  problems <- rep(NA_character_, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    res <- terraclimate_check_variable(rows$url[i], TERRACLIMATE_CODES[[rows$dataset[i]]])
    if (length(res$problem) > 0) {
      problems[i] <- res$problem
    } else {
      available_time[i] <- res$available_time
      message("resolve_terraclimate(): ", rows$dataset[i], " ok, ", res$available_time)
    }
  }

  failed <- !is.na(problems)
  if (all(failed)) {
    stop(
      "resolve_terraclimate(): all ", nrow(rows), " variables failed verification -- a systemic change, ",
      "not a flake. The usual cause is the file-name pattern (",
      terraclimate_filename("<code>"), ", built in R/download.R; the start year in it changed once ",
      "already) or the host. Reasons:\n- ",
      paste(paste0(rows$dataset, ": ", problems), collapse = "\n- ")
    )
  }

  # Only the verified rows are returned: a variable that failed keeps whatever
  # the manifest already says about it (see FAILURE SEMANTICS).
  ok <- !failed
  out <- tibble::tibble(
    survey = "terraclimate", dataset = rows$dataset[ok],
    geo_level = rows$geo_level[ok], year = rows$year[ok],
    url = rows$url[ok], available_time = available_time[ok], resolver = "terraclimate"
  )
  if (any(failed)) {
    attr(out, "partial_failures") <- paste0("terraclimate/", rows$dataset[failed], ": ", problems[failed])
  }
  out
}
