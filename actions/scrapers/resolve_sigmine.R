# actions/scrapers/resolve_sigmine.R
#
# SIGMINE (Sistema de Informacoes Geograficas da Mineracao, ANM) publishes
# national shapefile zips for active and inactive mining processes in a plain
# IIS directory listing at:
#   https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/
#
# Datasets owned by this resolver:
#   - sigmine_active:   BRASIL.zip
#   - sigmine_inactive: PROCESSOS_INATIVOS.zip
#
# VERIFIED LIVE (2026-11-17):
#
#   1. DIRECTORY LISTING & DOWNLOAD ENDPOINTS.
#      - https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/ returns
#        HTTP 200 HTML with <A HREF="/SIGMINE/PROCESSOS_MINERARIOS/<name>">
#        entries (BRASIL.zip, PROCESSOS_INATIVOS.zip, plus per-UF zips/kmzs).
#      - Plain curl.exe without a User-Agent receives HTTP 403 on ANM files;
#        sending a browser-like User-Agent returns HTTP 200. (R's libcurl also
#        works, and we pass SIGMINE_USER_AGENT consistently across all requests.)
#      - Metadata spreadsheet (https://dadosabertos.anm.gov.br/SIGMINE/
#        metadados-sigmine.ods) confirms daily regeneration ("Frequencia de
#        atualizacao: Diaria"), so `version` is deliberately left blank --
#        stamping Last-Modified would open a noisy PR every week when nothing in
#        the URL, schema, or year coverage changed.
#
#   2. ARCHIVE STRUCTURE & HTTP RANGE SUPPORT.
#      - dadosabertos.anm.gov.br advertises `Accept-Ranges: bytes` and answers
#        ranged GETs with HTTP 206 Partial Content.
#      - BRASIL.zip (~128 MB) contains 10 entries including BRASIL.shp (~101 MB
#        compressed) and BRASIL.dbf (~21 MB compressed, 270,578 rows).
#      - PROCESSOS_INATIVOS.zip (~151 MB) contains 10 entries including
#        PROCESSOS_INATIVOS.shp (~115 MB compressed) and PROCESSOS_INATIVOS.dbf
#        (~24 MB compressed, 666,125 rows).
#      - Using zip_remote_listing.R's zip_remote_entries() and
#        zip_remote_extract_entry(..., timeout_s = 1000) verifies that each zip
#        carries exactly one .shp and one .dbf and downloads only the .dbf
#        payload (skipping the >100 MB .shp geometries).
#
#   3. SCHEMA & available_time.
#      - Both .dbf files carry an integer `ANO` column (0 NAs, consecutive years):
#          BRASIL.dbf:             1934-2026
#          PROCESSOS_INATIVOS.dbf: 1935-2026
#      - Columns after janitor::clean_names()-equivalent normalization:
#          sigmine_active:   processo, numero, ano, area_ha, id, fase,
#                            ult_evento, nome, subs, uso, uf, ds_processo
#                            (R/sigmine.R uses positional across(nome:uf, ...),
#                            so the exact slice c("nome", "subs", "uso", "uf")
#                            is verified every run.)
#          sigmine_inactive: processo, numero, ano, area_ha, id, ds_processo
#      - available_time is reported as a min-max window (e.g. "1934-2026") for
#        years > 1900, matching resolve_aneel.R's aneel_generation_available_time()
#        for process/plant registers.
#
#   4. PARTIAL FAILURES.
#      - sigmine_active and sigmine_inactive are verified independently: if one
#        archive fails while the other succeeds, the successful row is returned
#        with the failure message attached as `attr(out, "partial_failures")`.
#        `stop()` is called only if every dataset fails (or the directory listing
#        itself is unreachable).

SIGMINE_DIR_URL <- "https://dadosabertos.anm.gov.br/SIGMINE/PROCESSOS_MINERARIOS/"
SIGMINE_DOCS_URL <- "https://dadosabertos.anm.gov.br/"
SIGMINE_USER_AGENT <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

SIGMINE_DATASETS <- list(
  sigmine_active = list(
    file = "BRASIL.zip",
    required_cols = c(
      "processo", "numero", "ano", "area_ha", "id", "fase",
      "ult_evento", "nome", "subs", "uso", "uf", "ds_processo"
    ),
    nome_uf_slice = c("nome", "subs", "uso", "uf")
  ),
  sigmine_inactive = list(
    file = "PROCESSOS_INATIVOS.zip",
    required_cols = c("processo", "numero", "ano", "area_ha", "id", "ds_processo"),
    nome_uf_slice = NULL
  )
)

# Standalone clean_names() equivalent for ANM's DBF field names (avoids pulling
# janitor into actions/ while keeping DSProcesso -> ds_processo, ULT_EVENTO ->
# ult_evento, AREA_HA -> area_ha).
sigmine_clean_col_names <- function(nms) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", nms)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

# Extracts href targets from ANM's IIS directory listing HTML and resolves them
# against `dir_url`.
sigmine_parse_dir_links <- function(html, dir_url = SIGMINE_DIR_URL) {
  matches <- regmatches(
    html,
    gregexpr('href\\s*=\\s*"([^"]+)"', html, ignore.case = TRUE, perl = TRUE)
  )[[1]]
  if (length(matches) == 0L) {
    return(character(0))
  }
  hrefs <- sub('^href\\s*=\\s*"([^"]+)"$', "\\1", matches, ignore.case = TRUE, perl = TRUE)
  origin <- sub("^(https?://[^/]+).*$", "\\1", dir_url)
  vapply(hrefs, function(h) {
    if (grepl("^https?://", h, ignore.case = TRUE)) {
      h
    } else if (startsWith(h, "/")) {
      paste0(origin, h)
    } else {
      paste0(dir_url, h)
    }
  }, character(1), USE.NAMES = FALSE)
}

# Reads the extracted .dbf at `path`, verifies R/sigmine.R's column
# assumptions, and returns the "min-max" available_time window from `ano`.
sigmine_dbf_years_and_check <- function(path, dataset) {
  spec <- SIGMINE_DATASETS[[dataset]]
  if (is.null(spec)) {
    stop("unmapped SIGMINE dataset '", dataset, "'")
  }

  dat <- foreign::read.dbf(path, as.is = TRUE)
  cols <- sigmine_clean_col_names(names(dat))

  missing_cols <- setdiff(spec$required_cols, cols)
  if (length(missing_cols) > 0L) {
    stop(
      dataset, ": missing required DBF column(s): ",
      paste(missing_cols, collapse = ", "),
      " (found: ", paste(cols, collapse = ", "), ")"
    )
  }

  if (!is.null(spec$nome_uf_slice)) {
    i_nome <- match("nome", cols)
    i_uf <- match("uf", cols)
    actual_slice <- if (!is.na(i_nome) && !is.na(i_uf) && i_nome <= i_uf) {
      cols[i_nome:i_uf]
    } else {
      character(0)
    }
    if (!identical(actual_slice, spec$nome_uf_slice)) {
      stop(
        dataset, ": positional slice nome:uf expected ",
        paste(spec$nome_uf_slice, collapse = ", "),
        " but got ",
        if (length(actual_slice) > 0L) paste(actual_slice, collapse = ", ") else "reversed/invalid"
      )
    }
  }

  ano_col <- dat[[match("ano", cols)]]
  years <- suppressWarnings(as.integer(ano_col))
  years <- years[!is.na(years) & years > 1900L]
  if (length(years) == 0L) {
    stop(dataset, ": no usable ano values > 1900 in DBF")
  }

  paste(min(years), max(years), sep = "-")
}

# Verifies the remote zip's entries via HTTP range requests, extracts only the
# .dbf sidecar, and validates its schema and year window.
sigmine_verify_and_read_zip <- function(url, dataset) {
  entries <- zip_remote_entries(url, useragent = SIGMINE_USER_AGENT)
  entry_names <- names(entries)

  shp_entries <- grep("\\.shp$", entry_names, ignore.case = TRUE, value = TRUE)
  if (length(shp_entries) != 1L) {
    stop(
      dataset, ": expected 1 .shp in ", url, ", found ", length(shp_entries),
      if (length(shp_entries) > 0L) paste0(" (", paste(shp_entries, collapse = ", "), ")") else ""
    )
  }

  dbf_entries <- grep("\\.dbf$", entry_names, ignore.case = TRUE, value = TRUE)
  if (length(dbf_entries) != 1L) {
    stop(
      dataset, ": expected 1 .dbf in ", url, ", found ", length(dbf_entries),
      if (length(dbf_entries) > 0L) paste0(" (", paste(dbf_entries, collapse = ", "), ")") else ""
    )
  }

  raw_dbf <- zip_remote_extract_entry(
    url,
    entries[[dbf_entries[1L]]],
    timeout_s = 1000,
    useragent = SIGMINE_USER_AGENT
  )

  dbf_path <- tempfile(fileext = ".dbf")
  on.exit(unlink(dbf_path), add = TRUE)
  writeBin(raw_dbf, dbf_path)

  sigmine_dbf_years_and_check(dbf_path, dataset)
}

resolve_sigmine <- function(rows) {
  if (missing(rows) || is.null(rows) || nrow(rows) == 0L) {
    stop(
      "resolve_sigmine(): no manifest rows tagged resolver == 'sigmine' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("foreign", quietly = TRUE)) {
    stop("resolve_sigmine() needs the 'curl' and 'foreign' packages (CI-only; not package Imports).")
  }

  unknown <- setdiff(rows$dataset, names(SIGMINE_DATASETS))
  if (length(unknown) > 0L) {
    stop(
      "resolve_sigmine(): unmapped SIGMINE dataset(s): ",
      paste(unknown, collapse = ", ")
    )
  }

  h_dir <- curl::new_handle(timeout = 30, followlocation = TRUE, useragent = SIGMINE_USER_AGENT)
  dir_resp <- tryCatch(
    curl::curl_fetch_memory(SIGMINE_DIR_URL, handle = h_dir),
    error = function(e) {
      stop("resolve_sigmine(): failed to fetch ANM directory listing at ", SIGMINE_DIR_URL, ": ", conditionMessage(e))
    }
  )
  if (dir_resp$status_code != 200L) {
    stop(
      "resolve_sigmine(): ANM directory listing returned HTTP ", dir_resp$status_code,
      " at ", SIGMINE_DIR_URL
    )
  }

  dir_links <- sigmine_parse_dir_links(rawToChar(dir_resp$content), dir_url = SIGMINE_DIR_URL)
  zip_links <- dir_links[grepl("\\.zip$", dir_links, ignore.case = TRUE)]
  if (length(zip_links) == 0L) {
    stop("resolve_sigmine(): no .zip links found in ANM directory listing at ", SIGMINE_DIR_URL)
  }

  out_list <- list()
  failures <- character(0)

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    ds <- r$dataset
    expected_file <- SIGMINE_DATASETS[[ds]]$file

    res <- tryCatch({
      matched <- zip_links[basename(zip_links) == expected_file]
      if (length(matched) == 0L) {
        stop("'", expected_file, "' not listed in ", SIGMINE_DIR_URL)
      }
      target_url <- matched[1L]
      avail_time <- sigmine_verify_and_read_zip(target_url, ds)

      tibble::tibble(
        survey = r$survey,
        dataset = ds,
        geo_level = r$geo_level,
        year = r$year,
        url = target_url,
        docs_url = SIGMINE_DOCS_URL,
        available_time = avail_time,
        resolver = "sigmine"
      )
    }, error = function(e) e)

    if (inherits(res, "error")) {
      failures <- c(failures, paste0(ds, ": ", conditionMessage(res)))
    } else {
      out_list[[length(out_list) + 1L]] <- res
    }
  }

  if (length(out_list) == 0L) {
    stop("resolve_sigmine(): all datasets failed:\n- ", paste(failures, collapse = "\n- "))
  }

  out <- dplyr::bind_rows(out_list)
  if (length(failures) > 0L) {
    attr(out, "partial_failures") <- failures
  }
  out
}
