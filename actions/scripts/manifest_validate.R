# Shared validation helpers for actions/scripts/build_manifest.R.
#
# These functions implement the validation gate described in the migration
# plan: nothing is ever committed to the manifest unless it passes every
# check here. Kept in its own file (not the package's R/) because it is CI
# tooling, not runtime code -- actions/ is .Rbuildignore'd, so jsonlite/curl
# used by the HTTP probe never become package dependencies.

MANIFEST_ALL_COLS <- c(
  "survey", "dataset", "geo_level", "year", "sidra_code",
  "url", "docs_url", "available_time", "available_geo",
  "archive_file", "sheet", "layer_name", "version", "resolver"
)

KEY_COLS <- c("survey", "dataset", "geo_level", "year")

# Rows whose url is never actually fetched by download.file(), or whose url
# still carries an unresolved placeholder -- both are exempt from the HTTP
# check (see build_manifest.R validate_candidate()). SIDRA rows need no
# special case here: since the schema normalization, their landing page
# lives in docs_url and url is simply NA, already covered by the first
# condition.
is_http_exempt <- function(row) {
  is.na(row$url) ||
    identical(row$survey, "internal") ||
    identical(row$survey, "terraclimate") ||
    grepl("\\$(year|state|file_name)\\$", row$url)
}

# ---- 1-5: structural validation -------------------------------------------

validate_schema <- function(candidate) {
  errors <- character(0)

  missing_cols <- setdiff(MANIFEST_ALL_COLS, names(candidate))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("missing columns:", paste(missing_cols, collapse = ", ")))
  }
  if (!identical(names(candidate)[seq_along(MANIFEST_ALL_COLS)], MANIFEST_ALL_COLS)) {
    errors <- c(errors, "column order does not match MANIFEST_ALL_COLS")
  }
  if (!all(vapply(candidate, is.character, logical(1)))) {
    errors <- c(errors, "not every column is character")
  }

  errors
}

validate_row_count <- function(old, candidate, tolerance = 0.10) {
  errors <- character(0)
  lo <- floor(nrow(old) * (1 - tolerance))
  hi <- ceiling(nrow(old) * (1 + tolerance))
  if (nrow(candidate) < lo || nrow(candidate) > hi) {
    errors <- c(errors, sprintf(
      "row count %d outside +/-%.0f%% of previous %d (expected [%d, %d])",
      nrow(candidate), tolerance * 100, nrow(old), lo, hi
    ))
  }
  errors
}

validate_no_deletions <- function(old, candidate) {
  errors <- character(0)
  old_pairs <- unique(paste(old$survey, old$dataset, sep = "\r"))
  new_pairs <- unique(paste(candidate$survey, candidate$dataset, sep = "\r"))
  missing <- setdiff(old_pairs, new_pairs)
  if (length(missing) > 0) {
    errors <- c(errors, paste(
      "(survey, dataset) pairs removed (never allowed):",
      paste(gsub("\r", "/", missing), collapse = ", ")
    ))
  }
  errors
}

validate_unique_base_row <- function(candidate) {
  errors <- character(0)

  # every row is a real dataset row -- no survey-default (dataset = NA) row
  # exists under the self-sufficient-rows schema (see R/manifest.R)
  if (any(is.na(candidate$dataset))) {
    errors <- c(errors, "survey-default row(s) (dataset = NA) found -- not allowed")
  }

  # at most one dataset-base row (geo_level/year both NA) per (survey, dataset)
  base_rows <- candidate[
    !is.na(candidate$dataset) & is.na(candidate$geo_level) & is.na(candidate$year),
  ]
  dup <- duplicated(base_rows[, c("survey", "dataset")])
  if (any(dup)) {
    bad <- unique(paste(base_rows$survey[dup], base_rows$dataset[dup], sep = "/"))
    errors <- c(errors, paste("duplicate base row for:", paste(bad, collapse = ", ")))
  }

  override_keys <- candidate[, KEY_COLS]
  dup2 <- duplicated(override_keys)
  if (any(dup2)) {
    errors <- c(errors, "duplicate override key (survey, dataset, geo_level, year) found")
  }
  errors
}

# Every row must be self-sufficient at READ TIME: dataset_field() (R/
# manifest.R) does a single exact-key lookup with no fallthrough between
# rows, keyed on geo_level for a dataset that has any geo_level rows, or on
# year for one that has any year rows. That only works if the row set is
# actually COMPLETE relative to what the dataset's base row declares --
# this is the CI-side half of that contract (the runtime half is "read
# whatever row matches, or NA"; this validator is what stops a candidate
# from shipping a dataset that's missing a row for a geo_level/year it
# claims to support).
validate_key_completeness <- function(candidate) {
  errors <- character(0)

  parse_years_local <- function(x) {
    if (is.na(x) || !nzchar(x)) return(integer(0))
    toks <- trimws(strsplit(x, ",")[[1]])
    unlist(lapply(toks, function(tok) {
      if (grepl("-", tok, fixed = TRUE)) {
        b <- as.integer(trimws(strsplit(tok, "-", fixed = TRUE)[[1]]))
        seq.int(b[1], b[2])
      } else {
        as.integer(tok)
      }
    }))
  }

  base <- candidate[!is.na(candidate$dataset) & is.na(candidate$geo_level) & is.na(candidate$year), ]
  overrides <- candidate[!is.na(candidate$dataset) & (!is.na(candidate$geo_level) | !is.na(candidate$year)), ]

  for (i in seq_len(nrow(base))) {
    s <- base$survey[i]
    d <- base$dataset[i]
    ov <- overrides[overrides$survey == s & overrides$dataset == d, ]
    if (nrow(ov) == 0) next

    has_geo <- any(!is.na(ov$geo_level))
    has_year <- any(!is.na(ov$year))
    if (has_geo && has_year) {
      errors <- c(errors, paste0(
        s, "/", d, ": mixes geo_level and year overrides -- dataset_field() ",
        "can only key on one at a time"
      ))
      next
    }

    if (has_geo) {
      declared_raw <- base$available_geo[i]
      declared <- if (is.na(declared_raw)) character(0) else trimws(strsplit(declared_raw, ",")[[1]])
      declared <- tolower(declared[nzchar(declared)])
      present <- tolower(ov$geo_level[!is.na(ov$geo_level)])
      missing <- setdiff(declared, present)
      if (length(missing) > 0) {
        errors <- c(errors, paste0(
          s, "/", d, ": available_geo declares '", paste(missing, collapse = ", "),
          "' but no matching geo_level row exists"
        ))
      }
    }

    if (has_year) {
      declared <- parse_years_local(base$available_time[i])
      present <- as.integer(ov$year[!is.na(ov$year)])
      missing <- setdiff(declared, present)
      if (length(missing) > 0) {
        errors <- c(errors, paste0(
          s, "/", d, ": available_time declares year(s) ", paste(missing, collapse = ", "),
          " but no matching year row exists"
        ))
      }
    }
  }

  errors
}

validate_placeholders <- function(old, candidate) {
  errors <- character(0)
  extract_placeholders <- function(x) {
    regmatches(x, gregexpr("\\$[a-z_]+\\$", x))
  }
  key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  common <- intersect(key, old_key)
  for (k in common) {
    old_url <- old$url[old_key == k][1]
    new_url <- candidate$url[key == k][1]
    if (!setequal(extract_placeholders(old_url)[[1]], extract_placeholders(new_url)[[1]])) {
      errors <- c(errors, paste("placeholder set changed for", gsub("\r", "/", k)))
    }
    allowed <- c("$year$", "$state$", "$file_name$")
    bad <- setdiff(extract_placeholders(new_url)[[1]], allowed)
    if (length(bad) > 0) {
      errors <- c(errors, paste("unknown placeholder(s) in", gsub("\r", "/", k), ":", paste(bad, collapse = ", ")))
    }
  }
  errors
}

# ---- 6: HTTP validation ----------------------------------------------------

# Probes a single URL. Uses a HEAD first; falls back to a ranged GET for
# hosts that reject HEAD (observed for Google Drive/Docs and some EPE
# endpoints). Never throws -- returns ok = FALSE with a reason instead, so a
# single flaky host doesn't abort the whole validation run.
probe_url <- function(url, timeout_s = 15) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = NA, reason = "curl package not available"))
  }

  h <- curl::new_handle(nobody = TRUE, timeout = timeout_s, followlocation = TRUE)
  resp <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)

  if (is.null(resp) || !(resp$status_code %in% c(200, 206))) {
    h2 <- curl::new_handle(
      timeout = timeout_s, followlocation = TRUE,
      range = "0-0"
    )
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = h2), error = function(e) NULL)
  }

  if (is.null(resp)) {
    return(list(ok = FALSE, reason = "request failed (network/timeout)"))
  }
  if (!(resp$status_code %in% c(200, 206))) {
    return(list(ok = FALSE, reason = paste("HTTP", resp$status_code)))
  }

  headers <- curl::parse_headers_list(resp$headers)
  content_type <- headers[["content-type"]][1]
  content_length <- suppressWarnings(as.numeric(headers[["content-length"]][1]))

  list(
    ok = TRUE,
    status = resp$status_code,
    content_type = content_type,
    content_length = content_length
  )
}

validate_http <- function(candidate, changed_keys, min_bytes = 10000) {
  errors <- character(0)
  key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")
  rows <- candidate[key %in% changed_keys, ]
  rows <- rows[!vapply(seq_len(nrow(rows)), function(i) is_http_exempt(rows[i, ]), logical(1)), ]
  if (nrow(rows) == 0) return(errors)

  # Several self-sufficient rows can legitimately share one url (e.g. all
  # three mapbiomas_mining rows, or epe's per-geo_level overrides -- see
  # actions/scrapers/resolve_mapbiomas.R / resolve_epe.R). Probe each
  # DISTINCT url once, not once per row that happens to carry a copy of it.
  for (u in unique(rows$url)) {
    offenders <- rows[rows$url == u, ]
    label <- paste(unique(paste(offenders$survey, offenders$dataset, sep = "/")), collapse = ", ")

    probe <- probe_url(u)
    if (isFALSE(probe$ok)) {
      errors <- c(errors, sprintf("%s: HTTP check failed (%s) for %s", label, probe$reason, u))
      next
    }
    if (isTRUE(probe$ok)) {
      binary_ext <- grepl("\\.(zip|xlsx|csv|nc|tif|shp)$", u, ignore.case = TRUE)
      if (binary_ext && !is.na(probe$content_type) && grepl("text/html", probe$content_type, fixed = TRUE)) {
        errors <- c(errors, sprintf(
          "%s: server returned text/html for a binary-extension url (likely an error page): %s", label, u
        ))
      }
      if (binary_ext && !is.na(probe$content_length) && probe$content_length < min_bytes) {
        errors <- c(errors, sprintf("%s: response too small (%d bytes) for %s", label, probe$content_length, u))
      }
    }
  }

  errors
}

# ---- Change detection -------------------------------------------------------
#
# Every change now goes through the same path -- open a PR for human review
# (see .github/workflows/update-manifest.yaml) -- so there is no tiering to
# compute anymore, only "did this key's row change, or is it new".

detect_changes <- function(old, candidate) {
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  new_key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")

  changed_keys <- character(0)
  for (k in intersect(old_key, new_key)) {
    o <- old[old_key == k, MANIFEST_ALL_COLS][1, ]
    n <- candidate[new_key == k, MANIFEST_ALL_COLS][1, ]
    if (!identical(o, n)) {
      changed_keys <- c(changed_keys, k)
    }
  }
  added <- setdiff(new_key, old_key)
  changed_keys <- c(changed_keys, added)

  list(changed_keys = changed_keys, n_changed = length(changed_keys))
}
