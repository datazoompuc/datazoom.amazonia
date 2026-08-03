# Shared validation helpers for actions/scripts/build_manifest.R.
#
# These functions implement the validation gate described in the migration
# plan: nothing is ever committed to the manifest unless it passes every
# check here. Kept in its own file (not the package's R/) because it is CI
# tooling, not runtime code -- actions/ is .Rbuildignore'd, so jsonlite/curl
# used by the HTTP probe never become package dependencies.

MANIFEST_ALL_COLS <- c(
  "survey", "dataset", "geo_level", "year", "sidra_code",
  "available_time", "available_geo", "link",
  "archive_file", "sheet", "collection", "layer_name", "resolver"
)

KEY_COLS <- c("survey", "dataset", "geo_level", "year")

# Rows whose link is never actually fetched by download.file(), or whose link
# still carries an unresolved placeholder -- both are exempt from the HTTP
# check (see build_manifest.R validate_candidate()).
is_http_exempt <- function(row) {
  is.na(row$link) ||
    !is.na(row$sidra_code) ||
    identical(row$survey, "internal") ||
    identical(row$survey, "terraclimate") ||
    grepl("\\$(year|state|file_name)\\$", row$link)
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
  base_rows <- candidate[is.na(candidate$geo_level) & is.na(candidate$year), ]
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

validate_placeholders <- function(old, candidate) {
  errors <- character(0)
  extract_placeholders <- function(x) {
    regmatches(x, gregexpr("\\$[a-z_]+\\$", x))
  }
  key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  common <- intersect(key, old_key)
  for (k in common) {
    old_link <- old$link[old_key == k][1]
    new_link <- candidate$link[key == k][1]
    if (!setequal(extract_placeholders(old_link)[[1]], extract_placeholders(new_link)[[1]])) {
      errors <- c(errors, paste("placeholder set changed for", gsub("\r", "/", k)))
    }
    allowed <- c("$year$", "$state$", "$file_name$")
    bad <- setdiff(extract_placeholders(new_link)[[1]], allowed)
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

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    if (is_http_exempt(row)) next

    probe <- probe_url(row$link)
    if (isFALSE(probe$ok)) {
      errors <- c(errors, sprintf(
        "%s/%s: HTTP check failed (%s) for %s",
        row$survey, row$dataset, probe$reason, row$link
      ))
      next
    }
    if (isTRUE(probe$ok)) {
      binary_ext <- grepl("\\.(zip|xlsx|csv|nc|tif|shp)$", row$link, ignore.case = TRUE)
      if (binary_ext && !is.na(probe$content_type) && grepl("text/html", probe$content_type, fixed = TRUE)) {
        errors <- c(errors, sprintf(
          "%s/%s: server returned text/html for a binary-extension link (likely an error page): %s",
          row$survey, row$dataset, row$link
        ))
      }
      if (binary_ext && !is.na(probe$content_length) && probe$content_length < min_bytes) {
        errors <- c(errors, sprintf(
          "%s/%s: response too small (%d bytes) for %s",
          row$survey, row$dataset, probe$content_length, row$link
        ))
      }
    }
  }

  errors
}

# ---- Tiering ---------------------------------------------------------------

# A changed row is Tier A only if every version-looking token embedded in
# the link is byte-identical to before (collection numbers, BACI/PRODES
# version stamps, dated upload paths) AND no non-link column changed. Any
# other difference is Tier B: it may change the SHAPE of the downloaded
# data (a new MapBiomas sheet, a new PRODES raster legend), which no HTTP
# check can validate, and would break already-installed package versions.
VERSION_TOKEN_RE <- "(collection_[0-9]+|COL\\.?[0-9]+|V[0-9]{6}|/20[0-9]{2}/[0-9]{2}/|_20[0-9]{2}\\.)"

file_ext <- function(url) {
  if (is.na(url)) return(NA_character_)
  tolower(sub(".*\\.([a-zA-Z0-9]+)(\\?.*)?$", "\\1", url))
}

classify_row_change <- function(old_row, new_row) {
  non_link_changed <- !identical(
    old_row[setdiff(MANIFEST_ALL_COLS, "link")],
    new_row[setdiff(MANIFEST_ALL_COLS, "link")]
  )
  if (non_link_changed) {
    return("B")
  }
  if (identical(old_row$link, new_row$link)) {
    return("none")
  }

  # A changed file extension (e.g. a source silently swapping .csv for
  # .zip on the same resource, as ANEEL was observed doing live for
  # energy_enterprises_distributed while building this) means external_download()'s
  # file_extension inference and its downstream read function (fread vs
  # read_sf vs unzip) may now be wrong -- that is exactly the kind of
  # "shape of the data changed" risk Tier B exists for, even though no
  # version-looking token in the URL changed at all.
  if (!identical(file_ext(old_row$link), file_ext(new_row$link))) {
    return("B")
  }

  old_tokens <- regmatches(old_row$link, gregexpr(VERSION_TOKEN_RE, old_row$link))[[1]]
  new_tokens <- regmatches(new_row$link, gregexpr(VERSION_TOKEN_RE, new_row$link))[[1]]

  if (setequal(old_tokens, new_tokens)) "A" else "B"
}

classify_candidate <- function(old, candidate) {
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  new_key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")

  tiers <- character(0)
  changed_keys <- character(0)

  for (k in intersect(old_key, new_key)) {
    tier <- classify_row_change(old[old_key == k, ][1, ], candidate[new_key == k, ][1, ])
    if (tier != "none") {
      tiers <- c(tiers, tier)
      changed_keys <- c(changed_keys, k)
    }
  }
  added <- setdiff(new_key, old_key)
  if (length(added) > 0) {
    tiers <- c(tiers, rep("B", length(added)))
    changed_keys <- c(changed_keys, added)
  }

  overall <- if (length(tiers) == 0) "none" else if (all(tiers == "A")) "A" else "B"
  list(overall = overall, changed_keys = changed_keys, n_changed = length(changed_keys))
}
