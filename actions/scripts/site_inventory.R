# actions/scripts/site_inventory.R
#
# Shared infrastructure for "site link watchers" -- CI tooling that records
# what download links a source's own webpage currently advertises, the same
# way manifest_validate.R is shared infrastructure for the manifest itself.
# Sourced by build_manifest.R next to manifest_validate.R.
#
# Why this exists: resolve_mapbiomas.R used to watch
# brasil.mapbiomas.org/downloads/estatisticas/ by doing version ARITHMETIC --
# scrape a collection number, compare it numerically to what the Dataverse
# sub-resolver found, write docs_url if the site looked ahead. That only ever
# spoke for one dataset (mapbiomas_cover), could not see a link disappearing
# (no candidate found == indistinguishable from "nothing new"), and treated
# "10.1 vs 11" as a labeling convention MapBiomas doesn't actually owe us.
#
# This replaces that with an INVENTORY: a committed CSV
# (actions/watch/site_links.csv) recording every link a watched page
# currently advertises. Each run re-scrapes, diffs against the committed
# snapshot, and any addition/removal/move rides the same PR the manifest
# already opens (see build_manifest.R and update-manifest.yaml). The signal
# becomes an observed diff instead of a computed verdict -- "Coleção 11 link
# appeared, COL8.0 mining link disappeared" instead of "docs_url changed."
#
# ---- Division of responsibility -------------------------------------------
#
# This file: page-agnostic infrastructure only -- fetching, URL/label
# normalization, the CSV schema, diffing, and STRUCTURAL validation (schema,
# duplicate identity, malformed dates). It knows nothing about any one site's
# markup.
#
# actions/scrapers/watch_<source>.R: the actual scraping logic (regex
# strategies against ONE site's real markup) AND that source's own
# "is this scrape trustworthy" guards -- e.g. "did I find zero links on a
# page that fetched fine" or "did the count collapse vs last time". This
# mirrors exactly how resolve_<source>.R owns its own sanity checks today
# (see resolve_mapbiomas.R's dv_file_sheets()/resolve_via_dataverse() --
# "verify the sheet before trusting a hit" lives in the resolver, not in
# manifest_validate.R). A watcher signals failure by stop()ing, exactly like
# a resolver; build_manifest.R isolates it in the same tryCatch pattern.
#
# Deliberately NOT provided here: generic "strategy constructor" functions
# (e.g. a shared strategy_row_blocks()/strategy_proximity() library). Every
# resolve_*.R already implements its own scraping logic inline -- there is no
# shared strategy library among them despite several sharing structural
# habits (walk newest-first, verify before trust) -- and MapBiomas's <tr
# class="dl-row"> markup shares essentially nothing with, say, IPS's
# JS-bundle scraping or EPE's two differently-shaped pages. Forcing a
# generic abstraction over one real caller would be speculative complexity;
# watch_<source>.R files follow the resolver convention instead: copy the
# shape, write the regexes for your own page.

## ============================================================ ##
## Schema                                                       ##
## ============================================================ ##

INVENTORY_COLS <- c("source", "page", "label", "collection", "url", "host", "strategy", "first_seen")

# Identity = the key a diff is computed on. `url` is a VALUE under this key,
# not part of it -- that's what makes a moved link read as one changed row
# instead of a removal + an unrelated addition. See watch_<source>.R for how
# each source keeps this key unique on its own page(s).
INVENTORY_IDENTITY_COLS <- c("source", "page", "label", "collection")

## ============================================================ ##
## Fetching                                                     ##
## ============================================================ ##

# The followlocation=TRUE + force-UTF-8 fetch that is currently copy-pasted
# across resolve_ips.R/resolve_seeg.R/resolve_epe.R/resolve_mapbiomas.R.
# Consolidated here for watch_*.R to use; the existing resolvers are left
# untouched this pass (out of scope -- see the plan).
fetch_page <- function(url, timeout_s = 30) {
  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout_s, followlocation = TRUE)),
    error = function(e) NULL
  )
  if (is.null(resp)) stop("fetch_page(): request failed (network/timeout) for ", url)
  if (resp$status_code != 200) stop("fetch_page(): HTTP ", resp$status_code, " for ", url)
  txt <- rawToChar(resp$content)
  Encoding(txt) <- "UTF-8"
  txt
}

## ============================================================ ##
## Normalization -- the part that keeps the weekly diff quiet   ##
## ============================================================ ##

# Absolutize a possibly-relative href against the page it was found on,
# lowercase scheme+host, drop the fragment and default ports, drop
# cache-busting/tracking query params while KEEPING meaningful ones
# (?format=original, Google Drive's ?id=...), and sort the survivors. This
# is the single most important function for keeping the weekly diff quiet --
# an un-normalized WordPress URL set churns on ?ver=... alone. Idempotent:
# normalize_link_url(normalize_link_url(x, p), p) == normalize_link_url(x, p).
normalize_link_url <- function(href, page_url) {
  href <- trimws(href)
  if (!nzchar(href)) return(href)

  if (grepl("^//", href)) {
    scheme <- sub("^(https?):.*", "\\1", page_url, ignore.case = TRUE)
    href <- paste0(scheme, ":", href)
  } else if (grepl("^/", href)) {
    base <- regmatches(page_url, regexpr("^https?://[^/]+", page_url, ignore.case = TRUE))
    href <- paste0(base, href)
  } else if (!grepl("^https?://", href, ignore.case = TRUE)) {
    base_dir <- sub("[^/]*$", "", page_url)
    href <- paste0(base_dir, href)
  }

  href <- sub("#.*$", "", href) # fragments never affect what's downloaded

  m <- regmatches(href, regexec("^(https?)://([^/?]+)([^?]*)(?:\\?(.*))?$", href, ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(href) # malformed -- leave as-is rather than abort a whole run over one bad href

  scheme <- tolower(m[2])
  hostport <- m[3]
  path <- m[4]
  query <- m[5]

  host_parts <- strsplit(hostport, ":", fixed = TRUE)[[1]]
  host <- tolower(host_parts[1])
  port <- if (length(host_parts) > 1) host_parts[2] else NA_character_
  is_default_port <- (scheme == "http" && identical(port, "80")) || (scheme == "https" && identical(port, "443"))
  hostport_out <- if (is.na(port) || is_default_port) host else paste0(host, ":", port)

  if (!nzchar(path)) path <- "/"

  query_out <- ""
  if (!is.na(query) && nzchar(query)) {
    pairs <- strsplit(query, "&", fixed = TRUE)[[1]]
    pairs <- pairs[nzchar(pairs)]
    keys <- sub("=.*$", "", pairs)
    noise <- grepl("^(ver|v|utm_[a-z]+|fbclid|gclid|_gl|mc_cid|mc_eid|cache)$", keys, ignore.case = TRUE)
    pairs <- sort(pairs[!noise])
    if (length(pairs) > 0) query_out <- paste0("?", paste(pairs, collapse = "&"))
  }

  paste0(scheme, "://", hostport_out, path, query_out)
}

# The host a normalized url points at -- makes "moved to Drive" legible in
# the CSV at a glance, without needing to parse the url column by eye.
url_host <- function(url) {
  m <- regmatches(url, regexpr("^https?://([^/]+)", url, ignore.case = TRUE, perl = FALSE))
  sub("^https?://", "", m, ignore.case = TRUE)
}

# Strip tags, decode the handful of entities these pages actually use,
# collapse whitespace, and cap length so one runaway proximity match can't
# dump half a page into a CSV cell.
normalize_label <- function(txt, max_chars = 200) {
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&#8211;|&ndash;", "-", txt)
  txt <- gsub("&#8217;|&rsquo;", "'", txt)
  txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars - 3), "...")
  txt
}

## ============================================================ ##
## Read / write                                                 ##
## ============================================================ ##

# Mirrors build_manifest.R's read_manifest_csv() -- same column-typing and
# encoding rules, so the two CSVs behave identically under git/readr.
read_inventory <- function(path) {
  if (!file.exists(path)) {
    empty <- setNames(
      lapply(INVENTORY_COLS, function(x) character(0)),
      INVENTORY_COLS
    )
    return(tibble::as_tibble(empty))
  }
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

# Sorted deterministically so a diff is never reordering noise.
write_inventory <- function(df, path) {
  df <- df[, INVENTORY_COLS]
  df <- dplyr::arrange(df, source, page, label, dplyr::coalesce(collection, ""))
  readr::write_csv(df, path, na = "")
}

## ============================================================ ##
## Carrying first_seen forward                                  ##
## ============================================================ ##

# first_seen is set once, when an identity first appears, and never touched
# again -- deliberately NO last_seen column, which would change every run
# and turn this into a PR nobody reads every Monday. Call this after a
# watcher's raw output is assembled, before validating/diffing it.
carry_first_seen <- function(old, new, today = Sys.Date()) {
  old_key <- paste(old$source, old$page, old$label, old$collection, sep = "\r")
  new_key <- paste(new$source, new$page, new$label, new$collection, sep = "\r")
  idx <- match(new_key, old_key)
  carried <- old$first_seen[idx]
  new$first_seen <- ifelse(!is.na(idx) & !is.na(carried) & nzchar(carried), carried, as.character(today))
  new
}

## ============================================================ ##
## Diffing                                                       ##
## ============================================================ ##

diff_inventory <- function(old, new) {
  key <- function(df) paste(df$source, df$page, df$label, df$collection, sep = "\r")
  old_key <- key(old)
  new_key <- key(new)

  added <- new[!(new_key %in% old_key), , drop = FALSE]
  removed <- old[!(old_key %in% new_key), , drop = FALSE]

  changed <- list()
  for (k in intersect(old_key, new_key)) {
    o <- old[old_key == k, INVENTORY_COLS][1, ]
    n <- new[new_key == k, INVENTORY_COLS][1, ]
    # first_seen is carried forward by design -- never treat it changing
    # (or not) as a real diff; compare every OTHER column only.
    cmp_cols <- setdiff(INVENTORY_COLS, "first_seen")
    if (!identical(as.list(o[cmp_cols]), as.list(n[cmp_cols]))) {
      changed[[length(changed) + 1]] <- n
    }
  }
  changed_df <- if (length(changed) > 0) dplyr::bind_rows(changed) else new[0, INVENTORY_COLS]

  list(
    added = added, removed = removed, changed = changed_df,
    n = nrow(added) + nrow(removed) + nrow(changed_df)
  )
}

## ============================================================ ##
## Structural validation                                        ##
## ============================================================ ##
#
# Purely structural, same spirit as manifest_validate.R's validate_*()
# functions -- schema shape and internal consistency of the CANDIDATE file,
# not "does this scrape look trustworthy" (that's each watcher's own job,
# same as a resolver's own sanity checks -- see the file header).

validate_inventory <- function(new) {
  errors <- character(0)

  missing_cols <- setdiff(INVENTORY_COLS, names(new))
  if (length(missing_cols) > 0) {
    return(paste("inventory: missing columns:", paste(missing_cols, collapse = ", ")))
  }

  ident <- paste(new$source, new$page, new$label, new$collection, sep = "\r")
  dup <- duplicated(ident)
  if (any(dup)) {
    errors <- c(errors, paste(
      "inventory: duplicate identity (source/page/label/collection):",
      paste(unique(gsub("\r", " / ", ident[dup])), collapse = " | ")
    ))
  }

  if (any(is.na(new$url) | !nzchar(new$url))) {
    errors <- c(errors, "inventory: row(s) with empty url")
  }

  has_date <- !is.na(new$first_seen) & nzchar(new$first_seen)
  bad_dates <- new$first_seen[has_date & is.na(suppressWarnings(as.Date(new$first_seen)))]
  if (length(bad_dates) > 0) {
    errors <- c(errors, paste("inventory: malformed first_seen date(s):", paste(unique(bad_dates), collapse = ", ")))
  }

  errors
}
