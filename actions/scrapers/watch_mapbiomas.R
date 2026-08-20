# actions/scrapers/watch_mapbiomas.R
#
# Site-link inventory watcher for
# https://brasil.mapbiomas.org/downloads/estatisticas/ -- see
# actions/scripts/site_inventory.R's header for what this mechanism is and
# why it replaced the old docs_url-writing site watcher that used to live in
# resolve_mapbiomas.R (see that file's own header for what's left there:
# Dataverse only, this page no longer touches the manifest at all).
#
# Auto-discovered by build_manifest.R the same way resolve_<source>.R is:
# filename convention (watch_<source>.R -> watch_<source>()), no registry.
#
# ---- The page's own markup is not stable -- confirmed, not assumed -------
#
# A Wayback Machine capture from 2026-06-23 showed a completely different
# template running on this exact URL: an accordion layout
# (wp-block-aab-group-accordion / aagb_accordion_* classes), where a
# Collection 10.1 file was described in plain paragraph text with its DOI
# cited inline -- "BIOMAS, ESTADOS E MUNICÍPIOS (COLEÇÃO 10.1) – COBERTURA
# ... (DOI: https://doi.org/10.58053/MapBiomas/SJZOLT)" -- linking a Google
# Drive URL with NO collection number anywhere in the URL itself. The live
# page checked 2026-08-19 has zero accordion markup and zero DOI mentions:
# a full template rebuild happened in under two months. That is the same
# failure class that broke the original resolver (an unfollowed redirect),
# and it's why this file runs FOUR independent strategies and merges their
# output instead of betting on one selector. Fixtures for both templates
# live at tests/testthat/fixtures/site_html/mapbiomas_202606_accordion.html
# and mapbiomas_202608_table.html; test-site-inventory.R exercises each
# strategy against both directly (see mb_strategy_*() below) so a future
# "helpful" simplification back down to one selector fails loudly.
#
# 1. mb_strategy_filename()  -- MapBiomas's own "COL.N" filename convention.
# 2. mb_strategy_row_block() -- today's <tr class="dl-row"> table, incl.
#                                rows (e.g. Google-Drive-hosted) #1 can't see.
# 3. mb_strategy_proximity() -- "Coleção N" text within ~300 chars of ANY
#                                href, regardless of what markup wraps it.
#                                THE DURABLE BASELINE: the one strategy that
#                                matches BOTH the June accordion page and
#                                today's table page unmodified, because both
#                                literally contain "Coleção N" near the link.
# 4. mb_strategy_doi()       -- MapBiomas's stable DOI prefix
#                                (10.58053/MapBiomas/), template-independent
#                                by construction. Dormant today (current
#                                template cites no DOIs -- checked), but
#                                June's did, right in the link's description.
#
# Re-verify all four against the LIVE page at maintenance time, not just
# trust this comment -- given a confirmed full rebuild already happened once
# in under two months, treat every one of these patterns as perishable.
#
# Each mb_strategy_*() takes raw page HTML and returns a list of candidates
# -- list(label, collection, url, strategy, specificity) -- with no
# knowledge of fetching, normalization, or merging (those live in
# watch_mapbiomas() / site_inventory.R). Kept as standalone functions,
# deliberately not object-y "strategy classes", purely so
# tests/testthat/test-site-inventory.R can call each one directly against a
# saved fixture without needing to fake a network fetch.

## ============================================================ ##
## Strategy 1: filename regex over every .xlsx href             ##
## ============================================================ ##
## Misses Google-Drive-hosted rows (no collection token in a Drive URL).

mb_strategy_filename <- function(html) {
  hrefs <- unique(regmatches(html, gregexpr('href="[^"]*\\.xlsx"', html, ignore.case = TRUE))[[1]])
  hrefs <- sub('^href="', "", hrefs)
  hrefs <- sub('"$', "", hrefs)
  out <- list()
  for (href in hrefs) {
    m <- regmatches(href, regexpr("COL\\.?\\s*([0-9]+(?:\\.[0-9]+)?)", href, ignore.case = TRUE))
    if (length(m) && nzchar(m)) {
      col <- sub(".*?([0-9]+(?:\\.[0-9]+)?)$", "\\1", m)
      out[[length(out) + 1]] <- list(
        label = basename(href), collection = col, url = href,
        strategy = "filename", specificity = 1
      )
    }
  }
  out
}

## ============================================================ ##
## Strategy 2: today's structured markup                        ##
## ============================================================ ##
## <tr class="dl-row"> blocks, each row-scoped so its collection tag, name,
## and link can never come from two different rows.

mb_strategy_row_block <- function(html) {
  blocks <- regmatches(html, gregexpr('<tr class="dl-row"[\\s\\S]*?</tr>', html, perl = TRUE))[[1]]
  out <- list()
  for (block in blocks) {
    tag_m <- regmatches(block, regexpr('dl-tag">\\s*Cole[çc][ãa]o\\s*([0-9]+(?:\\.[0-9]+)?)', block, ignore.case = TRUE, perl = TRUE))
    href_m <- regmatches(block, regexpr('class="dl-act"[^>]*href="([^"]+)"', block, ignore.case = TRUE))
    name_m <- regmatches(block, regexpr('dl-row__nome">([^<]*)<', block, ignore.case = TRUE))
    if (length(tag_m) && nzchar(tag_m) && length(href_m) && nzchar(href_m)) {
      col <- sub(".*?([0-9]+(?:\\.[0-9]+)?)$", "\\1", tag_m)
      link <- sub('.*href="([^"]+)".*', "\\1", href_m)
      label <- if (length(name_m) && nzchar(name_m)) sub('.*">([^<]*)<', "\\1", name_m) else basename(link)
      out[[length(out) + 1]] <- list(
        label = label, collection = col, url = link,
        strategy = "row_block", specificity = 3
      )
    }
  }
  out
}

## ============================================================ ##
## Strategy 3: template-agnostic proximity scan                 ##
## ============================================================ ##
## For every href on the page, look at a window of surrounding RAW text for
## "Coleção N"/"Collection N", regardless of what markup wraps it. This is
## what would have matched BOTH the June accordion page and today's table
## page unmodified -- the durable baseline, treat 1/2 as corroboration.
##
## Deliberately skips doi.org hrefs: a DOI citation link sits in the SAME
## "Coleção N" text as the real download link it's citing (see the June
## fixture, where both hrefs live in one paragraph), so without this filter
## every citation produces a redundant near-duplicate candidate alongside
## the real download -- and Strategy 4 already owns DOI mentions on their
## own terms. This keeps Strategy 3 focused on actual download links.

mb_strategy_proximity <- function(html, window_chars = 300) {
  href_matches <- gregexpr('href="[^"]+"', html)[[1]]
  if (href_matches[1] == -1) return(list())
  href_lens <- attr(href_matches, "match.length")
  out <- list()
  for (i in seq_along(href_matches)) {
    pos <- href_matches[i]
    len <- href_lens[i]
    href_raw <- substr(html, pos, pos + len - 1)
    link <- sub('^href="', "", href_raw)
    link <- sub('"$', "", link)
    if (grepl("^https?://(?:dx\\.)?doi\\.org/", link, ignore.case = TRUE)) next
    window_start <- max(1, pos - window_chars)
    window_end <- min(nchar(html), pos + len + window_chars)
    window <- substr(html, window_start, window_end)
    m <- regmatches(window, regexpr("Cole[çc][ãa]o\\s*([0-9]+(?:\\.[0-9]+)?)", window, ignore.case = TRUE))
    if (length(m) && nzchar(m)) {
      col <- sub(".*?([0-9]+(?:\\.[0-9]+)?)$", "\\1", m)
      out[[length(out) + 1]] <- list(
        label = normalize_label(window), collection = col, url = link,
        strategy = "proximity", specificity = 2
      )
    }
  }
  out
}

## ============================================================ ##
## Strategy 4: opportunistic DOI-mention scan                   ##
## ============================================================ ##
## Template-independent by construction -- a DOI doesn't care what HTML
## wraps it. Dormant today (current template cites no DOIs at all), but
## June's page did, right in the link's own description text.

mb_strategy_doi <- function(html) {
  doi_matches <- gregexpr("10\\.58053/MapBiomas/[A-Za-z0-9]+", html)[[1]]
  if (doi_matches[1] == -1) return(list())
  doi_lens <- attr(doi_matches, "match.length")
  dois <- unique(mapply(function(p, l) substr(html, p, p + l - 1), doi_matches, doi_lens))
  out <- list()
  for (doi in dois) {
    # If the DOI is itself the href of a link, use that link's own target;
    # otherwise it's plain-text-cited and the DOI resolver URL IS the link.
    href_m <- regmatches(html, regexpr(paste0('href="[^"]*', doi, '[^"]*"'), html, fixed = FALSE))
    link <- if (length(href_m) && nzchar(href_m)) {
      sub('^href="', "", sub('"$', "", href_m))
    } else {
      paste0("https://doi.org/", doi)
    }
    out[[length(out) + 1]] <- list(
      label = paste("DOI", doi), collection = NA_character_, url = link,
      strategy = "doi", specificity = 4
    )
  }
  out
}

## ============================================================ ##
## Merge: dedup candidates sharing (collection, normalized url) ##
## ============================================================ ##
## Keep the highest-specificity hit. Ties in practice only occur between
## 1/2/3 (a DOI url essentially never coincides with a download url): #2's
## structured markup outranks #3's durable-but-looser proximity match,
## which outranks #1's bare filename regex; #4 is highest because when a
## DOI mention DOES land on the same url as another strategy, that's the
## strongest possible corroboration available.

mb_merge_candidates <- function(candidates, page_url) {
  if (length(candidates) == 0) return(list())
  key <- vapply(
    candidates,
    function(c) paste(c$collection, normalize_link_url(c$url, page_url), sep = "\r"),
    character(1)
  )
  best <- list()
  for (i in seq_along(candidates)) {
    k <- key[i]
    if (is.null(best[[k]]) || candidates[[i]]$specificity > best[[k]]$specificity) {
      best[[k]] <- candidates[[i]]
    }
  }
  best
}

## ============================================================ ##
## The watcher itself                                           ##
## ============================================================ ##

watch_mapbiomas <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("watch_mapbiomas() needs 'curl' (CI-only; not a package Import).")
  }

  site_url <- "https://brasil.mapbiomas.org/downloads/estatisticas/"
  html <- fetch_page(site_url) # throws on fetch/HTTP failure -- that's a real watcher failure, let it propagate

  all_candidates <- c(
    mb_strategy_filename(html),
    mb_strategy_row_block(html),
    mb_strategy_proximity(html),
    mb_strategy_doi(html)
  )

  ## ---- Total-miss guard -----------------------------------------------------
  ## The page fetched fine (fetch_page() would have thrown otherwise) but if
  ## EVERY strategy found nothing, that's not "nothing new to report" -- there
  ## is no second mechanism left to fall back on (unlike the old design, where
  ## a Dataverse sub-resolver could still let good rows through). A backstop,
  ## deliberately looser than any real strategy, only changes the MESSAGE
  ## (page looks MapBiomas-shaped but unparseable vs. genuinely unrecognizable)
  ## -- either way this is a real watcher failure: stop().
  if (length(all_candidates) == 0) {
    backstop_hit <- grepl("mapbiomas|Cole[çc][ãa]o|Collection|\\.xlsx|drive\\.google", html, ignore.case = TRUE)
    stop(
      "watch_mapbiomas(): fetched ", site_url, " successfully (HTTP 200) but ",
      "extracted ZERO links across all four strategies (filename COL-regex, ",
      "dl-row/dl-tag blocks, proximity 'Coleção N' scan, DOI scan). ",
      if (backstop_hit) {
        "The page still contains MapBiomas-shaped content (a 'mapbiomas'/'Coleção'/'.xlsx'/'drive.google' token), so it likely restructured again in a way none of these patterns match."
      } else {
        "The page contains NOTHING recognizable at all -- not even a bare 'mapbiomas'/'.xlsx' token -- so it has almost certainly been rebuilt from scratch."
      },
      " Re-verify the page by hand before assuming this is transient."
    )
  }

  best <- mb_merge_candidates(all_candidates, site_url)
  new_rows <- dplyr::bind_rows(lapply(best, function(c) {
    url_norm <- normalize_link_url(c$url, site_url)
    tibble::tibble(
      source = "mapbiomas", page = site_url,
      label = normalize_label(c$label), collection = c$collection,
      url = url_norm, host = url_host(url_norm), strategy = c$strategy
    )
  }))

  ## ---- Shrink guard ----------------------------------------------------------
  ## `rows` is this source's PREVIOUSLY COMMITTED inventory rows (same
  ## convention as resolve_<source>(rows) receiving its owned manifest rows).
  ## If the new scrape found fewer than half of what was there last time for
  ## THIS page, that is a scrape regression, not evidence the site removed
  ## most of its links -- mirrors validate_row_count()'s posture on the
  ## manifest itself. Never write a shrunk snapshot; stop() instead.
  prev_for_page <- rows[!is.na(rows$page) & rows$page == site_url, ]
  if (nrow(prev_for_page) > 0 && nrow(new_rows) < ceiling(nrow(prev_for_page) * 0.5)) {
    stop(
      "watch_mapbiomas(): link count for ", site_url, " dropped from ",
      nrow(prev_for_page), " to ", nrow(new_rows), " (more than 50% fewer) ",
      "in one run. Treating this as a scrape failure, not a real mass ",
      "removal -- re-verify the page by hand before trusting this count."
    )
  }

  new_rows
}
