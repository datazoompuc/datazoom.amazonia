# actions/scrapers/resolve_mapbiomas.R
#
# MapBiomas is split across three genuinely different mechanisms, mirrored
# here as three independent, best-effort sub-resolvers. A failure/no-match
# in one never blocks the others -- each simply omits the rows it could not
# confidently resolve, which build_manifest.R's merge logic already treats
# as "leave those rows unchanged" (see actions/scripts/build_manifest.R).
#
#   (a) Google Cloud Storage JSON listing API (storage.googleapis.com) --
#       NOT scraping, a documented public API. Used to discover whether a
#       newer, fully-populated collection exists for the Brazil-wide
#       coverage/deforestation statistics.
#   (b) WordPress HTML scraping (brasil.mapbiomas.org/estatisticas/) -- the
#       one genuine HTML-scraping case here: readLines-equivalent fetch +
#       regex over <a href="...xlsx"> links, classified by filename
#       keyword. This is the real thing the whole "where is scraping"
#       question was about.
#   (c) Legacy S3 bucket (mapbiomas-br-site.s3.amazonaws.com) for
#       irrigation/water -- intentionally NOT touched here (resolver stays
#       NA on those rows in the manifest); nothing on that host indicates
#       version or freshness, so it is HTTP-checked only, never resolved.
#
# Verified live against the real site while writing this (2026-08-03): the
# WordPress page has been restructured since the hardcoded URLs were
# written. `mapbiomas_mining` genuinely moved from COL8.0 to COL9.0 -- a
# real, detectable update. `mapbiomas_cover`/`mapbiomas_transition`'s old
# single-Brazil-wide-file URLs have no unambiguous replacement anymore (the
# site now serves per-region files instead), so this resolver deliberately
# does NOT guess a replacement for those -- it omits them rather than risk
# writing a wrong URL. A human has to look at that one.

`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_mapbiomas <- function(rows) {
  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_mapbiomas() needs the 'jsonlite' and 'curl' packages (CI-only; not package Imports).")
  }

  out <- list()

  ## -- (b) WordPress scraping: real HTML, real regex ------------------------

  wp_html <- tryCatch({
    resp <- curl::curl_fetch_memory(
      "https://brasil.mapbiomas.org/estatisticas/",
      handle = curl::new_handle(timeout = 30)
    )
    if (resp$status_code != 200) stop("HTTP ", resp$status_code)
    txt <- rawToChar(resp$content)
    Encoding(txt) <- "UTF-8"
    txt
  }, error = function(e) {
    message("resolve_mapbiomas(): could not fetch the WordPress statistics page: ", conditionMessage(e))
    NULL
  })

  if (!is.null(wp_html)) {
    hrefs <- unique(regmatches(wp_html, gregexpr('href="[^"]*\\.xlsx"', wp_html, ignore.case = TRUE))[[1]])
    hrefs <- sub('^href="', "", hrefs)
    hrefs <- sub('"$', "", hrefs)

    # mining: single Brazil-wide file, collection number embedded in the
    # name itself (TABELA-MINERACAO-MAPBIOMAS-COL9.0.xlsx) -- both
    # geo_level rows (municipality, indigenous_land) share this one URL.
    mining_hit <- grep("TABELA-MINERACAO-MAPBIOMAS-COL", hrefs, value = TRUE, ignore.case = TRUE)
    if (length(mining_hit) >= 1) {
      col <- stringr::str_match(mining_hit[1], "COL([0-9]+)\\.")[, 2]
      # version is written ONLY on mining_base, not on the two override rows
      # below -- they already inherit it via the 5-tier lookup (R/manifest.R),
      # and writing it explicitly here made every run re-add a value
      # test-manifest-schema.R's anti-duplication check (correctly) strips
      # back out, an infinite Tier-B-PR loop caught live 2026-08-07. url stays
      # on both override rows since mining_base intentionally carries none.
      out$mining_municipality <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_mining",
        geo_level = "municipality", year = NA_character_,
        url = mining_hit[1]
      )
      out$mining_indigenous <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_mining",
        geo_level = "indigenous_land", year = NA_character_,
        url = mining_hit[1]
      )
      out$mining_base <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_mining",
        geo_level = NA_character_, year = NA_character_,
        version = col
      )
    }

    # cover (indigenous_land) / transition (biome): only touched if the
    # OLD single-file naming still exists on the page. As of this writing
    # it does not (site restructured to per-region files) -- see the file
    # header above. Left in so this starts working again automatically
    # the day MapBiomas republishes a matching Brazil-wide file.
    indigenous_hit <- grep("INDIGENOUS_LANDS", hrefs, value = TRUE, ignore.case = TRUE)
    biomes_hit <- grep("COL\\.[0-9]+-BIOMES", hrefs, value = TRUE, ignore.case = TRUE)

    if (length(indigenous_hit) >= 1) {
      out$cover_indigenous <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_cover",
        geo_level = "indigenous_land", year = NA_character_,
        url = indigenous_hit[1]
      )
    }
    if (length(biomes_hit) >= 1) {
      out$transition_biome <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_transition",
        geo_level = "biome", year = NA_character_,
        url = biomes_hit[1]
      )
    }
  }

  ## -- (a) GCS JSON listing: discover a newer, FULLY POPULATED collection --

  gcs_list <- function(prefix, delimiter = "/") {
    url <- paste0(
      "https://storage.googleapis.com/storage/v1/b/mapbiomas-public/o",
      "?prefix=", utils::URLencode(prefix, reserved = TRUE),
      "&delimiter=", utils::URLencode(delimiter, reserved = TRUE)
    )
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 30)), error = function(e) NULL)
    if (is.null(resp) || resp$status_code != 200) return(NULL)
    jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  }

  # Guards against unrelated folders (e.g. "collection_71", a soil-beta
  # project observed live in this bucket, numerically far ahead of the
  # real land-use collection number) being mistaken for "the latest
  # collection" -- only look a few numbers ahead of what we already have,
  # and only accept a candidate if its statistics/ folder actually has a
  # "coverage" file in it (collection_10 was live but still EMPTY when
  # this was written -- correctly produces no match, not a wrong one).
  find_newer_gcs_collection <- function(current = 9L, look_ahead = 5L) {
    base <- gcs_list("initiatives/brasil/")
    if (is.null(base)) return(NULL)
    prefixes <- unlist(base$prefixes %||% list())
    candidates <- prefixes[grepl("^initiatives/brasil/collection_[0-9]+/$", prefixes)]
    nums <- as.integer(sub(".*collection_([0-9]+)/$", "\\1", candidates))
    nums <- nums[!is.na(nums) & nums > current & nums <= current + look_ahead]
    if (length(nums) == 0) return(NULL)

    for (n in sort(nums, decreasing = TRUE)) {
      listing <- gcs_list(paste0("initiatives/brasil/collection_", n, "/statistics/"), delimiter = "")
      names_ <- vapply(listing$items %||% list(), function(x) x$name, character(1))
      if (any(grepl("coverage", names_, ignore.case = TRUE))) {
        return(list(n = n, names = names_))
      }
    }
    NULL
  }

  newer <- tryCatch(find_newer_gcs_collection(), error = function(e) {
    message("resolve_mapbiomas(): GCS collection lookup failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(newer)) {
    cover_file <- grep("coverage", newer$names, value = TRUE, ignore.case = TRUE)
    if (length(cover_file) >= 1) {
      out$cover_base <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_cover",
        geo_level = NA_character_, year = NA_character_,
        url = paste0("https://storage.googleapis.com/mapbiomas-public/", cover_file[1]),
        version = as.character(newer$n)
      )
    }
    defor_file <- grep("deforestation", newer$names, value = TRUE, ignore.case = TRUE)
    if (length(defor_file) >= 1) {
      out$deforestation_regeneration <- tibble::tibble(
        survey = "mapbiomas", dataset = "mapbiomas_deforestation_regeneration",
        geo_level = NA_character_, year = NA_character_,
        url = paste0("https://storage.googleapis.com/mapbiomas-public/", defor_file[1]),
        version = as.character(newer$n)
      )
    }
  }

  if (length(out) == 0) {
    stop(
      "resolve_mapbiomas(): found no confidently-matched update on either the ",
      "WordPress page or the GCS bucket. This can legitimately mean 'nothing ",
      "changed' -- check manually before treating it as a scraper break."
    )
  }

  dplyr::bind_rows(out)
}
