# actions/scrapers/resolve_ips.R
#
# ipsamazonia.org.br is a React single-page app -- its HTML shell (~1.3KB)
# has no data links at all. VERIFIED LIVE (2026-08-04): the app's own JS
# bundle (linked from the shell as /static/js/main.<hash>.js) hardcodes the
# current workbook's URL as a literal string:
#   painel.ipsamazonia.org.br/uploads/IPS_Amazonia_<year>_<hash>.xlsx
# (painel.ipsamazonia.org.br is the Strapi CMS backing the site). Two
# static GETs -- the HTML shell, then the JS bundle it points at -- no
# browser/headless rendering needed to reach it.
#
# DETECT-ONLY, same reasoning as resolve_seeg.R: load_ips() (R/ips.R:104)
# reads FOUR year-sheets ("2014", "2018", "2021", "2023") out of one Google
# Drive workbook; the Strapi file found here is a single year's workbook, a
# different shape. Emits only `version` (the year found) + `docs_url`,
# fanned out across every IPS dataset row in `rows` (the manifest rows
# tagged resolver == "ips") -- never `url`, same reasoning as
# resolve_seeg.R for why a detect-only resolver must not emit a `url` the
# package isn't ready to read yet.

resolve_ips <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_ips(): no manifest rows tagged resolver == 'ips' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_ips() needs the 'curl' package (CI-only; not a package Import).")
  }

  fetch <- function(url, timeout_s = 30) {
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout_s, followlocation = TRUE)),
      error = function(e) NULL
    )
    if (is.null(resp) || resp$status_code != 200) {
      return(NULL)
    }
    txt <- rawToChar(resp$content)
    Encoding(txt) <- "UTF-8"
    txt
  }

  shell <- fetch("https://ipsamazonia.org.br/")
  if (is.null(shell)) {
    stop("resolve_ips(): could not fetch https://ipsamazonia.org.br/.")
  }

  bundle_hit <- regmatches(shell, regexpr('src="(/static/js/main\\.[a-f0-9]+\\.js)"', shell))
  if (length(bundle_hit) == 0 || !nzchar(bundle_hit)) {
    stop(
      "resolve_ips(): could not find a /static/js/main.<hash>.js bundle ",
      "reference in ipsamazonia.org.br's HTML shell. Site build may have ",
      "changed."
    )
  }
  bundle_path <- sub('^src="', "", bundle_hit)
  bundle_path <- sub('"$', "", bundle_path)

  bundle <- fetch(paste0("https://ipsamazonia.org.br", bundle_path))
  if (is.null(bundle)) {
    stop("resolve_ips(): could not fetch the JS bundle at ", bundle_path, ".")
  }

  hit <- regmatches(
    bundle,
    regexpr("painel\\.ipsamazonia\\.org\\.br/uploads/IPS_Amazonia_([0-9]{4})_[a-f0-9]+\\.xlsx", bundle)
  )
  if (length(hit) == 0 || !nzchar(hit)) {
    stop(
      "resolve_ips(): no 'IPS_Amazonia_<year>_<hash>.xlsx' reference found ",
      "in the JS bundle. Site build may have changed."
    )
  }

  year <- sub(".*IPS_Amazonia_([0-9]{4})_.*", "\\1", hit)

  tibble::tibble(
    survey = "ips", dataset = unique(rows$dataset),
    geo_level = NA_character_, year = NA_character_,
    version = year, docs_url = "https://ipsamazonia.org.br/"
  )
}
