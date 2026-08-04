# actions/scrapers/resolve_seeg.R
#
# SEEG's own dados/ page (seeg.eco.br/dados/) links a versioned XLSX
# directly in static HTML -- no API, nothing rendered client-side. VERIFIED
# LIVE (2026-08-04): a plain GET returns HTTP 403 without a browser-like
# User-Agent; WITH one it returns 200 and links
# "Dados-municipais-resumido-CO2e-GWP-AR5-13.0.xlsx" (258 MB).
#
# This is a DETECT-ONLY resolver: it never touches `url`. load_seeg()
# (R/seeg.R:129) hardcodes the sheet name "BD GEE Municipios GWP-AR5" and a
# fixed column range (x2000:x2018), verified against the Google Drive file
# the manifest still points at -- migrating to the new v13.0 municipal file
# means re-verifying those against its actual layout first (a 258MB
# download), which is out of scope for this CI script.
#
# Emitting only `version` + `docs_url` on the "seeg" survey-default row
# means: the FIRST run after a real SEEG release opens exactly one Tier B
# PR recording the new version number (classify_row_change() treats any
# `version` change as Tier B, see actions/scripts/manifest_validate.R).
# Every subsequent run is a no-op until the number changes again -- a
# resolver that emitted `url` here without R/seeg.R being ready to read it
# would instead open an IDENTICAL Tier B PR every single scheduled run.

resolve_seeg <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_seeg() needs the 'curl' package (CI-only; not a package Import).")
  }

  h <- curl::new_handle(
    timeout = 30,
    useragent = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    )
  )
  resp <- tryCatch(curl::curl_fetch_memory("https://seeg.eco.br/dados/", handle = h), error = function(e) NULL)

  if (is.null(resp) || resp$status_code != 200) {
    stop(
      "resolve_seeg(): could not fetch https://seeg.eco.br/dados/ (",
      if (is.null(resp)) "request failed" else paste("HTTP", resp$status_code), "). ",
      "This page returns 403 to requests without a browser User-Agent -- ",
      "confirm the User-Agent set in this resolver still works before ",
      "assuming the site itself changed."
    )
  }

  html <- rawToChar(resp$content)
  Encoding(html) <- "UTF-8"

  hit <- regmatches(html, regexpr("Dados-municipais-resumido-CO2e-GWP-AR5-([0-9.]+)\\.xlsx", html))
  if (length(hit) == 0 || !nzchar(hit)) {
    stop(
      "resolve_seeg(): no 'Dados-municipais-resumido-CO2e-GWP-AR5-<version>.xlsx' ",
      "link found on seeg.eco.br/dados/. Site layout may have changed."
    )
  }

  version <- sub(".*-([0-9.]+)\\.xlsx$", "\\1", hit)

  tibble::tibble(
    survey = "seeg", dataset = NA_character_,
    geo_level = NA_character_, year = NA_character_,
    version = version, docs_url = "https://seeg.eco.br/dados/"
  )
}
