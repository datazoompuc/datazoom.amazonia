# actions/scrapers/resolve_epe.R
#
# EPE's site (epe.gov.br) is a SharePoint-backed site ("sites-pt", document
# library folders named "PublicacoesArquivos/publicacao-N/topico-N/..." are
# SharePoint's own internal naming). The plan for this resolver was to
# readLines() the BEN publications landing page and regex for
# `PublicacoesArquivos/publicacao-\d+/topico-\d+/[^"]+\.xlsx` links.
#
# VERIFIED LIVE while writing this (2026-08-03) that this does NOT work:
# every candidate landing page we could find
# (.../publicacoes/balanco-energetico-nacional,
# .../publicacoes/balanco-energetico-nacional-2024,
# .../publicacoes-dados-abertos/dados-abertos, .../publicacoes) returns a
# 200 with a full HTML page, but the ONLY ".xlsx" string present on any of
# them is a generic client-side script ("forca download de arquivos
# office") that applies to `a[href]` elements added to the DOM -- the
# actual document links are rendered by a SharePoint document-library web
# part AFTER page load (client-side/AJAX), not present in the HTML
# `readLines()`/`curl::curl_fetch_memory()` ever sees.
#
# A `readLines()` + regex resolver -- the same mechanism that works for
# MapBiomas's WordPress page -- genuinely cannot reach these links. Doing
# so would require either reverse-engineering the SharePoint REST API
# (`_api/web/GetFolderByServerRelativeUrl(...)`) or a headless browser,
# both out of scope for this lightweight CI resolver.
#
# Rather than fabricate a match, this resolver documents the finding and
# fails cleanly: build_manifest.R records it as a failed resolver, the EPE
# rows in the manifest are left exactly as they are (hand-maintained), and
# nothing downstream breaks. This is the safety property the whole
# architecture is built around -- see WEBSCRAPING_BETA_REPORT.pdf.
#
# `consumer_energy_consumption`/`industrial_energy_consumption` (the
# version-free Dados_abertos_Consumo_Mensal.xlsx) are intentionally not
# attempted here either -- there is no version token to resolve, only an
# HTTP-check, which the validation gate already does for every changed row.

resolve_epe <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_epe() needs the 'curl' package (CI-only; not a package Import).")
  }

  candidate_pages <- c(
    "https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/balanco-energetico-nacional",
    "https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes",
    "https://www.epe.gov.br/pt/publicacoes-dados-abertos/dados-abertos"
  )

  link_pattern <- "PublicacoesArquivos/publicacao-[0-9]+/topico-[0-9]+/[^\"]+\\.xlsx"
  found <- character(0)

  for (page in candidate_pages) {
    html <- tryCatch({
      resp <- curl::curl_fetch_memory(page, handle = curl::new_handle(timeout = 20, followlocation = TRUE))
      if (resp$status_code != 200) NULL else {
        txt <- rawToChar(resp$content)
        Encoding(txt) <- "UTF-8"
        txt
      }
    }, error = function(e) NULL)

    if (is.null(html)) next
    hits <- regmatches(html, gregexpr(link_pattern, html, ignore.case = TRUE))[[1]]
    found <- c(found, hits)
  }

  found <- unique(found)

  if (length(found) == 0) {
    stop(
      "resolve_epe(): none of the candidate EPE landing pages exposed a ",
      "PublicacoesArquivos/publicacao-N/topico-N/*.xlsx link in static HTML. ",
      "This site serves its document library via a client-side SharePoint ",
      "web part, not in the HTML a simple GET/readLines() sees -- confirmed ",
      "by inspecting the fetched pages directly. A working resolver here ",
      "would need the SharePoint REST API or a headless browser; both are ",
      "out of scope for this CI script. The EPE manifest rows are left ",
      "untouched (hand-maintained)."
    )
  }

  ben_hit <- grep("Anexo", found, value = TRUE, ignore.case = TRUE)
  panel_hit <- grep("Cap.tulo", found, value = TRUE, ignore.case = TRUE, perl = TRUE)

  out <- list()
  if (length(ben_hit) >= 1) {
    years <- stringr::str_match(ben_hit[1], "([0-9]{4})\\s*a\\s*([0-9]{4})")
    out$national_energy_balance <- tibble::tibble(
      survey = "epe", dataset = "national_energy_balance",
      geo_level = NA_character_, year = NA_character_,
      link = paste0("https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/publicacoes/", ben_hit[1]),
      available_time = if (!is.na(years[1, 2])) paste(years[1, 2], years[1, 3], sep = "-") else NA_character_
    )
  }
  if (length(panel_hit) >= 1) {
    out$energy_state_panel <- tibble::tibble(
      survey = "epe", dataset = "energy_state_panel",
      geo_level = NA_character_, year = NA_character_,
      link = paste0("https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/publicacoes/", panel_hit[1])
    )
  }

  dplyr::bind_rows(out)
}
