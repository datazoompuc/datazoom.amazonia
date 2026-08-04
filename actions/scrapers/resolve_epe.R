# actions/scrapers/resolve_epe.R
#
# EPE's site mixes two genuinely different serving mechanisms, and the
# previous version of this file (2026-08-03) mistakenly generalized from one
# to the other. Both were re-verified live on 2026-08-04:
#
#   - /pt/publicacoes-dados-abertos/dados-abertos/* pages (the "Dados
#     Abertos" section) serve their document link directly in the static
#     HTML a plain GET sees. dashboard.epe.gov.br (a separate host, EPE's
#     online book reader) does too, for every chapter/annex checked.
#   - /pt/publicacoes-dados-abertos/publicacoes/balanco-energetico-nacional-ben
#     (the "Publicacoes" section) genuinely is SharePoint-backed: every
#     candidate landing page on that path returns full HTML with no .xlsx
#     string except a generic client-side "force download" script that
#     applies to <a> elements injected AFTER page load by a document-library
#     web part -- confirmed by fetching those pages directly, the actual
#     links are simply not present in what curl/readLines() sees. That
#     finding stands; it's just not the whole EPE site, and it no longer
#     matters for national_energy_balance -- see below.
#
# `energy_state_panel` is intentionally NOT resolved (see its own section).

resolve_epe <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_epe() needs the 'curl' package (CI-only; not a package Import).")
  }

  fetch_html <- function(url, timeout_s = 20) {
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

  out <- list()

  ## -- consumer/industrial energy consumption: one shared file, static HTML ----
  # VERIFIED LIVE (2026-08-04): this "Dados Abertos" page links
  # Dados_abertos_Consumo_Mensal.xlsx directly in the HTML -- no SharePoint
  # web part involved, unlike the "Publicacoes" pages above.

  consumo_html <- fetch_html(
    "https://www.epe.gov.br/pt/publicacoes-dados-abertos/dados-abertos/dados-do-consumo-mensal-de-energia-eletrica"
  )
  if (!is.null(consumo_html)) {
    hit <- regmatches(
      consumo_html,
      regexpr('href="[^"]*Dados_abertos_Consumo_Mensal\\.xlsx"', consumo_html, ignore.case = TRUE)
    )
    if (length(hit) > 0 && nzchar(hit)) {
      href <- sub('^href="', "", hit)
      href <- sub('"$', "", href)
      url <- paste0("https://www.epe.gov.br", href)

      out$consumer <- tibble::tibble(
        survey = "epe", dataset = "consumer_energy_consumption",
        geo_level = NA_character_, year = NA_character_, url = url
      )
      out$industrial <- tibble::tibble(
        survey = "epe", dataset = "industrial_energy_consumption",
        geo_level = NA_character_, year = NA_character_, url = url
      )
    }
  }

  ## -- national_energy_balance: the BEN dashboard's consolidated table ---------
  # VERIFIED LIVE (2026-08-04): the BEN "Dados Abertos" page's only data
  # link is dashboard.epe.gov.br/apps/livro-ben/#anexo, labelled "Anexos IX.
  # Balancos Energeticos Consolidados desde 1970 ...". That dashboard's own
  # anexo_9.html (Anexo IX) links BOTH 56 one-file-per-year workbooks AND a
  # single VERSION-FREE, already-tidy table
  # (dados/tabela_balanco_energitico_consolidado.xlsx: columns
  # grupo/tipo/fonte/ano/valor, 1970-2025, 73696 rows) -- downloaded and
  # inspected directly. R/epe.R now reads that table straight (see its
  # header comment), so this resolver only needs to (a) confirm the table's
  # link still exists on the page, and (b) read the newest per-year
  # filename to refresh available_time -- the table itself carries no
  # version stamp in its URL to diff on.

  ben_html <- fetch_html("https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/anexo_9.html")
  if (!is.null(ben_html)) {
    has_table <- grepl(
      'href="dados/tabela_balanco_energitico_consolidado\\.xlsx"',
      ben_html,
      ignore.case = TRUE
    )
    year_hits <- regmatches(
      ben_html,
      gregexpr("matriz_balanco_energitico_consolidado_([0-9]{4})\\.xlsx", ben_html)
    )[[1]]
    years <- unique(as.integer(sub(".*_([0-9]{4})\\.xlsx$", "\\1", year_hits)))

    if (has_table && length(years) > 0) {
      out$ben <- tibble::tibble(
        survey = "epe", dataset = "national_energy_balance",
        geo_level = NA_character_, year = NA_character_,
        available_time = paste(min(years), max(years), sep = "-")
      )
    }
  }

  ## -- energy_state_panel: intentionally NOT attempted -------------------------
  # VERIFIED LIVE (2026-08-04) that the manifested URL
  # (".../publicacao-145/topico-515/Capitulo 8 (Dados Estaduais).xlsx") now
  # returns 404, and that no faithful replacement exists:
  #   - EPE restructured the Anuario Estatistico de Energia Eletrica from
  #     8+ chapters to 4 (dashboard.epe.gov.br/apps/anuario-livro); its
  #     stable workbook (anuario-workbook.xlsx -- also checked in the 2022
  #     and 2024 dated editions) has sheets named "Tabela X.Y", none
  #     numbered "8.x".
  #   - The closest surviving table, "Tabela 2.5 - Geracao eletrica por
  #     regiao e unidade da federacao (GWh)", is STATE TOTALS ONLY -- it
  #     drops the per-source breakdown (hidro/eolica/solar/nuclear/termo/
  #     .../diesel, 17 columns) that R/epe.R's energy_state_panel parser
  #     reads (R/epe.R:187-236).
  #   - "Dados brutos.xlsx", linked from the Anuario's own dados-abertos
  #     page, is CONSUMPTION data (consumers/consumption by state/sector/
  #     voltage tier) -- confirmed by downloading and inspecting it -- not
  #     generation, so it is not a candidate either.
  # Rather than silently narrow the dataset to state totals, this is left
  # for a human decision (see manifest_validate.R's is_http_exempt() /
  # build_manifest.R's --check-all, which keeps this row's 404 visible on
  # every scheduled run instead of it being noticed only when someone tries
  # load_epe("energy_state_panel")). The manifest's docs_url for this row
  # points at the Anuario publication page so a future maintainer starts
  # from the same place this investigation did.

  if (length(out) == 0) {
    stop(
      "resolve_epe(): neither the consumo-mensal page nor the BEN dashboard ",
      "yielded a confident match. EPE's site layout may have changed again."
    )
  }

  dplyr::bind_rows(out)
}
