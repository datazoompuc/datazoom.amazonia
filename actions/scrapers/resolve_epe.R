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
# `energy_state_panel` WAS intentionally left unresolved (its old URL was
# discontinued with no replacement found) until 2026-09-08, when a live
# re-investigation of the same dashboard.epe.gov.br BEN book
# national_energy_balance already reads found its Chapter 8 page serves a
# working substitute -- see that section below for what was verified.
#
# 2026-09-08 (audit follow-up): this resolver covers 3 independent
# sub-sources (consumo-mensal page, BEN Anexo IX, BEN Chapter 8) in one
# function, but used to signal failure only via one final
# `if (length(out) == 0) stop()` -- meaning any ONE block silently failing
# while the other two succeeded produced no error, no resolver_failed entry,
# and no PR/issue: the affected row(s) just went stale with zero visibility.
# (This is exactly how energy_state_panel's dead URL likely persisted for as
# long as it did.) Every block now tracks its own failure reason into
# `block_failures`; if some (not all) blocks fail, the successfully-resolved
# rows are still returned (never discarded -- that would reintroduce the
# "one broken thing blocks everyone else" bug the resolver_failed/exit-code
# machinery exists to prevent), but the failure list rides along as a
# `partial_failures` attribute build_manifest.R reads and reports
# separately. See that script's header for what it does with this.
#
# Also added: `national_energy_balance` and `energy_state_panel` now verify
# the candidate table's actual COLUMNS (via verify_table_columns() below),
# not just that its filename is still linked -- the "a matching sheet name
# isn't enough either" lesson from resolve_mapbiomas.R, applied here at
# much smaller scale (EPE's consolidated tables are tens of KB to a few MB,
# not MapBiomas's 78MB+ Dataverse candidates walked newest-first -- no
# verification cache is warranted for files this small and this un-walked).

resolve_epe <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_epe(): no manifest rows tagged resolver == 'epe' -- ",
      "cannot tell which rows to update. Check the resolver column."
    )
  }
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

  # Downloads a candidate .xlsx and checks its header row carries every
  # column in `required_cols` -- catching "the link still resolves but the
  # file was restructured" instead of only checking the filename. A genuine
  # download/parse failure THROWS rather than returning FALSE: conflating
  # "couldn't check" with "checked and it's wrong" is exactly the bug class
  # that once cascaded resolve_mapbiomas.R's newest-first walk to a wrong
  # candidate on a slow connection (see that resolver's own header) -- the
  # caller is expected to tell the two apart, not this function.
  verify_table_columns <- function(url, required_cols, timeout_s = 60) {
    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp), add = TRUE)
    resp <- tryCatch(
      curl::curl_fetch_disk(url, tmp, handle = curl::new_handle(timeout = timeout_s, followlocation = TRUE)),
      error = function(e) stop("download failed: ", conditionMessage(e))
    )
    if (resp$status_code != 200) {
      stop("download failed: HTTP ", resp$status_code)
    }
    header <- tryCatch(
      names(openxlsx::read.xlsx(tmp, sheet = 1, rows = 1)),
      error = function(e) stop("could not read the downloaded file as xlsx: ", conditionMessage(e))
    )
    all(required_cols %in% header)
  }

  out <- list()
  block_failures <- character(0)

  ## -- consumer/industrial energy consumption: one shared file, static HTML ----
  # VERIFIED LIVE (2026-08-04): this "Dados Abertos" page links
  # Dados_abertos_Consumo_Mensal.xlsx directly in the HTML -- no SharePoint
  # web part involved, unlike the "Publicacoes" pages above.
  #
  # Both datasets' base row AND their state/subsystem/region geo_level
  # overrides point at this one shared file -- under the old 5-tier model
  # the overrides left url NA and inherited it from the base row; under
  # self-sufficient rows there is no inheritance, so every one of those
  # rows must be written explicitly here or it would silently go stale the
  # next time this url actually changes (this is what write onto every
  # geo_level in `rows` below fixes).

  consumo_html <- fetch_html(
    "https://www.epe.gov.br/pt/publicacoes-dados-abertos/dados-abertos/dados-do-consumo-mensal-de-energia-eletrica"
  )
  if (is.null(consumo_html)) {
    block_failures <- c(
      block_failures,
      "consumer/industrial_energy_consumption: could not fetch the Dados Abertos consumption page"
    )
  } else {
    hit <- regmatches(
      consumo_html,
      regexpr('href="[^"]*Dados_abertos_Consumo_Mensal\\.xlsx"', consumo_html, ignore.case = TRUE)
    )
    if (length(hit) > 0 && nzchar(hit)) {
      href <- sub('^href="', "", hit)
      href <- sub('"$', "", href)
      url <- paste0("https://www.epe.gov.br", href)

      consumer_geo <- rows$geo_level[rows$dataset == "consumer_energy_consumption"]
      industrial_geo <- rows$geo_level[rows$dataset == "industrial_energy_consumption"]

      out$consumer <- tibble::tibble(
        survey = "epe", dataset = "consumer_energy_consumption",
        geo_level = consumer_geo, year = NA_character_, url = url
      )
      out$industrial <- tibble::tibble(
        survey = "epe", dataset = "industrial_energy_consumption",
        geo_level = industrial_geo, year = NA_character_, url = url
      )
    } else {
      block_failures <- c(
        block_failures,
        "consumer/industrial_energy_consumption: page fetched but Dados_abertos_Consumo_Mensal.xlsx link not found -- page may have restructured"
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
  # header comment), so this resolver needs to (a) confirm the table's link
  # still exists on the page, (b) read the newest per-year filename to
  # refresh available_time -- the table itself carries no version stamp in
  # its URL to diff on -- and (c, added 2026-09-08) actually verify the
  # table's columns, not just its filename.

  ben_html <- fetch_html("https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/anexo_9.html")
  if (is.null(ben_html)) {
    block_failures <- c(block_failures, "national_energy_balance: could not fetch the BEN Anexo IX page")
  } else {
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

    if (!has_table || length(years) == 0) {
      block_failures <- c(
        block_failures,
        "national_energy_balance: page fetched but the consolidated table link and/or yearly filenames were not found -- page may have restructured"
      )
    } else {
      cols_ok <- tryCatch(
        verify_table_columns(
          "https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/dados/tabela_balanco_energitico_consolidado.xlsx",
          c("grupo", "tipo", "fonte", "ano", "valor")
        ),
        error = function(e) {
          block_failures <<- c(
            block_failures,
            paste0("national_energy_balance: column verification failed -- ", conditionMessage(e))
          )
          NA
        }
      )
      if (isTRUE(cols_ok)) {
        out$ben <- tibble::tibble(
          survey = "epe", dataset = "national_energy_balance",
          geo_level = NA_character_, year = NA_character_,
          available_time = paste(min(years), max(years), sep = "-")
        )
      } else if (isFALSE(cols_ok)) {
        block_failures <- c(
          block_failures,
          "national_energy_balance: the consolidated table's columns no longer include grupo/tipo/fonte/ano/valor -- R/epe.R will need updating before this URL is safe to push"
        )
      }
      # cols_ok == NA means verify_table_columns() threw and its own
      # error() handler above already recorded the failure -- don't
      # double-record it here.
    }
  }

  ## -- energy_state_panel: BEN Chapter 8's consolidated generation table -------
  # VERIFIED LIVE (2026-09-08): the old URL
  # (".../publicacao-145/topico-515/Capitulo 8 (Dados Estaduais).xlsx") is
  # long dead, as recorded in NEWS.md's earlier "no faithful replacement
  # exists" entry -- two real candidates were ruled out then (the Anuario's
  # restructured "Tabela 2.5" is state-totals-only, no per-source breakdown;
  # "Dados brutos.xlsx" turned out to be consumption data, re-confirmed live
  # again this session by actually downloading and reading its real header:
  # TipoConsumidor/Setor Economico/Tipo Tensao/Consumidores/Consumo columns,
  # nothing about generation). What was NOT checked back then: the same
  # dashboard.epe.gov.br BEN "livro" this resolver's national_energy_balance
  # block already reads has its own Chapter 8 page,
  # dashboard.epe.gov.br/apps/livro-ben/livro/pt/capitulo_8.html, linking a
  # single already-tidy, version-free table --
  # dados/tabela_geracao_eletricidade_por_fonte.xlsx (macro_grupo/grupo/
  # fonte/ano/valor, long format, 6721 rows, 2011-2025) -- alongside 15
  # one-file-per-year workbooks, the same consolidated-vs-yearly split
  # national_energy_balance's own Anexo IX page has. Downloaded and
  # inspected directly: `grupo` carries all 26 states + DF + a "Brasil"
  # national total; `fonte` has exactly 16 values (a "Geracao total" plus 15
  # real sources) that map 1:1 onto the 16 non-uf/ano columns
  # energy_state_panel's treatment code already produces. R/epe.R now reads
  # this table long and pivots it -- see its own header comment for the
  # fonte->column map and the one capitalization fix it applies to 3 state
  # names ("Mato Grosso Do Sul" etc -- the source capitalizes the
  # connector, the old hand-parsed sheet didn't).

  panel_html <- fetch_html("https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/capitulo_8.html")
  if (is.null(panel_html)) {
    block_failures <- c(block_failures, "energy_state_panel: could not fetch the BEN Chapter 8 page")
  } else {
    has_table <- grepl(
      'href="dados/tabela_geracao_eletricidade_por_fonte\\.xlsx"',
      panel_html,
      ignore.case = TRUE
    )
    year_hits <- regmatches(
      panel_html,
      gregexpr("matriz_geracao_eletricidade_por_fonte_([0-9]{4})\\.xlsx", panel_html)
    )[[1]]
    years <- unique(as.integer(sub(".*_([0-9]{4})\\.xlsx$", "\\1", year_hits)))

    if (!has_table || length(years) == 0) {
      block_failures <- c(
        block_failures,
        "energy_state_panel: page fetched but the consolidated table link and/or yearly filenames were not found -- page may have restructured"
      )
    } else {
      cols_ok <- tryCatch(
        verify_table_columns(
          "https://dashboard.epe.gov.br/apps/livro-ben/livro/pt/dados/tabela_geracao_eletricidade_por_fonte.xlsx",
          c("macro_grupo", "grupo", "fonte", "ano", "valor")
        ),
        error = function(e) {
          block_failures <<- c(
            block_failures,
            paste0("energy_state_panel: column verification failed -- ", conditionMessage(e))
          )
          NA
        }
      )
      if (isTRUE(cols_ok)) {
        out$panel <- tibble::tibble(
          survey = "epe", dataset = "energy_state_panel",
          geo_level = NA_character_, year = NA_character_,
          available_time = paste(min(years), max(years), sep = "-")
        )
      } else if (isFALSE(cols_ok)) {
        block_failures <- c(
          block_failures,
          "energy_state_panel: the consolidated table's columns no longer include macro_grupo/grupo/fonte/ano/valor -- R/epe.R will need updating before this URL is safe to push"
        )
      }
    }
  }

  if (length(out) == 0) {
    stop(
      "resolve_epe(): none of the 3 EPE blocks (consumer/industrial ",
      "consumption, national_energy_balance, energy_state_panel) yielded ",
      "a confident match. Reasons:\n- ",
      paste(block_failures, collapse = "\n- ")
    )
  }

  result <- dplyr::bind_rows(out)
  if (length(block_failures) > 0) {
    # At least one block found nothing this run, but at least one other DID
    # succeed -- don't stop() here (that would also discard the rows that
    # DID resolve, reintroducing the exact "one broken thing blocks
    # everyone else" bug the resolver_failed/exit-code-11 machinery exists
    # to prevent at the whole-source level). Attach the failure(s) instead
    # as an attribute build_manifest.R reads and folds into a separate
    # resolver_partial_failed signal -- see that script's header.
    attr(result, "partial_failures") <- block_failures
  }
  result
}
