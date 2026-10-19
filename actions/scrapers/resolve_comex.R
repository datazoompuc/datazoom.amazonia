# actions/scrapers/resolve_comex.R
#
# COMEX (Brazil's foreign-trade statistics, ComexStat "dados abertos") is
# published as one bulk CSV per dataset per year, at predictable paths
#   <host>/balanca/bd/comexstat-bd/mun/{EXP,IMP}_<year>_MUN.csv   (by municipality + SH4)
#   <host>/balanca/bd/comexstat-bd/ncm/{EXP,IMP}_<year>.csv       (by NCM)
# and the manifest keeps them as `$year$` templates (R/download.R fills the
# placeholder with plain text replacement). So this resolver does URL probing,
# like resolve_baci.R, and what it keeps current is `available_time` (one
# range per dataset) and the host in `url`. Everything below was measured
# live on 2026-10-17 -- re-measure before trusting any of it:
#
# 1. THE HOST MOVED, AND THE OLD ONE'S CERTIFICATE EXPIRED. The ministry's own
#    open-data page (https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/
#    estatisticas/base-de-dados-bruta, "Estatisticas de Comercio Exterior em
#    Dados Abertos") links every file on balanca.mdic.gov.br. The manifest
#    still pointed at balanca.economia.gov.br, whose `*.economia.gov.br`
#    certificate expired 2026-10-03 23:59:59 GMT (still served on the 17th):
#    every verified HTTPS request failed. balanca.mdic.gov.br serves the SAME
#    files -- identical sizes and Last-Modified for the 2025 files of all four
#    datasets -- so COMEX_HOST is the new one. No fallback to the old host on
#    purpose: a transient failure of the new one would otherwise flip the
#    manifest back and forth and open a PR each time.
#
# 2. BOTH HOSTS SEND AN INCOMPLETE TLS CHAIN. They present only the leaf
#    certificate, not the intermediate CA ("AC SERPRO AR46 OV TLS CA 2025" for
#    mdic, Sectigo R36 for economia). Windows and browsers fetch a missing
#    intermediate themselves (AIA), so a Windows run is fine; libcurl+OpenSSL
#    with a Mozilla-style root bundle -- i.e. a GitHub Actions ubuntu runner,
#    any Linux user -- stops with "unable to get local issuer certificate".
#    Reproduced here with this R's libcurl forced to OpenSSL
#    (CURL_SSL_BACKEND=openssl) and Git's Mozilla bundle: mdic fails, and
#    succeeds (HTTP 200) once the intermediate is added to the bundle. The
#    intermediate's CA Issuers URL is
#    http://secure.globalsign.com/cacert/acserproar46ovtlsca2025.crt, signed
#    by GlobalSign Root R46 (in Mozilla's store). The mdic leaf itself is only
#    valid until 2026-10-22. This resolver therefore NEVER weakens TLS
#    verification: on a runner that cannot verify the chain it stop()s with a
#    message that says why (see comex_transport_error()). Making CI trust the
#    intermediate is a workflow decision, not something to hide in here:
#    the composite action .github/actions/trust-serpro-intermediate (added
#    2026-10-18; used by update-manifest.yaml, check-load-br-trade.yaml and,
#    for its load_br_trade job, check-dz-amazonia.yaml) downloads it from
#    that AIA URL, refuses it unless it is the expected CA and chains to a
#    root the runner already trusts, and adds it to the trust store. This
#    fixes CI only: an ordinary Linux user still hits the incomplete chain.
#    A scan of every host the manifest points at (same
#    Linux-like libcurl, 2026-10-18) found only the two balanca.* hosts with
#    this problem; the other 20 verify, or fail for non-TLS reasons.
#
# 3. A MISSING FILE ANSWERS 200. Every path that does not exist -- 2027, a
#    made-up name, the whole `ncmv2/` folder -- returns HTTP 200,
#    `Content-Type: text/plain`, 1420 bytes, Last-Modified 2019-09-03: the
#    source of Joomla's index.php, served as text. So a status code or a HEAD
#    proves nothing. A real file is `text/csv` (206 on a ranged GET; Accept-
#    Ranges: bytes is supported). This resolver sends ONE ranged GET
#    (bytes=0-4095) per candidate year and calls it a file only if the
#    content-type is text/csv; the same bytes give the header row.
#
# 4. WHAT EXISTS. Real files for 1997 through 2026 for all four datasets
#    (30 files each, nothing below 1997 -- the 1989-1996 series is a separate
#    NBM-coded product the loader does not read). The `ncmv2/IMP_<year>_V2.csv`
#    pattern mentioned in a comment in R/comex.R does not exist for any year
#    or for exports either (all placeholders), so `ncm/` is right for every
#    year. 2026 is a partial year (January-September as of this date; the
#    files' tails and the page's "Ultimos dados disponiveis" agree) and is
#    included in available_time on purpose: it is a real file the loader reads.
#
# 5. CADENCE (Manual de utilizacao dos dados estatisticos do comercio exterior
#    brasileiro, v1.1, 2020, https://<host>/balanca/manual/Manual.pdf, sections
#    5.1, 5.2.3, 9.2): consolidated data are published once a month, "ate o 8o
#    dia util", the current year's whole file is rewritten each time and its
#    earlier months may change; the previous year is reprocessed once, in
#    February, and then frozen. Last-Modified agrees (2025 files: 2026-02-05;
#    2026 files: 2026-10-06). So available_time moves once a year, when the
#    new year's file appears, and nothing here is stamped with a monthly
#    `version` (that would open a manifest PR every month for no reader).
#
# 6. THE API AGREES, AND IS NOT USED. https://api-comexstat.mdic.gov.br
#    (documented, valid TLS chain even from OpenSSL): /general/dates/years and
#    /cities/dates/years both return min 1997 / max 2026, and
#    /general/dates/updated returns {updated: 2026-10-06, year: 2026,
#    monthNumber: 09} -- the files' Last-Modified date and their last month.
#    It would work from CI today, but it describes the interactive query base,
#    not the bulk files this package downloads, and answering from it would
#    mean returning a range nobody verified against the files (SKILL.md: never
#    return a guessed result). Its max year is a handy human cross-check.
#
# 7. THE LOADER'S COLUMN ASSUMPTIONS HOLD, so they are enforced here too:
#    a candidate whose header lacks a column R/comex.R reads makes the whole
#    resolver stop() (see COMEX_DATASETS$required). Verified 2026-10-17 on the
#    header of 1997, 2025 and 2026 (identical per dataset) and, now, of every
#    probed year on each run. Extra columns are fine and expected: the NCM
#    import files also carry VL_FRETE and VL_SEGURO, which the loader ignores.
#    The loader also downloads two dictionaries (NCM.csv, NCM_SH.csv, Latin-1)
#    with URLs hard-coded in R/comex.R's load_trade_dic(), which no resolver
#    touches. They are byte-identical on both hosts (same MD5, 2026-10-18)
#    and R/comex.R now points them at COMEX_HOST's host; keep the two in step
#    (test-comex-resolver.R checks it).
#
# Emits one row per manifest row it owns (every real dataset, never NA), with
# `url` (the `$year$` template on COMEX_HOST), `docs_url` and `available_time`.

COMEX_HOST <- "https://balanca.mdic.gov.br"
COMEX_BASE <- paste0(COMEX_HOST, "/balanca/bd/comexstat-bd/")
COMEX_DOCS_URL <- "https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta"

# Raw header columns R/comex.R selects for each dataset (before clean_names()).
COMEX_MUN_COLUMNS <- c("CO_ANO", "CO_MES", "SH4", "CO_PAIS", "SG_UF_MUN", "CO_MUN", "KG_LIQUIDO", "VL_FOB")
COMEX_NCM_COLUMNS <- c(
  "CO_ANO", "CO_MES", "CO_NCM", "CO_UNID", "CO_PAIS", "SG_UF_NCM", "CO_VIA", "CO_URF",
  "QT_ESTAT", "KG_LIQUIDO", "VL_FOB"
)
COMEX_DATASETS <- list(
  export_mun = list(path = "mun/EXP_$year$_MUN.csv", required = COMEX_MUN_COLUMNS),
  import_mun = list(path = "mun/IMP_$year$_MUN.csv", required = COMEX_MUN_COLUMNS),
  export_prod = list(path = "ncm/EXP_$year$.csv", required = COMEX_NCM_COLUMNS),
  import_prod = list(path = "ncm/IMP_$year$.csv", required = COMEX_NCM_COLUMNS)
)

# A transport-level failure (no HTTP response at all) -> a stop() message that
# names the cause when it is one we have already met on this host.
comex_transport_error <- function(url, curl_msg) {
  reason <- if (grepl("certificate has expired|CERT_EXPIRED|certificate expired", curl_msg, ignore.case = TRUE)) {
    paste0(
      "the server's TLS certificate has expired (that is what took the old host, ",
      "balanca.economia.gov.br, down on 2026-10-03)"
    )
  } else if (grepl("issuer certificate|UNTRUSTED|unable to verify|incomplete", curl_msg, ignore.case = TRUE)) {
    paste0(
      "this runner cannot verify the server's TLS certificate chain. The server sends its ",
      "certificate without the intermediate CA, which Windows and browsers fetch on their own ",
      "but OpenSSL does not (see this file's header, point 2): add the intermediate (",
      "'AC SERPRO AR46 OV TLS CA 2025', http://secure.globalsign.com/cacert/acserproar46ovtlsca2025.crt) ",
      "to the runner's trust store -- never turn verification off. The \"Trust SERPRO's ",
      "intermediate CA\" workflow step (.github/actions/trust-serpro-intermediate) does exactly that; ",
      "if this is a CI run, its log says why it did not"
    )
  } else {
    "no HTTP response (DNS, connection, timeout or TLS failure)"
  }
  stop("resolve_comex(): could not reach ", url, " -- ", reason, ". curl said: ", curl_msg)
}

# The column names on the first line of a (partial) CSV body, upper-cased,
# without quotes or a BOM. NUL bytes dropped and invalid UTF-8 replaced so a
# garbage body can never make this itself error out.
comex_header_columns <- function(content) {
  if (length(content) == 0) {
    return(character(0))
  }
  txt <- iconv(rawToChar(content[content != as.raw(0)]), from = "UTF-8", to = "UTF-8", sub = "?")
  line <- sub("[\r\n].*$", "", sub("^﻿", "", txt))
  toupper(trimws(gsub('"', "", strsplit(line, ";", fixed = TRUE)[[1]], fixed = TRUE)))
}

resolve_comex <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop(
      "resolve_comex(): no manifest rows tagged resolver == 'comex' -- ",
      "cannot tell which datasets to update. Check the resolver column."
    )
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_comex() needs the 'curl' package (CI-only; not a package Import).")
  }
  unknown <- setdiff(unique(rows$dataset), names(COMEX_DATASETS))
  if (length(unknown) > 0) {
    stop(
      "resolve_comex(): manifest row(s) for dataset(s) ", paste(unknown, collapse = ", "),
      " are tagged resolver == 'comex' but this resolver only knows ",
      paste(names(COMEX_DATASETS), collapse = ", "), "."
    )
  }

  # One handle reused for every probe (keep-alive). Redirects are NOT followed:
  # a 3xx would be the host telling us the layout moved, and following it
  # silently (possibly to a login page, possibly to plain http) hides that.
  h <- curl::new_handle(timeout = 30, connecttimeout = 15, followlocation = FALSE)
  curl::handle_setheaders(h, Range = "bytes=0-4095")

  # file / absent / (anything else: stop). Verified live: a missing path is a
  # 200 text/plain placeholder (header point 3), so only content-type tells.
  probe <- function(url) {
    resp <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(resp, "error")) comex_transport_error(url, conditionMessage(resp))

    headers <- tryCatch(curl::parse_headers_list(resp$headers), error = function(e) list())
    ctype <- headers[["content-type"]]
    ctype <- if (is.null(ctype)) "" else ctype

    if (resp$status_code %in% c(200L, 206L)) {
      if (grepl("^text/csv", ctype, ignore.case = TRUE)) {
        return(list(kind = "file", columns = comex_header_columns(resp$content)))
      }
      return(list(kind = "absent"))
    }
    if (resp$status_code == 404L) {
      return(list(kind = "absent"))
    }
    stop(
      "resolve_comex(): unexpected HTTP ", resp$status_code, " for ", url, " (content-type '", ctype,
      "') -- neither a CSV (200/206) nor a clean 404, so this resolver cannot tell whether the file exists."
    )
  }

  this_year <- as.integer(format(Sys.Date(), "%Y"))
  candidate_years <- 1995:(this_year + 1) # two below the first real year, one past the current one

  ranges <- list()
  for (dataset in unique(rows$dataset)) {
    spec <- COMEX_DATASETS[[dataset]]
    template <- paste0(COMEX_BASE, spec$path)
    real <- integer(0)
    bad_header <- character(0)
    for (year in candidate_years) {
      p <- probe(sub("$year$", year, template, fixed = TRUE))
      if (p$kind != "file") next
      real <- c(real, year)
      missing_cols <- setdiff(spec$required, p$columns)
      if (length(missing_cols) > 0) {
        bad_header <- c(bad_header, paste0(year, ": missing ", paste(missing_cols, collapse = ", ")))
      }
    }

    if (length(real) == 0) {
      stop(
        "resolve_comex(): no real CSV (text/csv) found for '", dataset, "' at ", template,
        " for any year ", min(candidate_years), "-", max(candidate_years), " -- the path scheme or the host may have changed."
      )
    }
    if (length(bad_header) > 0) {
      stop(
        "resolve_comex(): '", dataset, "' files no longer carry the columns R/comex.R reads -- ",
        paste(utils::head(bad_header, 5), collapse = "; "),
        if (length(bad_header) > 5) paste0(" (and ", length(bad_header) - 5, " more years)") else "",
        ". Fix R/comex.R's column handling before publishing this range."
      )
    }
    # available_time is a single range, so a hole inside it would silently
    # promise a year that does not download.
    holes <- setdiff(seq(min(real), max(real)), real)
    if (length(holes) > 0) {
      stop(
        "resolve_comex(): '", dataset, "' has real files for ", min(real), "-", max(real),
        " except year(s) ", paste(holes, collapse = ", "), " -- available_time cannot express a gap."
      )
    }
    ranges[[dataset]] <- paste(min(real), max(real), sep = "-")
  }

  tibble::tibble(
    survey = "comex",
    dataset = rows$dataset,
    geo_level = rows$geo_level,
    year = rows$year,
    url = paste0(COMEX_BASE, vapply(rows$dataset, function(d) COMEX_DATASETS[[d]]$path, character(1))),
    docs_url = COMEX_DOCS_URL,
    available_time = unname(unlist(ranges[rows$dataset]))
  )
}
