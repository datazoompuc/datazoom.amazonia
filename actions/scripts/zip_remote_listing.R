# actions/scripts/zip_remote_listing.R
#
# Lets a resolver verify what's actually INSIDE a remote zip -- filenames,
# and (for small text sidecars) real content -- without downloading the
# whole archive. PRODES' raster zip runs ~133MB, BACI's ~2.4GB; both only
# need a few KB read to answer "is my assumption about this file's contents
# still true".
#
# Built and verified LIVE this session against both real archives (not just
# designed): a ranged GET of the last few KB recovers the zip's End Of
# Central Directory (EOCD) record plus the full central directory, which
# lists every entry's name, compressed/uncompressed size, and the BYTE
# OFFSET of its own local header -- from there, a second small ranged GET
# (that entry's local header + compressed payload only) is enough to
# reconstruct a minimal, valid, single-entry zip in memory and hand it to
# utils::unzip() for the actual decompression.
#
# Why reconstruct-a-mini-zip instead of decompressing the raw deflate stream
# directly: memDecompress(type = "raw") -- the obvious one-call answer --
# is NOT available in every R build (confirmed live in this session's R
# 4.3.2: "'arg' should be one of \"unknown\", \"gzip\", \"bzip2\", \"xz\",
# \"none\""). Wrapping the payload back into a tiny real zip file and letting
# utils::unzip() (base R, always available) do the inflate is what actually
# works -- verified against both a hand-built local test zip AND the real
# live PRODES zip's .qml sidecar (76 lines of real QGIS style XML came back
# byte-correct).
#
# No new package dependencies: curl is already CI-only in every resolver
# that uses this; everything else (readBin, utils::unzip) is base R.
#
# Every genuine failure here THROWS rather than returning an empty/NULL
# result -- "couldn't check" must never be conflated with "checked and it's
# fine", the same convention resolve_epe.R's verify_table_columns() and
# resolve_mapbiomas.R's dv_get_json() already apply. A ZIP64 archive (needed
# past ~4GB or 65535 entries -- neither real archive here is anywhere close)
# also throws rather than being silently mis-parsed by the 32-bit reader
# below.
#
# Split into a fetch layer (curl, HTTP) and a pure parse layer (raw bytes in,
# structures out) deliberately -- tests/testthat/test-zip-remote-listing.R
# exercises the parse layer directly against a byte-range function backed by
# a LOCAL file (no HTTP, no network, no test-server dependency) while
# zip_remote_entries()/zip_remote_extract_entry() (the two functions every
# resolver actually calls) are the thin curl-backed wrappers around it,
# proven live against both real archives this session.

read_u16 <- function(buf, off) as.integer(buf[off]) + 256L * as.integer(buf[off + 1])
read_u32 <- function(buf, off) {
  b <- as.integer(buf[off:(off + 3)])
  b[1] + b[2] * 256 + b[3] * 65536 + b[4] * 16777216
}
zip_u32_bytes <- function(x) {
  x <- as.integer(x)
  as.raw(c(bitwAnd(x, 0xFF), bitwAnd(bitwShiftR(x, 8), 0xFF), bitwAnd(bitwShiftR(x, 16), 0xFF), bitwAnd(bitwShiftR(x, 24), 0xFF)))
}
zip_u16_bytes <- function(x) {
  x <- as.integer(x)
  as.raw(c(bitwAnd(x, 0xFF), bitwAnd(bitwShiftR(x, 8), 0xFF)))
}

# ---- Pure parse layer: raw bytes in, structures out, no HTTP -------------

# `get_range(from, to)` must return the raw bytes for the INCLUSIVE 0-indexed
# byte range [from, to] of the zip, exactly like a ranged HTTP GET would.
# `total_size` is the zip's total byte length.
zip_entries_from_range_fn <- function(get_range, total_size, initial_tail = 8192L, max_tail = 2L * 1024L * 1024L) {
  eocd_sig <- as.raw(c(0x50, 0x4B, 0x05, 0x06))
  find_eocd <- function(buf) {
    n <- length(buf)
    if (n < 22) return(NA_integer_)
    for (i in seq(n - 21, 1, by = -1)) {
      if (identical(buf[i:(i + 3)], eocd_sig)) return(i)
    }
    NA_integer_
  }

  tail_n <- min(initial_tail, total_size)
  raw_tail <- get_range(total_size - tail_n, total_size - 1)
  eocd_off <- find_eocd(raw_tail)
  while (is.na(eocd_off) && tail_n < max_tail && tail_n < total_size) {
    tail_n <- min(tail_n * 4L, max_tail, total_size)
    raw_tail <- get_range(total_size - tail_n, total_size - 1)
    eocd_off <- find_eocd(raw_tail)
  }
  if (is.na(eocd_off)) {
    stop(
      "zip_entries_from_range_fn(): could not find the End Of Central Directory record ",
      "within the last ", tail_n, " bytes -- not a zip, or an unusually large comment ",
      "field pushed the EOCD further back than expected"
    )
  }

  # A ZIP64 EOCD locator (sig 50 4B 06 07) sits right before a plain EOCD
  # only when the archive needed ZIP64 -- this 32-bit reader can't safely
  # parse that, so fail loudly instead of silently truncating offsets.
  if (eocd_off >= 21) {
    maybe_zip64_loc <- raw_tail[(eocd_off - 20):(eocd_off - 17)]
    if (identical(maybe_zip64_loc, as.raw(c(0x50, 0x4B, 0x06, 0x07)))) {
      stop("zip_entries_from_range_fn(): this is a ZIP64 archive -- not supported by this 32-bit reader")
    }
  }

  cd_count <- read_u16(raw_tail, eocd_off + 10)
  cd_size <- read_u32(raw_tail, eocd_off + 12)
  cd_offset <- read_u32(raw_tail, eocd_off + 16)
  if (cd_count == 0xFFFF || cd_size == 0xFFFFFFFF || cd_offset == 0xFFFFFFFF) {
    stop("zip_entries_from_range_fn(): EOCD carries ZIP64 sentinel values -- not supported by this 32-bit reader")
  }

  cd_start_in_tail <- cd_offset - (total_size - tail_n) + 1
  if (cd_start_in_tail < 1) {
    # Central directory starts before our current tail window -- fetch a
    # window that covers it fully.
    raw_tail <- get_range(cd_offset, total_size - 1)
    cd_start_in_tail <- 1
  }

  pos <- cd_start_in_tail
  out <- list()
  for (i in seq_len(cd_count)) {
    sig <- raw_tail[pos:(pos + 3)]
    if (!identical(sig, as.raw(c(0x50, 0x4B, 0x01, 0x02)))) {
      stop("zip_entries_from_range_fn(): central directory entry ", i, " has a bad signature -- parse desynced")
    }
    method <- read_u16(raw_tail, pos + 10)
    mod_time <- raw_tail[(pos + 12):(pos + 13)]
    mod_date <- raw_tail[(pos + 14):(pos + 15)]
    crc32 <- raw_tail[(pos + 16):(pos + 19)]
    comp_size <- read_u32(raw_tail, pos + 20)
    uncomp_size <- read_u32(raw_tail, pos + 24)
    fname_len <- read_u16(raw_tail, pos + 28)
    extra_len <- read_u16(raw_tail, pos + 30)
    comment_len <- read_u16(raw_tail, pos + 32)
    local_hdr_off <- read_u32(raw_tail, pos + 42)
    if (comp_size == 0xFFFFFFFF || uncomp_size == 0xFFFFFFFF || local_hdr_off == 0xFFFFFFFF) {
      stop("zip_entries_from_range_fn(): entry ", i, " carries ZIP64 sentinel values -- not supported by this 32-bit reader")
    }
    fname_raw <- raw_tail[(pos + 46):(pos + 46 + fname_len - 1)]
    fname <- rawToChar(fname_raw)
    out[[fname]] <- list(
      method = method, comp_size = comp_size, uncomp_size = uncomp_size,
      local_hdr_off = local_hdr_off, crc32 = crc32, mod_time = mod_time,
      mod_date = mod_date, fname_raw = fname_raw
    )
    pos <- pos + 46 + fname_len + extra_len + comment_len
  }
  out
}

# `get_range(from, to)` here covers just [entry$local_hdr_off, entry$local_hdr_off + 30 + header_pad + entry$comp_size].
zip_extract_entry_from_range_fn <- function(get_range, entry, header_pad = 1024L) {
  raw_entry <- get_range(entry$local_hdr_off, entry$local_hdr_off + 30L + header_pad + entry$comp_size)

  if (length(raw_entry) < 30 || !identical(raw_entry[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))) {
    stop("zip_extract_entry_from_range_fn(): local file header signature not found at the expected offset -- offset bookkeeping is wrong")
  }
  lh_fname_len <- read_u16(raw_entry, 27)
  lh_extra_len <- read_u16(raw_entry, 29)
  payload_start <- 1L + 30L + lh_fname_len + lh_extra_len
  payload_end <- payload_start + entry$comp_size - 1L
  if (payload_end > length(raw_entry)) {
    stop(
      "zip_extract_entry_from_range_fn(): local header's filename/extra fields (",
      lh_fname_len + lh_extra_len, " bytes) exceeded header_pad=", header_pad,
      " -- the fetched range didn't cover the full payload, retry with a larger header_pad"
    )
  }
  payload <- raw_entry[payload_start:payload_end]

  fname_raw <- entry$fname_raw
  n <- length(fname_raw)
  local_header <- c(
    as.raw(c(0x50, 0x4B, 0x03, 0x04)), as.raw(c(0x14, 0x00)), as.raw(c(0x00, 0x00)),
    zip_u16_bytes(entry$method), entry$mod_time, entry$mod_date, entry$crc32,
    zip_u32_bytes(entry$comp_size), zip_u32_bytes(entry$uncomp_size),
    zip_u16_bytes(n), zip_u16_bytes(0L), fname_raw
  )
  central_entry <- c(
    as.raw(c(0x50, 0x4B, 0x01, 0x02)), as.raw(c(0x14, 0x00, 0x14, 0x00)), as.raw(c(0x00, 0x00)),
    zip_u16_bytes(entry$method), entry$mod_time, entry$mod_date, entry$crc32,
    zip_u32_bytes(entry$comp_size), zip_u32_bytes(entry$uncomp_size),
    zip_u16_bytes(n), zip_u16_bytes(0L), zip_u16_bytes(0L), zip_u16_bytes(0L), zip_u16_bytes(0L),
    zip_u32_bytes(0L), zip_u32_bytes(0L), fname_raw
  )
  cd_start <- length(local_header) + length(payload)
  eocd <- c(
    as.raw(c(0x50, 0x4B, 0x05, 0x06)), as.raw(c(0x00, 0x00, 0x00, 0x00)),
    zip_u16_bytes(1L), zip_u16_bytes(1L), zip_u32_bytes(length(central_entry)), zip_u32_bytes(cd_start), zip_u16_bytes(0L)
  )
  mini_zip <- c(local_header, payload, central_entry, eocd)

  mini_path <- tempfile(fileext = ".zip")
  on.exit(unlink(mini_path), add = TRUE)
  writeBin(mini_zip, mini_path)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  extracted <- tryCatch(
    utils::unzip(mini_path, exdir = out_dir),
    error = function(e) stop("zip_extract_entry_from_range_fn(): reconstructed zip failed to unzip: ", conditionMessage(e))
  )
  if (length(extracted) != 1) {
    stop("zip_extract_entry_from_range_fn(): expected exactly 1 file from the reconstructed zip, got ", length(extracted))
  }
  readBin(extracted, what = "raw", n = entry$uncomp_size)
}

# ---- curl-backed wrappers: what every resolver actually calls ------------

zip_remote_entries <- function(url, timeout_s = 30, initial_tail = 8192L, max_tail = 2L * 1024L * 1024L) {
  head_handle <- curl::new_handle(nobody = TRUE, timeout = timeout_s, followlocation = TRUE)
  head_resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = head_handle),
    error = function(e) stop("zip_remote_entries(): HEAD request failed for ", url, ": ", conditionMessage(e))
  )
  if (head_resp$status_code != 200) {
    stop("zip_remote_entries(): HEAD returned HTTP ", head_resp$status_code, " for ", url)
  }
  hdr_txt <- rawToChar(head_resp$headers)
  cl_match <- regmatches(hdr_txt, regexpr("(?i)content-length:\\s*[0-9]+", hdr_txt, perl = TRUE))
  if (length(cl_match) == 0) {
    stop("zip_remote_entries(): no Content-Length header from ", url, " -- can't locate the zip's tail")
  }
  total_size <- as.numeric(sub(".*:\\s*", "", cl_match))

  get_range <- function(from, to) {
    h <- curl::new_handle(timeout = timeout_s, followlocation = TRUE)
    curl::handle_setheaders(h, "Range" = sprintf("bytes=%.0f-%.0f", from, to))
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = h),
      error = function(e) stop("zip_remote_entries(): ranged GET failed for ", url, ": ", conditionMessage(e))
    )
    if (resp$status_code != 206) {
      stop(
        "zip_remote_entries(): expected HTTP 206 (Partial Content) for a ranged GET, got ",
        resp$status_code, " -- server may not support Range requests for ", url
      )
    }
    resp$content
  }

  zip_entries_from_range_fn(get_range, total_size, initial_tail, max_tail)
}

# Extracts and decompresses ONE entry (by the metadata zip_remote_entries()
# already returned for it) via a second small ranged GET, without
# downloading anything else in the archive. See this file's header for why
# this goes through a reconstructed mini-zip + utils::unzip() rather than
# memDecompress().
zip_remote_extract_entry <- function(url, entry, timeout_s = 30, header_pad = 1024L) {
  get_range <- function(from, to) {
    h <- curl::new_handle(timeout = timeout_s, followlocation = TRUE)
    curl::handle_setheaders(h, "Range" = sprintf("bytes=%.0f-%.0f", from, to))
    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = h),
      error = function(e) stop("zip_remote_extract_entry(): ranged GET failed for ", url, ": ", conditionMessage(e))
    )
    if (resp$status_code != 206) {
      stop("zip_remote_extract_entry(): expected HTTP 206, got ", resp$status_code, " for ", url)
    }
    resp$content
  }

  zip_extract_entry_from_range_fn(get_range, entry, header_pad)
}
