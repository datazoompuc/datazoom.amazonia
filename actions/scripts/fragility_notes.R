# actions/scripts/fragility_notes.R
#
# Pure function, no top-level execution -- split out of build_manifest.R
# (which is NOT safe to source() directly; it runs the whole resolver
# pipeline over the network at the top level) so it can be sourced on its
# own, by build_manifest.R AND by tests/testthat/test-fragility-notes.R, the
# same way manifest_validate.R and site_inventory.R already are.
#
# One generic mechanism, used for every source that tags its treatment code
# this way (R/mapbiomas.R and R/epe.R today; any future R/<source>.R that
# adopts the convention needs nothing added here). Originally
# mapbiomas-only (mapbiomas_fragility.R / collect_mapbiomas_fragility_notes())
# with a near-duplicate epe_fragility.R added alongside it -- collapsed into
# this single file instead, since the logic was identical and the only real
# difference was which path/label to grep.
#
# collect_fragility_notes(r_path, label) greps r_path for "# FRAGILE:" tags
# and pairs each with the line number of the code it sits above -- see
# R/mapbiomas.R's mapbiomas_treat() header for the tagging convention.
# `label` is what prefixes each note (e.g. "R/mapbiomas.R", "R/epe.R"),
# since the file's own path on disk (a full normalizePath()) isn't what a
# reviewer wants to see. Reads the live file every call, so this can never
# drift out of sync with real line numbers the way a hand-copied list
# would after the next refactor. build_manifest.R uses this to tell a PR
# reviewer exactly which lines to re-check when a manifest row changes,
# instead of a generic "check R/*.R" reminder.
collect_fragility_notes <- function(r_path, label) {
  if (!file.exists(r_path)) return(character(0))
  lines <- readLines(r_path, warn = FALSE)
  is_comment <- grepl("^\\s*#", lines)
  tag_lines <- grep("^\\s*#\\s*FRAGILE:", lines)

  notes <- character(0)
  for (start in tag_lines) {
    # extend through consecutive following comment lines (the rest of this
    # tag's explanation), then find the next actual code line -- that's
    # the anchor a reviewer should look at.
    end <- start
    while (end + 1 <= length(lines) && is_comment[end + 1]) end <- end + 1
    anchor <- end + 1
    while (anchor <= length(lines) && !nzchar(trimws(lines[anchor]))) anchor <- anchor + 1
    if (anchor > length(lines)) anchor <- end

    text <- sub("^\\s*#\\s*", "", lines[start:end])
    text <- sub("^FRAGILE:\\s*", "", text)
    text <- paste(text, collapse = " ")

    notes <- c(notes, sprintf("%s:%d -- %s", label, anchor, text))
  }
  notes
}

# FRAGILITY_SOURCES drives build_manifest.R's post-run PR-body check
# generically: one row per source whose treatment code carries "# FRAGILE:"
# tags worth re-checking after that source's manifest rows change. Adding a
# new source's fragility notes to a PR body is then just adding a row here
# -- no new wrapper file, no new block in build_manifest.R. `changed_prefix`
# is the detect_changes() key prefix (see manifest_validate.R) that
# identifies a changed row as belonging to this source.
FRAGILITY_SOURCES <- list(
  list(changed_prefix = "mapbiomas", r_path = file.path("R", "mapbiomas.R"), label = "R/mapbiomas.R", treat_fn = "mapbiomas_treat()"),
  list(changed_prefix = "epe", r_path = file.path("R", "epe.R"), label = "R/epe.R", treat_fn = "epe_energy_state_panel_treat()"),
  list(changed_prefix = "prodes", r_path = file.path("R", "prodes.R"), label = "R/prodes.R", treat_fn = "load_prodes()"),
  list(changed_prefix = "baci", r_path = file.path("R", "baci.R"), label = "R/baci.R", treat_fn = "load_baci()"),
  # The dictionary load_baci_dic() reads is a comex_dic row (hs_codes, shared
  # with load_br_trade(); it had a row of its own, "baci_dic", until 2026-10-28);
  # build_manifest.R matches `survey\r` exactly, so "baci" above never fires for
  # it, yet two of R/baci.R's tags are about that dictionary. Any comex_dic change
  # lists them, including a change to ncm_codes, which BACI does not read:
  # harmless, and cheaper than touching build_manifest.R's matching.
  list(changed_prefix = "comex_dic", r_path = file.path("R", "baci.R"), label = "R/baci.R", treat_fn = "load_baci()"),
  list(changed_prefix = "ips", r_path = file.path("R", "ips.R"), label = "R/ips.R", treat_fn = "load_ips()"),
  list(changed_prefix = "seeg", r_path = file.path("R", "seeg.R"), label = "R/seeg.R", treat_fn = "load_seeg()"),
  # pam's treatment code is load_pam() (R/pam.R) plus the SIDRA request
  # sidra_download() (R/download.R) builds for it, so one row per file, as
  # for terraclimate below.
  list(changed_prefix = "pam", r_path = file.path("R", "pam.R"), label = "R/pam.R", treat_fn = "load_pam()"),
  list(changed_prefix = "pam", r_path = file.path("R", "download.R"), label = "R/download.R", treat_fn = "sidra_download()"),
  # terraclimate's treatment code lives in two files: load_climate() (R/climate.R)
  # and the NCSS request external_download() builds (R/download.R). The loop in
  # build_manifest.R reads ONE file per row, so there is one row per file; a
  # changed terraclimate row lists the tags of both.
  list(changed_prefix = "terraclimate", r_path = file.path("R", "climate.R"), label = "R/climate.R", treat_fn = "load_climate()"),
  list(changed_prefix = "terraclimate", r_path = file.path("R", "download.R"), label = "R/download.R", treat_fn = "external_download()"),
  # The certificate fallback for balanca.mdic.gov.br (R/balanca_tls.R) has two
  # notes: the host name it applies to, and the hard-coded certificates it trusts.
  # They matter when a row on that host changes, so they ride on the dictionary
  # rows ("comex_dic", "baci_dic"): those change rarely, and resolve_comex.R
  # moves the "comex" data rows and these together when the host changes. The
  # "comex" rows themselves are left out on purpose -- their available_time moves
  # every month and the note would be noise.
  list(changed_prefix = "comex_dic", r_path = file.path("R", "balanca_tls.R"), label = "R/balanca_tls.R", treat_fn = "balanca_with_fallback()"),
  list(changed_prefix = "baci_dic", r_path = file.path("R", "balanca_tls.R"), label = "R/balanca_tls.R", treat_fn = "balanca_with_fallback()"),
  list(changed_prefix = "aneel", r_path = file.path("R", "aneel.R"), label = "R/aneel.R", treat_fn = "load_aneel()"),
  list(changed_prefix = "deter", r_path = file.path("R", "deter.R"), label = "R/deter.R", treat_fn = "load_deter()"),
  list(changed_prefix = "comex", r_path = file.path("R", "comex.R"), label = "R/comex.R", treat_fn = "load_br_trade()"),
  # The dictionaries' manifest rows are their own survey ("comex_dic"), and
  # build_manifest.R matches `survey\r` exactly, so "comex" above never fires
  # for them -- yet two of R/comex.R's tags are about the dictionaries (the
  # manifest-sourced URL, and the Latin-1/column assumptions). A run that
  # changes both a comex and a comex_dic row lists the same notes twice;
  # harmless, and cheaper than touching build_manifest.R's matching.
  list(changed_prefix = "comex_dic", r_path = file.path("R", "comex.R"), label = "R/comex.R", treat_fn = "load_br_trade()"),
  list(changed_prefix = "degrad", r_path = file.path("R", "degrad.R"), label = "R/degrad.R", treat_fn = "load_degrad()")
)
