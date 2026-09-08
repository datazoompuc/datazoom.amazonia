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
  list(changed_prefix = "baci", r_path = file.path("R", "baci.R"), label = "R/baci.R", treat_fn = "load_baci()")
)
