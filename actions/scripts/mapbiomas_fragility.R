# actions/scripts/mapbiomas_fragility.R
#
# Pure function, no top-level execution -- split out of build_manifest.R
# (which is NOT safe to source() directly; it runs the whole resolver
# pipeline over the network at the top level) so it can be sourced on its
# own, by build_manifest.R AND by tests/testthat/test-mapbiomas-fragility.R,
# the same way manifest_validate.R and site_inventory.R already are.
#
# Greps R/mapbiomas.R for "# FRAGILE:" tags and pairs each with the line
# number of the code it sits above -- see that file's mapbiomas_treat()
# header for the tagging convention. Reads the LIVE file every call, so
# this can never drift out of sync with real line numbers the way a
# hand-copied list would after the next refactor. build_manifest.R uses
# this to tell a PR reviewer exactly which lines to re-check when a
# MapBiomas manifest row changes, instead of a generic "check R/*.R"
# reminder.
collect_mapbiomas_fragility_notes <- function(mapbiomas_r_path) {
  if (!file.exists(mapbiomas_r_path)) return(character(0))
  lines <- readLines(mapbiomas_r_path, warn = FALSE)
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

    notes <- c(notes, sprintf("R/mapbiomas.R:%d -- %s", anchor, text))
  }
  notes
}
