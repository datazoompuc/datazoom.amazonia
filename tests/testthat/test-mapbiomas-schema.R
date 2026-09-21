# tests/testthat/test-mapbiomas-schema.R
#
# Unit tests for R/mapbiomas.R's mapbiomas_treat() -- the treatment step
# that reads a downloaded MapBiomas workbook and normalizes it. Collection
# 10 didn't just move where files live (that's the manifest/resolver's
# job), it restructured several of the workbooks themselves -- new column
# names, a new window-column prefix, a new state_acronym/state collision,
# a wide-shaped water/biome sheet the old code assumed was already long.
# See NEWS.md and mapbiomas_treat()'s own header for the full story.
#
# Every fixture tibble below carries REAL column headers, transcribed from
# the live workbooks via direct HTTP range reads on 2026-08-24 (not
# invented) -- see the comment above each one for which file/sheet it
# reproduces. This is what makes these tests fast: no multi-hundred-
# megabyte download needed to exercise a header-shape bug.

mb_param <- function(dataset, geo_level = NULL, language = "eng") {
  list(source = "mapbiomas", dataset = dataset, geo_level = geo_level, language = language, raw_data = FALSE)
}

test_that("COVERAGE_10 (Dataverse file 254, GCS-mirrored) resolves cleanly", {
  # inst/extdata/manifest/v1/datasets_link.csv now points mapbiomas_cover/
  # municipality here (see resolve_mapbiomas.R's header for why 254, not
  # 535). Real header: ID, country, biome, state, municipality,
  # "municipality - state", geocode, feature_id, class, class_level_0..4,
  # 1985..2024. No state_acronym -- this is the shape WITHOUT the
  # collision; the next test exercises the collision directly.
  dat <- tibble::tibble(
    ID = 1, country = "Brasil", biome = "Amazonia", state = "Acre",
    municipality = "Rio Branco", `municipality - state` = "Rio Branco - AC",
    geocode = 1200401, feature_id = 1,
    class = "3", class_level_0 = "Natural", class_level_1 = "1. Forest",
    class_level_2 = "1.1. Forest Formation", class_level_3 = "1.1. Forest Formation",
    class_level_4 = "1.1. Forest Formation",
    `1985` = 100, `2024` = 90
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_cover", "municipality"))

  expect_true("municipality_code" %in% names(out))
  expect_equal(sum(names(out) == "state"), 1)
  expect_false("state_acronym" %in% names(out))
  expect_false(any(c("id", "feature_id") %in% names(out)))
  expect_true("2024" %in% out$year)
})

test_that("COVERAGE_10.1 shape (state + state_acronym collision) resolves without erroring", {
  # Dataverse file 535's real shape -- no longer downloaded (see manifest),
  # but kept as a regression test: if a future collection reintroduces this
  # exact collision, mapbiomas_treat() must still handle it, not just the
  # resolver's column check. Real header: country, biome, state,
  # state_acronym, municipality, class_id, class_level_0..4, 1985..2024.
  dat <- tibble::tibble(
    country = "Brasil", biome = "Amazonia", state = "Acre", state_acronym = "AC",
    municipality = "Rio Branco", class_id = 3,
    class_level_0 = "Natural", class_level_1 = "1. Forest",
    class_level_2 = "1.1. Forest Formation", class_level_3 = "1.1. Forest Formation",
    class_level_4 = "1.1. Forest Formation",
    `1985` = 100, `2024` = 90
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_cover", "municipality"))

  expect_equal(sum(names(out) == "state"), 1)
  expect_false("state_acronym" %in% names(out))
  expect_false("class_id" %in% names(out))
})

test_that("COVERAGE_INDIGENOUS_TERRITORIES (Dataverse file 266) finds the territory column by name, not position", {
  # Real header: country, biome, state, state_acronym, indigenous_territories,
  # class_id, class_level_0..4, 1985..2024. Position 2 is "biome" here, not
  # "territory" -- the old tidyr::extract(col = 2, ...) parsed biome names
  # with a territory regex and silently produced three all-NA columns.
  # "Alto Rio Purus (1201)" is a real value, verified live.
  dat <- tibble::tibble(
    country = "Brasil", biome = "Amazonia", state = "Acre", state_acronym = "AC",
    indigenous_territories = "Alto Rio Purus (1201)", class_id = 3,
    class_level_0 = "Natural", class_level_1 = "1. Forest",
    class_level_2 = "1.1. Forest Formation", class_level_3 = "1.1. Forest Formation",
    class_level_4 = "1.1. Forest Formation",
    `1985` = 100
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_cover", "indigenous_land"))

  expect_true("biome" %in% names(out))
  expect_equal(unique(out$biome), "Amazonia")
  expect_equal(unique(out$territory_name), "Alto Rio Purus")
  expect_equal(unique(out$territory_code), "1201")
})

test_that("TRANSITION_10 (p-prefixed window columns) pivots correctly", {
  # Real header (Dataverse file 457, sheet TRANSITION_10): ID, country,
  # biome, state, class_from, class_to, class_level_1_from..4_from,
  # class_level_1_to..4_to, p1985_1986 ... p2010_2020. The old
  # pivot_longer(starts_with("x"), names_prefix = "x") selected ZERO
  # columns here -- Collection 9's window columns were bare numbers
  # ("1985_1986"), which janitor::clean_names() x-prefixes; Collection
  # 10's are already p-prefixed, so janitor leaves them alone.
  dat <- tibble::tibble(
    ID = 1, country = "Brasil", biome = "Amazonia", state = "Acre",
    class_from = 0, class_to = 3,
    class_level_1_from = "6. Not Observed", class_level_2_from = "6. Not Observed",
    class_level_3_from = "6. Not Observed", class_level_4_from = "6. Not Observed",
    class_level_1_to = "1. Forest", class_level_2_to = "1.1. Forest Formation",
    class_level_3_to = "1.1. Forest Formation", class_level_4_to = "1.1. Forest Formation",
    p1985_1986 = 10, p1990_1995 = 20
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_transition", "biome"))

  expect_true("year" %in% names(out))
  expect_setequal(unique(out$year), c("1985_1986", "1990_1995"))
  expect_true("biome" %in% names(out))
})

test_that("TRANSITION_9 (x-prefixed window columns, municipality) still pivots the same way", {
  # Regression guard for the unchanged geo_level -- mapbiomas_transition/
  # municipality has no Dataverse substitute and still points at the old
  # Collection 9 GCS file (see resolve_mapbiomas.R's header). year values
  # must come out identically shaped to TRANSITION_10's above
  # ("1985_1986", not "x1985_1986" or anything else).
  dat <- tibble::tibble(
    country = "Brasil", state = "Acre", municipality = "Rio Branco",
    `municipality - state` = "Rio Branco - AC", geocode = 1200401, feature_id = 1,
    class_from = "Forest", class_to = "Pasture",
    class_level_1_from = "Forest", class_level_2_from = "Forest",
    class_level_3_from = "Forest", class_level_4_from = "Forest",
    class_level_1_to = "Farming", class_level_2_to = "Pasture",
    class_level_3_to = "Pasture", class_level_4_to = "Pasture",
    `1985_1986` = 5, `1990_1995` = 8
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_transition", "municipality"))

  expect_setequal(unique(out$year), c("1985_1986", "1990_1995"))
  expect_true("municipality_code" %in% names(out))
})

test_that("pt language: class_level_N_from/_to are renamed, not left untouched", {
  # Real header, both live collections currently in use for this dataset:
  # TRANSITION_9 (GCS, municipality) and TRANSITION_10 (Dataverse, biome)
  # both use class_level_1_from..4_from / class_level_1_to..4_to -- verified
  # live 2026-09-01 by streaming TRANSITION_9's actual header via HTTP range
  # reads, not assumed. There is no from_level_N/to_level_N PREFIX form in
  # real data -- the original rule matched that shape and therefore matched
  # nothing here, silently. This is the direct regression guard for the fix.
  dat <- tibble::tibble(
    country = "Brasil", state = "Acre", municipality = "Rio Branco",
    `municipality - state` = "Rio Branco - AC", geocode = 1200401, feature_id = 1,
    class_from = "Forest", class_to = "Pasture",
    class_level_1_from = "Forest", class_level_2_from = "Forest",
    class_level_3_from = "Forest", class_level_4_from = "Forest",
    class_level_1_to = "Farming", class_level_2_to = "Pasture",
    class_level_3_to = "Pasture", class_level_4_to = "Pasture",
    `1985_1986` = 5, `1990_1995` = 8
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_transition", "municipality", language = "pt"))

  expect_true(all(c("class_level_1_de", "class_level_2_de", "class_level_3_de", "class_level_4_de") %in% names(out)))
  expect_true(all(c("class_level_1_para", "class_level_2_para", "class_level_3_para", "class_level_4_para") %in% names(out)))
  expect_false(any(grepl("_from$|_to$", names(out))))
})

test_that("DEFORESTATION's renamed municipality code column (geocode_municipality) is caught", {
  # Real header (Dataverse file 485, sheet DEFORESTATION): country, biome,
  # state, municipality, geocode_municipality, class, transition_name,
  # class_level_0..4, 1987..2024. Collection 9's combined file called this
  # column "geocode"; Collection 10 split deforestation into its own file
  # and renamed it -- the old rename map's exact-name case_match() never
  # fired, silently leaving municipality_code missing.
  dat <- tibble::tibble(
    country = "Brasil", biome = "Amazonia", state = "Acre", municipality = "Rio Branco",
    geocode_municipality = 1200401, class = 3, transition_name = "Deforestation",
    class_level_0 = "Anthropic", class_level_1 = "3. Farming",
    class_level_2 = "3.1 Pasture", class_level_3 = "3.1 Pasture", class_level_4 = "3.1 Pasture",
    `1987` = 5, `2024` = 2
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_deforestation_regeneration", "municipality"))

  expect_true("municipality_code" %in% names(out))
  expect_false("geocode_municipality" %in% names(out))
  expect_true("1987" %in% out$year)
})

test_that("WATER_BIOME_ANNUAL (wide, no code/name pair) pivots instead of erroring", {
  # Real header (Dataverse file 344, sheet WATER_BIOME_ANNUAL): BIOME,
  # 1985..2024. The old water/biome branch assumed every water sheet was
  # already long with a code/name pair to rename (true for
  # WATER_CITY_ANNUAL below, false here) -- renaming columns that don't
  # exist is a silent no-op in dplyr::rename(), so the real bug was that
  # nothing ever reshaped this sheet into year/value at all.
  dat <- tibble::tibble(BIOME = "Amazonia", `1985` = 100, `2024` = 90)
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_water", "biome"))

  expect_true(all(c("biome", "year", "value") %in% names(out)))
  expect_setequal(unique(out$year), c("1985", "2024"))
})

test_that("WATER_CITY_ANNUAL (already long) keeps its unchanged shape", {
  # Real header (Dataverse file 344, sheet WATER_CITY_ANNUAL): code,
  # municipality, year, area_ha, state, "municipality - state". Already
  # long -- no pivot needed or attempted; only the code -> municipality_code
  # rename applies. Regression guard: this geo_level was never broken.
  dat <- tibble::tibble(
    code = 1200401, municipality = "Rio Branco", year = "2020", area_ha = 500,
    state = "AC", `municipality - state` = "Rio Branco - AC"
  )
  out <- mapbiomas_treat(dat, mb_param("mapbiomas_water", "municipality"))

  expect_true("municipality_code" %in% names(out))
  expect_equal(out$year, "2020")
})
