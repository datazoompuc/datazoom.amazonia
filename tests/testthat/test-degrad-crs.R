# tests/testthat/test-degrad-crs.R
#
# load_degrad()'s per-year CRS harmonization (R/degrad.R, harmonize_degrad_year_crs()).
#
# The ten raw DEGRAD shapefiles do not share a CRS (read live 2026-10-12): the
# 2007-2009 files carry none at all, 2010-2014 embed SAD69 (with a TOWGS84) and
# 2015-2016 embed SIRGAS 2000. Binding years from different groups used to
# fail with "arguments have different crs" (2009:2010, 2014:2015, 2007:2016;
# only ranges inside one group worked), and the SAD69 -> SIRGAS 2000 datum
# shift was never applied because the projection used internally names no
# datum (datazoompuc/datazoom.amazonia#350). Every year is now transformed to
# SIRGAS 2000 right after download, before the years are combined.
#
# The real files are 4-30MB each and come from a slow INPE server, so these
# tests mock external_download() (same convention as test-aneel-schema.R and
# the other loader tests) with three tiny synthetic polygons placed at the SAME
# lon/lat but declared in the CRS each group of real files carries:
#   2009 -> no CRS (the code assigns its SAD69 approximation), 2010 -> SAD69
#   with TOWGS84, 2016 -> SIRGAS 2000. Those three years touch both group
#   boundaries (2009|2010 and 2010|2016) in one call.
#
# Verified against the real files on 2026-10-16 (not by these tests; the monthly
# actions/scripts/check_all_loads.R run is what exercises them): SAD69-era
# polygons end up 59.5-75.0 m from where the raw coordinates would sit if read
# as SIRGAS 2000, SIRGAS 2000 ones 0.0 m; and 2007:2016 loaded at once returns
# exactly what the ten single-year loads return.

# One real Legal Amazon municipality code from the package's own sysdata, so
# load_degrad()'s Legal Amazon filter keeps the fixture.
degrad_muni_code <- datazoom.amazonia::municipalities$code_muni[
  datazoom.amazonia::municipalities$legal_amazon == 1
][1]

# A square municipality in SIRGAS 2000 around (-60, -5), far larger than the
# DEGRAD polygons, so st_intersection() clips nothing.
degrad_geo_fixture <- function() {
  box <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(-61, -6), c(-59, -6), c(-59, -4), c(-61, -4), c(-61, -6)))),
    crs = 4674
  )
  sf::st_sf(
    code_muni = degrad_muni_code, name_muni = "Fixture", code_state = 11,
    abbrev_state = "RO", name_state = "Rondonia", code_region = 1,
    name_region = "Norte", geom = box
  )
}

# The "+towgs84" string load_degrad() itself assigns to years with no CRS --
# also what the real 2010-2014 .prj files carry (same shift to 0.01 m).
degrad_sad69_towgs84 <- "+proj=longlat +ellps=aust_SA +towgs84=-66.8700,4.3700,-38.5200,0.0,0.0,0.0,0.0 +no_defs"

# A small DEGRAD-like polygon at the same lon/lat in every year; columns named
# like the real raw files (upper case, before clean_names()), plus the `year`
# column external_download() adds for DEGRAD.
degrad_year_fixture <- function(year, crs) {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-60.01, -5.01), c(-59.99, -5.01), c(-59.99, -4.99), c(-60.01, -4.99), c(-60.01, -5.01)
  ))))
  if (!is.na(crs)) poly <- sf::st_set_crs(poly, crs)
  sf::st_sf(
    LINKCOLUMN = "x", SCENE_ID = 1, CLASS_NAME = "DEGRADACAO", PATHROW = "231060",
    VIEW_DATE = "2010-01-01", JULDAY = 1, year = year, geometry = poly
  )
}

degrad_fixtures_by_year <- list(
  "2009" = function() degrad_year_fixture(2009, NA),
  "2010" = function() degrad_year_fixture(2010, sf::st_crs(degrad_sad69_towgs84)),
  "2016" = function() degrad_year_fixture(2016, sf::st_crs(4674))
)

degrad_fake_download <- function(dataset = NULL, source = NULL, year = NULL, ...) {
  if (identical(dataset, "geo_municipalities")) {
    return(degrad_geo_fixture())
  }
  degrad_fixtures_by_year[[as.character(year)]]()
}

test_that("load_degrad() accepts years whose raw files carry different CRSs", {
  testthat::local_mocked_bindings(external_download = degrad_fake_download, .package = "datazoom.amazonia")

  # crosses both group boundaries: no CRS | SAD69 | SIRGAS 2000
  expect_no_error(dat <- load_degrad("degrad", time_period = c(2009, 2010, 2016), language = "eng"))

  expect_s3_class(dat, "sf")
  expect_equal(sort(unique(dat$year)), c(2009, 2010, 2016))
  expect_equal(nrow(dat), 3)
  expect_true(all(sf::st_is_valid(dat)))
  expect_false(any(sf::st_is_empty(dat)))

  # the output CRS is still the equal-area projection operation_crs defines
  expect_match(sf::st_crs(dat)$proj4string, "+proj=aea", fixed = TRUE)
  expect_s3_class(dat$area, "units")
})

test_that("SAD69-era polygons get the real datum shift (about 60-75 m); SIRGAS 2000 ones do not", {
  testthat::local_mocked_bindings(external_download = degrad_fake_download, .package = "datazoom.amazonia")

  dat <- load_degrad("degrad", time_period = c(2009, 2010, 2016), language = "eng")
  centroid <- function(y) sf::st_centroid(sf::st_geometry(dat[dat$year == y, ]))
  metres <- function(a, b) as.numeric(sf::st_distance(a, b)) # projected (Albers) metres

  # same lon/lat, declared SAD69 (2010) vs SIRGAS 2000 (2016): measured 69.5 m at
  # (-60, -5) on this machine's PROJ; the bounds only need to separate "a real
  # datum shift" (tens of metres) from "none" (0 m, what the code did before).
  expect_gt(metres(centroid(2010), centroid(2016)), 55)
  expect_lt(metres(centroid(2010), centroid(2016)), 80)

  # a year with no CRS is labelled with the same SAD69 TOWGS84 as the real
  # SAD69 files, so it is shifted the same way
  expect_lt(metres(centroid(2009), centroid(2010)), 1)
  expect_gt(metres(centroid(2009), centroid(2016)), 55)
})

test_that("raw_data = TRUE still returns each year's own, untouched CRS", {
  testthat::local_mocked_bindings(external_download = degrad_fake_download, .package = "datazoom.amazonia")

  raw <- load_degrad("degrad", raw_data = TRUE, time_period = c(2009, 2010, 2016))

  expect_type(raw, "list")
  expect_length(raw, 3)
  expect_true(is.na(sf::st_crs(raw[[1]])))
  expect_false(is.na(sf::st_crs(raw[[2]])))
  expect_equal(sf::st_crs(raw[[2]]), sf::st_crs(degrad_sad69_towgs84))
  expect_equal(sf::st_crs(raw[[3]]), sf::st_crs(4674))
})
