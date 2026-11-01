# tests/testthat/test-sigmine.R
#
# load_sigmine() treatment (R/sigmine.R): column names in pt/eng, conversion of
# area from hectares to square meters, "DADO NAO CADASTRADO" -> NA, and the
# municipality-level join (one row per process, centroid outside every
# municipality kept with missing municipality fields).
#
# The real ANM file is ~121MB, so these tests mock external_download() (same
# convention as test-degrad-crs.R) with a few tiny synthetic polygons and two
# synthetic municipalities, A (lon -61 to -59) and B (lon -59 to -57).
#
#   1/2020: two polygons, first in A (area 2 ha), second in B (area 5 ha)
#   2/2021: one polygon in A, company registered as "DADO NAO CADASTRADO"
#   3/2022: one polygon outside both municipalities (lon -50)

sigmine_square <- function(x, y, d = 0.1) {
  sf::st_polygon(list(rbind(
    c(x, y), c(x + d, y), c(x + d, y + d), c(x, y + d), c(x, y)
  )))
}

sigmine_active_fixture <- function() {
  geom <- sf::st_sfc(
    sigmine_square(-60.5, -5.5),   # 1/2020, in A
    sigmine_square(-58.5, -5.5),   # 1/2020, in B
    sigmine_square(-60.0, -5.0),   # 2/2021, in A
    sigmine_square(-50.0, -5.0),   # 3/2022, outside
    crs = 4674
  )
  sf::st_sf(
    processo = c("1/2020", "1/2020", "2/2021", "3/2022"),
    numero = c(1, 1, 2, 3),
    ano = c(2020, 2020, 2021, 2022),
    area_ha = c(2, 5, 3, 4),
    id = c("a", "b", "c", "d"),
    fase = "AUTORIZACAO DE PESQUISA",
    ult_evento = "EVENTO",
    nome = c("EMPRESA X", "EMPRESA X", "DADO NAO CADASTRADO", "EMPRESA Y"),
    subs = c("OURO", "OURO", "FERRO", "COBRE"),
    uso = "INDUSTRIAL",
    uf = "AM",
    ds_processo = c("desc 1", "desc 1", "desc 2", "desc 3"),
    geometry = geom
  )
}

sigmine_municipality_fixture <- function() {
  box_a <- sf::st_polygon(list(rbind(c(-61, -6), c(-59, -6), c(-59, -4), c(-61, -4), c(-61, -6))))
  box_b <- sf::st_polygon(list(rbind(c(-59, -6), c(-57, -6), c(-57, -4), c(-59, -4), c(-59, -6))))
  sf::st_sf(
    code_muni = c(1100015, 1100023),
    name_muni = c("Muni A", "Muni B"),
    code_state = 11,
    abbrev_state = "RO",
    name_state = "Rondonia",
    code_region = 1,
    name_region = "Norte",
    geom = sf::st_sfc(box_a, box_b, crs = 4674)
  )
}

local_sigmine_mock <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    external_download = function(dataset, source, ...) {
      if (identical(dataset, "geo_municipalities")) {
        sigmine_municipality_fixture()
      } else {
        sigmine_active_fixture()
      }
    },
    .package = "datazoom.amazonia",
    .env = env
  )
}

test_that("state level, eng: one row per polygon, renamed columns, area in m2", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "state", language = "eng")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 4)
  expect_true(all(c("process", "company", "mineral", "process_description", "area_m2") %in% names(out)))
  expect_false("ds_processo" %in% names(out))
  expect_equal(out$area_m2, c(2, 5, 3, 4) * 10000)
})

test_that("state level, pt: process description is descricao_processo", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "state", language = "pt")

  expect_true("descricao_processo" %in% names(out))
  expect_false(any(c("ds_processo", "process_description") %in% names(out)))
})

test_that("DADO NAO CADASTRADO becomes NA", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "state", language = "eng")

  expect_true(is.na(out$company[out$process == "2/2021"]))
  expect_false(anyNA(out$company[out$process != "2/2021"]))
})

test_that("municipality level: one row per process, no geometry", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_false(inherits(out, "sf"))
  expect_equal(nrow(out), 3)
  expect_equal(anyDuplicated(out$process), 0L)
  expect_true(all(c("municipality_code", "municipality", "state", "process_description") %in% names(out)))
})

test_that("municipality level: only the first polygon of a process is kept", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "municipality", language = "eng")

  p1 <- out[out$process == "1/2020", ]
  expect_equal(p1$municipality, "Muni A")
  expect_equal(p1$area_m2, 2 * 10000)
})

test_that("municipality level: centroid outside every municipality keeps the process with NA municipality", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "municipality", language = "eng")

  p3 <- out[out$process == "3/2022", ]
  expect_equal(nrow(p3), 1)
  expect_true(is.na(p3$municipality))
  expect_true(is.na(p3$municipality_code))
})

test_that("municipality level, pt: column names", {
  local_sigmine_mock()
  out <- load_sigmine("sigmine_active", raw_data = FALSE, geo_level = "municipality", language = "pt")

  expect_true(all(c("cod_municipio", "municipio", "uf", "descricao_processo") %in% names(out)))
})
