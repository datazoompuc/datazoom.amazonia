# load_seeg() / seeg_land -- the fifth and last sector migrated to the
# v13.0 "Dados" schema (see R/seeg.R's header for the full schema
# investigation). Biome and the full transition matrix (old NIVEL 3/NIVEL 6)
# are confirmed dropped for this sector -- the largest loss of any migrated
# sector, called out explicitly to the user, not just documented in code.
# Offline: external_download() is mocked with a small synthetic fixture that
# mirrors the REAL v13.0 column layout, same convention as
# test-seeg-residuals.R's seeg_v13_fixture().

seeg_land_fixture <- function() {
  base <- tibble::tibble(
    `Emissão/Remoção/Bunker` = c("Emissão", "Remoção", "Emissão"),
    `Setor de emissão` = c("Mudança de Uso da Terra e Floresta", "Mudança de Uso da Terra e Floresta", "Agropecuária"),
    `Categoria emissora` = c("Alterações de uso da terra", "Remoção por vegetação secundária", "Cultivo de arroz"),
    `Sub-categoria emissora` = c("Desmatamento", "Regeneração", "Cultivo em sistema irrigado inundado"),
    Recorte = c("Fora de área protegida", "Em área protegida", "Diretas"),
    `Atividade geral` = c("Pecuária", "Vegetação nativa", "Agricultura"),
    Estado = c("Paraná", "Amazonas", "Amazonas"),
    Município = c("Abatiá (PR)", "Manaus (AM)", "Manaus (AM)"),
    `ID Território` = c("14100103", "11302603", "11302603")
  )

  years <- 1970:2024
  year_cols <- stats::setNames(
    lapply(years, function(y) if (y == 2018) c(50, 8, 999) else c(0, 0, 999)),
    as.character(years)
  )

  dplyr::bind_cols(base, tibble::as_tibble(year_cols))
}

test_that("seeg_land/municipality/pt: only Mudanca de Uso da Terra e Floresta rows kept", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_land_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "municipality", language = "pt")

  expect_equal(nrow(dat), 2 * 55)
  expect_setequal(
    names(dat),
    c("Ano", "municipio", "estado", "ibge", "setor", "categoria_emissao",
      "subcategoria_emissao", "recorte", "atividade_geral", "tipo_emissao", "Valor")
  )
  expect_true(all(dat$setor == "Mudanca de Uso da Terra e Floresta"))
  expect_setequal(dat$tipo_emissao, c("Emissao", "Remocao"))
})

test_that("seeg_land/municipality/eng: every value translated, none left NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_land_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_setequal(
    names(dat),
    c("year", "city", "state", "ibge", "sector", "emission_category",
      "emission_subcategory", "emission_scope", "general_activity", "emission_type", "value")
  )
  expect_true(all(dat$sector == "Land Use Change and Forestry"))
  expect_setequal(dat$emission_scope, c("Outside protected area", "In protected area"))
  expect_setequal(dat$emission_type, c("Emission", "Removal"))

  expect_false(anyNA(dat$emission_category))
  expect_false(anyNA(dat$emission_subcategory))
  expect_false(anyNA(dat$emission_scope))
  expect_false(anyNA(dat$general_activity))
  expect_false(anyNA(dat$emission_type))
})

test_that("seeg_land: literal 'NA' recorte string translates to 'Not applicable', not a real NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      fixture <- seeg_land_fixture()
      fixture$Recorte[1] <- "NA"
      fixture
    },
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "municipality", language = "eng")
  na_scope_rows <- dat[dat$emission_category == "Land use change", ]

  expect_false(anyNA(na_scope_rows$emission_scope))
  expect_true(all(na_scope_rows$emission_scope == "Not applicable"))
})

test_that("seeg_land/state/pt and country/pt: totals reconcile with municipality level", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_land_fixture(),
    .package = "datazoom.amazonia"
  )

  dat_muni <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "municipality", language = "pt")
  dat_state <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "state", language = "pt")
  dat_country <- load_seeg(dataset = "seeg_land", raw_data = FALSE, geo_level = "country", language = "pt")

  total_muni <- sum(dat_muni$Valor[dat_muni$Ano == "2018"])
  total_state <- sum(dat_state$Valor[dat_state$Ano == "2018"])
  total_country <- sum(dat_country$Valor[dat_country$Ano == "2018"])

  expect_equal(total_muni, 58) # 50 + 8
  expect_equal(total_state, total_muni)
  expect_equal(total_country, total_muni)
  expect_false("estado" %in% names(dat_country))
})
