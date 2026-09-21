# load_seeg() / seeg_energy -- the fourth sector migrated to the v13.0
# "Dados" schema (see R/seeg.R's header for the full schema investigation).
# Offline: external_download() is mocked with a small synthetic fixture that
# mirrors the REAL v13.0 column layout, same convention as
# test-seeg-residuals.R's seeg_v13_fixture().

seeg_energy_fixture <- function() {
  base <- tibble::tibble(
    `Emissão/Remoção/Bunker` = "Emissão",
    `Setor de emissão` = c("Energia", "Energia", "Agropecuária"),
    `Categoria emissora` = c("Transportes", "Industrial", "Cultivo de arroz"),
    `Sub-categoria emissora` = c("Rodoviário", "Cimento", "Cultivo em sistema irrigado inundado"),
    Recorte = c("Emissões pela queima de combustíveis", "Emissões fugitivas", "Diretas"),
    `Atividade geral` = c("Transporte de carga", "Cimento", "Agricultura"),
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

test_that("seeg_energy/municipality/pt: only Energia rows kept, recorte preserved", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_energy_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_energy", raw_data = FALSE, geo_level = "municipality", language = "pt")

  expect_equal(nrow(dat), 2 * 55)
  expect_setequal(
    names(dat),
    c("Ano", "municipio", "estado", "ibge", "setor", "categoria_emissao",
      "subcategoria_emissao", "recorte", "atividade_geral", "tipo_emissao", "Valor")
  )
  expect_true(all(dat$setor == "Energia"))
  expect_setequal(dat$recorte, c("Emissoes pela queima de combustiveis", "Emissoes fugitivas"))
})

test_that("seeg_energy/municipality/eng: every value translated, none left NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_energy_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_energy", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_setequal(
    names(dat),
    c("year", "city", "state", "ibge", "sector", "emission_category",
      "emission_subcategory", "emission_scope", "general_activity", "emission_type", "value")
  )
  expect_true(all(dat$sector == "Energy"))
  expect_setequal(dat$emission_scope, c("Fuel combustion emissions", "Fugitive emissions"))
  expect_setequal(dat$general_activity, c("Freight transport", "Cement"))

  expect_false(anyNA(dat$emission_category))
  expect_false(anyNA(dat$emission_subcategory))
  expect_false(anyNA(dat$emission_scope))
  expect_false(anyNA(dat$general_activity))
  expect_false(anyNA(dat$emission_type))
})

test_that("seeg_energy/state/pt and country/pt: totals reconcile with municipality level", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_energy_fixture(),
    .package = "datazoom.amazonia"
  )

  dat_muni <- load_seeg(dataset = "seeg_energy", raw_data = FALSE, geo_level = "municipality", language = "pt")
  dat_state <- load_seeg(dataset = "seeg_energy", raw_data = FALSE, geo_level = "state", language = "pt")
  dat_country <- load_seeg(dataset = "seeg_energy", raw_data = FALSE, geo_level = "country", language = "pt")

  total_muni <- sum(dat_muni$Valor[dat_muni$Ano == "2018"])
  total_state <- sum(dat_state$Valor[dat_state$Ano == "2018"])
  total_country <- sum(dat_country$Valor[dat_country$Ano == "2018"])

  expect_equal(total_muni, 58) # 50 + 8
  expect_equal(total_state, total_muni)
  expect_equal(total_country, total_muni)
  expect_false("estado" %in% names(dat_country))
})
