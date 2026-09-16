# load_seeg() / seeg_farming -- the second sector migrated to the v13.0
# "Dados" schema (see R/seeg.R's header for the full schema investigation).
# Offline: external_download() is mocked with a small synthetic fixture that
# mirrors the REAL v13.0 column layout, same convention as
# test-seeg-residuals.R's seeg_v13_fixture().

seeg_farming_fixture <- function() {
  base <- tibble::tibble(
    `Emissão/Remoção/Bunker` = c("Emissão", "Emissão NCI", "Remoção NCI", "Emissão"),
    `Setor de emissão` = c("Agropecuária", "Agropecuária", "Agropecuária", "Resíduos"),
    `Categoria emissora` = c(
      "Cultivo de arroz", "Solos manejados", "Solos manejados", "Disposição final"
    ),
    `Sub-categoria emissora` = c(
      "Cultivo em sistema irrigado inundado",
      "Aumento do estoque de C no solo",
      "Redução do estoque de C no solo",
      "Disposição final em aterros sanitários"
    ),
    Recorte = c("Diretas", "Diretas", "Diretas", "Residuos solidos"),
    `Atividade geral` = c("Agricultura", "Silvicultura", "Pecuária", "Saneamento Básico"),
    Estado = c("Paraná", "Paraná", "Amazonas", "Amazonas"),
    Município = c("Abatiá (PR)", "Abatiá (PR)", "Manaus (AM)", "Manaus (AM)"),
    `ID Território` = c("14100103", "14100103", "11302603", "11302603")
  )

  years <- 1970:2024
  year_cols <- stats::setNames(
    lapply(years, function(y) if (y == 2018) c(100, 20, 5, 999) else c(0, 0, 0, 0)),
    as.character(years)
  )

  dplyr::bind_cols(base, tibble::as_tibble(year_cols))
}

test_that("seeg_farming/municipality/pt: renames applied, only Agropecuaria rows kept", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_farming_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "municipality", language = "pt")

  # Only the 3 Agropecuaria rows survive the sector filter, x55 years each.
  expect_equal(nrow(dat), 3 * 55)
  expect_setequal(
    names(dat),
    c("Ano", "municipio", "estado", "ibge", "setor", "categoria_emissao",
      "subcategoria_emissao", "via_emissao", "atividade_geral", "tipo_emissao", "Valor")
  )
  expect_true(all(dat$setor == "Agropecuaria"))

  # NCI bunker values kept distinct, not merged into Emissao/Remocao.
  expect_setequal(dat$tipo_emissao, c("Emissao", "Emissao NCI", "Remocao NCI"))
})

test_that("seeg_farming/municipality/eng: every value translated, none left NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_farming_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_setequal(
    names(dat),
    c("year", "city", "state", "ibge", "sector", "emission_category",
      "emission_subcategory", "emission_pathway", "general_activity", "emission_type", "value")
  )
  expect_true(all(dat$sector == "Agriculture and Livestock"))
  expect_setequal(dat$emission_type, c("Emission", "Emission NCI", "Removal NCI"))
  expect_setequal(dat$general_activity, c("Agriculture", "Forestry", "Livestock"))

  # No column should have an NA introduced by a missed translation.
  expect_false(anyNA(dat$emission_category))
  expect_false(anyNA(dat$emission_subcategory))
  expect_false(anyNA(dat$emission_pathway))
  expect_false(anyNA(dat$general_activity))
  expect_false(anyNA(dat$emission_type))
})

test_that("seeg_farming/state/pt and country/pt: totals reconcile with municipality level", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_farming_fixture(),
    .package = "datazoom.amazonia"
  )

  dat_muni <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "municipality", language = "pt")
  dat_state <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "state", language = "pt")
  dat_country <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "country", language = "pt")

  total_muni <- sum(dat_muni$Valor[dat_muni$Ano == "2018"])
  total_state <- sum(dat_state$Valor[dat_state$Ano == "2018"])
  total_country <- sum(dat_country$Valor[dat_country$Ano == "2018"])

  expect_equal(total_muni, 125) # 100 + 20 + 5
  expect_equal(total_state, total_muni)
  expect_equal(total_country, total_muni)
  expect_false("estado" %in% names(dat_country))
})

test_that("seeg_farming does not leak Residuos-sector rows through the sector filter", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_farming_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_farming", raw_data = FALSE, geo_level = "municipality", language = "pt")
  expect_false("Disposicao final" %in% dat$categoria_emissao)
})
