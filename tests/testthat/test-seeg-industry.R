# load_seeg() / seeg_industry -- the third sector migrated to the v13.0
# "Dados" schema (see R/seeg.R's header for the full schema investigation).
# Offline: external_download() is mocked with a small synthetic fixture that
# mirrors the REAL v13.0 column layout, same convention as
# test-seeg-residuals.R's seeg_v13_fixture().

seeg_industry_fixture <- function() {
  base <- tibble::tibble(
    `Emissão/Remoção/Bunker` = "Emissão",
    `Setor de emissão` = c("Processos Industriais", "Processos Industriais", "Agropecuária"),
    `Categoria emissora` = c("Produção de metais", "Produção e uso de HFCs", "Cultivo de arroz"),
    `Sub-categoria emissora` = c(
      "Produção de alumínio", "Montagem de equipamento ou produto", "Cultivo em sistema irrigado inundado"
    ),
    Recorte = c("Emissão efetiva", "Emissão potencial", "Diretas"),
    `Atividade geral` = c("Metalurgia", "Transporte de carga", "Agricultura"),
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

test_that("seeg_industry/municipality/pt: only Processos Industriais rows kept, recorte preserved", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_industry_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_industry", raw_data = FALSE, geo_level = "municipality", language = "pt")

  expect_equal(nrow(dat), 2 * 55)
  expect_setequal(
    names(dat),
    c("Ano", "municipio", "estado", "ibge", "setor", "categoria_emissao",
      "subcategoria_emissao", "recorte", "atividade_geral", "tipo_emissao", "Valor")
  )
  expect_true(all(dat$setor == "Processos Industriais"))
  expect_setequal(dat$recorte, c("Emissao efetiva", "Emissao potencial"))
})

test_that("seeg_industry/municipality/eng: every value translated, none left NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_industry_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_industry", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_setequal(
    names(dat),
    c("year", "city", "state", "ibge", "sector", "emission_category",
      "emission_subcategory", "emission_scope", "general_activity", "emission_type", "value")
  )
  expect_true(all(dat$sector == "Industrial Processes"))
  expect_setequal(dat$emission_scope, c("Actual emission", "Potential emission"))
  expect_setequal(dat$general_activity, c("Metallurgy", "Freight transport"))

  expect_false(anyNA(dat$emission_category))
  expect_false(anyNA(dat$emission_subcategory))
  expect_false(anyNA(dat$emission_scope))
  expect_false(anyNA(dat$general_activity))
  expect_false(anyNA(dat$emission_type))
})

test_that("seeg_industry/state/pt and country/pt: totals reconcile with municipality level", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_industry_fixture(),
    .package = "datazoom.amazonia"
  )

  dat_muni <- load_seeg(dataset = "seeg_industry", raw_data = FALSE, geo_level = "municipality", language = "pt")
  dat_state <- load_seeg(dataset = "seeg_industry", raw_data = FALSE, geo_level = "state", language = "pt")
  dat_country <- load_seeg(dataset = "seeg_industry", raw_data = FALSE, geo_level = "country", language = "pt")

  total_muni <- sum(dat_muni$Valor[dat_muni$Ano == "2018"])
  total_state <- sum(dat_state$Valor[dat_state$Ano == "2018"])
  total_country <- sum(dat_country$Valor[dat_country$Ano == "2018"])

  expect_equal(total_muni, 58) # 50 + 8
  expect_equal(total_state, total_muni)
  expect_equal(total_country, total_muni)
  expect_false("estado" %in% names(dat_country))
})
