# load_seeg() / seeg_residuals -- the first sector migrated to the v13.0
# "Dados" schema (see R/seeg.R's header for the full schema investigation).
# Offline: external_download() is mocked with a small synthetic fixture that
# mirrors the REAL v13.0 column layout (Emissão/Remoção/Bunker, Setor de
# emissão, Categoria emissora, Sub-categoria emissora, Recorte, Atividade
# geral, Estado, Município, ID Território, year columns) and the two real
# quirks confirmed live this session: the " (UF)" suffix on município, and
# the single "1BR"/unallocated pseudo-row (see R/seeg.R's seeg_residuals
# header comment).

seeg_v13_fixture <- function() {
  base <- tibble::tibble(
    `Emissão/Remoção/Bunker` = "Emissão",
    `Setor de emissão` = c(
      "Resíduos", "Resíduos", "Resíduos", "Agropecuária"
    ),
    `Categoria emissora` = c(
      "Disposição final", "Efluentes domésticos", "Efluentes líquidos industriais",
      "Cultivo de arroz"
    ),
    `Sub-categoria emissora` = c(
      "Disposição final em aterros sanitários",
      "Tratamento e despejo de efluentes domésticos",
      "Produção de Cerveja",
      "Irrigado"
    ),
    Recorte = c("Residuos solidos", "Efluentes Líquidos", "Diretas", "Diretas"),
    `Atividade geral` = c(
      "Saneamento Básico", "Saneamento Básico", "Produção Industrial", "Agricultura"
    ),
    Estado = c("Paraná", "Amazonas", "Não Alocado", "Goiás"),
    Município = c("Abatiá (PR)", "Manaus (AM)", "BR (NA)", "Abadia de Goias (GO)"),
    `ID Território` = c("14100103", "11302603", "1BR", "15200050")
  )

  # The real v13.0 Dados sheet always carries the full 1970..2024 year span
  # (confirmed live, see R/seeg.R's header) -- load_seeg() hardcodes
  # x1970:x2024 against that confirmed real range, so this fixture must
  # carry the same span rather than a token subset. Every year is 0 except
  # 2018/2019, which carry the same values the old 2-year fixture used.
  years <- 1970:2024
  year_cols <- stats::setNames(
    lapply(years, function(y) {
      if (y == 2018) c(100, 200, 5, 999) else if (y == 2019) c(110, 210, 6, 999) else c(0, 0, 0, 0)
    }),
    as.character(years)
  )

  dplyr::bind_cols(base, tibble::as_tibble(year_cols))
}

test_that("seeg_residuals/municipality/pt: renames, translations skipped, 1BR row coerced to NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_v13_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_residuals", raw_data = FALSE, geo_level = "municipality", language = "pt")

  # Only the 3 Residuos rows survive the sector filter, x55 years each
  # (the fixture carries the full confirmed real 1970..2024 span).
  expect_equal(nrow(dat), 3 * 55)
  expect_setequal(
    names(dat),
    c("Ano", "municipio", "estado", "ibge", "setor", "categoria_emissao",
      "subcategoria_emissao", "tipo_residuo", "atividade_geral", "tipo_emissao", "Valor")
  )

  # Real categories are already clean Portuguese words -- no case_when
  # translation needed/applied, just ASCII-stripped.
  expect_true(all(dat$categoria_emissao %in% c(
    "Disposicao final", "Efluentes domesticos", "Efluentes liquidos industriais"
  )))

  # The 1BR/unallocated row: municipio/estado/ibge all NA, value preserved
  # for the 2 years the fixture gives it a real (non-zero) value.
  unallocated <- dat[is.na(dat$municipio), ]
  expect_equal(nrow(unallocated), 55)
  expect_true(all(is.na(unallocated$estado)))
  expect_true(all(is.na(unallocated$ibge)))
  expect_setequal(
    unallocated$Valor[unallocated$Ano %in% c("2018", "2019")],
    c(5, 6)
  )

  # A real municipality: " (UF)" suffix stripped, ibge = id_territorio minus
  # its leading "1". %in% (not ==) so the NA-municipio row isn't pulled in.
  abatia <- dat[dat$municipio %in% "Abatia", ]
  expect_equal(unique(abatia$ibge), "4100103")
  expect_equal(unique(abatia$estado), "Parana")
  expect_setequal(abatia$Valor[abatia$Ano %in% c("2018", "2019")], c(100, 110))
})

test_that("seeg_residuals/municipality/eng: values translated, 1BR row coerced to NA", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_v13_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_residuals", raw_data = FALSE, geo_level = "municipality", language = "eng")

  expect_setequal(
    names(dat),
    c("year", "city", "state", "ibge", "sector", "emission_category",
      "emission_subcategory", "waste_stream", "general_activity", "emission_type", "value")
  )
  expect_true(all(dat$sector == "Waste"))
  expect_setequal(
    dat$emission_category,
    c("Final disposal", "Domestic effluents", "Industrial liquid effluents")
  )

  unallocated <- dat[is.na(dat$city), ]
  expect_equal(nrow(unallocated), 55)
  expect_true(all(is.na(unallocated$state)))
})

test_that("seeg_residuals/state/pt: real sum over municipalities, Nao Alocado is its own state bucket", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_v13_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_residuals", raw_data = FALSE, geo_level = "state", language = "pt")

  expect_true("Nao Alocado" %in% dat$estado)
  na_row <- dat[dat$estado == "Nao Alocado" & dat$Ano == "2018", ]
  expect_equal(na_row$Valor, 5)

  parana_row <- dat[dat$estado == "Parana" & dat$Ano == "2018", ]
  expect_equal(parana_row$Valor, 100)
})

test_that("seeg_residuals/country/pt: sums across every state including the unallocated row", {
  testthat::local_mocked_bindings(
    external_download = function(...) seeg_v13_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_seeg(dataset = "seeg_residuals", raw_data = FALSE, geo_level = "country", language = "pt")

  # 3 distinct category combos (disposal, domestic effluent, industrial effluent) x 55 years.
  expect_equal(nrow(dat), 3 * 55)
  expect_false("estado" %in% names(dat))

  industrial <- dat[dat$categoria_emissao == "Efluentes liquidos industriais" & dat$Ano == "2018", ]
  expect_equal(industrial$Valor, 5) # only the unallocated row has this category here
})
