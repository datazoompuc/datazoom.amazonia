# tests/testthat/test-epe-schema.R
#
# Unit tests for R/epe.R's epe_energy_state_panel_treat() -- extracted out
# of load_epe() specifically so it can be exercised without a network
# download (see R/epe.R's header, and the "closures inside resolve_<source>()
# can't be unit-tested" gotcha in the manifest-maintenance skill, which this
# follows the same pattern as mapbiomas_treat() to work around).
#
# The fixture below reproduces the REAL shape of
# tabela_geracao_eletricidade_por_fonte.xlsx (macro_grupo/grupo/fonte/ano/
# valor, long format), verified live 2026-09-08 -- see resolve_epe.R's own
# header for the investigation. All 16 real `fonte` values are included for
# each state/year so the pivot produces a complete row.

FONTE_VALUES <- c(
  "Geração total", "Hidro", "Eólica", "Solar", "Nuclear", "Termo",
  "Bagaço de cana", "Lenha", "Lixívia", "Out. Fontes renováveis",
  "Carvão vapor", "Gás natural", "Gás de coqueria", "Óleo combustível",
  "Óleo diesel", "Out. Fontes não renováveis"
)

# One (grupo, macro_grupo, ano) block: 16 fonte rows, arbitrary distinct
# values so a wrong pivot/rename is easy to spot in a failing assertion.
make_block <- function(grupo, macro_grupo, ano, base_value = 1) {
  tibble::tibble(
    macro_grupo = macro_grupo,
    grupo = grupo,
    fonte = FONTE_VALUES,
    ano = ano,
    valor = base_value + seq_along(FONTE_VALUES)
  )
}

build_fixture <- function() {
  dplyr::bind_rows(
    make_block("Amazonas", "Norte", 2024, base_value = 0),
    make_block("Mato Grosso Do Sul", "Centro Oeste", 2024, base_value = 100),
    make_block("São Paulo", "Sudeste", 2024, base_value = 200),
    make_block("Brasil", "Brasil", 2024, base_value = 900)
  )
}

test_that("energy_state_panel pivots long-format fonte rows into 16 named columns", {
  out <- epe_energy_state_panel_treat(build_fixture())

  expected_cols <- c(
    "uf", "ano", "total_produzido", "hidro", "eolica", "solar", "nuclear",
    "termo", "cana", "lenha", "lixivia", "outras_fontes_renovaveis",
    "carvao_vapor", "gas_natural", "gas_de_coqueira", "combustivel",
    "diesel", "outras_fontes_nao_renovaveis", "amz_legal"
  )
  expect_setequal(names(out), expected_cols)
  expect_equal(nrow(out), 3) # Amazonas, Mato Grosso do Sul, Sao Paulo -- "Brasil" dropped
})

test_that("the 'Brasil' aggregate row is dropped, not pivoted in as a 28th state", {
  out <- epe_energy_state_panel_treat(build_fixture())
  expect_false("Brasil" %in% out$uf)
  expect_false(any(grepl("brasil", out$uf, ignore.case = TRUE)))
})

test_that("fonte values map to the exact columns load_epe()'s English rename expects", {
  out <- epe_energy_state_panel_treat(make_block("Acre", "Norte", 2020, base_value = 0))
  # base_value = 0, so fonte_map's Nth entry (in FONTE_VALUES order) is worth N
  row <- out[out$uf == "Acre" & out$ano == 2020, ]
  expect_equal(row$total_produzido, 1) # "Geração total" is FONTE_VALUES[1]
  expect_equal(row$hidro, 2) # "Hidro" is FONTE_VALUES[2]
  expect_equal(row$outras_fontes_nao_renovaveis, 16) # last entry
})

test_that("the source's 'Do'/'De' capitalization is normalized to the standard lowercase connector", {
  out <- epe_energy_state_panel_treat(build_fixture())
  expect_true("Mato Grosso do Sul" %in% out$uf)
  expect_false("Mato Grosso Do Sul" %in% out$uf)
})

test_that("accented state names are transliterated to ASCII", {
  out <- epe_energy_state_panel_treat(build_fixture())
  expect_true("Sao Paulo" %in% out$uf)
  expect_false("São Paulo" %in% out$uf)
})

test_that("amz_legal is 1 for a Legal Amazon state and 0 otherwise", {
  out <- epe_energy_state_panel_treat(build_fixture())
  expect_equal(out$amz_legal[out$uf == "Amazonas"], 1)
  expect_equal(out$amz_legal[out$uf == "Mato Grosso do Sul"], 0) # NOT the same as "Mato Grosso"
  expect_equal(out$amz_legal[out$uf == "Sao Paulo"], 0)
})

test_that("an unrecognized fonte value stops with a clear message instead of silently dropping data", {
  bad <- make_block("Acre", "Norte", 2020)
  bad$fonte[1] <- "Uma Fonte Nova"
  expect_error(
    epe_energy_state_panel_treat(bad),
    "new/changed .fonte. values"
  )
})

test_that("every (uf, ano) key has all 16 fonte columns populated -- no partial pivot", {
  out <- epe_energy_state_panel_treat(build_fixture())
  value_cols <- setdiff(names(out), c("uf", "ano", "amz_legal"))
  expect_equal(length(value_cols), 16)
  expect_true(all(!is.na(as.matrix(out[value_cols]))))
})
