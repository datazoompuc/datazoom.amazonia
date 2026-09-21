# tests/testthat/test-aneel-schema.R
#
# load_aneel(dataset = "energy_generation") -- since 2026-09-18's migration
# off the dead GitLab xlsx onto ANEEL's own CKAN-hosted SIGA CSV (see
# resolve_aneel.R's header and NEWS.md), this dataset is read via
# data.table::fread() instead of readxl::read_xlsx(). The xlsx source
# stored mda_*/num_coord_* (power/coordinate) columns as real Excel
# numerics; the CSV source stores the exact same fields as
# Brazilian-locale strings ("1400,00", "-20,12479858"), same convention
# energy_enterprises_distributed already had to handle. Column names below
# reproduce the real siga-empreendimentos-geracao.csv header (verified
# live 2026-09-21), not a guess.
#
# fread() itself auto-parses a comma-free numeric column (no thousand
# separator, no decimal comma) as integer -- mda_potencia_fiscalizada_kw
# does this for real, so the fixture reproduces that mixed-type shape
# instead of making every mda_/num_coord_ column a string.

aneel_generation_fixture <- function() {
  tibble::tibble(
    DatGeracaoConjuntoDados = c("2026-09-01", "2026-09-01"),
    NomEmpreendimento = c("Usina Um", "Usina Dois"),
    IdeNucleoCEG = c("8", "9"),
    CodCEG = c("PCH.PH.MG.000008-6.1", "PCH.PH.MG.000009-4.1"),
    SigUFPrincipal = c("MG", "MG"),
    SigTipoGeracao = c("PCH", "UHE"),
    DscFaseUsina = c("Operacao", "Operacao"),
    DscOrigemCombustivel = c("Hidrica", "Hidrica"),
    DscFonteCombustivel = c("Potencial hidraulico", "Potencial hidraulico"),
    DscTipoOutorga = c("Autorizacao", "Concessao"),
    NomFonteCombustivel = c("Potencial hidraulico", "Potencial hidraulico"),
    DatEntradaOperacao = c("1953-11-20", "1937-01-01"),
    MdaPotenciaOutorgadaKw = c("1400,00", "100000,50"),
    MdaPotenciaFiscalizadaKw = c(1400L, 100000L), # fread already parsed this one as integer -- no comma in the source
    MdaGarantiaFisicaKw = c(",00", "1.234,56"), # leading-comma edge case + a real thousand separator
    IdcGeracaoQualificada = c("Nao", "Sim"),
    NumCoordNEmpreendimento = c("-20,12479858", "-29,06475000"),
    NumCoordEEmpreendimento = c("-43,87020250", "-51,55555000"),
    DatInicioVigencia = c("2000-07-07", "2005-01-01"),
    DatFimVigencia = c("2030-07-07", "2035-01-01"),
    DscPropriRegimePariticipacao = c("100% para X", "100% para Y"),
    DscSubBacia = c("41 - Das Velhas - Sao Francisco", "10 - Grande"),
    DscMuninicpios = c("Nova Lima - MG", "Ouro Preto - MG")
  )
}

test_that("energy_generation's mda_*/num_coord_* columns are parsed to numeric, not left as Brazilian-locale strings", {
  testthat::local_mocked_bindings(
    external_download = function(...) aneel_generation_fixture(),
    .package = "datazoom.amazonia"
  )

  # language = "pt": the eng-only rename map (fixed 2026-09-21 to also cover
  # mda_*/num_coord_* -> granted_power_kw/business_north_coordinate/etc, see
  # R/aneel.R) doesn't touch these column names, so this test can check the
  # numeric-cleaning step in isolation from the rename-map naming choices.
  dat <- load_aneel(dataset = "energy_generation", language = "pt")

  numeric_cols <- c(
    "mda_potencia_outorgada_kw", "mda_potencia_fiscalizada_kw",
    "mda_garantia_fisica_kw", "num_coord_n_empreendimento", "num_coord_e_empreendimento"
  )
  for (col in numeric_cols) {
    expect_true(is.numeric(dat[[col]]), info = col)
  }

  expect_equal(dat$mda_potencia_outorgada_kw, c(1400, 100000.5))
  expect_equal(dat$mda_garantia_fisica_kw, c(0, 1234.56))
  expect_equal(dat$num_coord_n_empreendimento, c(-20.12479858, -29.06475))
  expect_equal(dat$num_coord_e_empreendimento, c(-43.8702025, -51.55555))
})

test_that("an already-numeric mda_* column (fread's own auto-parse) survives the cleaning step unchanged", {
  testthat::local_mocked_bindings(
    external_download = function(...) aneel_generation_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_aneel(dataset = "energy_generation", language = "pt")
  expect_equal(dat$mda_potencia_fiscalizada_kw, c(1400, 100000))
})

test_that("energy_generation's eng rename map maps mda_*/num_coord_* to the expected English names", {
  testthat::local_mocked_bindings(
    external_download = function(...) aneel_generation_fixture(),
    .package = "datazoom.amazonia"
  )

  dat <- load_aneel(dataset = "energy_generation", language = "eng")
  expect_equal(dat$granted_power_kw, c(1400, 100000.5))
  expect_equal(dat$fiscalized_power_kw, c(1400, 100000))
  expect_equal(dat$physical_guarantee_kw, c(0, 1234.56))
  expect_equal(dat$business_north_coordinate, c(-20.12479858, -29.06475))
  expect_equal(dat$business_east_coordinate, c(-43.8702025, -51.55555))
})

test_that("energy_enterprises_distributed's own numeric cleaning still passes after being pulled out of the dataset-specific branch", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      tibble::tibble(
        NomEmpreendimento = c("A", "B"),
        MdaPotenciaInstaladaKw = c("5,50", "10,00"),
        NumCoordNEmpreendimento = c("-8,05", "-9,10"),
        DscModalidadeHabilitado = c("Compartilhada", "Auto consumo remoto")
      )
    },
    .package = "datazoom.amazonia"
  )

  dat <- load_aneel(dataset = "energy_enterprises_distributed")
  expect_equal(dat$installed_power_kw, c(5.5, 10))
  expect_equal(dat$business_north_coordinate, c(-8.05, -9.1))
  expect_true(is.numeric(dat$installed_power_kw))
})

# energy_development_budget's vlr_* (CDE discount/charge amount) columns --
# raw column names below reproduce the real cde-beneficiarios-rede-basica
# CSV header (verified live 2026-09-21). Note: this dataset's encoding fix
# (fread(encoding = "UTF-8"), replacing a long-standing wrong "Latin-1"
# assumption -- see R/download.R's aneel branch and NEWS.md) happens at the
# download layer, before external_download() is ever called from load_aneel(),
# so it isn't reachable through this mock -- it was verified live instead
# (real "Rede Básica" round-trips correctly; previously came through as
# "Rede BÃ¡sica"). This test only covers what R/aneel.R itself does: the
# numeric cleaning.
test_that("energy_development_budget's vlr_* columns are parsed to numeric, not left as Brazilian-locale strings", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      tibble::tibble(
        DatGeracaoConjuntoDados = c("23/07/2026 09:24:08", "23/07/2026 09:24:08"),
        NumAno = c(2023L, 2023L),
        NomMes = c("Janeiro", "Janeiro"),
        NomAgente = c("USINA A", "USINA B"),
        NumCNPJ = c("40551996000148", "24222394000190"),
        DscGrupoTarifario = c("Rede Basica", "Rede Basica"),
        IdcClasseConsumidor = c(9L, 9L),
        IdcTipo = c(7L, 7L),
        VlrDesconto = c("59643,00", "1.234,56"), # thousand-separator case
        VlrCobranca = c("189225,00", "")
      )
    },
    .package = "datazoom.amazonia"
  )

  # language = "pt": the eng-only rename map doesn't touch vlr_* names, so
  # this test can check the numeric-cleaning step in isolation.
  dat <- load_aneel(dataset = "energy_development_budget", year = 2023, language = "pt")

  expect_true(is.numeric(dat$vlr_desconto))
  expect_true(is.numeric(dat$vlr_cobranca))
  expect_equal(dat$vlr_desconto, c(59643, 1234.56))
  expect_equal(dat$vlr_cobranca, c(189225, NA_real_))
})

test_that("energy_development_budget's eng rename map maps vlr_*/num_ano/etc to the expected English names", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      tibble::tibble(
        DatGeracaoConjuntoDados = c("23/07/2026 09:24:08", "23/07/2026 09:24:08"),
        NumAno = c(2023L, 2023L),
        NomMes = c("Janeiro", "Janeiro"),
        NomAgente = c("USINA A", "USINA B"),
        NumCNPJ = c("40551996000148", "24222394000190"),
        DscGrupoTarifario = c("Rede Basica", "Rede Basica"),
        IdcClasseConsumidor = c(9L, 9L),
        IdcTipo = c(7L, 7L),
        VlrDesconto = c("59643,00", "1.234,56"),
        VlrCobranca = c("189225,00", "")
      )
    },
    .package = "datazoom.amazonia"
  )

  dat <- load_aneel(dataset = "energy_development_budget", year = 2023, language = "eng")
  expect_equal(dat$discount_value, c(59643, 1234.56))
  expect_equal(dat$charge_value, c(189225, NA_real_))
  expect_equal(dat$reference_year, c(2023L, 2023L))
  expect_equal(dat$agent_cnpj, c("40551996000148", "24222394000190"))
})

# energy_development_budget's dictionary used to have 17 "tipo_de_despesa"
# rows (CAFT CCEE, Programa Luz para Todos, ...) that never matched any real
# column -- the manifest's current resource (cde-beneficiarios-rede-basica)
# has no expense-by-category dimension at all. Dropped 2026-09-21 (verified
# against a locally saved copy of the real file, since dadosabertos.aneel.
# gov.br was unreachable at investigation time) and replaced with the one
# real, unambiguous value this resource has: dsc_grupo_tarifario is always
# "Rede Basica". idc_classe_consumidor (9/10) and idc_tipo (7/8) are bare
# ANEEL indicator codes with no verified meaning -- deliberately left
# untranslated at the VALUE level (only their column names are translated,
# via the eng rename map -- see R/aneel.R).
test_that("energy_development_budget's dictionary translates dsc_grupo_tarifario but leaves idc_* codes untranslated", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      tibble::tibble(
        DatGeracaoConjuntoDados = c("23/07/2026 09:24:08", "23/07/2026 09:24:08"),
        NumAno = c(2023L, 2023L),
        NomMes = c("Janeiro", "Janeiro"),
        NomAgente = c("USINA A", "USINA B"),
        NumCNPJ = c("40551996000148", "24222394000190"),
        DscGrupoTarifario = c("Rede Basica", "Rede Basica"),
        IdcClasseConsumidor = c(9L, 10L),
        IdcTipo = c(7L, 8L),
        VlrDesconto = c("59643,00", "1.234,56"),
        VlrCobranca = c("189225,00", "")
      )
    },
    .package = "datazoom.amazonia"
  )

  dat_pt <- load_aneel(dataset = "energy_development_budget", year = 2023, language = "pt")
  dat_eng <- load_aneel(dataset = "energy_development_budget", year = 2023, language = "eng")

  expect_equal(dat_pt$dsc_grupo_tarifario, c("rede_basica", "rede_basica"))
  expect_equal(dat_eng$tariff_group_description, c("basic_network", "basic_network"))
  # idc_* values pass through raw -- no dictionary entries exist for them
  expect_equal(dat_eng$consumer_class_indicator, c(9L, 10L))
  expect_equal(dat_eng$type_indicator, c(7L, 8L))
})

# energy_generation's dictionary (R/dictionary.R) used to key its ~variable
# column on short generic names ("fonte", "fase", "origem", "tipo", ...)
# that never matched any real post-janitor::clean_names() column
# (sig_tipo_geracao, dsc_fase_usina, ...), so the whole PT/EN value-recode
# step for this dataset was silently a no-op since it was added. Fixed by
# renaming the ~variable values to the real column identifiers -- verified
# live 2026-09-21 with zero mismatches against the real CSV's distinct
# values in every group. This test is what would have caught the original
# bug: it asserts the recode actually changes the value, not just that the
# pipeline runs without erroring.
test_that("energy_generation's dictionary recode actually translates raw ANEEL codes, not just passes them through", {
  testthat::local_mocked_bindings(
    external_download = function(...) aneel_generation_fixture(),
    .package = "datazoom.amazonia"
  )

  dat_pt <- load_aneel(dataset = "energy_generation", language = "pt")
  dat_eng <- load_aneel(dataset = "energy_generation", language = "eng")

  # aneel_generation_fixture()'s SigTipoGeracao values are "PCH"/"UHE".
  # Only the "eng" harmonization map renames sig_tipo_geracao ->
  # generation_type (see R/aneel.R) -- "pt" keeps the original column name,
  # just with the dictionary-translated value.
  expect_equal(dat_pt$sig_tipo_geracao, c("pequena_central_hidreletrica", "usina_hidreletrica"))
  expect_equal(dat_eng$generation_type, c("small_hydroelectric_center", "hydroelectric_power_plant"))
  expect_false(any(dat_eng$generation_type %in% c("PCH", "UHE")))
})

# energy_enterprises_distributed's dsc_modalidade_habilitado dictionary
# group used to have 4 var_codes ("Com Microgeracao ou Minigeracao
# distribuida", "Caracterizada como Autoconsumo remoto", ...) that were long
# descriptive strings matching zero real values -- the source uses short
# codes (Geracao na propria UC/Auto consumo remoto/Compartilhada/
# Condominio), the same ones R/aneel.R's separate case_when() block already
# expects for its own (different) business_type column. dsc_classe_consumo
# and dsc_fonte_geracao were correct but incomplete. All fixed and verified
# live 2026-09-21 against the real ~4.66M-row file -- see NEWS.md.
test_that("energy_enterprises_distributed's dictionary translates dsc_modalidade_habilitado, not just dsc_classe_consumo/dsc_fonte_geracao/dsc_porte", {
  testthat::local_mocked_bindings(
    external_download = function(...) {
      tibble::tibble(
        NomAgente = c("A", "B", "C"),
        DscClasseConsumo = c("Comercial", "Rural", "Residencial"),
        DscFonteGeracao = c("Etanol", "Oleo Diesel", "Radiacao solar"),
        DscPorte = c("Microgeracao", "Minigeracao", "Microgeracao"),
        DscModalidadeHabilitado = c("Geracao na propria UC", "Compartilhada", "Auto consumo remoto")
      )
    },
    .package = "datazoom.amazonia"
  )

  dat <- load_aneel(dataset = "energy_enterprises_distributed", language = "eng")

  expect_equal(dat$consumption_class_description, c("Commercial", "Rural", "Residential"))
  expect_equal(dat$generation_source_description, c("Ethanol", "Diesel Oil", "Solar radiation"))
  expect_equal(
    dat$business_type_description,
    c("Generation at Own Consumer Unit", "Shared Generation", "Remote Self-Consumption")
  )
  expect_false(any(dat$business_type_description == "Geracao na propria UC"))
})
