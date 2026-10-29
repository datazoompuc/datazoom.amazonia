# tests/testthat/test-ppm-variables.R
#
# Offline test for load_ppm()'s naming of IBGE's variable ids (the
# "<dataset>_variables" entries of load_dictionary(), applied by
# translate_sidra_variables()). Until this was a dictionary it was five
# case_when() blocks, one per dataset, with no default: an id nobody had listed
# became NA, and the pivot made it an unnamed na_v<category> column -- the bug
# load_pam() had with variable 2313. Nothing tested it: load_ppm() had no test
# of its output. Now an unlisted id is v<id> with a warning (the helper itself is
# tested in test-translate-variables.R); this file holds each dataset to the
# names it has always returned.
#
# sidra_download() is mocked (no network) and returns the shape the real one does
# for a Brazil-level query, janitor-cleaned, every column character except valor.
# The variables per table are the ones IBGE's metadata declares and sidrar returns
# (checked live 2026-10-22, "allxp" = everything but the "percentual do total
# geral" variables, which are never requested).

# dataset -> the variables its table reports (id = IBGE's name), its classification column (NULL: none) and two categories
PPM_FAKE <- list(
  ppm_livestock_inventory = list(vars = c("105" = "Efetivo dos rebanhos"), class = "tipo_de_rebanho", cats = c("0", "2670")),
  ppm_sheep_farming = list(vars = c("108" = "Ovinos tosquiados"), class = NULL, cats = "108"),
  ppm_cow_farming = list(vars = c("107" = "Vacas ordenhadas"), class = NULL, cats = "107"),
  ppm_animal_origin_production = list(
    vars = c("106" = "Producao de origem animal", "215" = "Valor da producao"),
    class = "tipo_de_produto_de_origem_animal", cats = c("0", "2682")
  ),
  ppm_aquaculture = list(
    vars = c("4146" = "Producao da aquicultura", "215" = "Valor da producao"),
    class = "tipo_de_produto_da_aquicultura", cats = c("0", "32861")
  )
)

# the stem each id has always had, in each language
STEMS <- list(
  pt = c("105" = "num", "108" = "num", "107" = "num", "106" = "quant", "215" = "valor", "4146" = "quant"),
  eng = c("105" = "num", "108" = "num", "107" = "num", "106" = "quant", "215" = "value", "4146" = "quant")
)

fake_value <- function(variable, cat) 1000 * match(cat, c("0", "2670", "2682", "32861", "108", "107")) + as.integer(variable) %% 997

# What sidra_download() returns: one row per variable x category. `extra` adds
# variables nobody has named: c(id = "IBGE's name").
fake_sidra <- function(dataset, extra = character(0)) {
  spec <- PPM_FAKE[[dataset]]
  variables <- c(spec$vars, extra)
  grid <- expand.grid(variavel_codigo = names(variables), cat = spec$cats, stringsAsFactors = FALSE)
  # sidrar's own column order matters: the pivots select the range variavel:<classification>_codigo
  out <- tibble::tibble(
    nivel_territorial_codigo = "1", nivel_territorial = "Brasil",
    unidade_de_medida_codigo = "1", unidade_de_medida = "Cabecas",
    valor = as.numeric(mapply(fake_value, grid$variavel_codigo, grid$cat)),
    brasil_codigo = "1", brasil = "Brasil",
    ano_codigo = "2023", ano = "2023",
    variavel_codigo = grid$variavel_codigo, variavel = unname(variables[grid$variavel_codigo])
  )
  if (!is.null(spec$class)) {
    out[[paste0(spec$class, "_codigo")]] <- grid$cat
    out[[spec$class]] <- paste("Categoria", grid$cat)
  }
  out
}

load_fake <- function(dataset, language, extra = character(0)) {
  testthat::local_mocked_bindings(sidra_download = function(...) fake_sidra(dataset, extra), .package = "datazoom.amazonia")
  suppressMessages(load_ppm(dataset = dataset, geo_level = "country", time_period = 2023, language = language))
}

# the columns load_ppm() must return besides the year: <stem>_v<category>, lower case
expected_columns <- function(dataset, language) {
  spec <- PPM_FAKE[[dataset]]
  ids <- names(spec$vars)
  unlist(lapply(ids, function(id) paste0(STEMS[[language]][[id]], "_v", spec$cats)))
}

test_that("the datasets used here are the manifest's five, each with a dictionary entry of its own", {
  committed <- link_table()[link_table()$survey == "ppm", ]
  expect_setequal(committed$dataset, names(PPM_FAKE))
  for (dataset in names(PPM_FAKE)) {
    expect_true(nrow(load_dictionary(paste0(dataset, "_variables"))) >= 1, info = dataset)
  }
})

for (dataset in names(PPM_FAKE)) {
  for (language in c("pt", "eng")) {
    test_that(sprintf("load_ppm('%s', language = '%s') names every variable, with no na_ column and no warning", dataset, language), {
      expect_no_warning(out <- load_fake(dataset, language))

      year <- if (language == "pt") "ano" else "year"
      expect_setequal(setdiff(names(out), c("geo_id", year)), expected_columns(dataset, language))
      expect_false(any(grepl("^na_", names(out))))

      # the figures arrive under the right names
      spec <- PPM_FAKE[[dataset]]
      for (id in names(spec$vars)) {
        for (cat in spec$cats) {
          column <- paste0(STEMS[[language]][[id]], "_v", cat)
          expect_equal(as.numeric(out[[column]]), fake_value(id, cat), info = paste(dataset, column)) # (the column also carries a label)
        }
      }
    })
  }
}

test_that("each dictionary entry names exactly the ids its table reports, and its names are the ones load_ppm() has always used", {
  for (dataset in names(PPM_FAKE)) {
    dic <- load_dictionary(paste0(dataset, "_variables"))
    ids <- names(PPM_FAKE[[dataset]]$vars)
    codes <- as.character(dic$var_code)
    expect_setequal(codes, ids)
    expect_equal(stats::setNames(dic$var_pt, codes)[ids], STEMS$pt[ids], info = dataset)
    expect_equal(stats::setNames(dic$var_eng, codes)[ids], STEMS$eng[ids], info = dataset)
  }
})

test_that("an id the dataset's entry does not name keeps its values under v<id>, never an na_ column, and the call warns once naming it", {
  for (dataset in names(PPM_FAKE)) {
    for (language in c("pt", "eng")) {
      warnings <- character(0)
      out <- withCallingHandlers(
        load_fake(dataset, language, extra = c("99999" = "Variavel nova")),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      info <- paste(dataset, language)
      spec <- PPM_FAKE[[dataset]]

      expect_false(any(grepl("^na_", names(out))), info = info)
      for (cat in spec$cats) {
        # tables 94 and 95 have no classification: the pivot names a column by the variable id twice
        column <- if (is.null(spec$class)) "v99999_v99999" else paste0("v99999_v", cat)
        expect_equal(as.numeric(out[[column]]), fake_value("99999", cat), info = info)
      }
      # the ids it does name are untouched by the extra one
      expect_true(all(expected_columns(dataset, language) %in% names(out)), info = info)
      expect_length(warnings, 1)
      expect_match(warnings, "99999 (Variavel nova)", fixed = TRUE)
      expect_match(warnings, "load_ppm()", fixed = TRUE)
      expect_match(warnings, "v<id>_v<category code>", fixed = TRUE)
    }
  }
})

test_that("a variable that belongs to another PPM table is not silently accepted: 215 on table 94 is unnamed there", {
  # Each dataset names only what its own table reports (the point of five entries, not one shared list): value (215) is
  # a variable of tables 74 and 3940, so if IBGE ever added it to the cow table it would be flagged, not named.
  warnings <- character(0)
  out <- withCallingHandlers(
    load_fake("ppm_cow_farming", "eng", extra = c("215" = "Valor da producao")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true("v215_v215" %in% names(out)) # (no classification: the pivot names the column by the variable id twice)
  expect_false("value_v107" %in% names(out))
  expect_match(warnings, "215 (Valor da producao)", fixed = TRUE)
})
