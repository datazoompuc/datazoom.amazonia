# tests/testthat/test-translate-variables.R
#
# Offline test for translate_sidra_variables() (R/dictionary.R): the step that
# names a SIDRA table's variable column from IBGE's variable code, shared by
# load_pam() and load_ppm(). It used to be a hand-written case_when() in each
# loader; a code nobody had listed became NA (load_pam: an unnamed na_v<crop>
# column for every permanent crop, 2026-10) and, in load_ppm(), still would.
# Here the dictionary is a stand-in, so what is tested is the mechanism: the
# names, both languages, and the fallback with its warning. That each loader's
# real entry names the ids its tables report is tested with the loader
# (test-pam-variables.R, test-ppm-variables.R).

FAKE_VARIABLES <- tibble::tribble(
  ~var_code, ~var_pt, ~var_eng,
  112, "rend_medio", "avg_yield",
  215, "valor", "value",
  8331, "area_plantada_dest_colheita", "planted_to_harvest_area",
  2313, "area_plantada_dest_colheita", "planted_to_harvest_area",
  100000, "grande", "big"
)

# IBGE's variable column, as the loaders hand it over: character, accents stripped
raw_variables <- function(codes, names = paste("Nome", codes)) {
  tibble::tibble(
    geo_id = "1", ano = "2023",
    variavel_codigo = as.character(codes), variavel = names,
    valor = seq_along(codes) * 10
  )
}

translate <- function(dat, language = "pt", dictionary = "fake_variables", caller = "load_fake()", ...) {
  seen <- new.env()
  out <- testthat::with_mocked_bindings(
    translate_sidra_variables(dat, dictionary, language, caller, ...),
    load_dictionary = function(dataset) {
      seen$key <- dataset
      FAKE_VARIABLES
    },
    .package = "datazoom.amazonia"
  )
  attr(out, "asked_for") <- seen$key
  out
}

test_that("a code is named from the dictionary, in the language asked for", {
  dat <- raw_variables(c("112", "215", "8331"))

  pt <- translate(dat, "pt")
  expect_equal(pt$variavel, c("rend_medio", "valor", "area_plantada_dest_colheita"))
  eng <- translate(dat, "eng")
  expect_equal(eng$variavel, c("avg_yield", "value", "planted_to_harvest_area"))
})

test_that("two ids can share one name, and a six-digit id is not read as scientific notation", {
  expect_equal(translate(raw_variables(c("8331", "2313")), "eng")$variavel, rep("planted_to_harvest_area", 2))
  # as.character(100000) is "1e+05"
  expect_equal(translate(raw_variables("100000"), "pt")$variavel, "grande")
})

test_that("nothing but the variable column changes: other columns, their order, and the rows", {
  dat <- raw_variables(c("215", "112", "215"))
  out <- translate(dat, "pt")
  expect_equal(names(out)[names(out) != "variavel"], setdiff(names(dat), "variavel"))
  expect_equal(names(out), names(dat))
  expect_equal(out$valor, dat$valor)
  expect_equal(out$variavel_codigo, dat$variavel_codigo)
  expect_equal(nrow(out), 3)
  expect_type(out$variavel, "character")
})

test_that("it reads the dictionary key it is given", {
  expect_equal(attr(translate(raw_variables("112"), dictionary = "ppm_something_variables"), "asked_for"), "ppm_something_variables")
})

test_that("an id the dictionary does not name keeps its values under v<id>, never NA, and the call warns naming it", {
  dat <- raw_variables(c("112", "99999", "215"), names = c("Rendimento", "Variavel nova", "Valor"))

  warnings <- character(0)
  out <- withCallingHandlers(
    translate(dat, "pt", caller = "load_fake()", columns = "v<id>_<crop>"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(out$variavel, c("rend_medio", "v99999", "valor"))
  expect_false(anyNA(out$variavel))
  expect_equal(out$valor, dat$valor) # the figure is still there
  expect_length(warnings, 1)
  expect_match(warnings, "99999 (Variavel nova)", fixed = TRUE) # IBGE's own name for it
  expect_match(warnings, "load_fake()", fixed = TRUE) # which loader
  expect_match(warnings, "v<id>_<crop>", fixed = TRUE) # what its columns look like
  expect_match(warnings, "github.com/datazoompuc/datazoom.amazonia/issues", fixed = TRUE)
})

test_that("several unnamed ids are all in ONE warning, each once however many rows carry it, in the order they appear", {
  dat <- raw_variables(c("99999", "112", "88888", "99999", "88888"), names = c("Um", "Rendimento", "Dois", "Um", "Dois"))

  warnings <- character(0)
  out <- withCallingHandlers(
    translate(dat, "eng"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(out$variavel, c("v99999", "avg_yield", "v88888", "v99999", "v88888"))
  expect_length(warnings, 1)
  expect_match(warnings, "99999 (Um), 88888 (Dois)", fixed = TRUE)
})

test_that("the fallback can never equal a real name, so it cannot be mistaken for one or collide with one", {
  out <- suppressWarnings(translate(raw_variables(c("99999", "112")), "pt"))
  real <- c(FAKE_VARIABLES$var_pt, FAKE_VARIABLES$var_eng)
  expect_false(out$variavel[1] %in% real)
  expect_false(any(grepl("^v[0-9]+$", real)))
})

test_that("with every id named there is no warning, and an empty table passes through", {
  expect_no_warning(translate(raw_variables(c("112", "215", "8331")), "pt"))
  expect_no_warning(empty <- translate(raw_variables(character(0)), "eng"))
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), names(raw_variables("112")))
})
