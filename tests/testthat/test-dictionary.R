# tests/testthat/test-dictionary.R
#
# Offline test for load_dictionary() (R/dictionary.R). It had none. It is one
# long if-chain of tibble::tribble() blocks, one per key, and holds two kinds of
# entry: PRODUCT dictionaries (a crop, a livestock category, a country: code ->
# label, with units; named after the dataset) and VARIABLE dictionaries (IBGE's
# variable id -> the stem of the column name; keys ending in "_variables", used
# by translate_sidra_variables()).
#
# The keys are read from the function itself, so an entry added later is covered
# without editing this file.

dictionary_keys <- function() {
  code <- paste(deparse(body(load_dictionary)), collapse = "\n")
  sub('dataset == "(.*)"', "\\1", unlist(regmatches(code, gregexpr('dataset == "[A-Za-z0-9_]+"', code))))
}
KEYS <- dictionary_keys()
VARIABLE_KEYS <- grep("_variables$", KEYS, value = TRUE)

test_that("the keys are read from the function, and none is written twice (a second block would silently replace the first)", {
  expect_gt(length(KEYS), 20)
  expect_true(all(c("temporary_crops", "ppm_aquaculture", "baci_countries") %in% KEYS))
  expect_equal(anyDuplicated(KEYS), 0L, info = paste(KEYS[duplicated(KEYS)], collapse = ", "))
})

test_that("every key gives back a non-empty table", {
  for (key in KEYS) {
    dic <- load_dictionary(key)
    expect_s3_class(dic, "data.frame")
    expect_gt(nrow(dic), 0)
  }
})

test_that("every PRODUCT dictionary a loader labels with has var_code, var_pt and var_eng, one row per code", {
  # the survey dictionaries load_pam(), load_ppm() and load_pevs() match their columns against
  product_keys <- setdiff(KEYS[grepl("^(pam|ppm|pevs|temporary|permanent|all_crops|corn|potato|peanut|beans)", KEYS)], VARIABLE_KEYS)
  expect_gt(length(product_keys), 5)
  for (key in product_keys) {
    dic <- load_dictionary(key)
    expect_true(all(c("var_code", "var_pt", "var_eng") %in% names(dic)), info = key)
    expect_equal(anyDuplicated(dic$var_code), 0L, info = paste(key, "repeats a var_code"))
  }
})

test_that("every VARIABLE dictionary is var_code / var_pt / var_eng and nothing else, with a name for every id in both languages", {
  # (the light shape on purpose: units describe a product, not a column name)
  skip_if(length(VARIABLE_KEYS) == 0, "no variable dictionary on this branch")
  for (key in VARIABLE_KEYS) {
    dic <- load_dictionary(key)
    expect_equal(names(dic), c("var_code", "var_pt", "var_eng"), info = key)
    expect_false(anyNA(dic), info = key)
    expect_equal(anyDuplicated(dic$var_code), 0L, info = paste(key, "names an id twice"))
    for (column in c("var_pt", "var_eng")) {
      expect_true(all(grepl("^[a-z][a-z0-9_]*$", dic[[column]])), info = paste(key, column, "must be a usable column-name stem"))
    }
    # a name that looked like the fallback (v + digits) would be indistinguishable from an unnamed id
    expect_false(any(grepl("^v[0-9]+$", c(dic$var_pt, dic$var_eng))), info = key)
  }
})

test_that("a variable dictionary never shares its key with a product dictionary", {
  skip_if(length(VARIABLE_KEYS) == 0, "no variable dictionary on this branch")
  stems <- sub("_variables$", "", VARIABLE_KEYS)
  # the product entry of the same survey (ppm_sheep_farming) is a different table under a different key
  expect_false(any(VARIABLE_KEYS %in% setdiff(KEYS, VARIABLE_KEYS)))
  for (stem in intersect(stems, KEYS)) {
    expect_false(identical(load_dictionary(stem), load_dictionary(paste0(stem, "_variables"))), info = stem)
  }
})
