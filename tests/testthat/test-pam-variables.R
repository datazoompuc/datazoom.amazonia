# tests/testthat/test-pam-variables.R
#
# Offline test for load_pam()'s naming of IBGE's variable ids (R/pam.R, the
# "Translation" step). Until 2026-10-21 that step named six ids and turned any
# other one into NA, so table 1613 (permanent crops), which reports "area
# destinada a colheita" as 2313 instead of 8331, came back with an unnamed
# na_v<crop> column for every crop (36 of permanent_crops' 180 columns).
#
# sidra_download() is mocked (no network) and returns the shape the real one
# does for a Brazil-level query: sidrar's 13 columns, janitor-cleaned, every
# one of them character except valor. The variable ids per table below are the ones IBGE's
# metadata declares and sidrar returns (checked live 2026-10-21, "allxp" =
# everything except the "percentual do total geral" variables, which are never
# requested).

# table -> the variables it reports (id = IBGE's name, accents spelled as escapes)
AREA_PLANTED <- c("109" = "Área plantada")
AREA_DESTINED <- c("2313" = "Área destinada à colheita")
AREA_PLANTED_OR_DESTINED <- c("8331" = "Área plantada ou destinada à colheita")
COMMON <- c(
  "216" = "Área colhida", "214" = "Quantidade produzida",
  "112" = "Rendimento médio da produção"
)
VALUE <- c("215" = "Valor da produção")

PAM_TABLE_VARIABLES <- list(
  `1612` = c(AREA_PLANTED, COMMON, VALUE),
  `1613` = c(AREA_DESTINED, COMMON, VALUE),
  `5457` = c(AREA_PLANTED_OR_DESTINED, COMMON, VALUE),
  `839` = c(AREA_PLANTED, COMMON), `1000` = c(AREA_PLANTED, COMMON),
  `1001` = c(AREA_PLANTED, COMMON), `1002` = c(AREA_PLANTED, COMMON)
)
# the label of the classification column IBGE gives each table
PAM_CLASSIFICATION <- c(
  `1612` = "temporarias", `1613` = "permanentes", `5457` = "temporarias_e_permanentes",
  `839` = "temporarias", `1000` = "temporarias", `1001` = "temporarias", `1002` = "temporarias"
)
# dataset -> its table, as the committed manifest says (checked below)
PAM_DATASET_TABLE <- c(
  permanent_crops = "1613", banana = "1613", temporary_crops = "1612", soybean = "1612",
  all_crops = "5457", corn = "839", peanut = "1000", potato = "1001", beans = "1002"
)

LABELS <- list(
  eng = c(
    "112" = "avg_yield", "214" = "quant", "215" = "value", "216" = "harvested_area",
    "109" = "planted_area", "8331" = "planted_to_harvest_area", "2313" = "planted_to_harvest_area"
  ),
  pt = c(
    "112" = "rend_medio", "214" = "quant", "215" = "valor", "216" = "area_colhida",
    "109" = "area_plantada", "8331" = "area_plantada_dest_colheita", "2313" = "area_plantada_dest_colheita"
  )
)

CROPS <- c("0", "2720") # the Total and one crop

# What sidra_download() returns: one row per variable x crop. `extra` adds
# variables the package has never heard of: c(id = "IBGE's name").
fake_value <- function(variable, crop) 1000 * match(crop, CROPS) + as.integer(variable) %% 997
fake_sidra <- function(table, extra = character(0)) {
  variables <- c(PAM_TABLE_VARIABLES[[table]], extra)
  grid <- expand.grid(variavel_codigo = names(variables), crop = CROPS, stringsAsFactors = FALSE)
  # sidrar's own column order matters: load_pam() pivots on the range
  # variavel:produto_das_lavouras_codigo, so ano must sit before variavel.
  # valor is the one numeric column.
  out <- tibble::tibble(
    nivel_territorial_codigo = "1", nivel_territorial = "Brasil",
    unidade_de_medida_codigo = "1", unidade_de_medida = "Hectares",
    valor = as.numeric(mapply(fake_value, grid$variavel_codigo, grid$crop)),
    brasil_codigo = "1", brasil = "Brasil",
    ano_codigo = "2023", ano = "2023",
    variavel_codigo = grid$variavel_codigo, variavel = unname(variables[grid$variavel_codigo])
  )
  out[[paste0("produto_das_lavouras_", PAM_CLASSIFICATION[[table]], "_codigo")]] <- grid$crop
  out[[paste0("produto_das_lavouras_", PAM_CLASSIFICATION[[table]])]] <- paste("Cultura", grid$crop)
  out
}

load_fake <- function(dataset, language, extra = character(0)) {
  table <- PAM_DATASET_TABLE[[dataset]]
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, ...) fake_sidra(as.character(sidra_code), extra),
    .package = "datazoom.amazonia"
  )
  load_pam(dataset = dataset, geo_level = "country", time_period = 2023, language = language)
}

year_col <- c(eng = "year", pt = "ano")

# ---- the datasets and the tables they read ----------------------------------

test_that("the datasets used here read the tables this file assumes", {
  committed <- link_table()[link_table()$survey == "pam", ]
  table_of <- sub("/.*", "", committed$sidra_code[match(names(PAM_DATASET_TABLE), committed$dataset)])
  expect_equal(unname(table_of), unname(PAM_DATASET_TABLE))
})

# ---- every variable the PAM tables report has a name -------------------------

for (dataset in names(PAM_DATASET_TABLE)) {
  for (language in c("eng", "pt")) {
    test_that(sprintf("load_pam('%s', language = '%s') names every variable, with no na_ column and no warning", dataset, language), {
      table <- PAM_DATASET_TABLE[[dataset]]
      ids <- names(PAM_TABLE_VARIABLES[[table]])

      expect_no_warning(out <- load_fake(dataset, language))

      expected <- as.vector(outer(unname(LABELS[[language]][ids]), CROPS, function(l, c) paste0(l, "_v", c)))
      expect_setequal(setdiff(names(out), c("geo_id", year_col[[language]])), expected)
      expect_false(any(grepl("^na_", names(out))))
      expect_false(anyDuplicated(names(out)) > 0)

      # a pure naming step: each value arrives under the name of its variable
      for (id in ids) {
        for (crop in CROPS) {
          col <- paste0(LABELS[[language]][[id]], "_v", crop)
          expect_equal(as.numeric(out[[col]]), fake_value(id, crop), info = paste(dataset, language, col))
        }
      }

      # the dictionary labels (the crop's name, "Total" for code 0) reach every
      # column, including the one that used to be na_v<crop>
      expect_equal(attr(out[[paste0(LABELS[[language]][[ids[1]]], "_v0")]], "label"), "Total")
      if (table == "1613") { # 2720 is banana: a code the permanent-crops dictionary knows
        for (id in ids) {
          label <- attr(out[[paste0(LABELS[[language]][[id]], "_v2720")]], "label")
          expect_true(length(label) == 1 && nzchar(label), info = paste(dataset, language, id))
        }
      }
    })
  }
}

test_that("table 1613's 'area destinada a colheita' (2313) takes the name 8331 has in all_crops", {
  perm <- load_fake("permanent_crops", "eng")
  all <- load_fake("all_crops", "eng")
  expect_true("planted_to_harvest_area_v2720" %in% names(perm))
  expect_true("planted_to_harvest_area_v2720" %in% names(all))
  perm_pt <- load_fake("permanent_crops", "pt")
  expect_true("area_plantada_dest_colheita_v2720" %in% names(perm_pt))
})

# ---- a variable nobody has named yet ---------------------------------------------

test_that("an unknown IBGE variable keeps its id in the column name, keeps its values, and warns naming it", {
  extra <- c("99999" = "Variável nova")
  for (language in c("eng", "pt")) {
    warnings <- character(0)
    out <- withCallingHandlers(
      load_fake("permanent_crops", language, extra),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_length(warnings, 1) # once per call, however many crops it covers
    expect_match(warnings, "99999 (Variavel nova)", fixed = TRUE) # IBGE's own name, transliterated like the rest
    expect_match(warnings, "v<id>_v<crop code>", fixed = TRUE)
    expect_match(warnings, "github.com/datazoompuc/datazoom.amazonia/issues", fixed = TRUE)

    expect_equal(as.numeric(out$v99999_v2720), fake_value("99999", "2720"))
    expect_equal(as.numeric(out$v99999_v0), fake_value("99999", "0"))
    expect_false(any(grepl("^na_", names(out))))
    expect_false(anyDuplicated(names(out)) > 0)
    # the known variables are untouched by the unknown one
    known <- setdiff(names(out), c("geo_id", year_col[[language]], "v99999_v2720", "v99999_v0"))
    expect_length(known, 10) # 5 variables x 2 crops
    expect_equal(as.numeric(out[[paste0(LABELS[[language]][["2313"]], "_v2720")]]), fake_value("2313", "2720"))
  }
})

test_that("several unknown variables are all named in one warning and cannot collide with each other", {
  extra <- c("99999" = "Nova um", "88888" = "Nova dois")
  warnings <- character(0)
  out <- withCallingHandlers(
    load_fake("temporary_crops", "eng", extra),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_match(warnings, "99999 (Nova um)", fixed = TRUE)
  expect_match(warnings, "88888 (Nova dois)", fixed = TRUE)
  expect_true(all(c("v99999_v2720", "v88888_v2720") %in% names(out)))
  expect_false(anyDuplicated(names(out)) > 0)
})

test_that("a fallback name ('v<id>') can never equal a real variable name", {
  for (labels in LABELS) {
    expect_false(any(grepl("^v[0-9]+$", labels)))
  }
})

# ---- a table number is its dataset's name ----------------------------------------

test_that("load_pam(dataset = <table number>) returns exactly what load_pam(dataset = <name>) does, treated and raw", {
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, ...) fake_sidra(as.character(sidra_code)),
    .package = "datazoom.amazonia"
  )
  for (dataset in c("permanent_crops", "temporary_crops", "all_crops", "corn", "peanut", "potato", "beans")) {
    table <- as.numeric(PAM_DATASET_TABLE[[dataset]])
    for (language in c("eng", "pt")) {
      for (raw in c(FALSE, TRUE)) {
        by_name <- load_pam(dataset = dataset, geo_level = "country", time_period = 2023, language = language, raw_data = raw)
        by_number <- load_pam(dataset = table, geo_level = "country", time_period = 2023, language = language, raw_data = raw)
        expect_identical(by_number, by_name, info = paste(dataset, language, raw))
      }
    }
  }
})


test_that("the \"pam_variables\" dictionary holds exactly the names load_pam() has always used, in both languages, and every id the tables report", {
  dic <- load_dictionary("pam_variables")
  codes <- as.character(dic$var_code)

  expect_setequal(codes, names(LABELS$eng))
  expect_equal(stats::setNames(dic$var_eng, codes)[names(LABELS$eng)], LABELS$eng)
  expect_equal(stats::setNames(dic$var_pt, codes)[names(LABELS$pt)], LABELS$pt)

  # every variable the seven tables report (their metadata, checked live 2026-10-21) has a name
  reported <- unique(unlist(lapply(PAM_TABLE_VARIABLES, names)))
  expect_true(all(reported %in% codes), info = paste(setdiff(reported, codes), collapse = ", "))
})
