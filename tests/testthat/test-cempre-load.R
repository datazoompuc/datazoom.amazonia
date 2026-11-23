# tests/testthat/test-cempre-load.R
#
# Offline tests for what load_cempre() asks IBGE for, year by year, and what it calls the
# result (R/cempre.R). The series lives in two SIDRA tables with the same structure: 6449 for
# 2006-2021, which IBGE closed ("serie encerrada em 2021"), and 9418 for 2022 onwards. Until
# 2026-11-23 load_cempre() knew only 6449 and the manifest's available_time stopped at 2020:
# load_cempre(time_period = 2022) stopped with "Error in Download." at country and state level
# and returned an EMPTY table at municipality level, and 2021 warned that it was out of range.
# There was no test of load_cempre() at all. sidra_download() is mocked (no network).

# ---- a fake sidra_download() ------------------------------------------------------------

# sidra_download()'s answer for one table, year, level and category, in its own column order
# (checked live 2026-11-23): one row per variable and place for the category asked for. The
# four variables are the ones both tables declare, in IBGE's order.
FAKE_VARIABLES <- c(
  `2585` = "Número de empresas e outras organizações", `707` = "Pessoal ocupado total",
  `708` = "Pessoal ocupado assalariado", `662` = "Salários e outras remunerações"
)
FAKE_UNITS <- c(`2585` = "Unidades", `707` = "Pessoas", `708` = "Pessoas", `662` = "Mil Reais")
GEO_COLUMNS <- list(
  country = c("brasil_codigo", "brasil"),
  state = c("unidade_da_federacao_codigo", "unidade_da_federacao"),
  municipality = c("municipio_codigo", "municipio")
)
GEO_CODES <- list(country = "1", state = c("11", "12"), municipality = c("1100015", "1100023"))
SECTOR_NAMES <- stats::setNames(c("Total", paste(LETTERS[1:21], "Secao")), cempre_sectors)

# a number a test can take apart: <variable> * 1e6 + <sector code> + <year> / 1e4, plus the
# place and the table in the digits after
fake_value <- function(table, variable, category, geo, year) {
  as.numeric(variable) * 1e6 + as.numeric(category) + as.numeric(year) / 1e4 + as.numeric(table) / 1e9 + as.numeric(geo) / 1e12
}

fake_cempre <- function(table, year, geo_level, category, extra_variable = NULL, empty = FALSE) {
  category <- as.character(unlist(category))
  variables <- c(names(FAKE_VARIABLES), extra_variable)
  names_of <- c(FAKE_VARIABLES, stats::setNames("Variavel nova", extra_variable))
  grid <- expand.grid(geo = GEO_CODES[[geo_level]], variavel_codigo = variables, stringsAsFactors = FALSE)
  if (empty) grid <- grid[0, ]
  out <- tibble::tibble(
    nivel_territorial_codigo = rep("1", nrow(grid)), nivel_territorial = rep("Brasil", nrow(grid)),
    unidade_de_medida_codigo = rep("1020", nrow(grid)),
    unidade_de_medida = unname(c(FAKE_UNITS, stats::setNames("Unidades", extra_variable))[grid$variavel_codigo]),
    valor = mapply(function(v, g) fake_value(table, v, category, g, year), grid$variavel_codigo, grid$geo)
  )
  out[[GEO_COLUMNS[[geo_level]][1]]] <- grid$geo
  out[[GEO_COLUMNS[[geo_level]][2]]] <- if (geo_level == "country") rep("Brasil", nrow(grid)) else paste("Lugar", grid$geo)
  out$ano_codigo <- rep(as.character(year), nrow(grid))
  out$ano <- rep(as.character(year), nrow(grid))
  out$variavel_codigo <- grid$variavel_codigo
  out$variavel <- unname(names_of[grid$variavel_codigo])
  out$classificacao_nacional_de_atividades_economicas_cnae_2_0_codigo <- rep(category, nrow(grid))
  out$classificacao_nacional_de_atividades_economicas_cnae_2_0 <- rep(unname(SECTOR_NAMES[category]), nrow(grid))
  out
}

# Runs load_cempre() with sidra_download() mocked; returns the result with the calls the mock
# saw in attr "calls" (one list per call: table, classific, category, year, geo_level).
load_fake <- function(..., extra_variable = NULL) {
  calls <- list()
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, classific = "all", category = "all", year, geo_level) {
      calls[[length(calls) + 1]] <<- list(table = sidra_code, classific = classific, category = category, year = year, geo_level = geo_level)
      fake_cempre(sidra_code, year, geo_level, category, extra_variable)
    },
    .package = "datazoom.amazonia"
  )
  out <- load_cempre(...)
  attr(out, "calls") <- calls
  out
}

test_that("the manifest says what these tests assume: one dataset, the open table with the CNAE and its Total", {
  committed <- link_table()[link_table()$survey == "cempre", ]
  expect_equal(nrow(committed), 1)
  expect_equal(committed$dataset, "cempre")
  expect_equal(committed$sidra_code, "9418/c12762/117897")
  expect_equal(parse_years(committed$available_time), 2006:2024)
})

# ---- what load_cempre() asks sidra_download() for ---------------------------------------

test_that("load_cempre() asks, year by year, the table that has the year: 6449 up to 2021, 9418 from 2022", {
  calls <- attr(load_fake(geo_level = "state", time_period = c(2006, 2020, 2021, 2022, 2024), raw_data = TRUE), "calls")
  expect_length(calls, 5)
  expect_equal(vapply(calls, function(x) x$table, numeric(1)), c(6449, 6449, 6449, 9418, 9418))
  expect_equal(vapply(calls, function(x) x$year, ""), c("2006", "2020", "2021", "2022", "2024"))
  expect_true(all(vapply(calls, function(x) x$geo_level == "state", NA)))
})

test_that("every request names the CNAE (c12762) and the Total (117897), never classific = 'all': the manifest's code is what is sent", {
  calls <- attr(load_fake(geo_level = "country", time_period = c(2021, 2022), raw_data = TRUE), "calls")
  for (call in calls) {
    expect_equal(call$classific, "c12762")
    expect_equal(call$category, list("117897"))
  }
})

test_that("cempre_table_for_year() is the whole map: a year of the closed table goes to it, anything later to the manifest's table", {
  expect_equal(cempre_table_for_year(2006, 9418), 6449)
  expect_equal(cempre_table_for_year("2021", 9418), 6449) # the loader passes the year as text
  expect_equal(cempre_table_for_year(2022, 9418), 9418)
  expect_equal(cempre_table_for_year("2024", 9418), 9418)
  expect_equal(cempre_closed_tables, list(list(table = 6449, last_year = 2021L)))
})

test_that("sectors = TRUE asks every one of the 22 sector codes for every year, each from the table of its year", {
  calls <- attr(load_fake(geo_level = "country", time_period = c(2021, 2022), sectors = TRUE, raw_data = TRUE), "calls")
  expect_length(calls, 2 * 22)
  by_year <- split(calls, vapply(calls, function(x) x$year, ""))
  for (year in c("2021", "2022")) {
    sent <- vapply(by_year[[year]], function(x) as.character(unlist(x$category)), "")
    expect_equal(sort(sent), sort(cempre_sectors), info = year)
    expect_true(all(vapply(by_year[[year]], function(x) x$classific == "c12762", NA)), info = year)
    expect_true(all(vapply(by_year[[year]], function(x) x$table == if (year == "2021") 6449 else 9418, NA)), info = year)
  }
})

test_that("a SIDRA table given as a number is the manifest dataset that reads it: 9418, or the closed 6449, is load_cempre(dataset = 'cempre')", {
  years <- c(2006, 2021, 2022)
  by_name <- load_fake(dataset = "cempre", geo_level = "state", time_period = years, raw_data = TRUE)
  for (number in c(9418, 6449)) {
    by_number <- load_fake(dataset = number, geo_level = "state", time_period = years, raw_data = TRUE)
    expect_equal(attr(by_number, "calls"), attr(by_name, "calls"), info = number)
    expect_identical(
      load_fake(dataset = number, geo_level = "state", time_period = years, sectors = TRUE),
      load_fake(dataset = "cempre", geo_level = "state", time_period = years, sectors = TRUE),
      info = number
    )
  }
  expect_equal(sidra_dataset_of_table("cempre", 9418), "cempre")
  expect_null(sidra_dataset_of_table("cempre", 6449)) # the manifest does not know the closed table: the loader does
})

test_that("a number the manifest does not know keeps the old behaviour: that table for every year, with the CNAE and the Total, and no rerouting", {
  calls <- attr(load_fake(dataset = 1234, geo_level = "country", time_period = c(2010, 2022), raw_data = TRUE, sectors = FALSE), "calls")
  expect_equal(vapply(calls, function(x) x$table, numeric(1)), c(1234, 1234))
  expect_equal(calls[[1]]$classific, "c12762")
  expect_equal(calls[[1]]$category, list("117897"))
  expect_error(load_cempre(dataset = "cempre_nonexistent", geo_level = "country", time_period = 2015), "not supported")
})

# ---- what load_cempre() calls the result ---------------------------------------------------------

test_that("the Total, with sectors = FALSE: one column per variable under IBGE's own name, one row per year and place, and no label", {
  out <- load_fake(geo_level = "state", time_period = c(2021, 2022))
  expect_equal(
    names(out),
    c("ano", "geo_id", "numero_de_empresas_e_outras_organizacoes", "pessoal_ocupado_assalariado", "pessoal_ocupado_total", "salarios_e_outras_remuneracoes")
  )
  expect_equal(nrow(out), 4)
  expect_equal(out$ano, rep(c("2021", "2022"), each = 2))
  expect_equal(out$geo_id, rep(GEO_CODES$state, 2))
  # each value comes from the table of its year
  expect_equal(out$pessoal_ocupado_total[1], fake_value(6449, "707", "117897", "11", 2021))
  expect_equal(out$pessoal_ocupado_total[3], fake_value(9418, "707", "117897", "11", 2022))
  expect_true(all(vapply(out, function(x) is.null(attr(x, "label")), NA)))
})

test_that("with sectors = TRUE a column is named <variable>_v<sector code>_<k> -- k being 1 for companies (2585), 2 for people employed (707), 3 for people on a payroll (708), 4 for wages (662) -- in both tables", {
  for (year in c(2021, 2022)) {
    out <- load_fake(geo_level = "country", time_period = year, sectors = TRUE)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 2 + 22 * 4)
    table <- if (year == 2021) 6449 else 9418
    for (sector in c("117897", "116830", "117892")) {
      for (v in list(c("2585", 1, "numero_de_empresas_e_outras_organizacoes"), c("707", 2, "pessoal_ocupado_total"),
                     c("708", 3, "pessoal_ocupado_assalariado"), c("662", 4, "salarios_e_outras_remuneracoes"))) {
        column <- sprintf("%s_v%s_%s", v[3], sector, v[2])
        expect_true(column %in% names(out), info = column)
        expect_equal(as.vector(out[[column]]), fake_value(table, v[1], sector, "1", year), info = column)
      }
    }
  }
})

test_that("the labels are the dictionary's, per column, in English and in Portuguese: the Total, a section, and every column carries one", {
  eng <- load_fake(geo_level = "country", time_period = 2022, sectors = TRUE)
  pt <- load_fake(geo_level = "country", time_period = 2022, sectors = TRUE, language = "pt")
  dic <- load_dictionary("cempre")
  value_columns <- setdiff(names(eng), c("ano", "geo_id"))
  expect_length(value_columns, 88)
  expect_false(any(vapply(eng[value_columns], function(x) is.null(attr(x, "label")), NA)))
  expect_false(any(vapply(pt[value_columns], function(x) is.null(attr(x, "label")), NA)))
  # the id_code of a column is the end of its name
  id_of <- sub("^.*_v", "", value_columns)
  expect_setequal(id_of, as.character(dic$id_code))
  for (column in c("pessoal_ocupado_total_v117897_2", "salarios_e_outras_remuneracoes_v116830_4")) {
    id <- sub("^.*_v", "", column)
    expect_equal(attr(eng[[column]], "label"), as.character(dic$var_eng[dic$id_code == id]), info = column)
    expect_equal(attr(pt[[column]], "label"), as.character(dic$var_pt[dic$id_code == id]), info = column)
  }
  expect_false(identical(attr(eng$pessoal_ocupado_total_v116830_2, "label"), attr(pt$pessoal_ocupado_total_v116830_2, "label")))
})

test_that("a label is found by the exact id at the end of the column's name: an id that is the tail of another one labels nothing it should not", {
  # PEVS had this bug: a new code (83410) contained an old one (3410) and took its label. Here a dictionary
  # row "7897_2" is the tail of sector 117897's "117897_2"; matched anywhere in a name it would relabel
  # that column (the loop goes through the dictionary in order, so the last match wins).
  real <- load_dictionary("cempre")
  clash <- real[real$id_code == "117897_2", ]
  clash$id_code <- "7897_2"
  clash$var_eng <- "WRONG LABEL"
  clash$var_pt <- "ROTULO ERRADO"
  testthat::local_mocked_bindings(load_dictionary = function(...) rbind(real, clash), .package = "datazoom.amazonia")
  out <- load_fake(geo_level = "country", time_period = 2022, sectors = TRUE)
  eng_label <- attr(out$pessoal_ocupado_total_v117897_2, "label")
  expect_equal(eng_label, as.character(real$var_eng[real$id_code == "117897_2"]))
  expect_false(any(vapply(out, function(column) identical(attr(column, "label"), "WRONG LABEL"), NA)))
  pt <- load_fake(geo_level = "country", time_period = 2022, sectors = TRUE, language = "pt")
  expect_equal(attr(pt$pessoal_ocupado_total_v117897_2, "label"), as.character(real$var_pt[real$id_code == "117897_2"]))
})

test_that("the Portuguese names are IBGE's own, cleaned, and the year is 'ano' in both languages (as always)", {
  out <- load_fake(geo_level = "country", time_period = 2022, language = "pt")
  expect_true(all(c("ano", "geo_id", "pessoal_ocupado_total") %in% names(out)))
  expect_equal(out$geo_id, "Brasil")
})

test_that("at every level the geography is geo_id: the code for state and municipality, IBGE's name for the country", {
  expect_equal(load_fake(geo_level = "state", time_period = 2022)$geo_id, GEO_CODES$state)
  expect_equal(load_fake(geo_level = "municipality", time_period = 2022)$geo_id, GEO_CODES$municipality)
  expect_equal(load_fake(geo_level = "country", time_period = 2021)$geo_id, "Brasil")
})

test_that("raw data is what sidra_download() returned, concatenated year by year", {
  raw <- load_fake(geo_level = "country", time_period = c(2021, 2022), raw_data = TRUE)
  expect_equal(nrow(raw), 8)
  expect_equal(raw$variavel_codigo, rep(c("2585", "707", "708", "662"), 2))
  expect_equal(raw$ano, rep(c("2021", "2022"), each = 4))
})

# ---- a variable IBGE adds -----------------------------------------------------------------------------

test_that("a variable nobody indexed is no NA: its column ends in _v<id> with a warning that says to report it (the 88-branch case_when() it replaces gave NA)", {
  expect_warning(
    out <- load_fake(geo_level = "country", time_period = 2022, sectors = TRUE, extra_variable = "9999"),
    "IBGE variable(s) with no index in load_cempre(): 9999", fixed = TRUE
  )
  new_columns <- grep("variavel_nova", names(out), value = TRUE)
  expect_length(new_columns, 22)
  expect_true(all(grepl("_v[0-9]+_v9999$", new_columns)))
  expect_false(anyNA(out[new_columns]))
  # nothing else changed: the four known variables are still in their columns
  expect_equal(sum(grepl("_v[0-9]+_[1-4]$", names(out))), 88)
  # and with sectors = FALSE there is no id_code to fall back on, so no warning
  expect_no_warning(load_fake(geo_level = "country", time_period = 2022, sectors = FALSE, extra_variable = "9999"))
})

test_that("cempre_id_code() is the sector's code, an underscore and the variable's k", {
  expect_equal(
    cempre_id_code(c("117897", "116830", "116830"), c("2585", "707", "662")),
    c("117897_1", "116830_2", "116830_4")
  )
  expect_equal(cempre_id_code("117897", "708"), "117897_3")
  expect_warning(expect_equal(cempre_id_code("117897", "9999"), "117897_v9999"), "9999", fixed = TRUE)
  expect_equal(cempre_variable_index, c(`2585` = 1L, `707` = 2L, `708` = 3L, `662` = 4L))
})

# ---- time_period -----------------------------------------------------------------------------------------

test_that("time_period is checked against the manifest's available_time: 2006 to 2024 are in it, 2005 and 2025 are not", {
  testthat::local_mocked_bindings(
    sidra_download = function(sidra_code, classific = "all", category = "all", year, geo_level) fake_cempre(sidra_code, year, geo_level, category),
    .package = "datazoom.amazonia"
  )
  for (year in c(2006, 2020, 2021, 2022, 2024)) {
    expect_no_warning(load_cempre(geo_level = "country", time_period = year))
  }
  for (year in c(2005, 2025)) {
    expect_warning(
      load_cempre(geo_level = "country", time_period = year),
      "time_period must be in 2006-2024",
      fixed = TRUE, info = year
    )
  }
})

# ---- the periodic check ----------------------------------------------------------------------------------------

test_that("check_all_loads.R checks cempre in a year of each table, both declared by the manifest", {
  script <- file.path(normalizePath(test_path("..", ".."), mustWork = FALSE), "actions", "scripts", "check_all_loads.R")
  skip_if_not(file.exists(script), "actions/ not present (stripped build)")

  exprs <- parse(script, keep.source = FALSE)
  settings <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("datasets_by_fn", "TIME_PERIOD_BY_FN", "TIME_PERIOD")) eval(e, settings)
  }
  expect_equal(settings$datasets_by_fn$load_cempre, "cempre")

  years <- settings$TIME_PERIOD_BY_FN$load_cempre
  expect_false(is.null(years))
  declared <- parse_years(link_table()[link_table()$survey == "cempre", ]$available_time)
  expect_true(all(years %in% declared))
  # one year of the closed table (6449) and one of the open one (9418), so each is asked once
  closed_last <- cempre_closed_tables[[1]]$last_year
  expect_true(any(years <= closed_last) && any(years > closed_last))
})
