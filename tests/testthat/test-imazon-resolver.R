# tests/testthat/test-imazon-resolver.R
#
# Offline test for actions/scrapers/resolve_imazon.R (added 2026-10-29). imazon_shp
# is one Google Drive file -- "imazon.rds", a Datazoom conversion of Imazon's
# Fronteiras_Amazonia_2020 shapefile, uploaded in 2022 to a contributor's own Drive;
# there is no official copy to follow (see that file's header). The resolver checks
# that the file is still there, still the same bytes, and still has the shape
# R/imazon.R relies on. Until then nothing did: load_imazon() was only exercised
# by the monthly "does it throw" check, and no test mentioned imazon at all.
#
# googledrive's three calls are mocked and the "download" copies a small sf object
# built here (no network, no sign-in); what is tested is what the resolver does with
# what Drive says and with what the file holds.

pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
actions_dir <- file.path(pkg_root, "actions")

skip_if_not(
  dir.exists(actions_dir),
  "actions/ not present (stripped build, e.g. installed package or CRAN tarball)"
)
skip_if_not_installed("googledrive")
skip_if_not_installed("sf")

resolver_env <- new.env()
source(file.path(actions_dir, "scrapers", "resolve_imazon.R"), local = resolver_env)
list2env(as.list(resolver_env), environment())

or <- function(x, y) if (is.null(x)) y else x

DRIVE_ID <- "1JHc2J_U8VXHVuWVsi8wVBnNzZ37y1ehv"
DRIVE_URL <- paste0("https://docs.google.com/uc?export=download&id=", DRIVE_ID)

manifest_rows <- tibble::tibble(
  survey = "imazon", dataset = "imazon_shp", geo_level = NA_character_, year = NA_character_,
  url = DRIVE_URL
)

# ---- a small stand-in for the real file ---------------------------------------------

# 4 municipalities, the five attribute columns and a geometry, as the real .rds has them.
fake_sf <- function(front = "Front_2020", values = 0:3, codes = c("1100015", "1100023", "1200013", "1200054"), drop = character(0), extra = list()) {
  square <- function(i) sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  d <- tibble::tibble(NM_MUNICIP = paste("Municipio", seq_along(codes)), CD_GEOCMU = codes, Area = 100, UF = "RO")
  for (f in front) d[[f]] <- values
  for (nm in names(extra)) d[[nm]] <- extra[[nm]]
  d <- d[, setdiff(names(d), drop)]
  sf::st_sf(d, geometry = sf::st_sfc(lapply(seq_along(codes), square), crs = 4674))
}
saved <- function(x) {
  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)
  f
}

# googledrive as Drive would answer: `file` is what the download delivers. `md5` defaults
# to the file's own, as an honest Drive reports it.
fake_drive <- function(file, md5 = NULL, trashed = FALSE, get_error = NULL, download_error = NULL, seen = new.env()) {
  seen$gets <- 0
  seen$downloads <- character(0)
  md5 <- or(md5, unname(tools::md5sum(file)))
  list(
    seen = seen,
    bindings = list(
      drive_deauth = function(...) invisible(NULL),
      drive_get = function(id, ...) {
        seen$gets <- seen$gets + 1
        seen$id <- as.character(id)
        if (!is.null(get_error)) stop(get_error)
        tibble::tibble(
          name = "imazon.rds", id = id,
          drive_resource = list(list(
            name = "imazon.rds", size = "20342788", modifiedTime = "2022-06-20T19:23:48.000Z",
            trashed = trashed, md5Checksum = md5, owners = list(list(displayName = "someone"))
          ))
        )
      },
      drive_download = function(file_arg, path, overwrite = FALSE, ...) {
        seen$downloads <- c(seen$downloads, as.character(file_arg))
        if (!is.null(download_error)) stop(download_error)
        file.copy(file, path, overwrite = TRUE)
        invisible(NULL)
      }
    )
  )
}
run <- function(drive, rows = manifest_rows) {
  b <- drive$bindings
  testthat::with_mocked_bindings(
    suppressMessages(resolve_imazon(rows)),
    drive_deauth = b$drive_deauth, drive_get = b$drive_get,
    drive_download = function(file, path, overwrite = FALSE, ...) b$drive_download(file, path, overwrite, ...),
    .package = "googledrive"
  )
}
error_message <- function(drive, rows = manifest_rows) {
  tryCatch(
    {
      run(drive, rows)
      "NO ERROR"
    },
    error = function(e) conditionMessage(e)
  )
}

# ---- the healthy file ------------------------------------------------------------------

test_that("a healthy file resolves to its own url, the year of its Front_<year> column, and its md5 as `version`", {
  file <- saved(fake_sf())
  drive <- fake_drive(file)
  out <- run(drive)

  expect_equal(nrow(out), 1)
  expect_equal(out$survey, "imazon")
  expect_equal(out$dataset, "imazon_shp")
  expect_equal(out$url, DRIVE_URL) # echoed: it is the one just verified
  expect_equal(out$available_time, "2020") # read from the file, not typed
  expect_equal(out$version, unname(tools::md5sum(file)))
  expect_false("docs_url" %in% names(out)) # there is no official page to point at
  expect_null(attr(out, "partial_failures"))
  expect_equal(drive$seen$id, DRIVE_ID)
  # the call load_imazon() makes: the manifest url, handed to drive_download()
  expect_equal(drive$seen$downloads, DRIVE_URL)
})

test_that("a changed file changes `version`, which is how a replaced upload reaches a pull request", {
  a <- run(fake_drive(saved(fake_sf(values = 0:3))))
  b <- run(fake_drive(saved(fake_sf(values = c(3, 2, 1, 0)))))
  expect_false(identical(a$version, b$version))
  expect_equal(a$available_time, b$available_time)
})

test_that("each owned row gets the answer, echoing its own geo_level and year; the file is checked once", {
  rows <- manifest_rows[c(1, 1), ]
  rows$dataset <- c("imazon_shp", "imazon_other")
  rows$geo_level <- c("municipality", "state")
  drive <- fake_drive(saved(fake_sf()))
  out <- run(drive, rows)

  expect_equal(out$dataset, c("imazon_shp", "imazon_other"))
  expect_equal(out$geo_level, c("municipality", "state"))
  expect_equal(out$version[1], out$version[2])
  expect_equal(drive$seen$gets, 1)
  expect_length(drive$seen$downloads, 1)
})

# ---- what Drive can say ----------------------------------------------------------------

test_that("Drive no longer giving the file's metadata stops, saying what that usually means", {
  msg <- error_message(fake_drive(saved(fake_sf()), get_error = "File not found: 1JHc"))
  expect_match(msg, "Drive does not give the metadata", fixed = TRUE)
  expect_match(msg, "no longer shared", fixed = TRUE)
  expect_match(msg, "File not found", fixed = TRUE)
})

test_that("a trashed file, or one with no checksum, stops", {
  expect_match(error_message(fake_drive(saved(fake_sf()), trashed = TRUE)), "trash", fixed = TRUE)
  drive <- fake_drive(saved(fake_sf()))
  drive$bindings$drive_get <- function(id, ...) {
    tibble::tibble(name = "imazon.rds", id = id, drive_resource = list(list(trashed = FALSE)))
  }
  expect_match(error_message(drive), "no md5 checksum", fixed = TRUE)
})

test_that("a failed download, or a download that is not the bytes Drive reports, stops", {
  expect_match(error_message(fake_drive(saved(fake_sf()), download_error = "quota exceeded")), "downloading .* failed: quota exceeded")
  expect_match(error_message(fake_drive(saved(fake_sf()), md5 = strrep("0", 32))), "is not the one Drive reports", fixed = TRUE)
})

test_that("a file readRDS() cannot read stops (the md5 can still be right: it is the file that is wrong)", {
  garbage <- tempfile(fileext = ".rds")
  writeLines("this is not a serialized R object", garbage)
  expect_match(error_message(fake_drive(garbage)), "not readable by readRDS()", fixed = TRUE)
})

# ---- the shape R/imazon.R relies on --------------------------------------------------------

test_that("every column R/imazon.R reads is required, and a missing one is named", {
  for (column in c(IMAZON_COLUMNS, IMAZON_LOADER_FRONT)) {
    x <- if (column == IMAZON_LOADER_FRONT) fake_sf(front = character(0)) else fake_sf(drop = column)
    problem <- imazon_check_object(x)$problem
    expect_gt(length(problem), 0, label = column)
    expect_true(any(grepl(column, problem, fixed = TRUE)), info = column)
  }
  expect_match(error_message(fake_drive(saved(fake_sf(drop = "UF")))), "no longer has the shape load_imazon\\(\\) relies on -- .*UF")
})

test_that("a healthy object reports no problem and its vintage", {
  check <- imazon_check_object(fake_sf())
  expect_equal(check$problem, character(0))
  expect_equal(check$year, 2020L)
})

test_that("an object that is not an sf, has no geometry, or no rows is refused", {
  plain <- as.data.frame(fake_sf())
  expect_match(imazon_check_object(plain)$problem, "does not hold an sf object", fixed = TRUE)
  expect_match(imazon_check_object(list(a = 1))$problem, "does not hold an sf object", fixed = TRUE)

  empty <- fake_sf()[0, ]
  expect_true(any(grepl("no rows", imazon_check_object(empty)$problem)))
})

test_that("a different vintage -- instead of Front_2020, or beside it -- is a failure: the loader's rename is a literal", {
  only_new <- imazon_check_object(fake_sf(front = "Front_2023"))
  expect_true(any(grepl("Front_2023", only_new$problem)))
  expect_true(any(grepl("version-aware", only_new$problem)))
  expect_true(any(grepl("missing: Front_2020", only_new$problem))) # and the column the loader needs is not there

  both <- imazon_check_object(fake_sf(front = c("Front_2020", "Front_2023")))
  expect_true(any(grepl("carries Front_2020, Front_2023 where R/imazon.R renames only Front_2020", both$problem, fixed = TRUE)))
  expect_equal(both$year, 2020L)
})

test_that("pressure values outside 0 to 3, or NA, and codes that are not numbers, are refused", {
  expect_true(any(grepl("other than 0 to 3", imazon_check_object(fake_sf(values = c(0, 1, 2, 4)))$problem)))
  expect_true(any(grepl("other than 0 to 3", imazon_check_object(fake_sf(values = c(0, 1, NA, 3)))$problem)))
  expect_true(any(grepl("CD_GEOCMU has values that are not numbers", imazon_check_object(fake_sf(codes = c("1100015", "ABC", "1200013", "1200054")))$problem)))
  # as the real file stores them: a factor or character column of digits is fine
  expect_equal(imazon_check_object(fake_sf(values = as.character(0:3)))$problem, character(0))
})

test_that("a file with the wrong shape fails the resolver, with the reasons in the message", {
  msg <- error_message(fake_drive(saved(fake_sf(front = "Front_2023", drop = "Area"))))
  expect_match(msg, "no longer has the shape load_imazon() relies on", fixed = TRUE)
  expect_match(msg, "Area", fixed = TRUE)
  expect_match(msg, "Front_2023", fixed = TRUE)
})

# ---- the url and the rows -----------------------------------------------------------------------

test_that("the Drive id is read from a Drive link, and from nothing else", {
  expect_equal(imazon_drive_id(DRIVE_URL), DRIVE_ID)
  expect_equal(imazon_drive_id("https://drive.google.com/open?id=abc_DEF-1"), "abc_DEF-1")
  expect_true(is.na(imazon_drive_id("https://example.org/uc?export=download&id=abc")))
  expect_true(is.na(imazon_drive_id("https://docs.google.com/uc?export=download")))
  expect_true(is.na(imazon_drive_id(NA_character_)))
  expect_true(is.na(imazon_drive_id(NULL)))
  expect_match(error_message(fake_drive(saved(fake_sf())), transform(manifest_rows, url = "https://example.org/imazon.rds")), "not a Google Drive link", fixed = TRUE)
})

test_that("no owned rows stops, and the script defines exactly one resolve_* binding", {
  expect_match(error_message(fake_drive(saved(fake_sf())), manifest_rows[0, ]), "no manifest rows tagged resolver == 'imazon'", fixed = TRUE)
  expect_equal(ls(resolver_env, pattern = "^(resolve_|watch_)"), "resolve_imazon")
})

# ---- R/imazon.R and the committed manifest ------------------------------------------------------------

test_that("the columns the resolver demands are exactly the ones R/imazon.R renames", {
  imazon_src <- file.path(pkg_root, "R", "imazon.R")
  skip_if_not(file.exists(imazon_src), "R/imazon.R not present")
  code <- readLines(imazon_src, warn = FALSE)
  code <- code[!grepl("^\\s*#", code) & !grepl("^#'", code)]
  # `"municipality_code" = "CD_GEOCMU",` -- the right-hand side of each rename
  raw_names <- unique(sub('^.*= "([^"]+)",?\\s*$', "\\1", grep('^\\s*"[^"]+" = "[^"]+",?\\s*$', code, value = TRUE)))
  expect_setequal(raw_names, c(IMAZON_COLUMNS, IMAZON_LOADER_FRONT))
})

test_that("the committed manifest row is owned by this resolver and carries what it last verified", {
  manifest_path <- file.path(pkg_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
  skip_if_not(file.exists(manifest_path), "manifest not present")
  m <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")

  row <- m[m$survey == "imazon", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$dataset, "imazon_shp")
  expect_equal(row$resolver, "imazon")
  expect_equal(imazon_drive_id(row$url), DRIVE_ID)
  expect_equal(row$available_time, "2020")
  expect_equal(row$available_geo, "municipality")
  expect_match(row$version, "^[0-9a-f]{32}$") # the md5 the last run recorded
})
