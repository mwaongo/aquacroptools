# Regression tests for the climate writers (.PLU / .ETo / .Tnx / .CLI).

test_that("write_climate() creates the four climate files and a CO2 file", {
  path <- withr::local_tempdir()
  files <- suppressMessages(
    write_climate(data = weather, path = path, site_name = "Wakanda")
  )

  expect_named(files, c("cli", "plu", "eto", "tnx"))
  expect_true(all(file.exists(unlist(files))))
  expect_true(file.exists(file.path(path, "MaunaLoa.CO2")))
  expect_identical(
    readLines(files$cli)[3:6],
    c("Wakanda.Tnx", "Wakanda.ETo", "Wakanda.PLU", "MaunaLoa.CO2")
  )
})

test_that("climate files round-trip through the readers", {
  path <- withr::local_tempdir()
  suppressMessages(write_climate(data = weather, path = path, site_name = "RT"))

  plu <- read_plu(file.path(path, "RT.PLU"))
  tnx <- read_tnx(file.path(path, "RT.Tnx"))
  eto <- read_eto(file.path(path, "RT.ETo"))

  expect_equal(as.numeric(plu[[ncol(plu)]]), weather$rain)
  expect_equal(as.numeric(tnx[[ncol(tnx) - 1]]), weather$tmin)
  expect_equal(as.numeric(tnx[[ncol(tnx)]]), weather$tmax)
  expect_equal(as.numeric(eto[[ncol(eto)]]), weather$et0)
})

test_that("write_climate() accepts tibbles containing NA", {
  # Regression: write_fwf() replaced NAs with a string via `x[is.na(x)] <- .`,
  # which a tibble rejects, so any tibble with a missing value errored out.
  path <- withr::local_tempdir()
  wna <- weather
  wna$rain[c(3, 7)] <- NA

  expect_no_error(
    suppressMessages(write_climate(data = wna, path = path, site_name = "NAtbl"))
  )
  expect_match(readLines(file.path(path, "NAtbl.PLU"))[11], "NA")
})

test_that("eol is applied to the data rows as well as the header", {
  # Regression: the header honoured `eol` but write_fwf() re-detected it from
  # the OS, so eol = "windows" on a unix host produced a mixed-ending file.
  path <- withr::local_tempdir()

  suppressMessages(
    write_climate(data = weather, path = path, site_name = "W", eol = "windows")
  )
  raw <- readBin(file.path(path, "W.PLU"), "raw", file.size(file.path(path, "W.PLU")))
  expect_identical(sum(raw == as.raw(13)), sum(raw == as.raw(10)))

  suppressMessages(
    write_climate(data = weather, path = path, site_name = "L", eol = "linux")
  )
  raw <- readBin(file.path(path, "L.PLU"), "raw", file.size(file.path(path, "L.PLU")))
  expect_identical(sum(raw == as.raw(13)), 0L)
})

test_that("eol matching is case-insensitive", {
  path <- withr::local_tempdir()
  expect_no_error(
    suppressMessages(
      write_climate(data = weather, path = path, site_name = "C", eol = "Windows")
    )
  )
})

test_that("values too wide for their column warn instead of running together", {
  # Regression: unrounded input silently produced "14.61234567890129.487654321098"
  # with no separator between tmin and tmax.
  path <- withr::local_tempdir()
  wide <- data.frame(
    year = 2020, month = 1, day = 1:2,
    tmin = c(14.612345678901, 9.1), tmax = c(29.487654321098, 30.2),
    rain = 0, et0 = 5
  )

  expect_warning(
    suppressMessages(write_climate(data = wide, path = path, site_name = "Wide")),
    "too wide"
  )
})

test_that("write_fwf() honours justify, widths, factors and replace_na", {
  file <- withr::local_tempfile()
  x <- data.frame(a = c(1, NA, 3), b = factor(c("x", "y", "z")))

  write_fwf(x, file, width = c(6, 4), justify = "lr", replace_na = "-9",
            append = FALSE, eol = "linux")
  expect_identical(readLines(file), c("1        x", "-9       y", "3        z"))

  write_fwf(x[1, ], file, width = c(6, 4), justify = "lr",
            append = TRUE, eol = "linux")
  expect_length(readLines(file), 4L)
})

test_that("quiet = TRUE suppresses progress messages", {
  path <- withr::local_tempdir()

  expect_message(
    write_climate(data = weather, path = path, site_name = "Loud"),
    "Writing climate files to"
  )
  expect_no_message(
    write_climate(data = weather, path = path, site_name = "Shh", quiet = TRUE)
  )
})

test_that("write_cli() auto-detects eol by default, like the other writers", {
  path <- withr::local_tempdir()
  suppressMessages(
    write_climate(data = weather, path = path, site_name = "S", quiet = TRUE)
  )

  cli_crs <- function(f) {
    raw <- readBin(f, "raw", file.size(f))
    sum(raw == as.raw(13))
  }

  # Default: matches the host, i.e. the same ending .PLU/.ETo/.Tnx just got.
  f <- write_cli(path = path, site_name = "S")
  expect_identical(cli_crs(f), if (get_os() == "windows") 6L else 0L)

  # Explicit values are still honoured.
  expect_identical(cli_crs(write_cli(path = path, site_name = "S", eol = "windows")), 6L)
  expect_identical(cli_crs(write_cli(path = path, site_name = "S", eol = "linux")), 0L)
})
