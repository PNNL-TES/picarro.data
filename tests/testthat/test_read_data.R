# test_read_data

context("test_read_data")

test_that("read_picarro_file", {

  # Nonexistent file
  expect_error(read_picarro_file("doesnt_exist.dat"))

  # Single file
  f <- list.files("sampledata/", pattern = "*.dat$", full.names = TRUE)
  x <- read_picarro_file(f[1])
  expect_is(x, "data.frame")

  # Compressed files
  f <- list.files("sampledata/", pattern = "*.zip$", full.names = TRUE)
  xzip <- read_picarro_file(f[1])
  f <- list.files("sampledata/", pattern = "*.gz$", full.names = TRUE)
  xgz <- read_picarro_file(f[1])
  expect_identical(x, xzip)
  expect_identical(x, xgz)
})

test_that("process_directory", {

  x <- process_directory("sampledata/", recursive = FALSE)
  expect_is(x, "data.frame")
  y <- process_directory("sampledata/", recursive = TRUE)
  expect_is(y, "data.frame")

  expect_true(length(unique(y$Filename)) > length(unique(x$Filename)))
})
