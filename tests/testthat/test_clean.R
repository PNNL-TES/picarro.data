
context("clean")

test_that("assign_sample_numbers", {

  # Handles NULL
  expect_silent(assign_sample_numbers(NULL))
  expect_null(assign_sample_numbers(NULL))

  # `MPVPosition` and `DATETIME` have to be present; `Sample_number` can't be
  df <- data.frame(MPVPosition = 1)
  expect_error(assign_sample_numbers(df))
  df <- data.frame(DATETIME = 1)
  expect_error(assign_sample_numbers(df))
  df <- data.frame(MPVPosition = 1, DATETIME = 1, Sample_number = 1)
  expect_error(assign_sample_numbers(df))

  df <- data.frame(DATETIME = seq(as.Date("2000/1/1"), by = "day", length.out = 5),
                   MPVPosition = c(4, 4, 2, 1, 1), x = "test")
  out <- assign_sample_numbers(df)
  expect_identical(nrow(out), nrow(df))
  expect_true("Sample_number" %in% colnames(out))
  expect_identical(length(unique(out$Sample_number)), 3L)

})

test_that("clean_data", {
  # Handles NULL
  expect_silent(clean_data(NULL))
  expect_null(clean_data(NULL))

  df <- data.frame(DATE = "2019-05-30", TIME = "19:55:32",
                   ALARM_STATUS = 0,
                   MPVPosition = c(4, 4.1, 2, 1, 1),
                   CO2_dry = 1, CH4_dry = 1, h2o_reported = 1)

  # Generates warning if time zone not specified
  expect_warning(clean_data(df))

  # Removes fractional valves
  ints <- sum(df$MPVPosition == floor(df$MPVPosition))
  out <- clean_data(df, tz = "America/New_York")
  expect_identical(nrow(out), ints)

  # Creates DATETIME field
  expect_true("DATETIME" %in% colnames(out))
  expect_identical(class(out$DATETIME), c("POSIXct", "POSIXt"))

  # Removes valves as requested
  out <- clean_data(df, remove_valves = c(2), tz = "UTC")
  expect_true(!2 %in% out$MPVPosition)
  out <- clean_data(df, remove_valves = c(1, 4), tz = "UTC")
  expect_true(all(!c(1, 4) %in% out$MPVPosition))
})

