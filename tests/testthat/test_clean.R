
context("assign_sample_numbers")

test_that("assign_sample_numbers", {

  # `MPVPosition` has to be present; `Sample_number` can't be
  df <- data.frame(Sample_number = 1)
  expect_error(assign_sample_numbers(df))
  df <- data.frame(MPVPosition = 1, Sample_number = 1)
  expect_error(assign_sample_numbers(df))

  df <- data.frame(MPVPosition = c(4, 4, 2, 1, 1), x = "test")
  out <- assign_sample_numbers(df)
  expect_identical(nrow(out), nrow(df))
  expect_true("Sample_number" %in% colnames(out))
  expect_identical(length(unique(out$Sample_number)), 3L)

  # Removes valves as requested
  out <- assign_sample_numbers(df, remove_valves = 2)
  expect_true(!2 %in% out$MPVPosition)
  out <- assign_sample_numbers(df, remove_valves = c(1, 4))
  expect_true(all(!c(1, 4) %in% out$MPVPosition))
})
