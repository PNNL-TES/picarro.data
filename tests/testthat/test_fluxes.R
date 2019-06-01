
context("fluxes")

test_that("compute_flux", {
  # Too few data, or mismatch
  expect_error(compute_flux(1, 401, volume_cm3 = 200, tair_C = 20))
  expect_error(compute_flux(1:10, 401:409, volume_cm3 = 200, tair_C = 20))

  # Bad data
  expect_error(compute_flux(1:10, -1:-10, volume_cm3 = 200, tair_C = 20))
  expect_error(compute_flux(1:10, 401:410, volume_cm3 = -200, tair_C = 20))
  expect_error(compute_flux(1:10, 401:410, volume_cm3 = 200, tair_C = 200))
  expect_error(compute_flux(1:10, 401:410, 200, tair_C = 20, pressure_kPa = -1))

  # Roughly reproduce the example from p. 1-24 of the Licor 8100A manual
  x <- compute_flux(
    time = seq(7.3, 120, length.out = 10), # figure 1-3
    gas_ppm = seq(434, 570, length.out = 10), # figure 1-3
    volume_cm3 = 7385.3,                      # LI-8100A system volume
    tair_C = 25                               # "tropical greenhouse"
  )
  x <- x * (10000 / 317.8) # convert to per m2 based on 20 cm diameter chamber

  # Hmm...we don't know the chamber area from the Licor example
  # If it's 20 cm, then the flux computed here is 1.9x too high
  # If it was a custom 27 cm diameter chamber (area = ~600 cm2) then things match
})

test_that("cumulative_evolution", {
  # Bad data
  expect_error(cumulative_evolution(1, 1:2))
  expect_error(cumulative_evolution(NA, 1))

  # Computes
  x <- cumulative_evolution(1:4, c(1, 1, 1, 1), interpolate = FALSE)
  expect_equal(x, 0:3)
  y <- cumulative_evolution(1:4, c(1, 1, 1, 1), interpolate = TRUE)
  expect_identical(x, y)

  # Interpolates
  z <- cumulative_evolution(1:4, c(1, 1, NA, 1), interpolate = TRUE)
  expect_identical(x, z)
})
