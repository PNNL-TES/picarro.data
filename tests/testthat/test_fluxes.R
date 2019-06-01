
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
  x <- compute_flux(seconds = seq(7.3, 120, length.out = 10),
                    gas_ppm = seq(434, 570, length.out = 10),
                    volume_cm3 = 7385.3,  # LI-8100A system volume
                    tair_C = 20)
  x <- x * (10000 / 317.8) # convert to per m2 based on 20 cm chamber
  # Hmm...we don't know the chamber area from the Licor example
  # If it's 20 cm, then the flux computed here is ~2x too high
  # If it was a 27 cm diameter chamber (area = ~600 cm2) then things match
})

