# fluxes.R


#' Compute respiration flux from concentration data.
#'
#' @param seconds Timestamps in (typically) seconds, numeric vector
#' @param gas_ppm Gas (e.g. CO2 or CH4) concentrations, ppmv, numeric vector
#' @param volume_cm3 System volume, cm3, numeric
#' @param tair_C Air temperature, C, numeric
#' @param pressure_kPa Air pressure, kPa, numeric
#' @param debug_plot Produce a plot of data and fitted model? Logical
#'
#' @return The gas flux, µmol per second.
#'
#' @references
#' \itemize{
#' \item{Steduto et al. (2002), "Automated closed-system canopy-chamber for continuous
#' field-crop monitoring of CO2 and H2O fluxes" AFM 111:171-186
#'  \url{http://dx.doi.org/10.1016/S0168-1923(02)00023-0}.}
#' \item{Campbell and Normal (1998), An Introduction to Environmental Biophysics,
#'  ISBN 978-1-4612-1626-1. New York: Springer-Verlag, 286 pp.}
#'  \item{LI-COR Biosciences (2015), Using the LI-8100A Soil Gas Flux System & the LI-8150
#'  Multiplexer, 984-11123. 238 pp.}
#' }
#' @note The computed is \emph{not} area- or mass-corrected.
#'
#' @export
#' @importFrom graphics abline plot title
#' @importFrom stats coefficients lm na.omit
#'
#' @examples
#' compute_flux(1:10, 401:410, volume_cm3 = 2000, tair_C = 20)
compute_flux <- function(seconds, gas_ppm, volume_cm3, tair_C,
                         pressure_kPa = 101.325, debug_plot = FALSE) {

  stopifnot(length(seconds) == length(gas_ppm))
  stopifnot(length(na.omit(seconds)) > 2)
  stopifnot(length(na.omit(gas_ppm)) > 2)
  stopifnot(gas_ppm >= 0)
  stopifnot(volume_cm3 > 0)
  stopifnot(tair_C < 100)
  stopifnot(pressure_kPa > 0)

  # We want to compute rate of change (CO2 ppm/s and CH4 ppb/s),
  # and then convert this to µmol/s using the ideal gas law:
  # A = dC/dt * V * Pa/RT (cf. Steduto et al. 2002), where
  # 	A is the flux (µmol/g/s)
  #	  dC/dt is raw respiration as above (mole fraction/s)
  # 	V is total chamber volume (cm3)
  #	  Pa is atmospheric pressure (kPa)
  #	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
  #	  T is air temperature (K)

  R <- 8.3145e+3			# cm3 kPa K−1 mol−1

  # Compute ppm/s change in gas concentration
  m <- lm(gas_ppm ~ seconds)
  gas_ppm_s <- unname(coefficients(m)["seconds"])

  if(debug_plot) {
    plot(seconds, gas_ppm)
    abline(coefficients(m))
    title(paste("gas_ppm_s = ", round(gas_ppm_s, 4)))
  }

  # Respiration, µmol/s via ideal gas law
  gas_ppm_s * volume_cm3 * pressure_kPa / (R * (tair_C + 273.15))
}


# cumulative_fluxes <- function(interpolate = TRUE) {
#
#   # Cumulative emissions
#   sdata %>%
#     arrange(DATETIME) %>%
#     group_by(Sample_number) %>%
#     # Compute incubation time
#     mutate(inctime_hours = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")) %>% round(2),
#            # interpolate missing fluxes
#            CO2_flux_mgC_hr_interp = approx(inctime_hours, CO2_flux_mgC_hr, xout = inctime_hours, rule = 2)[['y']],
#            CH4_flux_mgC_hr_interp = approx(inctime_hours, CH4_flux_mgC_hr, xout = inctime_hours, rule = 2)[['y']],
#            # ...and compute cumulative emissions
#            delta_hrs = (inctime_hours - lag(inctime_hours)),
#            CO2_flux_mgC = CO2_flux_mgC_hr_interp * delta_hrs,
#            cumCO2_flux_mgC_gSoil = c(0, cumsum(CO2_flux_mgC[-1] / DryMass_SoilOnly_g[-1])),
#            CH4_flux_mgC = CH4_flux_mgC_hr_interp * delta_hrs,
#            cumCH4_flux_mgC_gSoil = c(0, cumsum(CH4_flux_mgC[-1] / DryMass_SoilOnly_g[-1])),
#            label = if_else(inctime_hours == max(inctime_hours), SampleID, "")) %>%
#     ungroup
#
# }
