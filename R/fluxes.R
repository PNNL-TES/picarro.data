# fluxes.R


#' Compute respiration flux from concentration data.
#'
#' @param time Time (typically in seconds); numeric vector
#' @param gas_ppm Gas (e.g. CO2 or CH4) concentrations, ppmv; numeric vector
#' @param volume_cm3 System volume, cm3; numeric
#' @param tair_C Air temperature, C; numeric
#' @param pressure_kPa Air pressure, kPa; numeric
#' @param debug_plot Produce a plot of data and fitted model? Logical
#'
#' @return The gas flux, µmol per unit time.
#' @importFrom graphics abline plot title
#' @importFrom stats coefficients lm na.omit
#' @export
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
#' @details We derive the rate of concentration change via the slope of the
#' linear regression of all data points of concentrations versus time.
#' One problem with this method is that the linearity attributed to the
#' regression is incorrect in a strict sense, since the gradients in
#' water vapor and CO2 decrease over time in a closed chamber (see
#' discussion in Steduto et al. below). This method should thus not be
#' used for long measurement periods.
#'
#' @note The computed is \emph{not} area- or mass-corrected.
#'
#' @examples
#' # ten-second CO2 rise at room temperature
#' compute_flux(1:10, 401:410, volume_cm3 = 2000, tair_C = 20)
compute_flux <- function(time, gas_ppm, volume_cm3, tair_C,
                         pressure_kPa = 101.325, debug_plot = FALSE) {

  stopifnot(length(time) == length(gas_ppm))
  stopifnot(length(na.omit(time)) > 1)
  stopifnot(length(na.omit(gas_ppm)) > 1)
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

  # Compute ppm/time unit change in gas concentration
  m <- lm(gas_ppm ~ time)
  gas_ppm_time <- unname(coefficients(m)["time"])

  if(debug_plot) {
    plot(time, gas_ppm)
    abline(coefficients(m))
    title(paste("gas_ppm_s = ", round(gas_ppm_time, 4)))
  }

  # Respiration, µmol/time unit via ideal gas law
  gas_ppm_time * volume_cm3 * pressure_kPa / (R * (tair_C + 273.15))
}


#' Compute cumulative C evolved
#'
#' @param time Time, typically s or hr but arbitrary; numeric
#' @param flux Flux, in units of X/unit time; numeric
#' @param interpolate Interpolate missing values? Logical
#'
#' @return A vector of cumulative values of X evolved.
#' @export
#' @importFrom stats approx
#' @importFrom utils head
#'
#' @details X evolved at timestep \emph{i} is the
#' lag between \code{time[i]} and \code{time[i-1]} multiplied by the mean of
#' \code{flux[i]} and \code{flux[i-1]}.
#' The first timestamp value is considered as time zero, so its corresponding
#' cumulative value will always be zero.
#'
#' @examples
#' cumulative_evolution(1:4, c(1, 1, NA, 1))
#' cumulative_evolution(1:4, c(1, 1, 2, 1))
cumulative_evolution <- function(time, flux, interpolate = TRUE) {

  stopifnot(all(!is.na(time)))
  stopifnot(length(time) == length(flux))

  if(interpolate) {
    flux = approx(time, flux, xout = time, rule = 2)[['y']]
  }

  delta_time <- time[-1] - head(time, -1)

  intermediate_fluxes <- rep(NA_real_, length(delta_time))
  ivals <- head(seq_along(flux), -1)
  for(i in ivals) {
    intermediate_fluxes[i] <- mean(c(flux[i], flux[i+1]))
  }
  evolved <- intermediate_fluxes * delta_time
  c(0, cumsum(evolved))  # cumulative
}
