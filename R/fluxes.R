# fluxes.R


compute_flux <- function(Elapsed_seconds,
                         Gas_concentration_ppm,
                         Volume_cm3,
                         Molar_weight,
                         Tair_K,
                         Pa_kPa,
                         Mass_g) {
  # We want to compute rate of change (CO2 ppm/s and CH4 ppb/s),
  # and then convert this to mg C/s, using
  # A = dC/dt * V/M * Pa/RT (cf. Steduto et al. 2002), where
  # 	A is the flux (µmol/g/s)
  #	  dC/dt is raw respiration as above (mole fraction/s)
  # 	V is total chamber volume (cm3)
  #	  M is [dry] soil mass (g)
  #	  Pa is atmospheric pressure (kPa)
  #	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
  #	  T is air temperature (K)

  # The instrument tubing is 455 cm long by ID 1/16"
  V_tubing <- (1/16 * 2.54 / 2 ) ^ 2 * pi * 455
  # Headspace is in each is the total volume of the sleeve minus the soil volume
  ## Kaizad: changed radius to 3 xcm
  V_headspace <- (3 / 2) ^ 2 * pi * sdata$HeadSpace_Ht_cm
  # Replace missing headspace values with the mean
  V_headspace[is.na(V_headspace)] <- mean(V_headspace, na.rm = TRUE)

  # Internal volume of Picarro?
  V_picarro <- 9 # Assume same as PP-Systems

  sdata$V_cm3 <- V_tubing + V_headspace + V_picarro

  Pa 			<- 101						# kPa				(Richland is ~120 m asl)
  R 			<- 8.3145e+3			# cm3 kPa K−1 mol−1
  Tair    <- 273.1 + 20 # TODO: fluxdata$Temperature     # C -> K

  # Calculate mass-corrected respiration, µmol/s
  CO2_flux_µmol_g_s <-
    with(sdata,
         CO2_ppm_s / 1 * # from ppm/s to µmol/s
           V_cm3 * Pa / (R * Tair)) # ideal gas law
  CH4_flux_µmol_g_s <-
    with(sdata,
         CH4_ppb_s / 1000 * # from ppb/s to µmol/s
           V_cm3 * Pa / (R * Tair)) # ideal gas law

  # Calculate flux of mg C/hr
  sdata$CO2_flux_mgC_hr <- CO2_flux_µmol_g_s /
    1e6 * # to mol
    12 *  # to g C
    1000 * # to mg C
    60 * 60 # to /hr
  sdata$CH4_flux_mgC_hr <- CH4_flux_µmol_g_s /
    1e6 * # to mol
    16 *  # to g C
    1000 *  # to mg C
    60 * 60 # to /hr


}

cumulative_fluxes <- function(interpolate = TRUE) {

  # Cumulative emissions
  sdata %>%
    arrange(DATETIME) %>%
    group_by(Sample_number) %>%
    # Compute incubation time
    mutate(inctime_hours = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")) %>% round(2),
           # interpolate missing fluxes
           CO2_flux_mgC_hr_interp = approx(inctime_hours, CO2_flux_mgC_hr, xout = inctime_hours, rule = 2)[['y']],
           CH4_flux_mgC_hr_interp = approx(inctime_hours, CH4_flux_mgC_hr, xout = inctime_hours, rule = 2)[['y']],
           # ...and compute cumulative emissions
           delta_hrs = (inctime_hours - lag(inctime_hours)),
           CO2_flux_mgC = CO2_flux_mgC_hr_interp * delta_hrs,
           cumCO2_flux_mgC_gSoil = c(0, cumsum(CO2_flux_mgC[-1] / DryMass_SoilOnly_g[-1])),
           CH4_flux_mgC = CH4_flux_mgC_hr_interp * delta_hrs,
           cumCH4_flux_mgC_gSoil = c(0, cumsum(CH4_flux_mgC[-1] / DryMass_SoilOnly_g[-1])),
           label = if_else(inctime_hours == max(inctime_hours), SampleID, "")) %>%
    ungroup

}
