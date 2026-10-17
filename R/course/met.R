#**************************************************************
# The file met.R contains a collection of R functions for
# meteorological computations.
#
# Author(s): Dieter Scherer
# Date:      30.08.2022
#**************************************************************

#**************************************************************
# The function tests the functions in met.R (NYI). 
#
#**************************************************************      

met_test <- function() {
  
  return(TRUE)
}

#**************************************************************
# The function returns the value of a meteorological constant. 
#
# Arg:
#   const: A character string indicating which constant shall be
#          returned. E.g., if const is "mw_a", then the value of
#          mw_a is returned as 0.02897 (kg/mol). The attribute
#          units indicates the respective unit of the constant.
#          See list below for possible values of const:
#      
#   Molecular weights:
#   mw_a = 0.02897 (kg/mol) ; dry air
#   mw_w = 0.01802 (kg/mol) ; water
#
#   Gas constants:
#   R   = 8.3145 (J K-1 mol-1) ; universal gas constant
#   R_a = 287.05 (J K-1 kg-1)  ; gas constant of dry air
#   mixing_ratio = 461.51 (J K-1 kg-1)  ; gas constant of water vapor 
#
#   Heat capacities:
#   cp_a = 1005 (J K-1 kg-1) ; dry air at constant pressure
#   cp_v = 1860 (J K-1 kg-1) ; water vapor at constant pressure
#   c_w  = 4182 (J K-1 kg-1) ; water at 0 deg C
#   c_i  = 2105 (J K-1 kg-1) ; ice at 0 deg C
#
#   Densities:
#   air_dens = 1.292 (kg/m3) ; dry air at 0 deg C and 1013.25 hPa
#   d_w = 999.8 (kg/m3) ; water at 0 deg C
#   d_i = 916.7 (kg/m3) ; ice at 0 deg C
#
#   Molecular heat conductivities:
#   k_a = 2.53e-2 (W m-1 K-1) ; dry air
#   k_w = 0.598   (W m-1 K-1) ; water 
#   k_i = 2.2     (W m-1 K-1) ; ice
#
#   Latent heat:
#   lh_f = 0.334e6 (J/kg) ; fusion
#   lh_v = 2.500e6 (J/kg) ; vaporization
#   lh_s = 2.834e6 (J/kg) ; sublimation
#
#   Earth ellipsoid parameters (WGS-84):
#   wgs84_e_a = 6378.137d (km)
#   wgs84_e_b = 6356.752d (km)
#
#   Standard acceleration due to gravity:
#   g0 = 9.806 (m/s2) ; at 45 deg lat and 0 m a.s.l.
#
#   Earth's angular velocity:
#   v_ang = 7.2921235e-005 (rad/s) 
#
#   Mean solar constant and further radiometric constants:
#   sol_const = 1360.8  (W/m2)
#   st_boltz  = 5.67e-8 (W m-2 K-4)
#
#   Von Karman's constant:
#   karman = 0.4
#
# Returns:
#   The value of the constant in the respective unit (see above).
#   The attribute units contains the unit. If no valid constant
#   is specified in const, then NULL is returned. 
#
# Author(s): Dieter Scherer
# Date:      14.08.2019
#**************************************************************

met_const <- function(const) {

  #***************
  # Check argument
  #***************
  
  if (is.null(const))    {return(NULL)} # argument missing 
  if (length(const) > 1) {return(NULL)} # only one argument allowed 
  
  const <- tolower(trimws(const, "both")) # for easier string comparison
  
  #****************
  # Return constant 
  #****************

  # Molecular weights
  
  if (const == "mw_a") {val <- 0.02897; attr(val, "units") <- "kg/mol"; return(val)}
  if (const == "mw_w") {val <- 0.01802; attr(val, "units") <- "kg/mol"; return(val)}

  # Gas constants
  
  if (const == "r")   {val <- 8.3145; attr(val, "units") <- "J K-1 mol-1"; return(val)}
  if (const == "r_a") {val <- 287.05; attr(val, "units") <- "J K-1 kg-1";  return(val)}
  if (const == "mixing_ratio") {val <- 461.51; attr(val, "units") <- "J K-1 kg-1";  return(val)}
  
  # Heat capacities
  
  if (const == "cp_a") {val <- 1005; attr(val, "units") <- "J K-1 kg-1"; return(val)}
  if (const == "cp_v") {val <- 1860; attr(val, "units") <- "J K-1 kg-1"; return(val)}
  if (const == "c_w")  {val <- 4182; attr(val, "units") <- "J K-1 kg-1"; return(val)}
  if (const == "c_i")  {val <- 2105; attr(val, "units") <- "J K-1 kg-1"; return(val)}

  # Densities

  if (const == "air_dens") {val <- 1.292; attr(val, "units") <- "kg/m3"; return(val)}
  if (const == "d_w") {val <- 999.8; attr(val, "units") <- "kg/m3"; return(val)}
  if (const == "d_i") {val <- 916.7; attr(val, "units") <- "kg/m3"; return(val)}
  
  # Molecular heat conductivities

  if (const == "k_a") {val <- 2.53e-2; attr(val, "units") <- "W m-1 K-1"; return(val)}
  if (const == "k_w") {val <- 0.598;   attr(val, "units") <- "W m-1 K-1"; return(val)}
  if (const == "k_i") {val <- 2.2;     attr(val, "units") <- "W m-1 K-1"; return(val)}
  
  # Latent heat

  if (const == "lh_f") {val <- 0.334e6; attr(val, "units") <- "J/kg"; return(val)}
  if (const == "lh_v") {val <- 2.500e6; attr(val, "units") <- "J/kg"; return(val)}
  if (const == "lh_s") {val <- 2.834e6; attr(val, "units") <- "J/kg"; return(val)}

  # Earth ellipsoid parameters (WGS-84)

  if (const == "wgs84_e_a") {val <- 6378.137; attr(val, "units") <- "km"; return(val)}
  if (const == "wgs84_e_b") {val <- 6356.752; attr(val, "units") <- "km"; return(val)}
  
  # Standard acceleration due to gravity

  if (const == "g0") {val <- 9.806; attr(val, "units") <- "m/s2"; return(val)}
  
  # Earth's angular velocity

  if (const == "v_ang") {val <- 7.2921235e-5; attr(val, "units") <- "rad/s"; return(val)}
  
  # Mean solar constant and further radiometric constants

  if (const == "sol_const") {val <- 1360.8;  attr(val, "units") <- "W/m2";      return(val)}
  if (const == "st_boltz")  {val <- 5.67e-8; attr(val, "units") <- "W m-2 K-4"; return(val)}
  
  # Von Karman's constant

  if (const == "karman") {val <- 0.4; attr(val, "units") <- ""; return(val)}
  
  #***************************************
  # Return NULL since const is not in list
  #***************************************

  return(NULL)  
}

#**************************************************************
# The function computes potential temperature pot_temp from air
# temperature air_temp at altitude z using either the air pressure air_press
# at altitude z or the altitude difference dz.
#
# Args:
#   air_temp:      Air temperature (deg C) at altitude z.
#   air_press:      Air pressure (hPa) at altitude z. If present, then
#           pot_temp is computed for air_press_ref = 1000 hPa.
#           Default: dz is used for computing pot_temp, and thus, dz
#           must be present.
#   dz:     Altitude difference (m). If air_press is specified, then dz
#           is ignored. Default: air_press is used for computing pot_temp,
#           and thus, air_press must be present.
#   Kelvin: If TRUE, then pot_temp is returned in K.
#           Default: FALSE (deg C). 
#
# Returns:
#   Potential temperature pot_temp (deg C or K; see Kelvin).
#   If neither air_press or dz are specified, then NULL is returned. 
#
# Author(s): Dieter Scherer
# Date:      16.08.2019
#**************************************************************

met_pot_temp <- function(air_temp, air_press=NULL, dz=NULL, Kelvin=FALSE) {

  #****************
  # Check arguments
  #****************

  # Check if air_temp is specified
  
  if (is.null(air_temp)) {return(NULL)}
  
  N_air_temp <- length(air_temp)

  # Check if at least one of either air_press or dz is specified

  if (is.null(air_press) & is.null(dz)) {return(NULL)}

  #******************************************************************
  # If air_press is present then compute pot_temp for air_press_ref = 1000 hPa else use dz
  #******************************************************************

  air_temp <- air_temp + 273.15 # convert from deg C to K

  if (! is.null(air_press)) {
    N_air_press <- length(air_press)
    if (N_air_temp > 1 & N_air_press > 1 & N_air_temp != N_air_press) {return(NULL)} # not allowed

    k       <- met_const("R_a")/met_const("cp_a") # Poisson constant (for dry air)
    air_press_ref   <- 1000                               # (hPa), reference air pressure
    pot_temp     <- air_temp*(air_press_ref/air_press)^k                      # (K); at air pressure air_press_ref

  } else {

    N_dz <- length(dz)
    if ((N_air_temp > 1 & N_dz != 1) |
        (N_air_temp > 1 & N_dz > 1 & N_dz != N_air_temp)) {return(NULL)} # not allowed
    
    dTdz_da <- -met_const("g0")/met_const("cp_a") # (K/m); dry adiabatic lapse rate
    pot_temp     <- air_temp - dTdz_da*dz                     # (K); at altitude z+dz
  }
  
  #**************************
  # Return pot_temp in deg C or K?
  #**************************
  
  if (Kelvin) {
    attr(pot_temp, "units") <- "K"
  } else {
    pot_temp <- pot_temp - 273.15; attr(pot_temp, "units") <- "deg C"
  }
  
  return(pot_temp)
}

#**************************************************************
# The function computes virtual temperature virt_temp from air
# temperature air_temp and mixing ratios of water vapour mixing_ratio, liquid
# water r_w and ice r_i, the latter two being relevant for air
# in clouds.  
#
# Args:
#   air_temp:      Air temperature (deg C). Default: 0.
#   mixing_ratio:    Mixing ratio of water vapour (g/kg). Default: 0.
#   r_w:    Mixing ratio of liquid water (g/kg). Default: 0.
#   r_i:    Mixing ratio of ice (g/kg). Default: 0.
#   Kelvin: If TRUE, then virt_temp is returned in K.
#           Default: FALSE (deg C). 
#
# Returns:
#   Virtual temperature virt_temp (deg C or K; see Kelvin).
#
# Author(s): Dieter Scherer
# Date:      28.08.2019
#**************************************************************

met_virt_temp <- function(air_temp=0, mixing_ratio=0, r_w=0, r_i=0, Kelvin=FALSE) {

  #****************
  # Check arguments
  #****************
  
  N_air_temp   <- length(air_temp)
  N_mixing_ratio <- length(mixing_ratio)
  N_r_w <- length(r_w)
  N_r_i <- length(r_i)

  N <- max(c(N_air_temp, N_mixing_ratio, N_r_w, N_r_i))

  if (N_air_temp   > 1 & N_air_temp   != N) {return(NULL)}
  if (N_mixing_ratio > 1 & N_mixing_ratio != N) {return(NULL)}
  if (N_r_w > 1 & N_r_w != N) {return(NULL)}
  if (N_r_i > 1 & N_r_i != N) {return(NULL)}

  #****************************
  # Compute virtual temperature
  #****************************
  
  f <- met_const("mw_w")/met_const("mw_a") # (kg/kg)

  mixing_ratio <- 0.001*mixing_ratio  # convert from g/kg to kg/kg
  r_w <- 0.001*r_w  # convert from g/kg to kg/kg
  r_i <- 0.001*r_i  # convert from g/kg to kg/kg
  air_temp   <- air_temp + 273.15 # convert from deg C to K

  virt_temp <- air_temp*(1+mixing_ratio/f)/(1+mixing_ratio+r_w+r_i) # (K)

  #*****************************
  # Return virt_tempirt in deg C or K?
  #*****************************
  
  if (Kelvin) {
    attr(virt_temp, "units") <- "K"
  } else {
    virt_temp <- virt_temp - 273.15; attr(virt_temp, "units") <- "deg C"
  }

  return(virt_temp)
}

#**************************************************************
# The function computes density of air air_dens from temperature air_temp,
# which is either air temperature (dry air) or virtual
# temperature (moist air), and air pressure air_press
#
# Args:
#   air_temp: Air temperature or virtual temperature (deg C).
#      Default: 0 deg C.
#   air_press: Air pressure (hPa). Default: 1013.25 hPa.
#
# Returns:
#   Density of dry or moist air air_dens (kg/m3).
#
# Author(s): Dieter Scherer
# Date:      14.08.2019
#**************************************************************

met_air_dens <- function(air_temp=0, air_press=1013.25) {

  #****************
  # Check arguments
  #****************
  
  N_air_temp <- length(air_temp)
  N_air_press <- length(air_press)
  N   <- max(c(N_air_temp, N_air_press))

  if (N_air_temp > 1 & N_air_temp != N) {return(NULL)}
  if (N_air_press > 1 & N_air_press != N) {return(NULL)}

  idx <- ! is.na(air_press) & air_press < 0
  air_press[idx] <- NA
  
  #***********************
  # Compute density of air
  #***********************
  
  air_temp <- air_temp + 273.15 # convert from deg C to K
  air_press <- 100*air_press      # convert from hPa to Pa
  
  air_dens <- air_press/(met_const("R_a")*air_temp) # (kg/m3); ideal gas law for dry air

  #**************
  # Return result
  #**************
  
  attr(air_dens, "units") <- "kg/m3"
  
  return(air_dens)
}

#**************************************************************
# The function computes vapour pressure vap_press from air temperature
# air_temp and relative humidity rel_hum. By default, rel_hum is set to 100 %, thus
# vap_press is then saturation vapour pressure sat_press at air temperature air_temp.
#
# Args:
#   air_temp: Air temperature (deg C). Default: 0 deg C.
#   rel_hum: Relative humidity (%). Default: 100 %.
#
# Returns:
#   Vapour pressure vap_press (hPa).
#
# Author(s): Dieter Scherer, Marco Otto
# Date:      25.11.2025
#**************************************************************

met_vap_press <- function(air_temp=0, rel_hum=100) {

  #****************
  # Check arguments
  #****************

  N_air_temp <- length(air_temp)
  N_rel_hum <- length(rel_hum)
  N   <- max(c(N_air_temp, N_rel_hum))
  
  if (N_air_temp > 1 & N_air_temp != N) {return(NULL)}
  if (N_rel_hum > 1 & N_rel_hum != N) {return(NULL)}
  
  idx <- ! is.na(rel_hum) & (rel_hum > 100 | rel_hum < 0)
  rel_hum[idx] <- NA
  
  #*****************************************************
  # Set up Magnus formula for saturation vapour pressure
  #*****************************************************
  
  air_temp_min <- -50.9    # (deg C)
  
  c1    <- 6.1078   # (hPa)
  c2_p  <- 17.08085 #
  c2_n  <- 17.84362 #
  c2_n  <- 17.84362 #
  c3_p  <- 234.175  # (deg C)
  c3_n  <- 245.425  # (deg C)

  #***********************************
  # Compute saturation vapour pressure
  #***********************************
  
  sat_press <- rep(NA, N)
  
  idx <- ! is.na(air_temp) & air_temp >= 0
  sat_press[idx] <- c1*exp(c2_p*air_temp[idx]/(c3_p+air_temp[idx])) # (hPa); assuming water surface

  idx <- ! is.na(air_temp) & air_temp < 0
  sat_press[idx] <- c1*exp(c2_n*air_temp[idx]/(c3_n+air_temp[idx])) # (hPa); assuming ice surface for negative air_temp

  iair_temp_mindx <- ! is.na(air_temp) & air_temp < air_temp_min
  sat_press[idx] <- 0 # (hPa); Magnus formula is no longer valid
  
  #***********************************************
  # Compute vapour pressure from relative humidity
  #***********************************************

  rel_hum <- 0.01*rel_hum # convert from % to dimensionless value
  
  vap_press <- rel_hum*sat_press # (hPa)
  
  #**************
  # Return result
  #**************
  
  attr(vap_press, "units") <- "hPa"
  
  return(vap_press)
}

#**************************************************************
# The function computes absolute humidity abs_hum from air temperature
# air_temp and vapour pressure vap_press. If vap_press is not specified, then vap_press is set
# to saturation vapour pressure sat_press at air temperature air_temp.
#
# Args:
#   air_temp: Air temperature (deg C). Default: 0.
#   vap_press: Vapour pressure (hPa). Default: vap_press = sat_press(air_temp).
#
# Returns:
#   Absolute humidity abs_hum (g/m3).
#
# Author(s): Dieter Scherer, Marco Otto
# Date:      31.08.2022
#**************************************************************

met_abs_hum <- function(air_temp=0, vap_press=NULL) {

  #****************
  # Check arguments
  #****************

  N_air_temp <- length(air_temp)
  N_vap_press <- length(vap_press)
  N   <- max(c(N_air_temp, N_vap_press))
  
  if (N_air_temp > 1 & N_air_temp != N) {return(NULL)}
  if (N_vap_press > 1 & N_vap_press != N) {return(NULL)}

  # Apply default to vap_press

  sat_press <- met_vap_press(air_temp)                  # (hPa); saturation vapour pressure at air_temp
  if (is.null(sat_press)) {return(NULL)} # something went wrong
  
  if (is.null(vap_press)) {vap_press <- sat_press}       # apply default value for vap_press

  # Mask invalid values
  
  idx <- ! is.na(vap_press) & (vap_press > sat_press | vap_press < 0)
  vap_press[idx] <- NA

  #**************************
  # Compute absolute humidity
  #**************************
  
  air_temp <- air_temp + 273.15 # convert from deg C to K
  vap_press <- 100*vap_press      # convert from hPa to Pa
  
  abs_hum <- vap_press/(met_const("mixing_ratio")*air_temp) # (kg/m3); ideal gas law for water vapour

  abs_hum <- 1000*abs_hum # convert from kg/m3 to g/m3
 
  #**************
  # Return result
  #**************
  
  attr(abs_hum, "units") <- "g/m3"
  
  return(abs_hum)
}

#**************************************************************
# The function computes specific humidity spec_hum from vapour
# pressure vap_press and air pressure air_press.
#
# Args:
#   vap_press: Vapour pressure (hPa).
#   air_press: Air pressure (hPa). Default: 1013.25 hPa.
#
# Returns:
#   Specific humidity spec_hum (g/kg).
#
# Author(s): Dieter Scherer
# Date:      30.08.2022
#**************************************************************

met_spec_hum <- function(vap_press, air_press=1013.25) {
  
  #****************
  # Check arguments
  #****************

  N_vap_press <- length(vap_press)
  N_air_press <- length(air_press)
  N   <- max(c(N_vap_press, N_air_press))
  
  if (N_vap_press > 1 & N_vap_press != N) {return(NULL)}
  if (N_air_press > 1 & N_air_press != N) {return(NULL)}
  
  # Mask invalid values
  
  idx <- ! is.na(vap_press) & vap_press < 0
  vap_press[idx] <- NA

  idx <- ! is.na(air_press) & air_press < 0
  air_press[idx] <- NA
  
  #**************************
  # Compute specific humidity
  #**************************
  
  f <- met_const("mw_w")/met_const("mw_a") # (kg/kg)

  spec_hum <- f*vap_press/(air_press-(1-f)*vap_press) # (kg/kg); fraction of water vapour in moist air

  spec_hum <- 1000*spec_hum # convert from kg/kg to g/kg
  
  #**************
  # Return result
  #**************
  
  attr(spec_hum, "units") <- "g/kg"
  
  return(spec_hum)
}

#**************************************************************
# The function computes mixing ratio of water vapour mixing_ratio from
# vapour pressure vap_press and air pressure air_press.
#
# Args:
#   vap_press: Vapour pressure (hPa).
#   air_press: Air pressure (hPa). Default: 1013.25 hPa.
#
# Returns:
#   Mixing ratio of water vapour mixing_ratio (g/kg).
#
# Author(s): Dieter Scherer
# Date:      30.08.2022
#**************************************************************

met_mixing_ratio <- function(vap_press, air_press=1013.25) {
  
  #****************
  # Check arguments
  #****************

  N_vap_press <- length(vap_press)
  N_air_press <- length(air_press)
  N   <- max(c(N_vap_press, N_air_press))
  
  if (N_vap_press > 1 & N_vap_press != N) {return(NULL)}
  if (N_air_press > 1 & N_air_press != N) {return(NULL)}
  
  # Mask invalid values
  
  idx <- ! is.na(vap_press) & vap_press < 0
  vap_press[idx] <- NA
  
  idx <- ! is.na(air_press) & air_press < 0
  air_press[idx] <- NA
  
  #*********************
  # Compute mixing ratio
  #*********************
  
  f <- met_const("mw_w")/met_const("mw_a") # (kg/kg)

  mixing_ratio <- f*vap_press/(air_press-vap_press) # (kg/kg); fraction of water vapour in dry air

  mixing_ratio <- 1000*mixing_ratio # convert from kg/kg to g/kg
  
  #**************
  # Return result
  #**************
  
  attr(mixing_ratio, "units") <- "g/kg"
  
  return(mixing_ratio)
}

#**************************************************************
# The function computes air pressure air_press at altitude z from air
# pressure air_press_ref at altitude z_ref. The virtual temperature 
# virt_temp_ref at z_ref is used together with virtual temperature virt_temp
# at altitude z in the barometric formula to compute mean virtual
# temperature of atmospheric layers.
#
# Args:
#   air_press_ref:   Air pressure (hPa) at reference level. 
#            Default: 1013.25 hPa.
#   z_ref:   Altitude (m asl.) of the reference level.
#            Default: 0 m asl..
#   virt_temp_ref: Virtual temperature (deg C) at reference level.
#            Default: 15 deg C.
#   z:       Altitude (m asl) of level for which air_press shall be
#            computed. z must either be higher than z_ref and
#            increase monotonically, or lower than z_ref and
#            decrease monotonically. z may not contain NA values!
#            Default: a vertical profile above z_ref up to 11 km asl.
#            in 100 m steps.
#   virt_temp:     Virtual temperature (deg C) at altitude z.
#            Default: apply moist adiabatic lapse rate to virt_temp_ref.
#
# Returns:
#   Air pressure air_press (hPa) at altitude z. If air_press_ref or virt_temp_ref is an
#   array (e.g. a time series), then air_press can only be computed for
#   a single level z. In this case, virt_temp must have either one element
#   or as many as air_press_ref or virt_temp_ref.
#
# Author(s): Dieter Scherer
# Date:      14.08.2019
#**************************************************************

met_air_press <- function(air_press_ref=1013.25, z_ref=0, virt_temp_ref=15, z=NULL, virt_temp=NULL) {

  #****************
  # Check arguments
  #****************

  # Apply default to z 
  
  if (is.null(z)) {
    from <- 100*ceiling(0.01*(z_ref+1)) # (m asl.); first 100 metre step above z_ref
    z <- seq(from, to=11000, by=100)    # (m asl.); vertical profile up to 11 km
  }
  
  # Check array processing
  
  N_air_press_ref   <- length(air_press_ref)
  N_z_ref   <- length(z_ref)
  N_virt_temp_ref <- length(virt_temp_ref)
  N_z       <- length(z)
  N_virt_temp     <- length(virt_temp)
  N         <- max(c(N_air_press_ref, N_virt_temp_ref)) # number of values for air_press for a single level z

  if (N_z_ref != 1) {return(NULL)} 
  if (N_air_press_ref != 1 & N_virt_temp_ref != 1 & N_air_press_ref != N_virt_temp_ref) {return(NULL)}
  if (N > 1 & N_z > 1) {return(NULL)}
  if (N > 1 & N_virt_temp > 1 & N_virt_temp != N) {return(NULL)}

  N <- max(c(N, N_z)) # number of values for air_press to compute
  
  # Apply default to virt_temp  

  dz <- z - z_ref # (m); altitude difference

  if (is.null(virt_temp)) {
    dTdz_ma <- -0.0065          # (K/m);   standard moist adiabatic lapse rate
    virt_temp <- virt_temp_ref + dTdz_ma*dz # (deg C); compute virt_temp at level z
  }

  if (N_z == 1) {
    virt_temp_mean <- (virt_temp_ref+virt_temp)/2                           # mean virt_temp in atmospheric layer
  } else {
    virt_temp_mean <- (virt_temp_ref+virt_temp[1])/2                        # mean virt_temp in first atmospheric layer
    for (i in 2:N_z) {virt_temp_mean[i] <- (virt_temp[i-1]+virt_temp[i])/2} # mean virt_temp of further atmospheric layers 
  }

  # Mask invalid data
  
  idx <- ! is.na(air_press_ref) & air_press_ref < 0
  air_press_ref[idx] <- NA

  #*********************************
  # Compute air pressure at levels z  
  #*********************************
  
  virt_temp_mean <- virt_temp_mean + 273.15 # convert from deg C to K
  
  g   <- met_const("g0")  # standard gravity
  R_a <- met_const("R_a") # gas constant of dry air

  air_press <- rep(NA, N)

  idx <- !is.na(virt_temp_mean)    
  air_press[idx] <- air_press_ref[idx]*exp(-g*dz/(R_a*virt_temp_mean[idx])) # (hPa); barometric formula

  #**************
  # Return result
  #**************
  
  attr(air_press, "units") <- "hPa"
  
  return(air_press)
}

#**************************************************************
# The function computes the radius of the Earth for a given
# latitude lat.
#
# Args:
#   lat: Latitude (decimal degrees). Default: 45 deg.
#
# Returns:
#   Radius of the Earth (km).
#
# Author(s): Dieter Scherer
# Date:      29.08.2019
#**************************************************************

met_r_earth <- function(lat=45) {

  lat[lat <= -90 | lat >= 90] <- NA

  phi <- abs(lat)*pi/180
  
  a <- met_const("wgs84_e_a")
  b <- met_const("wgs84_e_b")

  r <- sqrt(a^2*cos(phi)^2+b^2*sin(phi)^2)

  #**************
  # Return result
  #**************
  
  attr(r, "units") <- "km"
  
  return(r)
}
  
#**************************************************************
# The function computes the Earth's gravity acceleration
# for a given latitude and height above sea level.
#
# Args:
#   lat: Latitude (decimal degrees). Default: 45 deg.
#   z:   Height above sea level (m asl). Default: 0 m asl.
#
# Returns:
#   Gravity acceleration (m/s2).
#
# Author(s): Dieter Scherer
# Date:      30.08.2019
#**************************************************************

met_g <- function(lat=45, z=0) {

  r <- 1000*met_r_earth(lat)

  phi <- abs(lat)*pi/180

  om2 <- met_const("v_ang")^2
  
  g0 <- met_const("g0")
  
  g = (g0*(1-0.0026373*cos(2*phi)-0.0000059*cos(2*phi)^2) + om2*r*cos(phi)^2)*1/(1+z/r)^2 -
      om2*r*cos(phi)^2*(1+z/r)

  #**************
  # Return result
  #**************
  
  attr(g, "units") <- "m/s2"
  
  return(g)
}

#**************************************************************
# The function computes wind speed from wind components u and v.
#
# Args:
#   u: Wind component in x-direction, i.e., towards E (m/s).
#   v: Wind component in y-direction, i.e., towards N (m/s).
#
# Returns:
#   Wind speed (m/s).
#
# Author(s): Dieter Scherer, Marco Otto, Ute Fehrenbach
# Date:      29.08.2019
#**************************************************************

met_wspeed <- function(u, v) {
 
  wspeed <- sqrt(u^2+v^2)
   
  attr(wspeed, "units") <- "m/s"

  return(wspeed)
}

#**************************************************************
# The function computes wind direction from wind components u and v.
#
# Args:
#   u: Wind component in x-direction, i.e., towards E (m/s).
#   v: Wind component in y-direction, i.e., towards N (m/s).
#
# Returns:
#   Wind direction (deg).
#
# Author(s): Dieter Scherer, Marco Otto, Ute Fehrenbach
# Date:      29.08.2019
#**************************************************************
                 
met_wdir <- function(u, v) {
  
  wdir <- (360+180+180/pi*atan2(u,v)) %% 360
  
  attr(wdir, "units") <- "deg"
  
  return(wdir)
}

#**************************************************************
# The function computes the declination of the sun.
#
# Args:
#   t: POSIXt time in CET. 
#
# Returns:
#   Declination of the sun at given time (deg). Positive values
#   are northern latitudes, while negative values indicate southern 
#   latitudes.
#
# Author(s): Dieter Scherer, Marco Otto, Ute Fehrenbach
# Date:      29.08.2019
#**************************************************************

met_sun_declin <- function(t) {

  t_ref <- as.POSIXct("1981-01-01 00:00") # UTC
  
  w <- 1.7202689e-2
  a <- c(23.265, 0.381, 0.171) * pi/180
  b <- c(-1.3865, -1.433, -1.0336)
  
  days <- as.double(t-t_ref, units="days") - 1/24 # subtract one hour
 
  declin <- 0.38 * pi/180

  for (i in 1:3) {declin <- declin + a[i]*sin(w*(i)*days+b[i])}
  
  declin <- 180/pi*declin
  
  # Return  
  
  return(declin)
}

