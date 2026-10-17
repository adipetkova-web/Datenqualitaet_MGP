#******************************************************************************
# The file AWS_diagnostics.R contains an R script for computing
# diagnostic variables from data read from AWS files. It uses R
# functions defined in the separate files AWS_tools.R and met.R
# (see documentation in these files).
#
# Author(s): Dieter Scherer, Ute Fehrenbach
# Date:      31.08.2019
#*******************************************************************************

# Compute air pressure air_press for each station using air_press in aws50 as reference (not used in 2019)

# aws20$air_press <- met_air_press(air_press_ref=aws50$air_press, z_ref=z["50"], virt_temp_ref=aws50$air_temp_3m, z=z["20"], virt_temp=aws20$air_temp_3m)
# aws20   <- AWS_set_units(aws20, units="hPa", columns="air_press") # set units
# 
# aws30$air_press <- met_air_press(air_press_ref=aws50$air_press, z_ref=z["50"], virt_temp_ref=aws50$air_temp_3m, z=z["30"], virt_temp=aws30$air_temp_3m)
# aws30   <- AWS_set_units(aws30, units="hPa", columns="air_press") # set units
# 
# aws40$air_press <- met_air_press(air_press_ref=aws50$air_press, z_ref=z["50"], virt_temp_ref=aws50$air_temp_3m, z=z["40"], virt_temp=aws40$air_temp_3m)
# aws40   <- AWS_set_units(aws40, units="hPa", columns="air_press") # set units


calc_diagnostics <- function(df, dz=0) {
  # Compute saturation vapor pressure sat_press and vapor pressure vap_press
  
  df$sat_press_1m <- met_vap_press(air_temp=df$air_temp_1m)
  df$sat_press_3m <- met_vap_press(air_temp=df$air_temp_3m)
  df$vap_press_1m <- met_vap_press(air_temp=df$air_temp_1m, rel_hum=df$rel_hum_1m)
  df$vap_press_3m <- met_vap_press(air_temp=df$air_temp_3m, rel_hum=df$rel_hum_3m)
  df      <- AWS_set_units(df, units=c("hPa","hPa","hPa","hPa"),
                              columns=c("sat_press_1m","sat_press_3m","vap_press_1m","vap_press_3m")) # set units
  
  # Compute absolute and specific humidity abs_hum and spec_hum, and mixing ratio mixing_ratio
  
  df$abs_hum_1m   <- met_abs_hum(air_temp=df$air_temp_1m, vap_press=df$vap_press_1m)
  df$spec_hum_1m   <- met_spec_hum(vap_press=df$vap_press_1m, air_press=df$air_press)
  df$mixing_ratio_1m <- met_spec_hum(vap_press=df$vap_press_1m, air_press=df$air_press)
  df$abs_hum_3m   <- met_abs_hum(air_temp=df$air_temp_3m, vap_press=df$vap_press_3m)
  df$spec_hum_3m   <- met_spec_hum(vap_press=df$vap_press_3m, air_press=df$air_press)
  df$mixing_ratio_3m <- met_mixing_ratio(vap_press=df$vap_press_3m, air_press=df$air_press)
  df        <- AWS_set_units(df, units=c("g/m3","g/kg","g/kg","g/m3","g/kg","g/kg"),
                                columns=c("abs_hum_1m","spec_hum_1m","mixing_ratio_1m","abs_hum_3m","spec_hum_3m","mixing_ratio_3m"))    # set units
  
  # Compute potential and virtual temperature
  
  df$pot_temp_1m <- met_pot_temp(air_temp=df$air_temp_1m, dz=dz+1)
  df$pot_temp_3m <- met_pot_temp(air_temp=df$air_temp_3m, dz=dz+3) 
  df$virt_temp_1m <- met_virt_temp(air_temp=df$air_temp_1m, mixing_ratio=df$mixing_ratio_1m)
  df$virt_temp_3m <- met_virt_temp(air_temp=df$air_temp_3m, mixing_ratio=df$mixing_ratio_3m) 
  df        <- AWS_set_units(df, units=c("deg C","deg C","deg C","deg C"),
                                columns=c("pot_temp_1m","pot_temp_3m","virt_temp_1m","virt_temp_3m"))  # set units
  
  # Compute vertical gradients (to be completed)
  
  df$dpot_temp_dz <- (df$pot_temp_3m - df$pot_temp_1m) / (3-1) # (K/m)
  df$dspec_hum_dz   <- (df$spec_hum_3m - df$spec_hum_1m) / (3-1)     # (g/(kg m))
  df         <- AWS_set_units(df, units=c("K/m","g/(kg m)"), columns=c("dpot_temp_dz","dspec_hum_dz")) # set units
  
  return(df)
}


calc_turb_fluxes <- function(df) {
  df$air_dens <- met_air_dens(air_temp=df$air_temp_3m, air_press=df$air_press)  # air density
  df$QH <- df$air_dens * met_const("cp_a") * df$cov_wT
  df$BR <- met_const("cp_a") * df$dpot_temp_dz / (met_const("lh_v") * df$dspec_hum_dz / 1000)
  df$QE <- df$QH / df$BR
  df$tke <- (df$cov_uu + df$cov_vv + df$cov_ww) / 2
  
  df <- AWS_set_units(df, units=c(QH = "W/m2", QE = "W/m2", BR = "1", tke = "m/s"))
  
  return(df)
}

agg_df <- function(df, interval_minutes) {
  cov_cols = c("cov_uu", "cov_uv", "cov_uw", "cov_uT",
               "cov_vv", "cov_vw", "cov_vT", "cov_ww",
               "cov_wT",  "cov_TT")
  normal_agg_cols = setdiff(colnames(df), cov_cols)
  df_agg    <- AWS_aggregate(df, columns = normal_agg_cols, method="mean", interval_minutes = interval_minutes)
  df_agg$prcp  <- df_agg$prcp*interval_minutes  # overwrite wrongly aggregated prcp with corrected values
  df_agg$wdir <- met_wdir(df_agg$u, df_agg$v) # overwrite wrongly aggregated wdir with corrected values
  
  for (cov_col in cov_cols) {
    vars <- strsplit(sub("cov\\_", "", cov_col), "")[[1]]
    vars <- ifelse(vars == "T", "virt_ac_temp", vars)
    cov_of_means <- AWS_aggregate(df, columns = vars, method = "cov", interval_minutes = interval_minutes)
    mean_of_covs <- AWS_aggregate(df, columns = cov_col, method = "mean", interval_minutes = interval_minutes)

    # Total covariance = covariance of means + mean of covariances
    df_agg[[cov_col]] <- cov_of_means$cov + mean_of_covs[[cov_col]]
    cov_unit = AWS_get_units(df, columns = cov_col)
    df_agg <- AWS_set_units(df_agg, cov_unit, columns = cov_col)
  }
  
  #***************************
  # Calculate turbulent fluxes
  #***************************
  
  df_agg <- calc_turb_fluxes(df_agg)
  
  return(df_agg)
}

# Example for computing statistics for wind sectors

# aws20       <- AWS_wind_sectors(aws20) # compute sector numbers
# air_temp_3m.sector <- vector(mode="double", length=12)
# for (i in 1:12) {air_temp_3m.sector[i] <- mean(aws20$air_temp_3m[aws20$sector == i], na.rm=TRUE)}
# plot(air_temp_3m.sector)







