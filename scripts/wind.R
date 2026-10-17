#########
# Setup #
#########

# make avaiable all required libraries
library("ggplot2")
library("reshape2")
library("dplyr")
library("purrr")
library("tidyr")
library("Cairo")
library("corrplot")

# source all required scripts
source("scripts/AWS/AWS_diagnostics.R")
source("scripts/AWS/AWS_read.R")
source("scripts/AWS/AWS_tools.R")
source("scripts/AWS/met.R")

# this is my user-defined script
source("scripts/plots_example.R")

data_dir = "data_2026"
stations <- c("20","30","40","50")
z <- c(69, 69, 69, 69) # (m asl.)

# Define first and last time to be read
t0 <- as.POSIXct("2026-09-01 11:00:00", tz="Etc/GMT-1") # UTC oder UTC+1?
t1 <- as.POSIXct("2026-09-02 07:00:00", tz="Etc/GMT-1") # end time for 2026

station_meta <- data.frame(
  station = stations,
  z = z,
  dz = z - min(z)
)

paths    <- paste(data_dir, paste0("AWS", stations), sep="/")

#############
# Read data #
#############

# Read AWS data from files 
aws20 <- AWS_read_data(paths[1], t0=t0, t1=t1, dz=station_meta$dz[station_meta$station == "20"])
aws30 <- AWS_read_data(paths[2], t0=t0, t1=t1, dz=station_meta$dz[station_meta$station == "30"])
aws40 <- AWS_read_data(paths[3], t0=t0, t1=t1, dz=station_meta$dz[station_meta$station == "40"])
aws50 <- AWS_read_data(paths[4], t0=t0, t1=t1, dz=station_meta$dz[station_meta$station == "50"])

# Aggregate
aws20_1h = agg_df(aws20, 60)
aws30_1h = agg_df(aws30, 60)
aws40_1h = agg_df(aws40, 60)
aws50_1h = agg_df(aws50, 60)

# Join into one data frame
aws_joined_1h = merge_dfs_with_suffix(list(aws20_1h, aws30_1h, aws40_1h, aws50_1h), stations)

#######
#wind
########

wind_base_names <- c(
  "u", "v", "w",             # wind components
  "wspeed", "wdir",          # wind speed and direction
  "sigma_wdir",              # std. dev. of wind direction
  "cov_uu", "cov_vv", "cov_ww",   # variances of wind components
  "cov_uv", "cov_uw", "cov_vw"   # covariances between wind components
)

wind_vars <- aws_joined_1h %>%
  select(t, matches(paste0("^(", paste(wind_base_names, collapse = "|"), ")_\\d+$")))


#sensor accuracy from spec
spec_speed_pct <- 0.015   # 1.5% RMS of reading
spec_dir_deg   <- 2       # 2 degrees

# circular difference in degrees, wrapped to [-180, 180]
circ_diff <- function(a, b) (a - b + 180) %% 360 - 180

station_pairs <- combn(stations, 2, simplify = FALSE)

check_variable <- function(df, var, pairs, accuracy_fun, diff_fun = `-`) {
  map_dfr(pairs, function(p) {
    s1 <- p[1]; s2 <- p[2]
    col1 <- paste0(var, "_", s1)
    col2 <- paste0(var, "_", s2)
    tibble(
      t = df$t,
      station_1 = s1,
      station_2 = s2,
      val_1 = df[[col1]],
      val_2 = df[[col2]],
      diff = diff_fun(df[[col1]], df[[col2]]),
      accuracy_1 = accuracy_fun(df[[col1]]),
      accuracy_2 = accuracy_fun(df[[col2]]),
      combined_accuracy = sqrt(accuracy_1^2 + accuracy_2^2),
      within_accuracy = abs(diff) <= combined_accuracy
    )
  })
}

# wind speed: accuracy scales with the reading (1.5% RMS)
speed_check <- check_variable(
  wind_vars, "wspeed", station_pairs,
  accuracy_fun = function(x) spec_speed_pct * x
)

# wind direction: fixed accuracy, circular difference
dir_check <- check_variable(
  wind_vars, "wdir", station_pairs,
  accuracy_fun = function(x) spec_dir_deg,
  diff_fun = circ_diff
)

# --- Summary per station pair ---
summary_speed <- speed_check %>%
  group_by(station_1, station_2) %>%
  summarise(
    n = sum(!is.na(diff)),
    pct_within_accuracy = mean(within_accuracy, na.rm = TRUE) * 100,
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    .groups = "drop"
  )

summary_dir <- dir_check %>%
  group_by(station_1, station_2) %>%
  summarise(
    n = sum(!is.na(diff)),
    pct_within_accuracy = mean(within_accuracy, na.rm = TRUE) * 100,
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    .groups = "drop"
  )

summary_speed
summary_dir


# Does one station disagree with everyone, or is it symmetric noise?
station_offenders <- function(check_df) {
  check_df %>%
    pivot_longer(c(station_1, station_2), names_to = "role", values_to = "station") %>%
    group_by(station) %>%
    summarise(
      pct_within_accuracy = mean(within_accuracy, na.rm = TRUE) * 100,
      mean_abs_diff_involving = mean(abs(diff), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(pct_within_accuracy)
}

station_offenders(speed_check)
station_offenders(dir_check)

#########wind speed mean and SD
wspeed_summary <- wind_vars %>%
  select(t, starts_with("wspeed_")) %>%
  pivot_longer(-t, names_to = "station", values_to = "wspeed") %>%
  mutate(station = sub("wspeed_", "", station)) %>%
  group_by(station) %>%
  summarise(
    mean = mean(wspeed, na.rm = TRUE),
    sd   = sd(wspeed, na.rm = TRUE),
    n    = sum(!is.na(wspeed))
  )

wspeed_summary

########wind direction mean and SD
library(circular)

wdir_summary <- wind_vars %>%
  select(t, starts_with("wdir_")) %>%
  pivot_longer(-t, names_to = "station", values_to = "wdir") %>%
  mutate(station = sub("wdir_", "", station)) %>%
  group_by(station) %>%
  summarise(
    circ_mean = as.numeric(mean.circular(circular(wdir, units = "degrees", template = "geographics"), na.rm = TRUE)),
    circ_sd   = as.numeric(sd.circular(circular(wdir, units = "degrees", template = "geographics"), na.rm = TRUE)),
    n = sum(!is.na(wdir))
  )

wdir_summary
