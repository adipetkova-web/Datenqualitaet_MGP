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
library("lubridate")

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


###########pyranometer####

daily_rad <- aws_joined_1h %>%
  select(t, starts_with("R_sw_d_")) 
