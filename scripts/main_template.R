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

data_dir = "data_2026"                                                 #### ADAPT! (to "data")
stations <- c("20","30","40","50")
z <- c(20, 30, 40, 50) # (m asl.)                                 #### ADAPT! (station heights above sea level)

# Define first and last time to be read
t0 <- as.POSIXct("2026-09-01 12:00:00", tz="UTC")       #### ADAPT! (start time of analysis)
t1 <- as.POSIXct("2026-09-02 12:00:00", tz="UTC")       #### ADAPT! (end time of analysis)

station_meta <- data.frame(
  station = stations,
  z = z,
  dz = z - min(z)
)

paths    <- paste(data_dir, paste0("AWS", stations), sep="/")

#############
# Read data #
#############

aws20 <- AWS_read_data(paths[1], t0=t0, t1=t1, dz=station_meta$dz[station_meta$station == "20"])

#### Continue...
