#**************************************************************
# The file AWS_read_func.R contains a collection of R functions
# for reading data from AWS files into data frames.
#
# Author(s): Dieter Scherer, Ute Fehrenbach, Marco Otto, Fred Meier
# Date:      02.09.2022
#**************************************************************

#**************************************************************
# The function reads data from a single AWS file into a data
# frame.
#
# Args:
#   file: AWS file name incl. path
#   t0:   POSIXct start time. Default: first timestamp in file.
#   t1:   POSIXct end time. Default: last timestamp in file. 
#
# Returns:
#   A data frame containing the data read from the file.
#   Timestamps are converted to POSIXct and stored in column t.
#   The attribute "units" is added to the data frame containing
#   a named character vector with units for each column.
#   In case of errors NULL is returned!
#
# Author(s): Dieter Scherer, Ute Fehrenbach, Fred Meier
# Date:      02.09.2022
#**************************************************************

AWS_read_file <- function(file, t0=NULL, t1=NULL) {

  #*************************************
  # Check if file exists and can be read
  #*************************************

  if (file.access(file, mode=4) != 0) {return(NULL)} # not a readable file  
  
  #*************************************
  # Read data from file into a dataframe 
  #*************************************
  
  # Read names of variables from file (2nd row)
  
  varnames <- scan(file, skip=1, nlines=1, sep=',', what=character(), quiet=TRUE)

  # Rename varnames
  
  varnames[varnames == "t"]             <- "virt_ac_temp"
  varnames[varnames == "uu"]            <- "cov_uu"
  varnames[varnames == "uv"]            <- "cov_uv"
  varnames[varnames == "uw"]            <- "cov_uw"
  varnames[varnames == "ut"]            <- "cov_uT"
  varnames[varnames == "vv"]            <- "cov_vv"
  varnames[varnames == "vw"]            <- "cov_vw"
  varnames[varnames == "vt"]            <- "cov_vT"
  varnames[varnames == "ww"]            <- "cov_ww"
  varnames[varnames == "wt"]            <- "cov_wT"
  varnames[varnames == "tt"]            <- "cov_TT"
  varnames[varnames == "Samples_Sonic"] <- "N_smpl"
  varnames[varnames == "Control_flag"]  <- "flag"
  varnames[varnames == "Temp_t"]        <- "air_temp_3m"
  varnames[varnames == "Temp_b"]        <- "air_temp_1m"
  varnames[varnames == "RH_t"]          <- "rel_hum_3m"
  varnames[varnames == "RH_b"]          <- "rel_hum_1m"
  varnames[varnames == "Prcp"]          <- "prcp"
  varnames[varnames == "p"]          <- "air_press"
  varnames[varnames == "ShortIn"]       <- "R_sw_d"
  varnames[varnames == "NetRad"]        <- "R_net"
  varnames[varnames == "wd"]            <- "wdir"
  varnames[varnames == "ws"]            <- "wspeed"
  varnames[varnames == "sigma_wd"]      <- "sigma_wdir"
  varnames[varnames == "TIMESTAMP"]     <- "t"        # must be the last 
  
  # Read data from file (5th row to the end)
  
  aws <- read.table(file, header=FALSE, col.names=varnames,na.strings="NAN",
                    skip=4, sep=",", stringsAsFactors=FALSE) # skip over header lines
  aws$RECORD <- NULL
  
  # Read units of variables from file (3rd row)
  
  varunits <- scan(file, skip=2, nlines=1, sep=',', what=character(), quiet=TRUE)

  # Rename varunits

  varunits[varunits == "TS"]      <- ""
  varunits[varunits == "RN"]      <- ""
  varunits[varunits == "Deg C"]   <- "deg C"
  varunits[varunits == "m^2/s^2"] <- "m2/s2"
  varunits[varunits == "K^2"]     <- "K2"
  varunits[varunits == "samples"] <- ""
  varunits[varunits == "Wm^-2"]   <- "W/m2"
  varunits[varunits == "Deg"]     <- "deg"
  
  names(varunits) <- varnames # give units the names of the corresponding variables

  #***************************************************************
  # Convert date and time into POSIXct timestamps, define timezone
  #***************************************************************
  
  aws$t <- as.POSIXct(aws$t, tz="Etc/GMT-1") # POSIXct time stamps
  aws <- aws[!duplicated(aws$t), ]  # Remove duplicates

  #colnames(aws)[colnames(aws)=="TIMESTAMP"] <- "t" # rename column TIMESTAMP to t  
  
  #***************************************
  # Select data between start and end time
  #***************************************
  
  N <- length(aws$t) # number of observations
  
  if (is.null(t0)) {t0 <- aws$t[1]}
  if (is.null(t1)) {t1 <- aws$t[N]}

  aws <- AWS_subset(aws, t0 = t0, t1 = t1)

  N <- length(aws$t) # number of observations in subset

  #*************************
  # Make time series regular
  #*************************
  
  dt <- aws$t[2] -aws$t[1]
  
  t_0 <- t0
  t_1 <- t1
  
  t   <- seq.POSIXt(from=t_0, to=t_1, by=dt) # new timestamps
  N   <- length(t)
  df  <- data.frame(t)
  
  aws <- merge.data.frame(df, aws, by="t", all=TRUE)
  
  rownames(aws) <- seq_len(nrow(aws))  # row numbering starts from 1 
  
  #***************************************
  # Store units as attribute of data frame
  #***************************************
  aws <- AWS_set_units(aws, varunits)
  
  return(aws)
}

#**************************************************************
# The function reads data from the AWS files of a station into
# a merged data frame.
#
# Args:
#   path: Path to the files of a station up to the point where their names become
#         "Control", "Fluxes", etc. E.g., "/home/user/AWS20/ABBA_20_"
#         for files like "/home/user/AWS20/ABBA_20_Control.dat"
#   t0:   POSIXct start time. Default: first timestamp in files.
#   t1:   POSIXct end time. Default: last timestamp in files. 
#   dz:   Height difference of the station to the reference (for pot_temp)
#
# Returns:
#   A data frame aws containing the data read from the files.
#   The attribute units is added to the data frame containing
#   a named character vector with units for each column.
#
# Author(s): Dieter Scherer, Ute Fehrenbach, Marco Otto
# Date:      28.08.2019
#**************************************************************

AWS_read_data <- function(path, t0=NULL, t1=NULL, dz=0) {
  if (attr(t0, "tzone") == "") stop("Time zone must be set")
  if (attr(t1, "tzone") == "") stop("Time zone must be set")
  
  patterns = c("Control", "Fluxes", "Meteo", "Wind")
  files = c()
  for (pattern in patterns) {
    
    file <- list.files(path, pattern = pattern, full.names = TRUE)
    if (length(file) != 1) {
      stop("Expected exactly one matching file for ", pattern, ", but found ", length(files))
    }
    if (!file.exists(file)) {
      stop("File doesn't exist: ", file)
    }
    files <- c(files, file)
  }

  ctl <- AWS_read_file(files[1], t0, t1)
  flx <- AWS_read_file(files[2], t0, t1)
  met <- AWS_read_file(files[3], t0, t1)
  wnd <- AWS_read_file(files[4], t0, t1)
  if (is.null(ctl)) stop(paste0("Could not read Control"))
  if (is.null(flx)) stop(paste0("Could not read Fluxes"))
  if (is.null(met)) stop(paste0("Could not read Meteo"))
  if (is.null(wnd)) stop(paste0("Could not read Wind"))
  
  #*************************************************
  # Interpolate control variables to minutely values
  #*************************************************
  
  ctl <- AWS_interpolate(ctl, t0=t0, t1=t1, dt="min")

  #*******************************************
  # Merge data frames into a single data frame
  #*******************************************

  # Time information taken from control data
  
  N  <- length(ctl$t)
  t0 <- ctl$t[1]
  t1 <- ctl$t[N]
  
  # Merge data frames
  
  aws <- merge.data.frame(ctl, flx, by="t", all=TRUE) # merge ctl and flx
  aws <- merge.data.frame(aws, met, by="t", all=TRUE) # append met to aws
  aws <- merge.data.frame(aws, wnd, by="t", all=TRUE) # append wnd to aws
  
  #************
  # Reset units
  #************
  
  aws = AWS_set_units(aws, AWS_get_units(ctl))
  aws = AWS_set_units(aws, AWS_get_units(flx))
  aws = AWS_set_units(aws, AWS_get_units(met))
  aws = AWS_set_units(aws, AWS_get_units(wnd))
  
  #**********************
  # Calculate diagnostics
  #**********************
  
  aws <- calc_diagnostics(aws, dz=dz)
  
  #*************************
  # Return merged data frame
  #*************************

  return(aws)
}

#**************************************************************
# The function reads time series data from a simple csv file
# into a data frame, i.e., the first line contains the column
# names, while data start at line 2. One column must have the
# name TIMESTAMP.
#
# Units must be set separately, since simple csv files don't 
# contain them!
#
# Args:
#   file: File name including path.
#
# Returns:
#   A data frame containing the data read from the file. The
#   timestamps are converted to POSIXct and stored in column t.
#
# Author(s): Dieter Scherer
# Date:      09.08.2019
#**************************************************************

AWS_read_csv <- function(file) {

  #*************************************
  # Check if file exists and can be read
  #*************************************
  
  if (file.access(file, mode=4) != 0) {return(NULL)} # not a readable file  
  
  #********************
  # Read data from file
  #********************
  
  data <- read.table(file, header=TRUE, sep=",", stringsAsFactors=FALSE)

  fmt = c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%d.%m.%Y %H:%M:%S",
          "%Y-%m-%d %H:%M",    "%Y/%m/%d %H:%M",    "%d.%m.%Y %H:%M",
          "%Y-%m-%d",          "%Y/%m/%d",          "%d.%m.%Y")

  #*******************
  # Convert timestamps
  #*******************
  
  data$t <- as.POSIXct(data$TIMESTAMP, tz="Etc/GMT-1", tryFormats=fmt) 

  data$TIMESTAMP <- NULL # remove from data frame
  
  #**********************************************
  # Reorder columns (t shall be the first column)
  #**********************************************
  
  columns <- colnames(data)
  
  columns <- columns[which(columns != "t")]
  
  data <- data[,c("t", columns)] # reorder columns (t first)
  
  #************
  # Return data
  #************
  
  return(data)
}
