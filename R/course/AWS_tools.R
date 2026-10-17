#************************************************************************
# The file AWS_tools.R contains a collection of R functions
# for managing data from AWS files read into data frames by
# AWS_read_file() or AWS_read_data().
#
# Author(s): Dieter Scherer, Ute Fehrenbach, Marco Otto, A. Holtmann
# Date:      30.08.2022
#************************************************************************

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(purrr)

#************************************************************************
# The function returns values of the units attribute from a
# AWS data frame.
#
# Args:
#   data:    Data frame containing AWS data.
#   columns: One or several column names for which units shall 
#            be returned. Default: all columns. 
#
# Returns:
#   A character vector containing the named units for each
#   column.
#
# Author(s): Dieter Scherer
# Date:      13.08.2019
#************************************************************************

AWS_get_units <- function(data, columns = NULL) {
  
  # Default: all columns
  if (is.null(columns)) {
    columns <- colnames(data)
  }
  
  units <- character(length(columns))
  for (i in seq_along(columns)) {
    if (columns[i] == "t") {
      next
    }
    u = attr(data[[columns[i]]], "units")
    if (is.null(u)) {
      u = ""
    }
    units[i] <- u
  }
  names(units) <- columns
  return(units)
}


#************************************************************************
# The function sets value(s) of the units attribute in a
# AWS data frame. Units can only be set for existing columns,
# thus, a new column must be added before its units string can
# be added.
#
# Args:
#   data:    Data frame containing AWS data.
#   units:   The units string for each column. If units is
#            a named character vector, column names are 
#            obtained from the names (the columns argument is
#            ignored).
#   columns: One or several column names for which units shall 
#            be set. Default: all columns.
#
# Returns:
#   The updated data frame.
#
# Author(s): Dieter Scherer
# Date:      13.08.2019
#************************************************************************

AWS_set_units <- function(data, units, columns=NULL) {
  
  #****************
  # Check arguments
  #****************
  
  if (! is.null(names(units))) {columns <- names(units)}

  if (is.null(columns)) {columns <- colnames(data)}

  if (length(units) != length(columns)) {stop("units and columns must have same length")}

  # Assign units attribute to each column
  for (i in seq_along(columns)) {
    if (!(columns[i] %in% colnames(data))) {
      next
    }
    attr(data[[columns[i]]], "units") <- units[i]
  }
  
  #**************************
  # Return updated data frame
  #**************************
  
  return(data)
}

#************************************************************************
# The function interpolates time series data stored in an AWS
# data frame. 
#
# Args:
#   data:    Data frame containing AWS data.
#   columns: One or several column names for which data shall 
#            be interpolated. Default: NULL (all columns).
#   t0:      POSIXct start time. Default: first timestamp in data.
#   t1:      POSIXct end time. Default: last timestamp in data.
#   dt:      A character string containing "sec", "min", "hour",
#            or "day". Default: "min".
#
# Returns:
#   A data frame containing the interpolated data.
#
# Author(s): Dieter Scherer
# Date:      06.08.2019
#************************************************************************

AWS_interpolate <- function(data, columns=NULL, t0=NULL, t1=NULL, dt="min") {

  #*******************
  # Columns specified?
  #*******************

  if (is.null(columns)) {columns <- colnames(data)}
  
  # Only data for existing columns can be interpolated
  
  idx <- columns %in% colnames(data)
  
  columns <- columns[idx]

  # Remove timestamps which serve as x-values for interpolation
  
  columns <- columns[which(columns != "t")]

  if (length(columns) == 0) {return(NULL)} # nothing to do

  #**************
  # Interpolation
  #**************
  
  # timestamps

  N <- length(data$t) # number of observations
  
  if (is.null(t0)) {t0 <- data$t[1]}
  if (is.null(t1)) {t1 <- data$t[N]}

  t_i <- as.array(data$t)
  t_o <- seq.POSIXt(from=t0, to=t1, by=dt)

  # Make new data frame for interpolated values
  
  data_ip <- data.frame(t=t_o) 
  
  # Loop over columns
  
  for (col in columns) {
    y <- as.array(data[,col])
    
    ok <- complete.cases(t_i, y)
    
    if (sum(ok) >= 2) {
      data_ip[col] <- (approx(
        x = t_i[ok],
        y = y[ok],
        xout = t_o
      ))[[2]]
    } else {
      data_ip[col] <- rep(NaN, length(t_o))
    }
  }
  
  #**************************
  # Set units and return data
  #**************************
  
  columns <- c("t", columns)
  
  units <- AWS_get_units(data, columns) 
  
  data_ip <- AWS_set_units(data_ip, units)
  
  return(data_ip)
}

#************************************************************************
# The function aggregates time series data stored in an AWS
# data frame. 
#
# Args:
#   data:    Data frame containing AWS data.
#   columns: One or several column names for which data shall 
#            be aggregated. Default: NULL (all columns).
#   method:  One or several character strings containing the
#            aggregation method, which be "mean", "median",
#            "min", "max", "sigma", "range", or "sum".
#            If more than one method is specified, then the
#            number of methods must match the number of columns.
#            There are further methods that work with two columns.
#            Method "cov" computes covariances for the two columns, while "cor",
#            compute correlation coefficients.
#            Default: "mean".
#   t0:      POSIXct start time. Default: first timestamp in data.
#   t1:      POSIXct end time. Default: last timestamp in data.
#   interval_minutes: Aggregation interval in minutes
#
# Returns:
#   A data frame containing the aggregated data.
#
# Author(s): Dieter Scherer, Robert Wallace, Marco Otto, Clara Warmuth
# Date:      30.08.2022
#************************************************************************

AWS_aggregate <- function(data, columns=NULL, method="mean", 
                          t0=NULL, t1=NULL, interval_minutes=60) {
  
  truncate_to_interval <- function(time, interval_minutes) {
    if (is.null(attr(time, "tzone"))) stop("Time zone must be set")
    tz <- attr(time, "tzone")
    origin <- as.POSIXct("1970-01-01 00:00:00", tz = tz)
    
    secs <- as.numeric(difftime(time, origin, units = "secs"))
    trunc_secs <- floor(secs / (interval_minutes * 60)) * (interval_minutes * 60)
    
    origin + trunc_secs
  }
  
  
  #*******************
  # Columns specified?
  #*******************
  
  if (is.null(columns)) {columns <- colnames(data)}
  
  # Only data for existing columns can be aggregated
  
  idx <- columns %in% colnames(data)
  
  columns <- columns[idx]
  
  # Remove timestamps since they serve as x-values for aggregation
  
  columns <- columns[columns != "t"]

  N_y <- length(columns) 
  
  if (N_y == 0) {return(NULL)} # nothing to do
  
  #**************************
  # Method(s) for aggregation
  #**************************

  double <- method[1] == "cov" | method[1] == "cor" # so far, only pearson is used

  if (double) {
    if (N_y != 2 | length(method) != 1) {return(NULL)}

  } else {

    if ((length(method) != 1) & (length(method) != N_y))
    {return(NULL)}
    
    if ((length(method) == 1) & (N_y > 1))
    {method <- rep(method, times=N_y)}
    
    names(method) <- columns # for indexing
  }

  #******************
  # Set-up timestamps
  #******************
  
  N <- length(data$t) # number of observations
  
  if (is.null(t0)) {t0 <- data$t[1]} # default: first timestamp in data
  if (is.null(t1)) {t1 <- data$t[N]} # default: last timestamp in data

  sub <- AWS_subset(data, t0 = t0, t1 = t1) # extract data within aggregation period
  
  t_i <- as.array(sub$t) # timestamps of observations within aggregation period
  N_i <- length(t_i)     # number of observations within aggregation period

  # New timestamps for aggregated data
  
  t_0 <- truncate_to_interval(t0, interval_minutes) + as.difftime(interval_minutes, units = "mins") 
  t_1 <- truncate_to_interval(t1, interval_minutes)
  
  t_o <- seq.POSIXt(from=t_0, to=t_1, by=paste(interval_minutes, "mins")) # new timestamps for end of aggregation intervals
  N_o <- length(t_o)                         # number of aggregation intervals

  N_a <- interval_minutes / as.numeric(difftime(data$t[2], data$t[1], units = "mins"))

  #************
  # Aggregation
  #************
  
  # Make new data frame for aggregated values
  
  data_agg <- data.frame(t=t_o) 
  
  if (! double) {

    #********************************************************
    # Loop over columns for single column aggregation methods
    #********************************************************

    for (col in columns) {
      y <- as.array(sub[, col]) # possibly, the original code had an error (data used instead of sub) 
      m <- method[col]
      
      agg <- array(N_o) # array for aggregated values filled with NA
      
      k <- 1 # initialize index k
      
      for (i in 1:N_o) {
        if (m == "mean")   {agg[i] <- mean(y[k:(k+N_a-1)], na.rm=TRUE) }
        if (m == "median") {agg[i] <- median(y[k:(k+N_a-1)], na.rm=TRUE) }
        if (m == "min")    {agg[i] <- min(y[k:(k+N_a-1)], na.rm=TRUE) }
        if (m == "max")    {agg[i] <- max(y[k:(k+N_a-1)], na.rm=TRUE) }
        if (m == "sigma")  {agg[i] <- sqrt(var(y[k:(k+N_a-1)], na.rm=TRUE)) }
        if (m == "range")  {agg[i] <- max(y[k:(k+N_a-1)], na.rm=TRUE)-min(y[k:(k+N_a-1)], na.rm=TRUE) }
        if (m == "sum")    {agg[i] <- sum(y[k:(k+N_a-1)], na.rm=TRUE) }
        k <- k + N_a # increase index k
      }
      
      agg[is.nan(agg)] <- NA
      
      data_agg[col] <- agg # store result in data frame
    }

  } else {

    #**********************************************
    # Compute aggregated values for the two columns
    #**********************************************

    x <- as.array(sub[, columns[1]]) 
    y <- as.array(sub[, columns[2]]) 

    agg <- array(N_o) # array for aggregated values filled with NA
    
    k <- 1 # initialize index k
    
    for (i in 1:N_o) {
      if (method == "cov") {agg[i] <- cov(x[k:(k+N_a-1)], y[k:(k+N_a-1)], use="everything")}
      if (method == "cor") {agg[i] <- cor(x[k:(k+N_a-1)], y[k:(k+N_a-1)], use="everything")}
      k <- k + N_a # increase index k
    }
    
    agg[is.nan(agg)] <- NA

    data_agg[method] <- agg # store result in data frame
  }

  #**************************
  # Set units and return data
  #**************************

  if (! double) {
    columns <- c("t", columns)
    units <- AWS_get_units(data, columns) 

  } else {

    if (method == "cov") {
      units <- AWS_get_units(data, columns) 
      units <- paste(units[1], units[2])
      names(units) <- method
    } else {
      units <- ""
    }
  }
    
  data_agg <- AWS_set_units(data_agg, units)
  
  return(data_agg)
}

#************************************************************************
# The function returns a subset from an AWS data frame 
# including the units attribute.
#
# Args:
#   data:   Data frame containing AWS data.
#   ...:    See R documentation of the subset function.
#
# Returns:
#   A subsetted data frame including the units attribute.
#
# Author(s): Dieter Scherer
# Date:      13.08.2019
#************************************************************************

AWS_subset <- function(data, t0 = NULL, t1 = NULL) {
  sub <- data
  if (!is.null(t0)) sub <- sub[sub$t >= t0, ]
  if (!is.null(t1)) sub <- sub[sub$t <= t1, ]
  
  # Restore units
  sub <- AWS_set_units(sub, units = AWS_get_units(data))
  return(sub)
}

#************************************************************************
# The function returns an AWS data frame that has been merged from two
# AWS data frames by using the timestamps t for merging including the
# units attribute.
#
# Args:
#   data1: First data frame containing AWS data.
#   data2: Second data frame to be merged with the first one.  
#
# Returns:
#   A merged data frame including the units attribute.
#
# Author(s): Dieter Scherer
# Date:      30.08.2019
#************************************************************************

AWS_merge <- function(data1, data2, columns=NULL) {

  N_col1 <- length(data1)
  N_col2 <- length(data2)

  # Merge data frames
  
  df <- merge.data.frame(data1, data2, by="t", all=TRUE)
  
  if (length(columns) == (N_col1+N_col2-2)) {colnames(df) <- c("t", columns)}  

  # Set units attribute and return result

  units1 <- AWS_get_units(data1)  
  units2 <- AWS_get_units(data2)  
  
  names(units1) <- (colnames(df))[1:N_col1]
  names(units2) <- c("",(colnames(df))[(N_col1+1):length(df)])

  df <- AWS_set_units(df, units=c(units1, units2[2:N_col2]))
  
  return(df)
}


#************************************************************************
# The function returns a merged AWS data frame including the units
# attribute in a wide format, i.e., all columns renamed by stations.
#
# Args:
#   data:    List containing at least two data frames of AWS stations.
#   station: Names of the stations. Default: indexes of data frames.
#
# Returns:
#   A merged data frame including the units attribute.
#
# Author(s): Dieter Scherer
# Date:      28.08.2019
#************************************************************************

AWS_as_wide <- function(data, stations=NULL) {

  #****************
  # Check arguments
  #****************
  
  if (is.null(data)) {return(NULL)}
  
  if (is.data.frame(data)) {return(data)} # nothing to do

  if (! is.list(data)) {return(NULL)}

  N_df <- length(data)

  if (is.null(stations)) {
    stations <- 1:N_df  
  } else {
    if (length(stations) != N_df) {return(NULL)}
  }
  
  #*******************************  
  # Make data frame in wide format
  #*******************************
 
  df_o  <- data[[1]]
  units <- AWS_get_units(df_o)
  
  colnames(df_o)[colnames(df_o) != "t"] <- 
    paste0(colnames(df_o)[colnames(df_o) != "t"], ".", stations[1])
  colnames <- colnames(df_o)

  names(units) <- colnames
  df_o <- AWS_set_units(df_o, units=units)
  
  for (i in 2:N_df) {
    df_i  <- data[[i]]
    units <- AWS_get_units(df_i)

    colnames(df_i)[colnames(df_i) != "t"] <- 
      paste0(colnames(df_i)[colnames(df_i) != "t"], ".", stations[i])
    colnames <- colnames(df_i)
    
    old_units <- AWS_get_units(df_o)
    df_o <- merge.data.frame(df_o, df_i, by="t", all=TRUE)
    df_o <- AWS_set_units(df_o, old_units)
    
    names(units) <- colnames
    df_o <- AWS_set_units(df_o, units=units)
  }
  
  #******************
  # Return data frame
  #******************
  
  return(df_o)
}

#************************************************************************
# The function returns a data frame in so called "long" format
# including the units as attribute and the names of the station to
# prepare data for ggplot2 use.
# 
# Args:
#   data:    List containing at least one data frame of an AWS station
#   station: Name(s) of the stations. Default: indexes of data frames
#
# Returns:
#   Data frame containing all data in long format (station,column,t,value,unit).
#
# Author(s): Marco Otto, Dieter Scherer
# Date:      29.08.2019
#************************************************************************

AWS_as_long <- function(data, stations=NULL) {

  #****************
  # Check arguments
  #****************
  
  if (is.null(data)) {return(NULL)}
  
  if (is.data.frame(data)) {
    data <- list(data)
    N_df <- 1
  } else {
    if (! is.list(data)) {return(NULL)}
    N_df <- length(data)
  }
  
  if (is.null(stations)) {
    stations <- 1:N_df  
  } else {
    if (length(stations) != N_df) {return(NULL)}
  }
  
  #*******************************  
  # Make data frame in long format
  #*******************************

  for (i in 1:N_df) {
    df_i  <- data[[i]]
    stat  <- stations[i]
    col   <- colnames(df_i)
    col   <- col[col != "t"]
    N_col <- length(col)
    units <- AWS_get_units(df_i)
    t     <- df_i$t
   
    if (i == 1) { 
      df_o  <- data.frame(station=factor(stat), column=factor(col[1]), t, 
                          value=df_i[,col[1]],  unit=units[col[1]], stringsAsFactors=FALSE)
    
      for (k in 2:N_col) {
        df_o <- rbind(df_o, data.frame(station=factor(stat), column=factor(col[k]), t, 
                      value=df_i[,col[k]], unit=units[col[k]], stringsAsFactors=FALSE))
      }
    } else {
      for (k in 1:N_col) {
        df_o <- rbind(df_o, data.frame(station=factor(stat), column=factor(col[k]), t, 
                        value=df_i[,col[k]], unit=units[col[k]], stringsAsFactors=FALSE))
      }
    }
  }
  
  #******************
  # Return data frame
  #******************
  
  return(df_o)
}

#************************************************************************
# The function plots time series from an AWS data frame. The 
# units attribute is used for annotation.
#
# Args:
#   data:        Data frame containing AWS data.
#   ycolumns:    One or several column names for which data shall 
#                be plotted against time (default) or other column.
#   xcolumns:    One column name to plot data against.
#   t0:          POSIXct start time. Default: first timestamp in data.
#   t1:          POSIXct end time. Default: last timestamp in data.
#   xlab:        Title of the x-axis. Default: "Time" if xcolumn is time. Else: xcolumn.
#   ylab:        Title of the y-axis. Default: NULL (title is automatically set).
#   color:       One or several character strings containing the
#                plot color (e.g. "red"). If more than one color is
#                specified, then the number of colors must match the
#                number of columns
#   linetype:    One or several line types: 
#                "blank"/0 (no line), "solid", "dashed", "dotted", "dotdash",
#                "longdash", "twodash", or integers 0–6.
#                If more than one line type is specified, then the number
#                of line types must match the number of columns.
#                Default: solid.
#   linewidth:   Linewidth!
#   symbol:      One or several point shapes:
#                "0", "1", ..., "9", "+", "x", "*", "o", "."
#                If more than one point shape is specified, then the number
#                of shapes must match the number of columns.
#                Default: no symbol.
#   symbol_size: Size of symbols
#   title:       Optional plot title
#   ylim:        Optional y-axis limits (2-element vector)
#   sunrise_sunset: Optional: c("HH:MM", "HH:MM")
#   night_shade_color: Color for shaded night period
#   legend:      If TRUE, draw legend.
#   output:      File path to save plot.
#
# Returns:
#   A `ggplot` object if `output = NULL`; otherwise, saves the plot to file and returns invisibly.
#
#************************************************************************

# symbol: "0", "1", ..., "9", "+", "x", "*", "o", "."
# linetype: "blank"/0 (no line), "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", or integers 0–6
AWS_plot <- function(data, 
                     ycolumns = NULL,
                     xcolumn = "t",
                     t0 = NULL, t1 = NULL,
                     xlab = "Time", ylab = NULL,
                     color = NULL, 
                     linetype = NULL,
                     linewidth = 0.5,
                     symbol = NULL, symbol_size = 2,
                     title = NULL,
                     ylim = NULL,
                     sunrise_sunset = NULL,
                     night_shade_color = "grey90",
                     legend = TRUE, output = NULL) {
  
  #*******************  
  # Column selection
  #******************
  if (is.null(ycolumns)) stop("Must specify ycolumns")
  ycolumns <- intersect(ycolumns, names(data))
  if (length(ycolumns) == 0) stop("No valid ycolumns selected")
  if (!(xcolumn %in% names(data))) stop("x variable not found in data")
  
  #*******************
  # Time filtering
  #*******************
  if (xcolumn == "t") {
    if (is.null(t0)) t0 <- min(data$t)
    if (is.null(t1)) t1 <- max(data$t)
    data <- AWS_subset(data, t0 = t0, t1 = t1)
  }
  
  #*******************
  # Reshape into long format
  #*******************
  df_long <- tidyr::pivot_longer(
    data,
    cols = all_of(ycolumns),
    names_to = "variable",
    values_to = "value"
  )
  
  # Attach units (if available)
  units <- AWS_get_units(data, ycolumns)
  unit_df <- data.frame(variable = ycolumns, unit = units, stringsAsFactors = FALSE)
  
  df_long <- dplyr::left_join(df_long, unit_df, by = "variable") %>%
    dplyr::mutate(variable = paste0(variable, " (", unit, ")"))
  
  var_names <- paste0(ycolumns, " (", units, ")")
  
  #*******************
  # Map colors
  #*******************
  
  color_map <- NULL
  if (!is.null(color)) {
    if (length(color) == 1L) {
      color_map <- setNames(rep(color, length(ycolumns)), var_names)
    } else if (length(color) == length(ycolumns)) {
      color_map <- setNames(color, var_names)
    } else {
      stop("Length of color must be 1 or match number of selected ycolumns")
    }
  }
  
  #*******************
  # Map symbols
  #*******************
  
  symbol_map <- NULL
  if (!is.null(symbol)) {
    if (length(symbol) == 1L) {
      symbol_map <- setNames(rep(symbol, length(ycolumns)), var_names)
    } else if (length(symbol) == length(ycolumns)) {
      symbol_map <- setNames(symbol, var_names)
    } else {
      stop("Length of symbol must be 1 or match number of selected ycolumns")
    }
  }
  #*******************
  # Map linetype
  #*******************
  
  if (is.null(linetype)) linetype <- "solid"
  lt_map <- NULL
  if (!is.null(linetype)) {
    if (length(linetype) == 1L) {
      lt_map <- setNames(rep(linetype, length(ycolumns)), var_names)
    } else if (length(linetype) == length(ycolumns)) {
      lt_map <- setNames(linetype, var_names)
    } else {
      stop("Length of linetype must be 1 or match number of selected ycolumns")
    }
  }
  all_blank <- all(lt_map %in% c("blank", "none", 0, NA))
  
  #*******************
  # Build plot
  #*******************
  plot_obj <- ggplot(df_long, aes(!!rlang::sym(xcolumn), y = value, color = variable,
                           linetype = variable, shape = variable))
  if (xcolumn == "t" && !is.null(sunrise_sunset)) {
    
    plot_start <- min(data$t)
    plot_end <- max(data$t)
    
    all_days <- seq(as.Date(plot_start), as.Date(plot_end), by = "day")
    
    # loop over days to add night shading
    for (i in seq_along(all_days)) {
      
      # sunset of previous day
      if (i == 1) {
        sunset <- plot_start
      } else {
        sunset <- as.POSIXct(paste(all_days[i-1], sunrise_sunset[2]), tz = "Etc/GMT-1")
        sunset <- pmax(sunset, plot_start)
      }
      
      # sunrise of current day
      sunrise <- as.POSIXct(paste(all_days[i], sunrise_sunset[1]), tz = "Etc/GMT-1")
      sunrise <- pmin(sunrise, plot_end)
      
      # only annotate if there is actually a night interval
      if (sunset < sunrise) {
        plot_obj <- plot_obj +
          annotate("rect", xmin = sunset, xmax = sunrise, ymin = -Inf, ymax = Inf,
                   fill = night_shade_color, color = night_shade_color)
      }
    }
    
    # Shade night after the last sunset until end of plot
    last_sunset <- as.POSIXct(paste(all_days[length(all_days)], sunrise_sunset[2]), tz = "Etc/GMT-1")
    if (last_sunset < plot_end) {
      plot_obj <- plot_obj +
        annotate("rect", xmin = last_sunset, xmax = plot_end, ymin = -Inf, ymax = Inf,
                 fill = "grey90", color = "grey90")
    }
  }
  
  
  # Add line
  if (!all_blank) {
    plot_obj <- plot_obj + geom_line(linewidth=linewidth)
  }
  
  # Add points only if symbols are provided
  if (!is.null(symbol)) {
    plot_obj <- plot_obj + geom_point(size = symbol_size)
  }
  
  if (!is.null(title)) {
    plot_obj <- plot_obj + ggtitle(title)
  }
  
  if (!is.null(ylim)) {
    plot_obj <- plot_obj + coord_cartesian(ylim = ylim)
  }
  
  # Labels and theme
  if (is.null(xlab)) xlab <- xcolumn  # default x-axis label
  plot_obj <- plot_obj + labs(
    x = xlab,
    y = if (is.null(ylab)) "" else ylab,
    color = "", shape = "", linetype = ""
  ) +
    theme_bw(base_size = 14, base_family = "sans") +
    theme(
      legend.position = if (legend) "right" else "none",
      legend.title = element_blank()
    )
  
  # Apply mappings
  if (!is.null(color_map)) plot_obj <- plot_obj + scale_color_manual(values = color_map)
  if (!is.null(symbol_map)) plot_obj <- plot_obj + scale_shape_manual(values = symbol_map)
  if (!is.null(lt_map))    plot_obj <- plot_obj + scale_linetype_manual(values = lt_map)
  
  #*******************
  # Save plot if output path is provided
  #*******************
  
  if (!is.null(output)) {
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    ext <- tools::file_ext(output)
    if (tolower(ext) == "png") {
      ggsave(filename = output, plot = plot_obj, width = 8, height = 5, type = "cairo")
    } else {
      ggsave(filename = output, plot = plot_obj, width = 8, height = 5)
    }
    invisible(plot_obj)
  } else {
    return(plot_obj)
  }
  
}




#************************************************************************
# The function uses wind direction stored in an AWS data frame to compute
# sector numbers ranging from 1 to N, where N is the number of sectors.
# Sector 1 contains North. Sectors are numbered clock-wise. Sector numbers
# are stored in an additional column in the AWS data frame.
#
# Args:
#   data: AWS data frame containing at least a column with wind directions.
#   N:    Number of wind direction sectors. Default: 12.
#
# Returns:
#   Data frame with one additional column containing the numbers of the
#   wind direction sectors.
#
# Author(s): Dieter Scherer, Ute Fehrenbach, Marco Otto
# Date:      31.08.2019
#************************************************************************

AWS_wind_sectors <- function(data, N=12) {

  #***********************
  # Compute sector numbers  
  #***********************
  
  wdir <- data$wdir # wind directions (deg)
    
  width <- 360/N # (deg)
  
  sector <- vector(mode="integer", length=length(wdir)) # vector for sector numbers
  
  sector[wdir > 360-width/2 | wdir <= width/2] <- 1 # North treated separately

  # Loop over remaining sectors
  
  for (i in 2:N) {
    start <- (i-2)*width + width/2              # lower boundary (deg) of sector 
    sector[wdir > start & wdir <= start+width] <- i # set sector number
  }

  sector[sector == 0] <- NA # set missing values to NA 

  #**************************
  # Return updated data frame
  #**************************

  data$sector <- sector
  
  data <- AWS_set_units(data, columns="sector", units="")
    
  return(data)
}


merge_dfs_with_suffix <- function(dfs, names_suffix) {
  if(length(dfs) != length(names_suffix)) {
    stop("Length of dfs and names_suffix must match")
  }
  
  # Add suffix to columns (except 't')
  dfs_tagged <- map2(dfs, names_suffix, ~ {
    df <- .x
    suffix <- .y
    df %>%
      rename_with(~ paste0(., "_", suffix), -t)
  })
  
  # Full join all by "t" and sort
  merged_df <- reduce(dfs_tagged, full_join, by = "t") %>%
    arrange(t)
  
  return(merged_df)
}

#*******************
#
#
#
#
#*******************

control_plot <- function(df, dir) {
  # Ensure dir ends with /
  if (!grepl("/$", dir)) dir <- paste0(dir, "/")
  dir.create(file.path(dir), showWarnings = FALSE)
  
  # Function to strip suffix like "_20"
  strip_suffix <- function(x) sub("_[0-9]+$", "", x)
  
  # Get unique base names (without suffix)
  base_names <- unique(strip_suffix(setdiff(names(df), "t")))
  
  # Loop over base names
  for (bn in base_names) {
    cols <- grep(paste0("^", bn, "_[0-9]+$"), names(df), value = TRUE)
    
    if (length(cols) > 0) {
      AWS_plot(
        df,
        ycolumns = cols,
        output = paste0(dir, bn, ".png")
      )
    }
  }
}

#*******************
#
# This function calculates albedo from the column R_sw_d
# assuming the pyranometer was flipped upside-down during
# the specified time period.
#
# Args:
#   df: data frame
#   t0: start time of flipped pyranometer
#   t1: end time of flipped pyranometer
#   ref_period_length: number of minutes before AND after flip period to take as swdown
#   buffer_length: Number of minutes before and after flip to not use as reference
#
#*******************

calc_albedo <- function(df, t0, t1, ref_period_length=5, buffer_length=5) {
  t0 <- as.POSIXct(t0, tz="Etc/GMT-1")
  t1 <- as.POSIXct(t1, tz="Etc/GMT-1")
  
  buf = 60 * buffer_length
  ref = 60 * ref_period_length
  
  before_flip = df[(df$t >= (t0 - buf - ref)) & (df$t < t0 - buf),]
  during_flip = df[(df$t >= t0) & (df$t < t1),]
  after_flip = df[(df$t >= t1 + buf) & (df$t < (t1 + buf + ref)),]
  flipped = mean(during_flip$R_sw_d, na.rm=TRUE)
  normal = (mean(before_flip$R_sw_d, na.rm=TRUE) + mean(after_flip$R_sw_d, na.rm=TRUE)) / 2
  return(flipped / normal)
}
