# ============================================================
# Nettostrahlung (R_net) — RMSD & Mean Absolute Deviation
#
# Vergleicht alle Nettostrahlungssensoren mit der automatisch
# bestimmten Referenzvariable.
#
# Zeitraum identisch zu net_radiation.R (15:00-07:00, voller
# Zeitraum Tag + Nacht), damit RMSD/MAD auf denselben Daten wie
# die Scatterplot-Regression basieren.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z <- c(69, 69, 69, 69)

t0 <- as.POSIXct("2026-09-01 15:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-02 07:00:00", tz = "Etc/GMT-1")

analysis_variable <- "R_net"

# 1-Minuten-Aufloesung (kein echtes Aggregieren)
agg_minutes <- 1

#### Daten einlesen ####

aws_joined_1h <- read_and_join_stations(stations, z, data_dir, t0, t1, agg_minutes)

#### Variablen bestimmen ####

measurement_columns <- get_measurement_columns(aws_joined_1h, analysis_variable)

usable_columns <- measurement_columns[
  sapply(measurement_columns, function(col) {
    sum(is.finite(safe_numeric(aws_joined_1h[[col]]))) > 0
  })
]

if (length(usable_columns) < 2) {
  stop("Es wurden weniger als zwei gueltige R_net-Variablen gefunden.")
}

#### Referenz bestimmen ####

reference_column <- find_best_reference(aws_joined_1h, usable_columns)

comparison_columns <- usable_columns[usable_columns != reference_column]

#### RMSD und MAD berechnen (inkl. Rohdaten fuer Boxplots) ####

results <- lapply(comparison_columns, function(col) {
  
  reference  <- safe_numeric(aws_joined_1h[[reference_column]])
  comparison <- safe_numeric(aws_joined_1h[[col]])
  timestamps <- aws_joined_1h$t
  
  valid <- is.finite(reference) & is.finite(comparison)
  
  reference  <- reference[valid]
  comparison <- comparison[valid]
  timestamps <- timestamps[valid]
  
  deviation <- comparison - reference
  
  regression <- calculate_regression(aws_joined_1h, reference_column, col)
  
  summary_row <- data.frame(
    variable_group = analysis_variable,
    reference_variable = reference_column,
    comparison_variable = col,
    n_pairs = length(deviation),
    RMSD = sqrt(mean(deviation^2)),
    MAD = mean(abs(deviation)),
    r_squared = regression$r_squared,
    slope = regression$slope,
    offset = regression$offset,
    residual_mean = regression$residual_mean,
    residual_sd = regression$residual_sd,
    equation = make_equation(regression$slope, regression$offset),
    stringsAsFactors = FALSE
  )
  
  deviation_rows <- data.frame(
    variable_group = analysis_variable,
    reference_variable = reference_column,
    comparison_variable = col,
    t = timestamps,
    deviation = deviation,
    stringsAsFactors = FALSE
  )
  
  list(summary = summary_row, deviations = deviation_rows)
})

results_table <- do.call(rbind, lapply(results, function(r) r$summary))
rownames(results_table) <- NULL

deviations_table <- do.call(rbind, lapply(results, function(r) r$deviations))
rownames(deviations_table) <- NULL

#### CSV speichern ####

write.csv(results_table, "R_net_RMSD_MAD.csv", row.names = FALSE)
write.csv(deviations_table, "R_net_deviations.csv", row.names = FALSE)