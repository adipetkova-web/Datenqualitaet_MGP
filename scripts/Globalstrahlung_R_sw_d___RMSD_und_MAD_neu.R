# ============================================================
# Globalstrahlung (R_sw_d) — RMSD & Mean Absolute Deviation
#
# Vergleicht alle Globalstrahlungssensoren mit der automatisch
# bestimmten Referenzvariable.
#
# Zeitraum und Tag/Nacht-Schwelle identisch zu
# pyranometer_R_sw_d.R (15:00-18:00, Tag = Station >= 10 W/m2),
# damit RMSD/MAD auf denselben Daten wie die Scatterplot-
# Regression basieren.
#
# Die Berechnung erfolgt nur fuer Tageswerte.
# Tag = mindestens eine Station >= radiation_threshold_wm2.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z <- c(69, 69, 69, 69)

t0 <- as.POSIXct("2026-09-01 15:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-01 18:00:00", tz = "Etc/GMT-1")

analysis_variable <- "R_sw_d"

radiation_threshold_wm2 <- 10

# 1-Minuten-Aufloesung (kein echtes Aggregieren)
agg_minutes <- 1

#### Daten einlesen ####

aws_joined_1h <- read_and_join_stations(stations, z, data_dir, t0, t1, agg_minutes)

#### Tagesmaske bestimmen ####

radiation_columns <- get_measurement_columns(aws_joined_1h, analysis_variable)

night_mask <- compute_night_mask(aws_joined_1h, radiation_columns, radiation_threshold_wm2)

day_mask <- !night_mask

if (all(is.na(day_mask))) {
  stop("Tagesmaske konnte nicht bestimmt werden.")
}

day_data <- aws_joined_1h[
  which(day_mask),
  ,
  drop = FALSE
]

#### Variablen bestimmen ####

measurement_columns <- get_measurement_columns(day_data, analysis_variable)

usable_columns <- measurement_columns[
  sapply(measurement_columns, function(col) {
    sum(is.finite(safe_numeric(day_data[[col]]))) > 0
  })
]

if (length(usable_columns) < 2) {
  stop("Es wurden weniger als zwei gueltige Globalstrahlungsvariablen gefunden.")
}

#### Referenz bestimmen ####

reference_column <- find_best_reference(day_data, usable_columns)

comparison_columns <- usable_columns[usable_columns != reference_column]

#### RMSD und MAD berechnen (inkl. Rohdaten fuer Boxplots) ####

results <- lapply(comparison_columns, function(col) {
  
  reference  <- safe_numeric(day_data[[reference_column]])
  comparison <- safe_numeric(day_data[[col]])
  timestamps <- day_data$t
  
  valid <- is.finite(reference) & is.finite(comparison)
  
  reference  <- reference[valid]
  comparison <- comparison[valid]
  timestamps <- timestamps[valid]
  
  deviation <- comparison - reference
  
  # Relative Abweichung in % des Referenzwerts - Pyranometer-Datenblatt
  # gibt die Genauigkeit als +/-5% an (nicht als fester W/m2-Wert), daher
  # brauchen wir diese Spalte fuer den Boxplot-Toleranzvergleich.
  percent_deviation <- deviation / reference * 100
  
  regression <- calculate_regression(day_data, reference_column, col)
  
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
    percent_deviation = percent_deviation,
    stringsAsFactors = FALSE
  )
  
  list(summary = summary_row, deviations = deviation_rows)
})

results_table <- do.call(rbind, lapply(results, function(r) r$summary))
rownames(results_table) <- NULL

deviations_table <- do.call(rbind, lapply(results, function(r) r$deviations))
rownames(deviations_table) <- NULL

#### CSV speichern ####

write.csv(results_table, "R_sw_d_RMSD_MAD.csv", row.names = FALSE)
write.csv(deviations_table, "R_sw_d_deviations.csv", row.names = FALSE)