# ============================================================
# Globalstrahlung (R_sw_d) — RMSD & Mean Absolute Deviation
#
# Vergleicht alle Globalstrahlungssensoren mit der automatisch
# bestimmten Referenzvariable.
#
# Die Berechnung erfolgt nur fuer Tageswerte.
# Tag = mindestens eine Station >= 1 W/m2.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z <- c(69, 69, 69, 69)

t0 <- as.POSIXct("2026-09-01 11:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-02 07:00:00", tz = "Etc/GMT-1")

analysis_variable <- "R_sw_d"

radiation_threshold_wm2 <- 1

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

#### RMSD und MAD berechnen ####

results <- lapply(comparison_columns, function(col) {
  
  reference <- safe_numeric(day_data[[reference_column]])
  
  comparison <- safe_numeric(day_data[[col]])
  
  valid <- is.finite(reference) & is.finite(comparison)
  
  reference <- reference[valid]
  comparison <- comparison[valid]
  
  deviation <- comparison - reference
  
  data.frame(
    variable_group = analysis_variable,
    reference_variable = reference_column,
    comparison_variable = col,
    n_pairs = length(deviation),
    RMSD = sqrt(mean(deviation^2)),
    MAD = mean(abs(deviation)),
    stringsAsFactors = FALSE
  )
})

results_table <- do.call(rbind, results)
rownames(results_table) <- NULL

#### CSV speichern ####

write.csv(results_table, "R_sw_d_RMSD_MAD.csv", row.names = FALSE)
