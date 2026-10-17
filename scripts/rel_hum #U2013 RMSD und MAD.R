# ============================================================
# rel_hum — RMSD & Mean Absolute Deviation
#
# Vergleicht alle rel_hum-Sensoren mit der automatisch
# bestimmten Referenzvariable.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z <- c(69, 69, 69, 69)

t0 <- as.POSIXct("2026-09-01 18:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-02 04:00:00", tz = "Etc/GMT-1")

analysis_variable <- "rel_hum"

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
  stop("Es wurden weniger als zwei gueltige rel_hum-Variablen gefunden.")
}

#### Referenz bestimmen ####

reference_column <- find_best_reference(aws_joined_1h, usable_columns)

comparison_columns <- usable_columns[usable_columns != reference_column]

#### RMSD und MAD berechnen ####

results <- lapply(comparison_columns, function(col) {
  
  reference <- safe_numeric(aws_joined_1h[[reference_column]])
  
  comparison <- safe_numeric(aws_joined_1h[[col]])
  
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

write.csv(results_table, "rel_hum_RMSD_MAD.csv", row.names = FALSE)
