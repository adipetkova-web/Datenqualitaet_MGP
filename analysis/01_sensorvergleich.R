# ============================================================
# 01_sensorvergleich.R — Statistik des Sensorvergleichs
#
# Fuer jede Variablengruppe (air_temp, rel_hum, R_sw_d, R_net):
#   1. Daten im festgelegten Zeitfenster einlesen (siehe R/config.R)
#   2. Referenzsensor bestimmen (Mittelwert am naechsten am Gruppenmittel)
#   3. Jeden anderen Sensor gegen die Referenz vergleichen:
#      RMSD, MAD, mittlere Abweichung, Regression (R^2, Steigung, Offset)
#
# Ausgabe (results/tables/):
#   sensorvergleich_statistik.csv   eine Zeile pro Sensor (Tabelle fuer den Bericht)
#   abweichungen_<gruppe>.csv       Abweichung pro Zeitpunkt (Grundlage der Boxplots)
#   messabdeckung.csv               Anteil gueltiger Werte pro Sensor
# ============================================================

source("R/helpers.R")

section("Sensorvergleich")

results <- lapply(names(VARIABLES), function(v) {
  cat("\n", VARIABLES[[v]]$title, " (", window_label(v), ")\n", sep = "")
  r <- compare_sensors(v)
  cat("  Referenz:", r$reference, "\n")
  r
})
names(results) <- names(VARIABLES)

stats <- bind_rows(lapply(results, `[[`, "stats"))
write_table(stats, "sensorvergleich_statistik.csv")

for (v in names(results)) {
  write_table(results[[v]]$deviations, paste0("abweichungen_", v, ".csv"))
}

write_table(bind_rows(lapply(names(VARIABLES), coverage_table)), "messabdeckung.csv")

# Kompakte Uebersicht in der Konsole
print(
  stats %>%
    transmute(Referenz = reference_variable, Sensor = comparison_variable, n = n_pairs,
              RMSD = round(RMSD, 3), MAD = round(MAD, 3), Bias = round(bias, 3),
              R2 = round(r_squared, 4), Gleichung = equation),
  row.names = FALSE
)
