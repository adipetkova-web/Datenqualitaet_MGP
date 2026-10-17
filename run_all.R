# ============================================================
# run_all.R — komplette Auswertung in einem Durchlauf
#
# In RStudio: Projekt "Datenqualitaet_MGP.Rproj" oeffnen, dann
#   source("run_all.R")
# oder im Terminal (im Projektordner):
#   Rscript run_all.R
#
# Erzeugt alle Tabellen in results/tables/ und alle Abbildungen
# in results/figures/. Einstellungen: R/config.R
# ============================================================

source("analysis/01_sensorvergleich.R")
source("analysis/02_abbildungen.R")
source("analysis/03_gradientenfehler.R")
# Psychrometer-Vergleich nur, wenn die Feld-Daten vorhanden sind
if (all(dir.exists(file.path(DATA_DIR, paste0("AWS", STATIONS, FIELD_SUFFIX))))) {
  source("analysis/04_psychrometer.R")
} else {
  message("Feld-Daten (data/raw/AWS<nr>", FIELD_SUFFIX, ") fehlen - Psychrometer-Vergleich uebersprungen.")
}

cat("\nFertig. Ergebnisse in results/\n")
