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
source("analysis/04_psychrometer.R")

cat("\nFertig. Ergebnisse in results/\n")
