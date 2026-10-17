# ============================================================
# Scatterplots — Lufttemperatur & relative Feuchte
#
# Vergleicht Temperatur- und Feuchtesensoren zwischen Stationen
# ueber den GESAMTEN Analysezeitraum, waehlt pro Variablengruppe
# eine Referenzvariable und erstellt Scatterplots + Regressions-
# statistik (R^2, Steigung, Offset).
#
# Kein Nachtfilter hier - der wird nur fuer die Globalstrahlung
# gebraucht (siehe pyranometer_R_sw_d.R), um dort das Sensor-
# Nullpunktsverhalten zu pruefen.
#
# Gemeinsame Funktionen liegen in AWS_scatter_helpers.R.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z        <- c(69, 69, 69, 69)

# Analysezeitraum
t0 <- as.POSIXct("2026-09-01 18:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-02 04:00:00", tz = "Etc/GMT-1")

# Nur diese Variablengruppen analysieren
analysis_variables <- c("air_temp", "rel_hum")

# PNG-Einstellungen
png_width  <- 500
png_height <- 500
png_dpi    <- 150

station_colors <- c(
  "20" = "gold", "30" = "darkgreen",
  "40" = "lightblue", "50" = "darkblue"
)

# Aggregationsintervall in Minuten (60 = stuendlich, wie im
# restlichen Projekt, z.B. wind.R)
agg_minutes <- 1

# WICHTIG: Bei diesem Testaufbau waren ALLE Temperatur-/Feuchte-
# sensoren physisch auf 1.6 m montiert. Die Spaltensuffixe "1m"
# und "3m" kommen so vom Datenlogger (Standard-Konvention fuer
# die spaeteren Messungen) und sind hier NUR Sensor-IDs, keine
# tatsaechliche Hoehe. Fuer echte Mehrhoehen-Messungen spaeter
# stimmen 1m/3m dann wieder mit der realen Hoehe ueberein.

subset_label <- paste0(
  "Fester Nachtzeitraum: ", format(t0, "%d.%m %H:%M"),
  " \u2013 ", format(t1, "%d.%m %H:%M")
)

#### Daten einlesen & zusammenfuehren ####

aws_joined_1h <- read_and_join_stations(stations, z, data_dir, t0, t1, agg_minutes)

#### Lineplots ####

for (v in analysis_variables) print(lineplot_func(aws_joined_1h, v))

#### Messabdeckung ####

coverage_table <- build_coverage_table(aws_joined_1h, analysis_variables, t0, t1)
section("MESSABDECKUNG")
print(coverage_table)
write.csv(coverage_table, "scatter_measurement_coverage.csv", row.names = FALSE)

#### Hauptanalyse ####

results <- lapply(analysis_variables, function(v) run_variable_analysis(aws_joined_1h, v, subset_label))
results <- results[!sapply(results, is.null)]

scatter_statistics <- do.call(rbind, lapply(results, function(r) r$stats_rows))
rownames(scatter_statistics) <- NULL

reference_table <- do.call(rbind, lapply(results, function(r) r$reference_row))
rownames(reference_table) <- NULL

write.csv(scatter_statistics, "scatter_statistics_air_temp_rel_hum.csv", row.names = FALSE)
write.csv(reference_table, "scatter_reference_variables.csv", row.names = FALSE)


#### Alles speichern & zippen (nur lokal in diesem Skript) ####

lineplot_dir <- "lineplots"
dir.create(lineplot_dir, showWarnings = FALSE, recursive = TRUE)

# 1) Lineplots erzeugen + speichern
for (v in analysis_variables) {
  p <- lineplot_func(aws_joined_1h, v)   # bei R_sw_d: + min_value = radiation_threshold_wm2
  if (!is.null(p)) {
    ggsave(file.path(lineplot_dir, paste0(v, "_zeitverlauf.png")),
           plot = p, width = png_width, height = png_height, units = "px", dpi = png_dpi)
  }
}

zip_filename <- paste0("alle_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")

files_to_zip <- c(
  # 2) CSV-Dateien (Statistik, Referenz, Messabdeckung)
  list.files(".", pattern = "^scatter_.*\\.csv$", full.names = TRUE),
  # 3) die gerade gespeicherten Lineplots
  list.files(lineplot_dir, pattern = "\\.png$", full.names = TRUE),
  # 4) die Scatterplots, die run_variable_analysis() weiter oben
  #    im Skript schon automatisch in scatter_<variable>/ gespeichert hat
  unlist(lapply(list.dirs(".", recursive = FALSE)[grepl("^\\./scatter_", list.dirs(".", recursive = FALSE))],
                list.files, pattern = "\\.png$", full.names = TRUE))
)

if (!requireNamespace("zip", quietly = TRUE)) install.packages("zip")
zip::zipr(zip_filename, files_to_zip[file.exists(files_to_zip)])
cat("Gespeichert:", zip_filename, "\n")