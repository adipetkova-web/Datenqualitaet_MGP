# ============================================================
# Globalstrahlung (R_sw_d) — Sensorvergleich
#
# Vergleicht die Globalstrahlungssensoren (Pyranometer) zwischen
# Stationen, beschraenkt auf die TAGESZEIT. Anders als bei
# Temperatur/Feuchte wird die Tag/Nacht-Grenze hier NICHT ueber
# eine feste Uhrzeit bestimmt, sondern dynamisch aus den
# gemessenen Globalstrahlungswerten selbst (Tag = mindestens eine
# Station >= radiation_threshold_wm2).
#
# Gemeinsame Funktionen liegen in AWS_scatter_helpers.R.
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Einstellungen ####

data_dir <- "data_2026"
stations <- c("20", "30", "40", "50")
z        <- c(69, 69, 69, 69)

# Analysezeitraum = kompletter Messzeitraum (Tag + Nacht), damit
# die Tag/Nacht-Unterscheidung ueberhaupt aus den Daten bestimmt
# werden kann.
t0 <- as.POSIXct("2026-09-01 15:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-01 18:00:00", tz = "Etc/GMT-1")

# Nur diese Variablengruppen analysieren
analysis_variables <- c("R_sw_d")

radiation_threshold_wm2 <- 10   # ab dieser Schwelle (W/m^2) gilt ein Zeitpunkt als "Tag"

# Plot-Groessen pro Typ (statt einer einzigen Groesse fuer alles -
# Einzelplot, Gruppenplot und Zeitverlauf haben unterschiedliche
# Formen und brauchen unterschiedliche Massse).
single_plot_width  <- 900
single_plot_height <- 900

group_plot_width      <- 1300  # 2 Facetten-Spalten (siehe facet_wrap ncol=2)
group_plot_row_height <- 550   # pro Facetten-Reihe - Gesamthoehe skaliert automatisch mit Anzahl Sensoren

lineplot_width  <- 1400
lineplot_height <- 800

png_dpi <- 150

station_colors <- c(
  "20" = "gold", "30" = "darkgreen",
  "40" = "lightblue", "50" = "darkblue"
)

# Aggregationsintervall in Minuten (60 = stuendlich, wie im
# restlichen Projekt, z.B. wind.R)
agg_minutes <- 1

#### Daten einlesen & zusammenfuehren ####

aws_joined_1h <- read_and_join_stations(stations, z, data_dir, t0, t1, agg_minutes)


#### Tagesmaske (aus Globalstrahlung) ####
# compute_night_mask() liefert TRUE fuer Zeitpunkte, an denen ALLE
# Stationen unter der Schwelle liegen (= Nacht). Tag ist einfach
# das logische Gegenteil: mindestens eine Station zeigt Strahlung.
radiation_columns <- get_measurement_columns(aws_joined_1h, "R_sw_d")
night_mask <- compute_night_mask(aws_joined_1h, radiation_columns, radiation_threshold_wm2)
day_mask <- !night_mask

cat(
  "Tag-Zeitpunkte:", sum(day_mask, na.rm = TRUE),
  "von", length(day_mask),
  paste0("(", format_number(100 * mean(day_mask, na.rm = TRUE), 1), "%)\n")
)

day_data <- if (all(is.na(day_mask))) {
  warning("Tagesmaske nicht verfuegbar - verwende vollen Datensatz.")
  aws_joined_1h
} else {
  aws_joined_1h[which(day_mask), , drop = FALSE]
}

subset_label <- paste0("Nur Tageswerte (Globalstrahlung >= ", radiation_threshold_wm2, " W/m\u00b2)")

#### Lineplot (voller Zeitraum, zur Kontrolle) ####

for (v in analysis_variables) print(lineplot_func(aws_joined_1h, v, min_value = radiation_threshold_wm2))

#### Messabdeckung (voller Zeitraum) ####

coverage_table <- build_coverage_table(aws_joined_1h, analysis_variables, t0, t1)
section("MESSABDECKUNG")
print(coverage_table)
write.csv(coverage_table, "scatter_measurement_coverage_R_sw_d.csv", row.names = FALSE)

#### Hauptanalyse (nur Tageswerte) ####

results <- lapply(analysis_variables, function(v) run_variable_analysis(day_data, v, subset_label))
results <- results[!sapply(results, is.null)]

scatter_statistics <- do.call(rbind, lapply(results, function(r) r$stats_rows))
rownames(scatter_statistics) <- NULL

reference_table <- do.call(rbind, lapply(results, function(r) r$reference_row))
rownames(reference_table) <- NULL


write.csv(scatter_statistics, "scatter_statistics_R_sw_d.csv", row.names = FALSE)
write.csv(reference_table, "scatter_reference_variables_R_sw_d.csv", row.names = FALSE)


#### Alles speichern & zippen (nur lokal in diesem Skript) ####
# Nur die Dateien DIESES Laufs - anhand von analysis_variables
# gefiltert, statt per genereller "scatter_*"-Suche (die wuerde
# auch Ausgaben frueherer Laeufe mit anderen Variablen einsammeln).

lineplot_dir <- "lineplots"
dir.create(lineplot_dir, showWarnings = FALSE, recursive = TRUE)

# 1) Lineplots erzeugen + speichern
for (v in analysis_variables) {
  p <- lineplot_func(aws_joined_1h, v, min_value = radiation_threshold_wm2)
  if (!is.null(p)) {
    ggsave(file.path(lineplot_dir, paste0(v, "_zeitverlauf.png")),
           plot = p, width = lineplot_width, height = lineplot_height, units = "px", dpi = png_dpi)
  }
}

zip_filename <- paste0("alle_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")

run_csv_files <- c(
  "scatter_measurement_coverage_R_sw_d.csv",
  "scatter_statistics_R_sw_d.csv",
  "scatter_reference_variables_R_sw_d.csv"
)
run_lineplot_files <- file.path(lineplot_dir, paste0(analysis_variables, "_zeitverlauf.png"))
run_scatter_dirs <- file.path(".", paste0("scatter_", analysis_variables))
run_scatter_png_files <- unlist(lapply(run_scatter_dirs, function(d) {
  if (dir.exists(d)) list.files(d, pattern = "\\.png$", full.names = TRUE) else character(0)
}))

files_to_zip <- unique(c(run_csv_files, run_lineplot_files, run_scatter_png_files))
files_to_zip <- files_to_zip[file.exists(files_to_zip)]

if (!requireNamespace("zip", quietly = TRUE)) install.packages("zip")
zip::zipr(zip_filename, files_to_zip)
cat("Gespeichert:", zip_filename, "(", length(files_to_zip), "Dateien)\n")