# ============================================================
# RMSD_boxplot_by_sensor.R
#
# Liest die per-Zeitpunkt Abweichungen (comparison - reference)
# aus den *_deviations.csv Dateien ein und erstellt PRO
# Variablengruppe einen Boxplot mit EINER Box JE SENSOR - zeigt
# also die zeitliche Streuung der Abweichung jedes einzelnen
# Sensors, nicht nur den einen aggregierten RMSD-Wert.
#
# Zusaetzlich:
#   - Mittelwert der Abweichung wird ueber jeder Box eingeblendet
#   - gestrichelte Linien bei +/- Herstellerangabe (Sensorgenauigkeit
#     laut Datenblatt) zur Einordnung, ob eine Abweichung im Rahmen
#     der erwarteten Sensorgenauigkeit liegt
#
# Erwartet, dass die *_deviations.csv Dateien bereits existieren
# (Erweiterung der jeweiligen ..._RMSD_und_MAD.R Skripte).
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Plot-Einstellungen (Fallback, falls nicht schon anderswo gesetzt) ####
if (!exists("single_plot_width"))  single_plot_width  <- 900
if (!exists("single_plot_height")) single_plot_height <- 900
if (!exists("png_dpi"))            png_dpi            <- 150

#### Einstellungen ####

deviation_files <- list(
  air_temp = "results_final/air_temp_deviations.csv",
  rel_hum  = "results_final/rel_hum_deviations.csv",
  R_sw_d   = "results_final/R_sw_d_deviations.csv",
  R_net    = "results_final/R_net_deviations.csv"
)

# "absolute": Boxplot zeigt die absolute Abweichung (comparison - reference)
#             in der jeweiligen physikalischen Einheit; Toleranzlinie ist ein
#             fester Wert in dieser Einheit.
# "percent":  Boxplot zeigt die relative Abweichung in Prozent des
#             Referenzwerts ((comparison - reference) / reference * 100);
#             Toleranzlinie ist ein fester Prozentwert. Noetig, wenn die
#             Herstellerangabe selbst schon ein Prozentwert vom Messwert ist
#             (z.B. Pyranometer: +/-5% der Tagessumme) statt eines festen
#             Werts in der Messeinheit - sonst ist die Linie nur bei einer
#             bestimmten Einstrahlung ueberhaupt richtig.
deviation_mode <- list(
  air_temp = "absolute",
  rel_hum  = "absolute",
  R_sw_d   = "percent",
  R_net    = "absolute"
)

# Herstellerangabe Sensorgenauigkeit (Datenblatt), als +/- Linie im Plot.
# Einheit haengt vom jeweiligen deviation_mode ab (siehe oben).
# air_temp: +/-0.4 Grad C       (absolute, Bereich +5 bis +40 Grad C)
# rel_hum:  +/-4 Prozentpunkte  (absolute, Bereich 0-100 %; Analysezeitraum
#           ist Nacht, wo RH oft Richtung Saettigung/Tau geht - dort gilt
#           die striktere +/-2%-Spec fuer 10-90% RH nicht mehr sicher)
# R_sw_d:   +/-5 %              (percent, Datenblatt: +/-5% der Tagessumme)
# R_net:    +/-30 W/m2          (absolute, GENAEHERT: das ist der
#           Richtungsfehler (0-60 Grad bei 1000 W/m2), nicht die
#           allgemeine Genauigkeit - nur grobe Orientierung. Datenblatt
#           gibt separat auch +/-5% "Sensor asymmetry" an - sag Bescheid,
#           falls R_net stattdessen auch im percent-Modus laufen soll.)
sensor_tolerance <- list(
  air_temp = 0.4,
  rel_hum  = 2,
  R_sw_d   = 5,
  R_net    = 30
)

output_directory <- "scatter_RMSD_overview"
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

#### Boxplot pro Sensor, getrennt je Variablengruppe ####

for (var_group in names(deviation_files)) {
  
  path <- deviation_files[[var_group]]
  if (!file.exists(path)) {
    warning(paste("Datei nicht gefunden, wird uebersprungen:", path))
    next
  }
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    warning(paste("Keine Zeilen in", path, "- wird uebersprungen."))
    next
  }
  
  mode      <- deviation_mode[[var_group]]
  tolerance <- sensor_tolerance[[var_group]]
  
  if (mode == "percent") {
    if (!"percent_deviation" %in% names(df)) {
      warning(paste0(
        "Spalte 'percent_deviation' fehlt in ", path,
        " - das zugehoerige ..._RMSD_und_MAD.R Skript muss zuerst neu ",
        "ausgefuehrt werden (aktualisierte Version noetig)."
      ))
      next
    }
    df$plot_value <- df$percent_deviation
    y_axis_label  <- "Abweichung (%)"
    tolerance_unit <- "%"
  } else {
    df$plot_value  <- df$deviation
    y_axis_label   <- paste0("Abweichung (", get_variable_label(var_group)$y, ")")
    tolerance_unit <- get_variable_label(var_group)$y
  }
  
  # Mittelwerte je Sensor, fuer die Beschriftung ueber den Boxen
  mean_labels <- aggregate(plot_value ~ comparison_variable, data = df, FUN = mean)
  mean_labels$label <- sprintf("%.2f", mean_labels$plot_value)
  
  # y-Position der Beschriftung: deutlich oberhalb des jeweiligen
  # Box-Maximums, damit sie klar von der Box und den Ausreisser-Punkten
  # getrennt ist und gut lesbar bleibt
  box_max <- aggregate(plot_value ~ comparison_variable, data = df, FUN = max)
  label_offset <- diff(range(df$plot_value, na.rm = TRUE)) * 0.10
  mean_labels$y_pos <- box_max$plot_value + label_offset
  
  deviation_boxplot <- ggplot(df, aes(x = comparison_variable, y = plot_value)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_hline(
      yintercept = c(-tolerance, tolerance),
      linetype = "dotted", color = "firebrick", linewidth = 0.6
    ) +
    geom_boxplot(fill = "#a6c8e0", outlier.color = "#2b5d8c", width = 0.5) +
    geom_text(
      data = mean_labels,
      aes(x = comparison_variable, y = y_pos, label = label),
      inherit.aes = FALSE, size = 3.3, fontface = "bold"
    ) +
    labs(
      title = paste(get_variable_label(var_group)$title, "\u2013 Sensorabweichung"),
      subtitle = paste0(
        "Ref: ", df$reference_variable[1],
        "  |  Toleranz: \u00b1", tolerance, " ", tolerance_unit, " (Datenblatt)"
      ),
      x = NULL, y = y_axis_label
    ) +
    scatter_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  
  print(deviation_boxplot)
  
  ggsave(
    filename = file.path(output_directory, paste0("deviation_boxplot_", var_group, ".png")),
    plot = deviation_boxplot,
    width = single_plot_width, height = single_plot_height,
    units = "px", dpi = png_dpi
  )
  
  cat("Boxplot gespeichert:", file.path(output_directory, paste0("deviation_boxplot_", var_group, ".png")), "\n")
}
