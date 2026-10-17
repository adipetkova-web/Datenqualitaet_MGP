# ============================================================
# RMSD_boxplot.R
#
# Liest die RMSD-Ergebnisse aus den einzelnen *_RMSD_MAD.csv
# Dateien ein und erstellt PRO Variablengruppe einen eigenen
# Boxplot (reine Verteilung, keine Einzelpunkte).
#
# Erwartet, dass folgende Dateien bereits existieren (Output
# der jeweiligen ..._RMSD_und_MAD.R Skripte):
#   air_temp_RMSD_MAD.csv, rel_hum_RMSD_MAD.csv,
#   R_sw_d_RMSD_MAD.csv,   R_net_RMSD_MAD.csv
# ============================================================

source("scripts/AWS/AWS_scatter_helpers.R")

#### Plot-Einstellungen (Fallback, falls nicht schon anderswo gesetzt) ####
# AWS_scatter_helpers.R erwartet diese Variablen, sie werden aber von
# diesem Skript nicht selbst definiert. Falls sie schon im Environment
# stehen (z.B. aus einem uebergeordneten Skript), werden sie hier NICHT
# ueberschrieben - sonst greifen folgende Standardwerte.
if (!exists("single_plot_width"))  single_plot_width  <- 1800
if (!exists("single_plot_height")) single_plot_height <- 1200
if (!exists("png_dpi"))            png_dpi            <- 150

#### Einstellungen ####

rmsd_files <- list(
  air_temp = "air_temp_RMSD_MAD.csv",
  rel_hum  = "rel_hum_RMSD_MAD.csv",
  R_sw_d   = "R_sw_d_RMSD_MAD.csv",
  R_net    = "R_net_RMSD_MAD.csv"
)

output_directory <- "scatter_RMSD_overview"
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

#### Boxplot pro Variablengruppe ####

for (var_group in names(rmsd_files)) {
  
  path <- rmsd_files[[var_group]]
  if (!file.exists(path)) {
    warning(paste("Datei nicht gefunden, wird uebersprungen:", path))
    next
  }
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    warning(paste("Keine Zeilen in", path, "- wird uebersprungen."))
    next
  }
  
  y_label <- get_variable_label(var_group)$y
  
  rmsd_boxplot <- ggplot(df, aes(x = variable_group, y = RMSD)) +
    geom_boxplot(fill = "#a6c8e0", outlier.color = "#2b5d8c", width = 0.4) +
    labs(
      title = paste(get_variable_label(var_group)$title, "\u2013 RMSD"),
      subtitle = paste0("Gegenueber Referenz ", df$reference_variable[1], " (n = ", nrow(df), " Vergleichsstationen)"),
      x = NULL, y = y_label
    ) +
    scatter_theme +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  print(rmsd_boxplot)
  
  ggsave(
    filename = file.path(output_directory, paste0("RMSD_boxplot_", var_group, ".png")),
    plot = rmsd_boxplot,
    width = single_plot_width, height = single_plot_height,
    units = "px", dpi = png_dpi
  )
  
  cat("Boxplot gespeichert:", file.path(output_directory, paste0("RMSD_boxplot_", var_group, ".png")), "\n")
}

rmsd_data <- do.call(rbind, lapply(names(rmsd_files), function(var_group) {
  path <- rmsd_files[[var_group]]
  if (!file.exists(path)) {
    warning(paste("Datei nicht gefunden, wird uebersprungen:", path))
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE)
  df
}))

rmsd_data$variable_group <- factor(
  rmsd_data$variable_group,
  levels = c("air_temp", "rel_hum", "R_sw_d", "R_net")
)

facet_labels <- setNames(
  paste0(names(rmsd_files), " (", sapply(names(rmsd_files), function(v) get_variable_label(v)$y), ")"),
  names(rmsd_files)
)

rmsd_boxplot <- ggplot(rmsd_data, aes(x = variable_group, y = RMSD)) +
  geom_boxplot(fill = "#a6c8e0", outlier.color = "#2b5d8c", width = 0.5) +
  facet_wrap(~ variable_group, scales = "free", ncol = 2,
             labeller = as_labeller(facet_labels)) +
  labs(
    title = "RMSD gegenueber automatisch bestimmter Referenzstation",
    x = NULL, y = "RMSD"
  ) +
  scatter_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 13, face = "bold")
  )

print(rmsd_boxplot)

