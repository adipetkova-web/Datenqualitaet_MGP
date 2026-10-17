# ============================================================
# 02_abbildungen.R — alle Abbildungen im gleichen Layout
#
# Pro Variablengruppe:
#   zeitverlauf_<gruppe>.png         Zeitreihe aller Sensoren
#   scatter_<gruppe>.png             alle Sensoren vs. Referenz (facettiert)
#   scatter/<gruppe>_<sensor>.png    einzelne Scatterplots
#   boxplot_rmsd_<gruppe>.png        Abweichung je Sensor, beschriftet mit RMSD
#
# Benoetigt results/tables/ aus 01_sensorvergleich.R.
# Ausgabe: results/figures/
# ============================================================

source("R/helpers.R")

stats_all <- read.csv(file.path(TABLE_DIR, "sensorvergleich_statistik.csv"), stringsAsFactors = FALSE)

# Sensoren sortiert nach Station, dann Sensor (1m vor 3m)
sensor_order <- function(cols) cols[order(station_of(cols), sensor_of(cols))]

#### Zeitverlauf ####

plot_timeseries <- function(v) {
  cfg  <- VARIABLES[[v]]
  df   <- read_stations(as_time(cfg$t0), as_time(cfg$t1))   # voller Zeitraum, auch bei day_only
  cols <- measurement_columns(df, v)
  hours <- as.numeric(difftime(as_time(cfg$t1), as_time(cfg$t0), units = "hours"))

  long <- bind_rows(lapply(cols, function(col) data.frame(
    t = df$t, value = safe_numeric(df[[col]]), sensor = col,
    station = station_of(col), level = ifelse(is.na(sensor_of(col)), "–", paste0(sensor_of(col), "-Sensor"))
  )))

  p <- ggplot(long, aes(t, value, group = sensor, color = station, linetype = level)) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    scale_color_stations() +
    scale_x_datetime(date_breaks = if (hours <= 4) "30 min" else "2 hours",
                     date_labels = "%d.%m.\n%H:%M", expand = c(0, 0)) +
    scale_y_continuous(labels = dec_comma()) +
    labs(title = paste(cfg$title, "– Zeitverlauf"),
         subtitle = window_label(v),
         x = "Zeit (UTC+1)", y = axis_label(v), linetype = "Sensor") +
    theme_mgp()

  if (all(long$level == "–")) p <- p + guides(linetype = "none")
  if (isTRUE(cfg$day_only)) {
    p <- p +
      geom_hline(yintercept = DAY_THRESHOLD_WM2, linetype = "dotted", color = "grey40") +
      labs(caption = paste0("Gepunktete Linie: Tag-Schwelle ", DAY_THRESHOLD_WM2,
                            " W/m² – nur Zeitpunkte darüber gehen in die Statistik ein."))
  }
  p
}

#### Scatterplots ####

scatter_data <- function(v) {
  dev <- read.csv(file.path(TABLE_DIR, paste0("abweichungen_", v, ".csv")), stringsAsFactors = FALSE)
  dev$panel <- factor(sensor_label(dev$comparison_variable),
                      levels = sensor_label(sensor_order(unique(dev$comparison_variable))))
  dev$station <- station_of(dev$comparison_variable)
  dev
}

scatter_annotation <- function(v) {
  s <- stats_all[stats_all$variable_group == v, ]
  data.frame(
    comparison_variable = s$comparison_variable,
    panel = factor(sensor_label(s$comparison_variable),
                   levels = sensor_label(sensor_order(s$comparison_variable))),
    label = paste0("R² = ", fmt(s$r_squared, 4), "\n", s$equation,
                   "\nRMSD = ", fmt(s$RMSD, 2), " ", VARIABLES[[v]]$unit,
                   "\nn = ", s$n_pairs)
  )
}

plot_scatter <- function(v, dev, ann, single = FALSE) {
  cfg <- VARIABLES[[v]]
  ref <- unique(dev$reference_variable)
  lim <- range(c(dev$reference, dev$value), na.rm = TRUE)

  p <- ggplot(dev, aes(reference, value)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(aes(color = station), size = 1.4, alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 0.7) +
    geom_label(data = ann, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
               hjust = -0.05, vjust = 1.05, size = 3.1, label.size = 0.2, lineheight = 0.95,
               fill = alpha("white", 0.85)) +
    scale_color_stations(guide = "none") +
    scale_x_continuous(labels = dec_comma()) +
    scale_y_continuous(labels = dec_comma()) +
    coord_equal(xlim = lim, ylim = lim) +
    labs(subtitle = paste0("Referenz: ", sensor_label(ref), "  |  ", window_label(v)),
         x = axis_label(v, paste0("Referenz (", sensor_label(ref), "):")),
         caption = "Gestrichelt: 1:1-Linie  |  Schwarz: lineare Regression") +
    theme_mgp()

  if (single) {
    p + labs(title = paste0(cfg$title, ": ", dev$panel[1], " vs. Referenz"),
             y = axis_label(v, paste0(dev$panel[1], ":")))
  } else {
    p + facet_wrap(~ panel, ncol = 4) +
      labs(title = paste(cfg$title, "– Sensoren vs. Referenz"), y = axis_label(v, "Sensor:"))
  }
}

#### Boxplots mit RMSD ####

plot_boxplot <- function(v) {
  cfg <- VARIABLES[[v]]
  dev <- scatter_data(v)
  s   <- stats_all[stats_all$variable_group == v, ]
  pct <- cfg$tol_mode == "percent"

  dev$y <- if (pct) dev$percent_deviation else dev$deviation
  unit  <- if (pct) "%" else cfg$unit

  # RMSD-Beschriftung ueber jeder Box (statt Mittelwert)
  lab <- data.frame(
    panel = factor(sensor_label(s$comparison_variable), levels = levels(dev$panel)),
    rmsd  = if (pct) s$RMSD_pct else s$RMSD
  )
  top <- max(c(dev$y, cfg$tol), na.rm = TRUE)
  lab$y <- top + 0.08 * diff(range(c(dev$y, -cfg$tol, cfg$tol), na.rm = TRUE))
  lab$label <- paste0("RMSD\n", fmt(lab$rmsd, 2))

  ggplot(dev, aes(panel, y)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_hline(yintercept = c(-cfg$tol, cfg$tol), linetype = "dashed", color = "firebrick", linewidth = 0.6) +
    geom_boxplot(aes(fill = station), alpha = 0.55, width = 0.55,
                 outlier.size = 0.9, outlier.alpha = 0.6) +
    geom_text(data = lab, aes(panel, y, label = label), inherit.aes = FALSE,
              size = 3.3, fontface = "bold", lineheight = 0.9, vjust = 0) +
    scale_fill_manual(values = STATION_COLORS, guide = "none") +
    scale_y_continuous(labels = dec_comma(), expand = expansion(mult = c(0.05, 0.18))) +
    labs(title = paste(cfg$title, "– Abweichung von der Referenz"),
         subtitle = paste0("Referenz: ", sensor_label(s$reference_variable[1]), "  |  ", window_label(v)),
         x = NULL,
         y = if (pct) "Abweichung vom Referenzwert (%)" else paste0("Abweichung Sensor − Referenz (", unit, ")"),
         caption = paste0("Rot gestrichelt: Messgenauigkeit laut Hersteller (", cfg$tol_source, ").  ",
                          "Zahl über der Box: RMSD in ", unit, ".")) +
    theme_mgp() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#### Alles erzeugen ####

for (v in names(VARIABLES)) {
  section(VARIABLES[[v]]$title)
  dev <- scatter_data(v)
  ann <- scatter_annotation(v)

  save_png(plot_timeseries(v), file.path(FIGURE_DIR, paste0("zeitverlauf_", v, ".png")), FIG_SIZE$timeseries)

  n_panels <- length(unique(dev$comparison_variable))
  save_png(plot_scatter(v, dev, ann), file.path(FIGURE_DIR, paste0("scatter_", v, ".png")),
           c(FIG_SIZE$scatter_group[1], if (n_panels > 4) FIG_SIZE$scatter_group[2] else 620))

  for (col in unique(dev$comparison_variable)) {
    save_png(plot_scatter(v, dev[dev$comparison_variable == col, ], ann[ann$comparison_variable == col, ], single = TRUE),
             file.path(FIGURE_DIR, "scatter", paste0(v, "_", col, ".png")), FIG_SIZE$scatter)
  }

  save_png(plot_boxplot(v), file.path(FIGURE_DIR, paste0("boxplot_rmsd_", v, ".png")), FIG_SIZE$boxplot)
}
