# ============================================================
# 03_gradientenfehler.R — Auswirkung der Sensorabweichungen auf Gradienten
#
# Frage aus der Rueckmeldung: "Hat das Auswirkungen auf die Analysen
# (z.B. Gradientenbetrachtungen)?"
#
# Idee: Bei der Vergleichsmessung hingen der 1m- und der 3m-Sensor
# JEDER Station auf derselben Hoehe (1,6 m). Physikalisch muesste
# ihre Differenz also 0 sein. Was trotzdem gemessen wird, ist reiner
# Sensorunterschied - und genau dieser Unterschied steckt spaeter im
# Feld in jedem Gradienten, den die Kurs-Skripte berechnen:
#
#   dpot_temp_dz = (pot_temp_3m - pot_temp_1m) / (3 m - 1 m)
#
# Ein Offset von 0,3 K zwischen den Sensoren erzeugt also einen
# Schein-Gradienten von 0,15 K/m, auch wenn die Luft perfekt
# durchmischt ist.
#
# Ausgabe:
#   results/tables/gradientenfehler.csv
#   results/figures/gradientenfehler_air_temp.png
#   results/figures/gradientenfehler_rel_hum.png
# ============================================================

source("R/helpers.R")

section("Schein-Gradienten aus der Vergleichsmessung")

DZ_FIELD <- 3 - 1   # Hoehenabstand der Sensoren im Feld (m)

gradient_bias <- bind_rows(lapply(c("air_temp", "rel_hum"), function(v) {
  df <- variable_data(v)
  bind_rows(lapply(STATIONS, function(st) {
    top    <- safe_numeric(df[[paste0(v, "_3m_", st)]])
    bottom <- safe_numeric(df[[paste0(v, "_1m_", st)]])
    ok     <- is.finite(top) & is.finite(bottom)
    d      <- top[ok] - bottom[ok]
    data.frame(
      variable_group = v, station = st, n = sum(ok),
      mean_diff_3m_minus_1m = mean(d),
      sd_diff = sd(d),
      rmsd_diff = sqrt(mean(d^2)),
      pseudo_gradient_per_m = mean(d) / DZ_FIELD,
      unit = diff_unit(v)
    )
  }))
}))

write_table(gradient_bias, "gradientenfehler.csv")
print(gradient_bias %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)

#### Abbildungen (eine pro Variable) ####

plot_gradient_bias <- function(v) {
  d <- gradient_bias[gradient_bias$variable_group == v, ]
  d$name <- factor(station_name(d$station), levels = STATION_CODES)
  u <- diff_unit(v)

  ggplot(d, aes(name, mean_diff_3m_minus_1m, color = station)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_errorbar(aes(ymin = mean_diff_3m_minus_1m - sd_diff, ymax = mean_diff_3m_minus_1m + sd_diff),
                  width = 0.15, linewidth = 0.7) +
    geom_point(size = 3.2) +
    geom_text(aes(label = paste0(fmt(pseudo_gradient_per_m, 2), " ", u, "/m")),
              nudge_x = 0.12, hjust = 0, size = 3.5, color = "grey20") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
    scale_color_manual(values = STATION_COLORS, guide = "none") +
    scale_y_continuous(labels = dec_comma()) +
    labs(
      title = paste0(VARIABLES[[v]]$title, " \u2013 Sensordifferenz 3m \u2212 1m"),
      subtitle = "Beide Sensoren auf 1,6 m montiert \u2013 im Feld erscheint diese Differenz als Schein-Gradient",
      x = NULL,
      y = paste0("Differenz 3m-Sensor \u2212 1m-Sensor (", u, ")"),
      caption = paste0("Punkt: Mittelwert, Balken: \u00b11 Standardabweichung (", window_label(v), ").\n",
                       "Zahl: Schein-Gradient = Differenz / 2 m.")
    ) +
    theme_mgp()
}

for (v in c("air_temp", "rel_hum")) {
  save_png(plot_gradient_bias(v), file.path(FIGURE_DIR, paste0("gradientenfehler_", v, ".png")), FIG_SIZE$gradient)
}
