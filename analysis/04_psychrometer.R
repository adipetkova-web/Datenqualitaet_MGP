# ============================================================
# 04_psychrometer.R — Vergleich mit dem Aspirationspsychrometer
#
# Aufgabe laut Rueckmeldung: Vergleichsmessungen mit dem
# Aspirationspsychrometer (Assmann) mit den AWS-Daten vergleichen.
# Gab es Unterschiede? Wie gross? Auswirkungen auf die Analysen?
#
# EINGABE: data/psychrometer/psychrometer_messungen.csv
#   (Vorlage mit Beispielzeilen: psychrometer_BEISPIEL.csv)
#
#   datum          JJJJ-MM-TT
#   uhrzeit        HH:MM in UTC+1 (gleiche Zeitzone wie die Logger!)
#   station        20, 30, 40 oder 50
#   sensor         1m oder 3m - neben welchem AWS-Sensor gemessen wurde
#   t_trocken      Trockentemperatur (Grad C)
#   t_feucht       Feuchttemperatur (Grad C)
#   luftdruck_hpa  optional; leer = Luftdruck der Station (Control-Datei)
#   bemerkung      optional
#
# Berechnung der relativen Feuchte (Psychrometerformel nach WMO,
# ventiliertes Assmann-Psychrometer):
#   e  = E(Tf) - A * p * (T - Tf),   A = 6,53e-4 * (1 + 0,000944 * Tf)  [1/K]
#   RH = e / E(T) * 100
#   E() = Saettigungsdampfdruck, Magnus-Formel aus R/course/met.R
#
# Die AWS-Werte werden als Mittel ueber +/- AVG_HALF_WINDOW_MIN
# Minuten um den Ablesezeitpunkt genommen.
#
# Ausgabe:
#   results/tables/psychrometer_vergleich.csv     jede Ablesung mit AWS-Wert
#   results/tables/psychrometer_zusammenfassung.csv  pro Sensor
#   results/figures/psychrometer_vergleich.png
# ============================================================

source("R/helpers.R")

AVG_HALF_WINDOW_MIN <- 2

section("Aspirationspsychrometer")

if (!file.exists(PSYCHRO_CSV)) stop("Datei fehlt: ", PSYCHRO_CSV)
psy <- read.csv(PSYCHRO_CSV, stringsAsFactors = FALSE, colClasses = c(station = "character"))

if (nrow(psy) == 0) {
  message("Noch keine Psychrometer-Messungen in ", PSYCHRO_CSV, " eingetragen - Schritt wird uebersprungen.\n",
          "Format siehe data/psychrometer/psychrometer_BEISPIEL.csv")
} else {

  psy$t <- as_time(paste(psy$datum, psy$uhrzeit))
  if (any(is.na(psy$t))) stop("Datum/Uhrzeit nicht lesbar in Zeile(n): ", paste(which(is.na(psy$t)), collapse = ", "))

  #### Passende AWS-Werte ####

  # Die Control-Datei (u.a. Luftdruck) ist stuendlich -> Lesefenster
  # auf volle Stunden mit etwas Puffer runden
  t_start <- as_time(format(min(psy$t) - 2 * 3600, "%Y-%m-%d %H:00:00"))
  t_end   <- as_time(format(max(psy$t) + 3 * 3600, "%Y-%m-%d %H:00:00"))
  aws <- read_stations(t_start, t_end)

  aws_mean <- function(col, t) {
    if (!col %in% names(aws)) return(NA_real_)
    sel <- abs(as.numeric(difftime(aws$t, t, units = "mins"))) <= AVG_HALF_WINDOW_MIN
    mean(safe_numeric(aws[[col]][sel]), na.rm = TRUE)
  }

  #### Psychrometer: relative Feuchte berechnen ####

  # Luftdruck: Eingabe aus der Tabelle, sonst Messwert der Station
  psy$air_press_aws <- mapply(function(st, t) aws_mean(paste0("air_press_", st), t), psy$station, psy$t)
  p <- ifelse(is.na(psy$luftdruck_hpa), psy$air_press_aws, psy$luftdruck_hpa)
  p[is.na(p)] <- 1013.25

  E_tf <- met_vap_press(psy$t_feucht, 100)
  E_t  <- met_vap_press(psy$t_trocken, 100)
  A    <- 6.53e-4 * (1 + 0.000944 * psy$t_feucht)
  e    <- E_tf - A * p * (psy$t_trocken - psy$t_feucht)
  psy$rh_psychro <- 100 * e / E_t

  psy$t_aws  <- mapply(function(st, s, t) aws_mean(paste0("air_temp_", s, "_", st), t), psy$station, psy$sensor, psy$t)
  psy$rh_aws <- mapply(function(st, s, t) aws_mean(paste0("rel_hum_", s, "_", st), t), psy$station, psy$sensor, psy$t)

  psy$diff_t  <- psy$t_aws  - psy$t_trocken     # AWS - Psychrometer
  psy$diff_rh <- psy$rh_aws - psy$rh_psychro

  write_table(psy, "psychrometer_vergleich.csv")

  summary_tab <- psy %>%
    group_by(station, sensor) %>%
    summarise(n = n(),
              bias_t = mean(diff_t, na.rm = TRUE),  rmsd_t = sqrt(mean(diff_t^2, na.rm = TRUE)),
              bias_rh = mean(diff_rh, na.rm = TRUE), rmsd_rh = sqrt(mean(diff_rh^2, na.rm = TRUE)),
              .groups = "drop")
  write_table(summary_tab, "psychrometer_zusammenfassung.csv")
  print(summary_tab, n = Inf)

  #### Abbildung ####

  long <- bind_rows(
    data.frame(var = "air_temp", station = psy$station, sensor = psy$sensor, diff = psy$diff_t),
    data.frame(var = "rel_hum",  station = psy$station, sensor = psy$sensor, diff = psy$diff_rh)
  )
  long$panel <- sapply(long$var, function(v) axis_label(v))
  tol <- data.frame(panel = sapply(c("air_temp", "rel_hum"), axis_label),
                    tol = c(VARIABLES$air_temp$tol, VARIABLES$rel_hum$tol))

  p <- ggplot(long, aes(paste0("AWS", station, " · ", sensor), diff, color = station)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_hline(data = tol, aes(yintercept = tol), linetype = "dashed", color = "firebrick") +
    geom_hline(data = tol, aes(yintercept = -tol), linetype = "dashed", color = "firebrick") +
    geom_point(size = 2.8, alpha = 0.8, position = position_jitter(width = 0.08, height = 0)) +
    facet_wrap(~ panel, scales = "free_y") +
    scale_color_manual(values = STATION_COLORS, guide = "none") +
    scale_y_continuous(labels = dec_comma()) +
    labs(title = "AWS-Sensoren vs. Aspirationspsychrometer",
         subtitle = paste0("Abweichung AWS − Psychrometer, AWS gemittelt über ±",
                           AVG_HALF_WINDOW_MIN, " min um die Ablesung"),
         x = NULL, y = "Abweichung AWS − Psychrometer",
         caption = "Rot gestrichelt: Messgenauigkeit der AWS-Sensoren laut Hersteller.") +
    theme_mgp() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  save_png(p, file.path(FIGURE_DIR, "psychrometer_vergleich.png"), FIG_SIZE$psychrometer)
}
