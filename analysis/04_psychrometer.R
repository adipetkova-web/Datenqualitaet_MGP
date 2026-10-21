# ============================================================
# 04_psychrometer.R — Feldmessung vs. Aspirationspsychrometer
#
# Aufgabe laut Rueckmeldung: Vergleichsmessungen mit dem
# Aspirationspsychrometer mit den AWS-Daten vergleichen.
# Gab es Unterschiede? Wie gross? Auswirkungen auf die Analysen?
#
# Das Psychrometer wurde bei der FELDmessung eingesetzt (eine Ablesung
# pro Standort). Verglichen wird deshalb mit den Feld-Daten in
# data/raw/AWS<nr>_Feld/.
#
# EINGABE: data/psychrometer/psychrometer_messungen.csv
#   datum          z.B. 3.09 / 03.09.2026 / 2026-09-03
#   uhrzeit        z.B. 9:32  in UTC+1 (wie die Logger, siehe PSYCHRO_TZ)
#   station        20, 30, 40 oder 50
#   t_trocken      Trockentemperatur (Grad C)
#   t_feucht       Feuchttemperatur (Grad C)
#   luftdruck_hpa  optional; fehlt/leer = Luftdruck der Station
#   bemerkung      optional
#   Trennzeichen , oder ; und Dezimalpunkt oder -komma gehen beide
#   (also auch direkt aus deutschem Excel als CSV gespeichert).
#
# 1) Relative Feuchte aus dem Psychrometer (WMO-Psychrometerformel,
#    ventiliertes Assmann-Psychrometer):
#      e  = E(Tf) - A * p * (T - Tf),  A = 6,53e-4 * (1 + 0,000944 * Tf)  [1/K]
#      RH = e / E(T) * 100
#    E() = Saettigungsdampfdruck (Magnus-Formel aus R/course/met.R),
#    T = Trocken-, Tf = Feuchttemperatur, p = Luftdruck (hPa).
#
# 2) AWS-Werte: Mittel ueber +/- PSYCHRO_AVG_HALF_WINDOW_MIN Minuten
#    um die Ablesung, vom Sensor auf Psychrometer-Hoehe
#    (PSYCHRO_SENSOR in R/config.R, hier der untere = 1m-Sensor).
#
# 3) Differenz AWS - Psychrometer (Rohwerte, ohne Offset-Korrektur).
#
# Ausgabe:
#   results/tables/psychrometer_vergleich.csv
#   results/figures/psychrometer_vergleich.png
# ============================================================

source("R/helpers.R")

section("Aspirationspsychrometer vs. Feldmessung")

#### Psychrometer-Tabelle einlesen ####

read_psychro <- function(file) {
  first <- readLines(file, n = 1, warn = FALSE)
  sep   <- if (grepl(";", first)) ";" else ","
  psy   <- read.csv(file, sep = sep, stringsAsFactors = FALSE, colClasses = "character",
                    strip.white = TRUE, fileEncoding = "UTF-8-BOM")
  names(psy) <- tolower(trimws(names(psy)))
  psy <- psy[rowSums(psy != "" & !is.na(psy)) > 0, , drop = FALSE]   # leere Zeilen weg

  need <- c("datum", "uhrzeit", "station", "t_trocken", "t_feucht")
  if (!all(need %in% names(psy))) stop("Spalten fehlen in ", file, ": ", paste(setdiff(need, names(psy)), collapse = ", "))

  num <- function(x) as.numeric(gsub(",", ".", x))
  psy$t_trocken <- num(psy$t_trocken)
  psy$t_feucht  <- num(psy$t_feucht)
  psy$luftdruck_hpa <- if ("luftdruck_hpa" %in% names(psy)) num(psy$luftdruck_hpa) else NA_real_
  psy$station <- trimws(psy$station)

  # Datum: "3.09", "3.09.", "03.09.2026", "2026-09-03"
  parse_date <- function(d) {
    d <- trimws(gsub(",", ".", d))
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", d)) return(as.Date(d))
    p <- strsplit(sub("\\.$", "", d), ".", fixed = TRUE)[[1]]
    if (length(p) == 2) p <- c(p, FIELD_YEAR)
    if (length(p) != 3) return(as.Date(NA))
    as.Date(sprintf("%s-%02d-%02d", p[3], as.integer(p[2]), as.integer(p[1])))
  }
  datum <- do.call(c, lapply(psy$datum, parse_date))
  zeit  <- sub("^(\\d{1,2}:\\d{2})(:\\d{2})?$", "\\1", trimws(psy$uhrzeit))

  # Ablesezeit in der notierten Zeitzone -> in Logger-Zeit (UTC+1) umrechnen
  t_local <- as.POSIXct(paste(datum, zeit), format = "%Y-%m-%d %H:%M", tz = PSYCHRO_TZ)
  psy$t   <- as.POSIXct(format(t_local, tz = TZ), tz = TZ)

  bad <- which(is.na(psy$t) | is.na(psy$t_trocken) | is.na(psy$t_feucht) | !psy$station %in% STATIONS)
  if (length(bad) > 0) stop("Zeile(n) ", paste(bad + 1, collapse = ", "), " in ", file,
                            " nicht lesbar (Datum, Uhrzeit, Station oder Temperatur).")
  if (any(psy$t_feucht > psy$t_trocken)) warning("Feuchttemperatur > Trockentemperatur - Spalten vertauscht?")
  psy
}

# Relative Feuchte (%) aus Trocken-/Feuchttemperatur und Luftdruck
psychro_rh <- function(t_dry, t_wet, p_hpa) {
  E_wet <- met_vap_press(t_wet, 100)
  E_dry <- met_vap_press(t_dry, 100)
  A     <- 6.53e-4 * (1 + 0.000944 * t_wet)
  e     <- E_wet - A * p_hpa * (t_dry - t_wet)
  100 * e / E_dry
}

psy <- read_psychro(PSYCHRO_CSV)
if (nrow(psy) == 0) stop("Keine Psychrometer-Messungen in ", PSYCHRO_CSV)
cat(nrow(psy), "Ablesungen eingelesen.\n")

#### Feld-Daten um die Ablesezeiten einlesen ####

# Control-Datei (u.a. Luftdruck) ist stuendlich -> auf volle Stunden runden, mit Puffer
t_start <- as_time(format(min(psy$t) - 2 * 3600, "%Y-%m-%d %H:00:00"))
t_end   <- as_time(format(max(psy$t) + 3 * 3600, "%Y-%m-%d %H:00:00"))
field   <- read_stations(t_start, t_end, suffix = FIELD_SUFFIX, z = FIELD_STATION_Z)

aws_mean <- function(col, t) {
  if (!col %in% names(field)) return(NA_real_)
  sel <- abs(as.numeric(difftime(field$t, t, units = "mins"))) <= PSYCHRO_AVG_HALF_WINDOW_MIN
  v <- safe_numeric(field[[col]][sel])
  if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
}

#### Psychrometer: Feuchte + Unsicherheit ####

psy$air_press_aws <- mapply(function(st, t) aws_mean(paste0("air_press_", st), t), psy$station, psy$t)
psy$p_used <- ifelse(is.na(psy$luftdruck_hpa), psy$air_press_aws, psy$luftdruck_hpa)
psy$p_used[is.na(psy$p_used)] <- 1013.25

psy$rh_psychro <- psychro_rh(psy$t_trocken, psy$t_feucht, psy$p_used)

# Unsicherheit der berechneten Feuchte durch die Ablesegenauigkeit:
# ungünstigste Kombination (Trocken +u, Feucht -u und umgekehrt)
u <- PSYCHRO_READ_UNC_K
psy$rh_psychro_unc <- pmax(
  abs(psychro_rh(psy$t_trocken + u, psy$t_feucht - u, psy$p_used) - psy$rh_psychro),
  abs(psychro_rh(psy$t_trocken - u, psy$t_feucht + u, psy$p_used) - psy$rh_psychro)
)

#### Vergleich pro Ablesung, Sensor und Variable ####

rows <- list()
for (i in seq_len(nrow(psy))) {
  for (sensor in PSYCHRO_SENSOR) {
    for (v in c("air_temp", "rel_hum")) {
      col     <- paste0(v, "_", sensor, "_", psy$station[i])
      aws_raw <- aws_mean(col, psy$t[i])
      ref_val <- if (v == "air_temp") psy$t_trocken[i] else psy$rh_psychro[i]
      rows[[length(rows) + 1]] <- data.frame(
        station = psy$station[i], t = format(psy$t[i], "%Y-%m-%d %H:%M"), sensor = sensor,
        variable_group = v, psychrometer = ref_val,
        psychrometer_unc = if (v == "air_temp") u else psy$rh_psychro_unc[i],
        aws = aws_raw,
        stringsAsFactors = FALSE
      )
    }
  }
}
cmp <- bind_rows(rows)
cmp$diff       <- cmp$aws - cmp$psychrometer
cmp$tol        <- sapply(cmp$variable_group, function(v) VARIABLES[[v]]$tol)
cmp$within_tol <- abs(cmp$diff) <= cmp$tol

write_table(cmp, "psychrometer_vergleich.csv")

if (all(is.na(cmp$aws))) {
  stop("Keine AWS-Feldwerte zu den Ablesezeiten gefunden. Zeitraum der Feld-Dateien und ",
       "PSYCHRO_TZ in R/config.R pruefen.")
}

cat("\nPsychrometer-Feuchte:\n")
print(data.frame(station = psy$station, zeit_UTC1 = format(psy$t, "%d.%m. %H:%M"),
                 T = psy$t_trocken, Tf = psy$t_feucht, p_hPa = round(psy$p_used, 1),
                 RH = round(psy$rh_psychro, 1), RH_unc = round(psy$rh_psychro_unc, 1)), row.names = FALSE)

cat("\nAbweichung AWS - Psychrometer:\n")
print(cmp %>% transmute(station, sensor, variable_group,
                        psychro = round(psychrometer, 2), aws = round(aws, 2), diff = round(diff, 2)),
      row.names = FALSE)

cat("\nUeber alle Stationen:\n")
print(cmp %>% group_by(variable_group, sensor) %>%
        summarise(n = sum(!is.na(diff)),
                  bias = mean(diff, na.rm = TRUE), rmsd = sqrt(mean(diff^2, na.rm = TRUE)),
                  .groups = "drop") %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

#### Abbildung ####

plot_df <- cmp %>%
  filter(!is.na(diff)) %>%
  mutate(panel = sapply(variable_group, axis_label),
         x = factor(station_name(station), levels = STATION_CODES))

tol_df <- distinct(plot_df, panel, tol)

p <- ggplot(plot_df, aes(x, diff)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_hline(data = tol_df, aes(yintercept = tol),  linetype = "dashed", color = "firebrick") +
  geom_hline(data = tol_df, aes(yintercept = -tol), linetype = "dashed", color = "firebrick") +
  geom_errorbar(aes(ymin = diff - psychrometer_unc, ymax = diff + psychrometer_unc, color = station),
                width = 0.15, alpha = 0.6) +
  geom_point(aes(color = station), size = 3.2) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_color_manual(values = STATION_COLORS, guide = "none") +
  scale_y_continuous(labels = dec_comma()) +
  labs(title = "AWS-Feldmessung vs. Aspirationspsychrometer",
       subtitle = paste0("Eine Ablesung pro Standort, AWS gemittelt über ±",
                         PSYCHRO_AVG_HALF_WINDOW_MIN, " min, ", PSYCHRO_SENSOR, "-Sensor"),
       x = NULL, y = "Abweichung AWS − Psychrometer",
       caption = paste0("Rot gestrichelt: Herstellergenauigkeit der AWS-Sensoren.\n",
                        "Fehlerbalken: Unsicherheit des Psychrometers bei ±", fmt(u, 1),
                        " K Ablesegenauigkeit.")) +
  theme_mgp() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p, file.path(FIGURE_DIR, "psychrometer_vergleich.png"), FIG_SIZE$psychrometer)
