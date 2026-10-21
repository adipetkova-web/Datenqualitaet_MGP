# ============================================================
# helpers.R — gemeinsame Funktionen fuer die Datenqualitaets-Analyse
#
# Einbinden mit:  source("R/helpers.R")
# (bindet automatisch config.R und die Kurs-Skripte in R/course/ ein)
#
# Inhalt:
#   1. Setup (Pakete, Kurs-Skripte, Arbeitsverzeichnis pruefen)
#   2. Daten einlesen (pro Variablengruppe und Zeitfenster)
#   3. Spaltennamen & Beschriftungen
#   4. Referenzwahl, Regression, RMSD/MAD
#   5. Einheitliches Plot-Layout
# ============================================================

#### 1. Setup ####

if (!file.exists("Datenqualitaet_MGP.Rproj")) {
  stop("Bitte im Projektordner ausfuehren (dort, wo Datenqualitaet_MGP.Rproj liegt). ",
       "In RStudio: Projekt oeffnen, dann laeuft alles relativ zum Projektordner.")
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(reshape2)
})

source("R/config.R")
source("R/course/met.R")
source("R/course/AWS_tools.R")
source("R/course/AWS_diagnostics.R")
source("R/course/AWS_read.R")

dir.create(TABLE_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_DIR, showWarnings = FALSE, recursive = TRUE)

section <- function(title) cat("\n==== ", title, " ====\n", sep = "")

as_time <- function(x) as.POSIXct(x, tz = TZ)

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

#### 2. Daten einlesen ####

# Liest alle Stationen fuer [t0, t1] ein und fuehrt sie zusammen.
# Spalten heissen danach <variable>_<station>, z.B. air_temp_1m_20.
#
# agg_df() beschriftet jeden Block mit dem Intervall-ENDE; hier wird
# auf Intervall-ANFANG umbenannt (nur Beschriftung, keine Werte
# werden verschoben).
#
# Ergebnisse werden zwischengespeichert, damit dasselbe Zeitfenster
# nicht mehrfach eingelesen wird.
.read_cache <- new.env()

#   suffix = ""      -> Vergleichsmessung (data/raw/AWS20)
#   suffix = "_Feld" -> Feldmessung      (data/raw/AWS20_Feld), z = FIELD_STATION_Z
read_stations <- function(t0, t1, agg_minutes = AGG_MINUTES, suffix = "", z = STATION_Z) {
  key <- paste(format(t0), format(t1), agg_minutes, suffix)
  if (!is.null(.read_cache[[key]])) return(.read_cache[[key]])

  dz    <- z - min(z)
  paths <- file.path(DATA_DIR, paste0("AWS", STATIONS, suffix))
  missing <- paths[!dir.exists(paths)]
  if (length(missing) > 0) stop("Ordner nicht gefunden: ", paste(missing, collapse = ", "))

  per_station <- lapply(seq_along(STATIONS), function(i) {
    raw <- AWS_read_data(paths[i], t0 = t0, t1 = t1, dz = dz[i])
    agg_df(raw, agg_minutes)
  })

  joined   <- merge_dfs_with_suffix(per_station, STATIONS)
  joined$t <- joined$t - agg_minutes * 60

  .read_cache[[key]] <- joined
  joined
}

# Tag = mindestens eine Station misst Globalstrahlung >= Schwelle.
# (Gegenteil von "alle Stationen unter der Schwelle".)
day_mask <- function(df, threshold = DAY_THRESHOLD_WM2) {
  cols <- measurement_columns(df, "R_sw_d")
  if (length(cols) == 0) stop("Keine Globalstrahlungs-Spalten gefunden.")
  m <- sapply(cols, function(col) safe_numeric(df[[col]]))
  apply(m, 1, function(row) {
    row <- row[is.finite(row)]
    if (length(row) == 0) return(FALSE)
    any(row >= threshold)
  })
}

# Daten fuer EINE Variablengruppe, so wie sie analysiert werden:
# richtiges Zeitfenster, ggf. nur Tageswerte.
variable_data <- function(var_group) {
  cfg <- VARIABLES[[var_group]]
  df  <- read_stations(as_time(cfg$t0), as_time(cfg$t1))
  if (isTRUE(cfg$day_only)) df <- df[day_mask(df), , drop = FALSE]
  df
}

#### 3. Spaltennamen & Beschriftungen ####

# Findet alle Messspalten einer Gruppe, z.B. air_temp_1m_20 oder R_sw_d_20
measurement_columns <- function(df, var_group) {
  pattern <- paste0("^", var_group, "_([0-9]+m_)?(", paste(STATIONS, collapse = "|"), ")$")
  cols <- names(df)[grepl(pattern, names(df))]
  # nur Spalten mit mindestens einem gueltigen Wert
  cols[sapply(cols, function(col) any(is.finite(safe_numeric(df[[col]]))))]
}

station_of <- function(col) sub(".*_", "", col)

# Stationsnummer -> Kuerzel fuer Plots, z.B. "20" -> "AK"
station_name <- function(st) unname(STATION_CODES[as.character(st)])

sensor_of <- function(col) {
  vapply(strsplit(col, "_", fixed = TRUE), function(parts) {
    n <- length(parts)
    if (n >= 2 && grepl("^[0-9]+m$", parts[n - 1])) parts[n - 1] else NA_character_
  }, character(1))
}

# "air_temp_1m_20" -> "AK · 1m-Sensor",  "R_sw_d_20" -> "AK"
sensor_label <- function(col) {
  s <- sensor_of(col)
  ifelse(is.na(s), station_name(station_of(col)),
         paste0(station_name(station_of(col)), " · ", s, "-Sensor"))
}

axis_label <- function(var_group, prefix = NULL) {
  cfg <- VARIABLES[[var_group]]
  paste0(if (!is.null(prefix)) paste0(prefix, " ") else "", cfg$title, " (", cfg$unit, ")")
}

window_label <- function(var_group) {
  cfg <- VARIABLES[[var_group]]
  paste0(format(as_time(cfg$t0), "%d.%m. %H:%M"), "–",
         format(as_time(cfg$t1), "%d.%m. %H:%M"), " (UTC+1), ", cfg$window)
}

# Zahlen mit deutschem Dezimalkomma (fuer Plots)
fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(round(x, digits), format = "f", digits = digits, decimal.mark = ","))
}

make_equation <- function(slope, offset, digits = 3) {
  if (is.na(slope) || is.na(offset)) return("nicht berechenbar")
  paste0("y = ", fmt(slope, digits), "x ", if (offset >= 0) "+ " else "- ", fmt(abs(offset), digits))
}

#### 4. Referenzwahl, Regression, RMSD/MAD ####

# Referenz = der Sensor, dessen Mittelwert am naechsten am
# Mittelwert aller Sensoren der Gruppe liegt.
find_reference <- function(df, cols) {
  means <- sapply(cols, function(col) mean(safe_numeric(df[[col]]), na.rm = TRUE))
  cols[which.min(abs(means - mean(means, na.rm = TRUE)))]
}

# Vergleicht jeden Sensor einer Gruppe mit der Referenz.
# Gibt zurueck:
#   reference   Name der Referenzspalte
#   stats       eine Zeile pro Vergleichssensor
#   deviations  Abweichung (Sensor - Referenz) pro Zeitpunkt
compare_sensors <- function(var_group, df = variable_data(var_group)) {
  cfg  <- VARIABLES[[var_group]]
  cols <- measurement_columns(df, var_group)
  if (length(cols) < 2) stop("Weniger als zwei gueltige Sensoren fuer ", var_group)

  ref  <- find_reference(df, cols)
  x    <- safe_numeric(df[[ref]])

  per_sensor <- lapply(setdiff(cols, ref), function(col) {
    y     <- safe_numeric(df[[col]])
    ok    <- is.finite(x) & is.finite(y)
    dev   <- y[ok] - x[ok]
    pct   <- dev / x[ok] * 100
    fit   <- lm(y[ok] ~ x[ok])
    slope <- unname(coef(fit)[2]); offset <- unname(coef(fit)[1])

    stats <- data.frame(
      variable_group      = var_group,
      reference_variable  = ref,
      comparison_variable = col,
      n_pairs     = sum(ok),
      RMSD        = sqrt(mean(dev^2)),
      MAD         = mean(abs(dev)),
      bias        = mean(dev),          # mittlere Abweichung (Sensor - Referenz)
      sd_dev      = sd(dev),
      RMSD_pct    = if (cfg$tol_mode == "percent") sqrt(mean(pct^2)) else NA_real_,
      r_squared   = summary(fit)$r.squared,
      slope       = slope,
      offset      = offset,
      residual_sd = sd(residuals(fit)),
      within_tol_pct = 100 * mean(abs(if (cfg$tol_mode == "percent") pct else dev) <= cfg$tol),
      equation    = make_equation(slope, offset),
      stringsAsFactors = FALSE
    )
    devs <- data.frame(
      variable_group = var_group, reference_variable = ref, comparison_variable = col,
      t = df$t[ok], reference = x[ok], value = y[ok],
      deviation = dev, percent_deviation = pct,
      stringsAsFactors = FALSE
    )
    list(stats = stats, devs = devs)
  })

  list(
    reference  = ref,
    stats      = bind_rows(lapply(per_sensor, `[[`, "stats")),
    deviations = bind_rows(lapply(per_sensor, `[[`, "devs"))
  )
}

# Anteil gueltiger Minutenwerte pro Sensor im Analysezeitraum
coverage_table <- function(var_group) {
  cfg <- VARIABLES[[var_group]]
  df  <- read_stations(as_time(cfg$t0), as_time(cfg$t1))
  expected <- as.numeric(difftime(as_time(cfg$t1), as_time(cfg$t0), units = "mins")) / AGG_MINUTES
  bind_rows(lapply(measurement_columns(df, var_group), function(col) {
    n <- sum(is.finite(safe_numeric(df[[col]])))
    data.frame(variable_group = var_group, variable = col, n_valid = n,
               n_expected = expected, coverage_pct = 100 * n / expected)
  }))
}

#### 5. Einheitliches Plot-Layout ####

theme_mgp <- function() {
  theme_minimal(base_size = BASE_FONT_SIZE) +
    theme(
      plot.title       = element_text(size = rel(1.3), face = "bold"),
      plot.subtitle    = element_text(size = rel(0.9), color = "grey35"),
      plot.caption     = element_text(size = rel(0.75), color = "grey45", hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.4),
      strip.text       = element_text(face = "bold", size = rel(0.95)),
      legend.position  = "right"
    )
}

scale_color_stations <- function(...) {
  scale_color_manual(values = STATION_COLORS, labels = station_name(names(STATION_COLORS)),
                     name = "Station", ...)
}

dec_comma <- function(...) scales::label_number(decimal.mark = ",", big.mark = ".", ...)

save_png <- function(plot, file, size) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggsave(file, plot = plot, width = size[1], height = size[2], units = "px", dpi = PNG_DPI, bg = "white")
  cat("  gespeichert:", file, "\n")
}

write_table <- function(df, name) {
  file <- file.path(TABLE_DIR, name)
  write.csv(df, file, row.names = FALSE)
  cat("  gespeichert:", file, "\n")
}
