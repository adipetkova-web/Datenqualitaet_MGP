# ============================================================
# config.R — alle Einstellungen an EINER Stelle
#
# Wird von jedem Analyse-Skript zuerst eingebunden. Wenn sich ein
# Zeitraum, eine Toleranz, eine Farbe oder eine Plotgroesse aendern
# soll: nur hier anpassen, alle Skripte uebernehmen es automatisch.
# ============================================================

#### Pfade ####

DATA_DIR    <- "data/raw"
TABLE_DIR   <- "results/tables"
FIGURE_DIR  <- "results/figures"
PSYCHRO_CSV <- "data/psychrometer/psychrometer_messungen.csv"

#### Stationen ####

STATIONS <- c("20", "30", "40", "50")

# Namen fuer Beschriftungen in Plots (aus data/metadata/AWS_Metadaten.xlsx).
# Ordner- und Spaltennamen bleiben bei den Nummern (AWS20, air_temp_1m_20 ...).
# Lieber ausgeschriebene Namen? Einfach z.B. "20" = "Acker" eintragen.
STATION_CODES <- c(
  "20" = "AK",   # Acker
  "30" = "MW",   # Mischwald
  "40" = "KF",   # Kiefernforst
  "50" = "FW"    # Feuchtwiese
)

# Waehrend der Vergleichsmessung standen alle vier Stationen am
# selben Ort -> gleiche Hoehe, dz = 0 fuer alle.
STATION_Z <- c(69, 69, 69, 69)  # m ue. NN

# Zeitzone der Logger: UTC+1. Achtung, POSIX-Konvention dreht das
# Vorzeichen um: "Etc/GMT-1" bedeutet UTC+1.
TZ <- "Etc/GMT-1"

# Zeitliche Aufloesung: 1 = Minutenwerte (keine Aggregation)
AGG_MINUTES <- 1

# WICHTIG zu "1m" / "3m":
# Bei der Vergleichsmessung waren ALLE Temperatur-/Feuchtesensoren
# auf 1,6 m montiert. Die Suffixe 1m/3m in den Spaltennamen kommen
# vom Logger (Temp_b = unten = "1m", Temp_t = oben = "3m") und sind
# hier nur Sensor-Kennungen, keine echte Messhoehe.

#### Feldmessung ####

# Feld-Daten liegen neben den Testdaten: data/raw/AWS20_Feld/ usw.
FIELD_SUFFIX    <- "_Feld"
FIELD_STATION_Z <- c(68, 63, 74, 63)  # m ue. NN, aus data/metadata/AWS_Metadaten.xlsx
FIELD_YEAR      <- 2026               # fuer Datumsangaben ohne Jahr ("3.09")

# Zeitzone der Psychrometer-Uhrzeiten in psychrometer_messungen.csv.
# Protokoll in UTC, beim Eintragen +1 h gerechnet -> UTC+1 = "Etc/GMT-1",
# gleiche Zeit wie die Logger.
PSYCHRO_TZ <- "Etc/GMT-1"

# Das Psychrometer wurde auf Hoehe des unteren Sensors gemessen
PSYCHRO_SENSOR <- "1m"

PSYCHRO_AVG_HALF_WINDOW_MIN <- 2    # AWS-Mittel ueber +/- 2 min um die Ablesung
PSYCHRO_READ_UNC_K          <- 0.1  # Ablesegenauigkeit der Thermometer (K)

#### Variablen ####
#
# Pro Variablengruppe:
#   title       Titel in Plots
#   unit        Einheit fuer Achsen
#   t0, t1      Analysezeitraum (UTC+1)
#   window      kurze Beschreibung des Zeitraums (fuer Untertitel)
#   day_only    TRUE = nur Zeitpunkte, an denen mind. eine Station
#               Globalstrahlung >= DAY_THRESHOLD_WM2 misst
#   tol         Herstellerangabe zur Messgenauigkeit (+/- tol)
#   tol_mode    "absolute" (in der Einheit) oder "percent" (% vom Referenzwert)
#   tol_source  woher die Toleranz kommt (wird im Plot angezeigt)

DAY_THRESHOLD_WM2 <- 10

VARIABLES <- list(
  air_temp = list(
    title = "Lufttemperatur", unit = "°C", diff_unit = "K",
    t0 = "2026-09-01 18:00:00", t1 = "2026-09-02 04:00:00",
    window = "fester Nachtzeitraum",
    day_only = FALSE,
    tol = 0.4, tol_mode = "absolute",
    tol_source = "CS215: ±0,4 °C (5–40 °C)"
  ),
  rel_hum = list(
    title = "Relative Luftfeuchte", unit = "%",
    t0 = "2026-09-01 18:00:00", t1 = "2026-09-02 04:00:00",
    window = "fester Nachtzeitraum",
    day_only = FALSE,
    # CS215: +/-2 % fuer 10-90 % RH, +/-4 % fuer 0-100 % RH.
    # Nachts geht RH Richtung 95 %, also gilt der weitere Bereich.
    tol = 4, tol_mode = "absolute",
    tol_source = "CS215: ±4 % (0–100 %)"
  ),
  R_sw_d = list(
    title = "Globalstrahlung", unit = "W/m²",
    t0 = "2026-09-01 15:00:00", t1 = "2026-09-01 18:00:00",
    window = paste0("nur Tag (≥ ", DAY_THRESHOLD_WM2, " W/m²)"),
    day_only = TRUE,
    # Datenblatt: +/-5 % bezogen auf Tagessummen -> als % vom Referenzwert
    tol = 5, tol_mode = "percent",
    tol_source = "Datenblatt: ±5 % (Tagessumme)"
  ),
  R_net = list(
    title = "Nettostrahlung", unit = "W/m²",
    # Start 15:00 statt 11:00, um die stark schwankende,
    # wolkenbedingte Phase am Vormittag auszuschliessen
    t0 = "2026-09-01 15:00:00", t1 = "2026-09-02 07:00:00",
    window = "Tag + Nacht",
    day_only = FALSE,
    # NR-Lite2: kein einfacher Gesamtwert im Datenblatt. 30 W/m2 ist
    # der Richtungsfehler (0-60 Grad bei 1000 W/m2) -> nur grobe Orientierung.
    tol = 30, tol_mode = "absolute",
    tol_source = "NR Lite2: ±30 W/m² (Richtungsfehler, Näherung)"
  )
)

#### Plot-Layout (fuer ALLE Abbildungen gleich) ####

STATION_COLORS <- c(
  "20" = "#D9A400",  # gold (dunkler als vorher, besser lesbar)
  "30" = "#1B7837",  # gruen
  "40" = "#4A9FD8",  # hellblau (kraeftiger als "lightblue")
  "50" = "#08306B"   # dunkelblau
)

BASE_FONT_SIZE <- 13
PNG_DPI        <- 150

FIG_SIZE <- list(                         # Breite x Hoehe in Pixel
  timeseries    = c(1600, 900),
  scatter       = c(900, 900),
  scatter_group = c(1600, 1000),
  boxplot       = c(1200, 900),
  gradient      = c(1200, 800),
  psychrometer  = c(1200, 900)
)
