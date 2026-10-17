# Datenqualität MGP 2026

Auswertung der **Vergleichsmessung der vier automatischen Wetterstationen (AWS20, AWS30, AWS40, AWS50)** im Rahmen des Meteorologischen Geländepraktikums (MGP) 2026.
Bevor die Stationen im Feld verteilt wurden, standen sie nebeneinander am selben Ort. Damit lässt sich prüfen, wie gut die Sensoren untereinander übereinstimmen, bevor die eigentlichen Standortunterschiede interpretiert werden.

Bericht: Adelina Petkova, Fiona Lingk

---

## Schnellstart

1. Repository klonen oder als ZIP herunterladen.
2. `Datenqualitaet_MGP.Rproj` in RStudio öffnen.
3. Einmalig Pakete installieren (falls noch nicht vorhanden):
   ```r
   install.packages(c("ggplot2", "dplyr", "tidyr", "purrr", "reshape2", "scales"))
   ```
4. Alles neu rechnen:
   ```r
   source("run_all.R")
   ```

Alle Tabellen landen in `results/tables/`, alle Abbildungen in `results/figures/`.
Ein kompletter Durchlauf dauert etwa 30 Sekunden.

---

## Ordnerstruktur

```
├── run_all.R                    ← startet die komplette Auswertung
├── R/
│   ├── config.R                 ← ALLE Einstellungen (Zeiträume, Toleranzen, Farben, Plotgrößen)
│   ├── helpers.R                ← gemeinsame Funktionen (Einlesen, Referenz, RMSD, Plot-Layout)
│   └── course/                  ← Einlese- und Meteorologie-Funktionen aus der Kursvorlage (unverändert)
├── analysis/
│   ├── 01_sensorvergleich.R     ← Statistik: RMSD, MAD, Bias, Regression pro Sensor
│   ├── 02_abbildungen.R         ← Zeitverläufe, Scatterplots, Boxplots (alle im gleichen Layout)
│   ├── 03_gradientenfehler.R    ← Auswirkung der Sensorabweichungen auf Gradienten
│   └── 04_psychrometer.R        ← Vergleich mit dem Aspirationspsychrometer
├── data/
│   ├── raw/AWS20 … AWS50/       ← Logger-Rohdaten (TOA5, Minutenwerte, UTC+1)
│   ├── metadata/                ← Stations-Metadaten, Koordinaten, SVF, CLC-Karten, Variablenliste
│   └── psychrometer/            ← hier die Psychrometer-Ablesungen eintragen
└── results/
    ├── tables/                  ← CSV-Ergebnisse
    └── figures/                 ← PNG-Abbildungen für den Bericht
```

---

## Methode

**Referenzsensor:** Für jede Messgröße wird der Sensor als Referenz genommen, dessen Mittelwert am nächsten am Mittelwert aller Sensoren liegt. Alle anderen Sensoren werden gegen diese Referenz verglichen.

**Kennzahlen pro Sensor** (`results/tables/sensorvergleich_statistik.csv`):

| Spalte | Bedeutung |
|---|---|
| `RMSD` | Root Mean Square Deviation, √mean((Sensor − Referenz)²) |
| `MAD` | Mean Absolute Deviation, mean(\|Sensor − Referenz\|) |
| `bias` | mittlere Abweichung (Sensor − Referenz), mit Vorzeichen |
| `RMSD_pct` | RMSD in % vom Referenzwert (nur Globalstrahlung) |
| `r_squared`, `slope`, `offset` | lineare Regression Sensor = slope · Referenz + offset |
| `within_tol_pct` | Anteil der Minutenwerte innerhalb der Herstellergenauigkeit |

**Zeitfenster** (einstellbar in `R/config.R`):

| Messgröße | Zeitraum (UTC+1) | Begründung |
|---|---|---|
| Lufttemperatur, rel. Feuchte | 01.09. 18:00 – 02.09. 04:00 | Nacht, keine Strahlungsfehler durch Sonneneinstrahlung |
| Globalstrahlung | 01.09. 15:00 – 18:00, nur Werte ≥ 10 W/m² | Tag, dynamisch aus den Messwerten bestimmt |
| Nettostrahlung | 01.09. 15:00 – 02.09. 07:00 | Tag + Nacht. Start 15:00 statt 11:00, um die wolkenbedingt stark schwankende Phase auszuschließen |

**Herstellergenauigkeit** (rot gestrichelte Linien in den Boxplots):

| Messgröße | Toleranz | Quelle |
|---|---|---|
| Lufttemperatur | ± 0,4 °C | CS215, 5–40 °C |
| rel. Feuchte | ± 4 % | CS215, 0–100 % (± 2 % gilt nur für 10–90 %; nachts bis ~95 %) |
| Globalstrahlung | ± 5 % | Datenblatt, bezogen auf Tagessummen |
| Nettostrahlung | ± 30 W/m² | NR Lite2, Richtungsfehler (nur grobe Näherung) |

### Wichtig: „1m“ und „3m“ sind hier keine Höhen

Bei der Vergleichsmessung hingen **alle** Temperatur-/Feuchtesensoren auf 1,6 m. Die Endungen `_1m` / `_3m` kommen vom Logger (`Temp_b` = unten, `Temp_t` = oben) und sind hier nur Sensor-Kennungen.
Genau deshalb ist die Vergleichsmessung nützlich: Der Unterschied zwischen dem 1m- und dem 3m-Sensor *derselben* Station ist reiner Sensorfehler (siehe nächster Abschnitt).

---

## Auswirkung auf Gradienten (`03_gradientenfehler.R`)

Die Kurs-Skripte berechnen im Feld z. B. `dpot_temp_dz = (pot_temp_3m − pot_temp_1m) / 2 m`.
Jeder systematische Unterschied zwischen den beiden Sensoren einer Station geht 1:1 in diesen Gradienten ein. Aus der Vergleichsmessung:

| Station | Temperatur 3m − 1m | Schein-Gradient |
|---|---|---|
| AWS20 | −0,30 K | −0,15 K/m |
| AWS30 | +0,29 K | +0,15 K/m |
| AWS40 | +0,16 K | +0,08 K/m |
| AWS50 | −0,06 K | −0,03 K/m |

Das liegt zwar innerhalb der Herstellergenauigkeit (±0,4 K pro Sensor), ist aber in der Größenordnung typischer nächtlicher Gradienten in Bodennähe. Bei AWS20 und AWS30 haben die Abweichungen außerdem entgegengesetzte Vorzeichen, sodass auch Unterschiede *zwischen* den Stationen verzerrt werden.
Daraus lässt sich ableiten, ob die Felddaten mit den Offsets aus der Vergleichsmessung korrigiert werden sollten.

---

## Psychrometer-Vergleich (`04_psychrometer.R`)

Das Skript ist fertig, es fehlen nur noch die Messwerte. Ablesungen in
`data/psychrometer/psychrometer_messungen.csv` eintragen (Format siehe `psychrometer_BEISPIEL.csv`):

```
datum,uhrzeit,station,sensor,t_trocken,t_feucht,luftdruck_hpa,bemerkung
2026-09-01,19:30,20,1m,16.8,14.2,,
```

- `uhrzeit` in **UTC+1** (wie die Logger)
- `sensor` = neben welchem AWS-Sensor gemessen wurde (`1m` oder `3m`)
- `luftdruck_hpa` darf leer bleiben → dann wird der Luftdruck der Station verwendet

Das Skript berechnet die relative Feuchte aus Trocken- und Feuchttemperatur (WMO-Psychrometerformel für ventilierte Psychrometer), mittelt die AWS-Werte über ±2 min um die Ablesung und vergleicht beides.

---

## Was nicht im Repository ist

- **Ältere Skriptversionen:** Der ursprüngliche Stand (alle Skripte aus `scripts/`, inkl. der „…_neu.R“-Varianten) ist im ersten Commit gesichert (`git show 70dcd8e`). Die neue Struktur liefert für alle Kennzahlen exakt dieselben Werte wie `results_final/` aus diesem Stand.
- **Gerätehandbücher und Kursunterlagen** (`doc/`): urheberrechtlich geschützt bzw. nicht unsere Inhalte.
- **`Testmessungen_Rothenburgstr/`**: ältere, kürzere Kopie derselben Loggerdateien (endet am 26.08.). Vollständig in `data/raw/` enthalten.
- **`test_data/`**: Beispieldaten von 2023 aus der Kursvorlage.
- **Alte Plot-Ordner und `alle_plots_*.zip`**: werden jetzt von `run_all.R` neu erzeugt.
