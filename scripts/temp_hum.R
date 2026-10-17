# ============================================================
# Scatterplots — AWS Sensorvergleich
#
# Vergleicht Sensoren (Temperatur, Feuchte) zwischen
# Stationen: wählt pro Variablengruppe eine Referenzvariable
# und erstellt Scatterplots + Regressionsstatistik (R^2,
# Steigung, Offset).
# ============================================================

#### Libraries ####

library("ggplot2")
library("reshape2")
library("dplyr")
library("purrr")
library("tidyr")
library("Cairo")
library("corrplot")

#### Projekt-Skripte ####

source("scripts/AWS/AWS_diagnostics.R")
source("scripts/AWS/AWS_read.R")
source("scripts/AWS/AWS_tools.R")
source("scripts/AWS/met.R")

#### Einstellungen ####

data_dir <- "data"
stations <- c("20", "30", "40", "50")
z        <- c(69, 69, 69, 69)

# Analysezeitraum
t0 <- as.POSIXct("2026-09-01 11:00:00", tz = "Etc/GMT-1")
t1 <- as.POSIXct("2026-09-02 07:00:00", tz = "Etc/GMT-1")

# Nur diese Variablengruppen analysieren
analysis_variables <- c("air_temp", "rel_hum")

# PNG-Einstellungen
png_width  <- 500
png_height <- 500
png_dpi    <- 150

station_colors <- c(
  "20" = "#F8766D", "30" = "#7CAE00",
  "40" = "#00BFC4", "50" = "#C77CFF"
)

# Kleiner Helfer, damit die Konsolen-Banner nicht 5x pro
# Abschnitt kopiert werden muessen.
section <- function(title) {
  cat("\n============================================\n")
  cat(title, "\n")
  cat("============================================\n\n")
}

#### Hilfsfunktionen ####
# Diese Funktionen sind der Grund, warum der Rest des Skripts
# unten NICHT fuer jede Station/Variable einzeln ausgeschrieben
# werden muss - sie kapseln die Logik einmal und werden dann
# in Schleifen wiederverwendet.

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# air_temp_1m_20 -> Station "20", Hoehe "1m"
parse_column_name <- function(column_name) {
  parts <- strsplit(column_name, "_", fixed = TRUE)[[1]]
  n <- length(parts)
  list(station = parts[n], height = parts[n - 1])
}
get_station_from_name <- function(column_name) parse_column_name(column_name)$station
get_height_from_name  <- function(column_name) parse_column_name(column_name)$height

# Alle Messspalten einer Variablengruppe finden
# (z.B. air_temp_1m_20, air_temp_3m_20, air_temp_1m_30, ...)
get_measurement_columns <- function(df, variable_group) {
  pattern <- paste0(
    "^", variable_group, "_(1m|3m)_(", paste(stations, collapse = "|"), ")$"
  )
  names(df)[grepl(pattern, names(df))]
}

# Haeufigstes Messintervall (Minuten) aus einem Zeitstempel-Vektor.
# Einzelne fehlende Werte veraendern das erkannte Intervall nicht.
detect_interval <- function(time_vector) {
  time_vector <- sort(unique(time_vector[!is.na(time_vector)]))
  if (length(time_vector) < 2) return(NA_real_)
  
  diffs <- diff(as.numeric(time_vector))
  diffs <- diffs[is.finite(diffs) & diffs > 0]
  if (length(diffs) == 0) return(NA_real_)
  
  diffs_min <- round(diffs / 60, 6)
  counts <- table(diffs_min)
  as.numeric(names(counts)[which.max(counts)])
}

calculate_expected_n <- function(start_time, end_time, interval_minutes) {
  if (is.na(interval_minutes) || interval_minutes <= 0) return(NA_real_)
  duration_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  floor(duration_minutes / interval_minutes) + 1
}

calculate_variable_mean <- function(df, column) {
  values <- safe_numeric(df[[column]])
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NA_real_)
  mean(values)
}

# Referenz = die Variable, deren Mittelwert am naechsten am
# Mittelwert aller verfuegbaren Variablen der Gruppe liegt
# ("liegt am mittigsten").
find_best_reference <- function(df, measurement_columns) {
  means <- sapply(measurement_columns, function(col) calculate_variable_mean(df, col))
  valid <- means[is.finite(means)]
  if (length(valid) == 0) return(NA_character_)
  
  group_mean <- mean(valid)
  distances <- abs(means - group_mean)
  distances[!is.finite(distances)] <- Inf
  names(means)[which.min(distances)]
}

# y = slope * x + offset   (x = Referenz, y = Vergleichsvariable)
# Nur Zeitpunkte, an denen beide Sensoren einen gueltigen Wert
# haben, gehen in die Regression ein.
calculate_regression <- function(df, reference_column, comparison_column) {
  x <- safe_numeric(df[[reference_column]])
  y <- safe_numeric(df[[comparison_column]])
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  n_pairs <- length(x)
  
  empty <- data.frame(
    n_pairs = n_pairs, r_squared = NA_real_, slope = NA_real_,
    offset = NA_real_, residual_mean = NA_real_, residual_sd = NA_real_
  )
  # Regression braucht mindestens zwei Werte und Variation in x
  if (n_pairs < 2 || length(unique(x)) < 2) return(empty)
  
  model <- lm(y ~ x)
  coefs <- coef(model)
  residuals <- y - predict(model)
  
  data.frame(
    n_pairs       = n_pairs,
    r_squared     = summary(model)$r.squared,
    slope         = unname(coefs[2]),
    offset        = unname(coefs[1]),
    residual_mean = mean(residuals, na.rm = TRUE),
    residual_sd   = if (length(residuals) > 1) sd(residuals, na.rm = TRUE) else NA_real_
  )
}

format_number <- function(x, digits = 3) {
  if (is.na(x)) return("NA")
  format(round(x, digits), nsmall = digits, scientific = FALSE)
}

make_equation <- function(slope, offset) {
  if (is.na(slope) || is.na(offset)) return("y = nicht berechenbar")
  sign <- if (offset >= 0) "+" else "-"
  paste0("y = ", format_number(abs(slope)), "x ", sign, " ", format_number(abs(offset)))
}

# R^2 + Gleichung als ein Annotation-Text (fuer die Plot-Labels)
annotation_text <- function(stats) {
  paste0("R\u00b2 = ", format_number(stats$r_squared), "\n", make_equation(stats$slope, stats$offset))
}

#### Daten einlesen & zusammenfuehren ####

section("Lese AWS-Daten ein...")

station_meta <- data.frame(station = stations, z = z, dz = z - min(z))
paths <- file.path("data_2026", paste0("AWS", stations))

aws_1min_list <- lapply(seq_along(stations), function(i) {
  raw <- AWS_read_data(paths[i], t0 = t0, t1 = t1, dz = station_meta$dz[i])
  agg_df(raw, 1)
})

aws_joined_1min <- merge_dfs_with_suffix(aws_1min_list, stations)

if (!"t" %in% names(aws_joined_1min)) {
  stop("Die zusammengefuehrten Daten enthalten keine Zeitspalte 't'.")
}

section("Verfuegbare Variablen:")

#### Variablen-Beschriftungen ####

variable_labels <- list(
  air_temp = list(title = "Lufttemperatur",       y = "Lufttemperatur (\u00b0C)"),
  rel_hum  = list(title = "Relative Luftfeuchte",  y = "Relative Luftfeuchte (%)")
)

get_variable_label <- function(variable_group) {
  labels <- variable_labels[[variable_group]]
  if (is.null(labels)) list(title = variable_group, y = variable_group) else labels
}


#### Lineplots #### 

lineplot_func <- function(variable_group) {
  measurement_columns <- get_measurement_columns(aws_joined_1min, variable_group)
  if (length(measurement_columns) == 0) {
    warning(paste("Keine Messvariablen fuer", variable_group, "gefunden."))
    return(NULL)
  }
  
  plot_data <- do.call(rbind, lapply(measurement_columns, function(col) {
    data.frame(
      t = aws_joined_1min$t,
      value = safe_numeric(aws_joined_1min[[col]]),
      variable = col,
      station = get_station_from_name(col),
      height = get_height_from_name(col),
      stringsAsFactors = FALSE
    )
  }))
  
  labels <- get_variable_label(variable_group)
  date_range_text <- paste(format(t0, "%d.%m.%Y %H:%M"), "\u2013", format(t1, "%d.%m.%Y %H:%M"))
  
  ggplot(plot_data, aes(x = t, y = value, group = variable, color = station)) +
    geom_line(na.rm = TRUE) +
    scale_color_manual(values = station_colors) +
    # Achse startet/endet exakt am Messzeitraum, kein Leerraum davor/danach
    scale_x_datetime(
      limits = c(t0, t1), expand = c(0, 0),
      date_breaks = "2 hours", date_minor_breaks = "1 hour",
      date_labels = "%d.%m\n%H:%M"
    ) +
    labs(
      title = paste(labels$title, "\u2013 Zeitverlauf"),
      subtitle = date_range_text,
      x = "Zeit", y = labels$y, color = "Station"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey40"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      # dichteres Gitter: Haupt- alle 2h, Neben-Gitter alle 1h
      panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.25)
    )
}

for (v in analysis_variables) print(lineplot_func(v))

#### Messabdeckung ####

coverage_results <- list()

for (current_group in analysis_variables) {
  measurement_columns <- get_measurement_columns(aws_joined_1min, current_group)
  if (length(measurement_columns) == 0) next
  
  for (col in measurement_columns) {
    values <- safe_numeric(aws_joined_1min[[col]])
    valid <- is.finite(values)
    n_valid <- sum(valid)
    
    detected_interval <- detect_interval(aws_joined_1min$t[valid])
    expected_n <- calculate_expected_n(t0, t1, detected_interval)
    
    if (is.na(expected_n) || expected_n <= 0) {
      coverage_fraction <- NA_real_
      coverage_percent  <- NA_real_
    } else {
      coverage_fraction <- n_valid / expected_n
      coverage_percent  <- coverage_fraction * 100
    }
    
    coverage_results[[length(coverage_results) + 1]] <- data.frame(
      variable_group = current_group, variable = col,
      station = get_station_from_name(col), height = get_height_from_name(col),
      interval_minutes = detected_interval, n = n_valid, expected_n = expected_n,
      n_fraction = coverage_fraction, n_percent = coverage_percent,
      stringsAsFactors = FALSE
    )
  }
}

coverage_table <- if (length(coverage_results) > 0) do.call(rbind, coverage_results) else data.frame()
rownames(coverage_table) <- NULL

section("MESSABDECKUNG")

write.csv(coverage_table, "scatter_measurement_coverage.csv", row.names = FALSE)

#### Scatterplot-Theme (gemeinsam fuer Einzel- & Gruppenplot) ####

scatter_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

#### Scatterplot-Funktion: einzelner Plot ####

create_single_scatter_plot <- function(data, variable_group, reference_column, comparison_column, stats) {
  x <- safe_numeric(data[[reference_column]])
  y <- safe_numeric(data[[comparison_column]])
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) == 0) return(NULL)
  
  plot_data <- data.frame(reference = x[valid], comparison = y[valid])
  comparison_station <- get_station_from_name(comparison_column)
  
  ggplot(plot_data, aes(x = reference, y = comparison)) +
    geom_point(color = station_colors[comparison_station], alpha = 0.45, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.9, na.rm = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.8) +
    annotate(
      "label", x = -Inf, y = Inf, label = annotation_text(stats),
      hjust = -0.08, vjust = 1.15, size = 4.3, fontface = "bold"
    ) +
    labs(
      title = paste(variable_group, "\u2013", comparison_column),
      subtitle = paste("Referenz:", reference_column),
      x = reference_column, y = comparison_column
    ) +
    scatter_theme
}

#### Scatterplot-Funktion: Gruppenplot (facettiert) ####

create_group_scatter_plot <- function(data, variable_group, reference_column, comparison_columns, stats_list) {
  x_ref <- safe_numeric(data[[reference_column]])
  
  plot_rows <- lapply(comparison_columns, function(col) {
    y <- safe_numeric(data[[col]])
    valid <- is.finite(x_ref) & is.finite(y)
    if (sum(valid) == 0) return(NULL)
    data.frame(
      reference = x_ref[valid], comparison = y[valid], variable = col,
      station = get_station_from_name(col), stringsAsFactors = FALSE
    )
  })
  plot_rows <- plot_rows[!sapply(plot_rows, is.null)]
  if (length(plot_rows) == 0) return(NULL)
  plot_data <- do.call(rbind, plot_rows)
  
  annotation_data <- do.call(rbind, lapply(comparison_columns, function(col) {
    data.frame(variable = col, annotation = annotation_text(stats_list[[col]]), stringsAsFactors = FALSE)
  }))
  
  ggplot(plot_data, aes(x = reference, y = comparison)) +
    geom_point(aes(color = station), alpha = 0.45, size = 1.6) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8, na.rm = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    scale_color_manual(values = station_colors, name = "Station") +
    geom_label(
      data = annotation_data, aes(x = -Inf, y = Inf, label = annotation),
      inherit.aes = FALSE, hjust = -0.08, vjust = 1.15, size = 3.8, fontface = "bold"
    ) +
    labs(
      title = paste(variable_group, "\u2013 Scatterplot-Gruppe"),
      subtitle = paste("Referenz:", reference_column), x = reference_column, y = NULL
    ) +
    scatter_theme +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      panel.spacing = unit(1.3, "lines")
    )
}

#### Hauptanalyse ####

scatter_statistics_list <- list()
reference_results_list  <- list()

for (current_group in analysis_variables) {
  section(paste("ANALYSE:", current_group))
  
  measurement_columns <- get_measurement_columns(aws_joined_1min, current_group)
  if (length(measurement_columns) == 0) {
    warning(paste("Keine Messvariablen fuer", current_group, "gefunden."))
    next
  }
  
  # Nur Variablen mit mindestens einem gueltigen Wert verwenden.
  # Variablen mit z.B. 50% fehlenden Daten bleiben erhalten.
  usable_columns <- measurement_columns[sapply(measurement_columns, function(col) {
    sum(is.finite(safe_numeric(aws_joined_1min[[col]]))) > 0
  })]
  if (length(usable_columns) == 0) {
    warning(paste("Keine gueltigen Daten fuer", current_group, "gefunden."))
    next
  }
  
  reference_column <- find_best_reference(aws_joined_1min, usable_columns)
  if (is.na(reference_column)) {
    warning(paste("Keine Referenzvariable fuer", current_group, "bestimmbar."))
    next
  }
  cat("Gewaehlte Referenzvariable:", reference_column, "\n\n")
  
  reference_results_list[[length(reference_results_list) + 1]] <- data.frame(
    variable_group = current_group, reference_variable = reference_column,
    reference_station = get_station_from_name(reference_column),
    reference_height  = get_height_from_name(reference_column),
    stringsAsFactors = FALSE
  )
  
  comparison_columns <- usable_columns[usable_columns != reference_column]
  if (length(comparison_columns) == 0) {
    cat("Keine Vergleichsvariablen vorhanden.\n")
    next
  }
  
  # Regression pro Vergleichsvariable EINMAL berechnen und dann
  # fuer Tabelle, Einzelplots und Gruppenplot wiederverwenden -
  # vorher wurde lm() fuer jedes Paar bis zu dreimal aufgerufen.
  stats_list <- setNames(
    lapply(comparison_columns, function(col) calculate_regression(aws_joined_1min, reference_column, col)),
    comparison_columns
  )
  
  for (col in comparison_columns) {
    s <- stats_list[[col]]
    scatter_statistics_list[[length(scatter_statistics_list) + 1]] <- data.frame(
      variable_group = current_group, reference_variable = reference_column,
      reference_station = get_station_from_name(reference_column),
      reference_height  = get_height_from_name(reference_column),
      variable = col, station = get_station_from_name(col), height = get_height_from_name(col),
      n_pairs = s$n_pairs, r_squared = s$r_squared, slope = s$slope, offset = s$offset,
      residual_mean = s$residual_mean, residual_sd = s$residual_sd,
      equation = make_equation(s$slope, s$offset),
      stringsAsFactors = FALSE
    )
  }
  
  output_directory <- paste0("scatter_", current_group)
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
  
  cat("Erstelle einzelne Scatterplots...\n")
  for (col in comparison_columns) {
    single_plot <- create_single_scatter_plot(aws_joined_1min, current_group, reference_column, col, stats_list[[col]])
    if (is.null(single_plot)) {
      cat("  Keine gemeinsamen Daten:", col, "\n")
      next
    }
    print(single_plot)
    ggsave(
      filename = file.path(output_directory, paste0(current_group, "_", col, "_vs_", reference_column, ".png")),
      plot = single_plot, width = png_width, height = png_height, units = "px", dpi = png_dpi
    )
  }
  
  cat("\nErstelle Gruppenplot...\n")
  group_plot <- create_group_scatter_plot(aws_joined_1min, current_group, reference_column, comparison_columns, stats_list)
  if (!is.null(group_plot)) {
    print(group_plot)
    ggsave(
      filename = file.path(output_directory, paste0(current_group, "_GROUP_", reference_column, ".png")),
      plot = group_plot, width = png_width, height = png_height, units = "px", dpi = png_dpi
    )
  }
}

#### Ergebnistabellen ####

scatter_statistics <- if (length(scatter_statistics_list) > 0) do.call(rbind, scatter_statistics_list) else data.frame()
rownames(scatter_statistics) <- NULL

reference_table <- if (length(reference_results_list) > 0) do.call(rbind, reference_results_list) else data.frame()
rownames(reference_table) <- NULL

section("SCATTER-STATISTIK")
print(scatter_statistics)

section("REFERENZVARIABLEN")
print(reference_table)

write.csv(scatter_statistics, "scatter_statistics_air_temp_rel_hum.csv", row.names = FALSE)
write.csv(reference_table, "scatter_reference_variables.csv", row.names = FALSE)

#### Abschlussinformation ####

section("ANALYSE ABGESCHLOSSEN")

cat("Analysierte Variablengruppen:\n")
print(analysis_variables)

cat("\nReferenzvariablen:\n")
print(reference_table)

cat("\nMessabdeckung:\n")
print(coverage_table)

cat("\nPNG-Ordner:\n")
for (v in analysis_variables) cat(paste0("  scatter_", v, "/\n"))

cat("\nCSV-Dateien:\n")
cat("  scatter_statistics_air_temp_rel_hum.csv\n")
cat("  scatter_reference_variables.csv\n")
cat("  scatter_measurement_coverage.csv\n")

cat("\n============================================\n")