# ============================================================
# AWS_scatter_helpers.R
#
# Gemeinsame Funktionen fuer die Scatterplot/Referenz-Analysen
# (Lufttemperatur/Feuchte, Globalstrahlung, ggf. weitere).
# Wird von den einzelnen Analyse-Skripten per source() eingebunden,
# damit die Logik nur EINMAL existiert und nicht pro Variable
# oder pro Skript erneut geschrieben werden muss.
#
# Ein Analyse-Skript, das diese Datei einbindet, muss VORHER
# folgende Variablen definiert haben:
#   stations, z, data_dir, t0, t1, station_colors,
#   single_plot_width, single_plot_height,
#   group_plot_width, group_plot_row_height,
#   lineplot_width, lineplot_height, png_dpi
# (und, falls run_variable_analysis(..., subset_label = "...")
#  mit einer Beschreibung des gefilterten Zeitraums genutzt wird)
# ============================================================

#### Libraries ####

library(ggplot2)
library(reshape2)
library(dplyr)
library(purrr)
library(tidyr)
library(Cairo)
library(corrplot)

#### Projekt-Skripte ####

source("scripts/AWS/AWS_diagnostics.R")
source("scripts/AWS/AWS_read.R")
source("scripts/AWS/AWS_tools.R")
source("scripts/AWS/met.R")

#### Konsolen-Ausgabe ####

section <- function(title) {
  cat("\n============================================\n")
  cat(title, "\n")
  cat("============================================\n\n")
}

#### Spaltennamen parsen ####

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# air_temp_1m_20 -> Station "20", Hoehe "1m"
# R_sw_d_50       -> Station "50", Hoehe NA (kein Hoehen-Token vorhanden)
# Station ist immer das letzte Element. Das vorletzte Element
# zaehlt nur als Hoehe, wenn es wirklich wie "1m"/"2m"/... aussieht -
# sonst wuerde z.B. bei "R_sw_d_50" faelschlich "d" als Hoehe gelesen,
# weil der Variablenname selbst Unterstriche enthaelt.
parse_column_name <- function(column_name) {
  parts <- strsplit(column_name, "_", fixed = TRUE)[[1]]
  n <- length(parts)
  station <- parts[n]
  height <- if (n >= 2 && grepl("^[0-9]+m$", parts[n - 1])) parts[n - 1] else NA_character_
  list(station = station, height = height)
}
get_station_from_name <- function(column_name) parse_column_name(column_name)$station
get_height_from_name  <- function(column_name) parse_column_name(column_name)$height

# Alle Messspalten einer Variablengruppe finden. Deckt zwei
# Namensschemata ab:
#   air_temp_1m_20   (Variable_Hoehe_Station - mehrere Hoehen)
#   R_sw_d_20        (Variable_Station - nur ein Messniveau)
# Die Hoehe ist deshalb optional, nicht auf "1m/3m" o.ae. fixiert.
get_measurement_columns <- function(df, variable_group) {
  pattern <- paste0(
    "^", variable_group, "_([0-9]+m_)?(", paste(stations, collapse = "|"), ")$"
  )
  names(df)[grepl(pattern, names(df))]
}

#### Messabdeckung ####

# Haeufigstes Messintervall (Minuten) aus einem Zeitstempel-Vektor.
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

# Baut die Messabdeckungs-Tabelle fuer eine Liste von Variablengruppen.
build_coverage_table <- function(data, analysis_variables, t0, t1) {
  coverage_results <- list()
  
  for (current_group in analysis_variables) {
    measurement_columns <- get_measurement_columns(data, current_group)
    if (length(measurement_columns) == 0) next
    
    for (col in measurement_columns) {
      values <- safe_numeric(data[[col]])
      valid <- is.finite(values)
      n_valid <- sum(valid)
      
      detected_interval <- detect_interval(data$t[valid])
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
  coverage_table
}

#### Daten einlesen & zusammenfuehren ####

# Liest alle Stationen ein und fuehrt sie zu einem gemeinsamen
# Datensatz zusammen. agg_minutes bestimmt das Aggregationsintervall
# (Standard: 60 = stuendlich, wie im Rest des Projekts, z.B. wind.R).
#
# agg_df() labelt jeden Aggregationsblock mit dem ENDE des
# Intervalls (der Block 11:00-11:59 bekommt den Zeitstempel 12:00).
# Das verschiebt hier NICHTS an den Rohdaten oder holt zusaetzliche
# Werte - es benennt nur die bereits vorhandenen Bloecke um, damit
# der Block 11:00-11:59 den Zeitstempel 11:00 traegt (Intervallanfang
# statt Intervallende).
read_and_join_stations <- function(stations, z, data_dir, t0, t1, agg_minutes = 60) {
  station_meta <- data.frame(station = stations, z = z, dz = z - min(z))
  paths <- file.path(data_dir, paste0("AWS", stations))
  
  data_agg_list <- lapply(seq_along(stations), function(i) {
    raw <- AWS_read_data(paths[i], t0 = t0, t1 = t1, dz = station_meta$dz[i])
    agg_df(raw, agg_minutes)
  })
  
  joined <- merge_dfs_with_suffix(data_agg_list, stations)
  if (!"t" %in% names(joined)) {
    stop("Die zusammengefuehrten Daten enthalten keine Zeitspalte 't'.")
  }
  
  # Umbenennung: Intervallende -> Intervallanfang
  joined$t <- joined$t - agg_minutes * 60
  joined
}

#### Nachtmaske ####

# Ein Zeitpunkt gilt als Nacht, wenn an ALLEN Stationen mit
# gueltigem Wert die Globalstrahlung unter threshold_wm2 liegt.
# "Alle Stationen" statt nur einer, damit ein einzelner
# verrauschter Sensor nicht faelschlich Tageszeit als Nacht markiert.
compute_night_mask <- function(data, radiation_columns, threshold_wm2 = 5) {
  if (length(radiation_columns) == 0) {
    warning("Keine Globalstrahlung-Spalten gefunden - Nachtmaske nicht verfuegbar.")
    return(rep(NA, nrow(data)))
  }
  radiation_matrix <- sapply(radiation_columns, function(col) safe_numeric(data[[col]]))
  apply(radiation_matrix, 1, function(row) {
    valid <- row[is.finite(row)]
    if (length(valid) == 0) return(NA)
    all(valid < threshold_wm2)
  })
}

#### Referenzvariable & Regression ####

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

# y = slope * x + offset  (x = Referenz, y = Vergleichsvariable)
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

annotation_text <- function(stats) {
  paste0("R\u00b2 = ", format_number(stats$r_squared), "\n", make_equation(stats$slope, stats$offset))
}

#### Variablen-Beschriftungen ####
# Titel + Achsenbeschriftung (mit Einheit) pro Variablengruppe.
# Neue Variable dazu? Einfach hier einen Eintrag ergaenzen - der
# Rest (Lineplot, Scatterplots, ...) funktioniert automatisch mit.
variable_labels <- list(
  air_temp = list(title = "Lufttemperatur",       y = "Lufttemperatur (\u00b0C)"),
  rel_hum  = list(title = "Relative Luftfeuchte",  y = "Relative Luftfeuchte (%)"),
  R_sw_d   = list(title = "Globalstrahlung",       y = "Globalstrahlung (W/m\u00b2)"),
  R_net    = list(title = "Nettostrahlung",        y = "Nettostrahlung (W/m\u00b2)")
)

get_variable_label <- function(variable_group) {
  labels <- variable_labels[[variable_group]]
  if (is.null(labels)) list(title = variable_group, y = variable_group) else labels
}

#### Lineplot ####

lineplot_func <- function(data, variable_group, min_value = NULL) {
  measurement_columns <- get_measurement_columns(data, variable_group)
  if (length(measurement_columns) == 0) {
    warning(paste("Keine Messvariablen fuer", variable_group, "gefunden."))
    return(NULL)
  }
  
  plot_data <- do.call(rbind, lapply(measurement_columns, function(col) {
    values <- safe_numeric(data[[col]])
    # Werte unter min_value werden NA statt geplottet - geom_line
    # bricht die Linie dort automatisch ab (Luecke).
    if (!is.null(min_value)) values[values < min_value] <- NA
    data.frame(
      t = data$t,
      value = values,
      variable = col,
      station = get_station_from_name(col),
      height = get_height_from_name(col),
      stringsAsFactors = FALSE
    )
  }))
  
  labels <- get_variable_label(variable_group)
  date_range_text <- paste(format(t0, "%d.%m.%Y %H:%M"), "\u2013", format(t1, "%d.%m.%Y %H:%M"))
  
  ggplot(plot_data, aes(x = t, y = value, group = variable, color = station)) +
    geom_line(na.rm = TRUE, linewidth = 0.7) +
    scale_color_manual(values = station_colors) +
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
      panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.25)
    )
}

#### Scatterplot-Theme (gemeinsam fuer Einzel- & Gruppenplot) ####

scatter_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

#### Scatterplot: einzelner Plot ####

create_single_scatter_plot <- function(data, variable_group, reference_column, comparison_column, stats) {
  x <- safe_numeric(data[[reference_column]])
  y <- safe_numeric(data[[comparison_column]])
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) == 0) return(NULL)
  
  plot_data <- data.frame(reference = x[valid], comparison = y[valid])
  comparison_station <- get_station_from_name(comparison_column)
  
  ggplot(plot_data, aes(x = reference, y = comparison)) +
    geom_point(color = station_colors[comparison_station], alpha = 0.85, size = 3) +
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

#### Scatterplot: Gruppenplot (facettiert) ####

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
    geom_point(aes(color = station), alpha = 0.85, size = 2.6) +
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

#### Kompletter Analyse-Durchlauf fuer eine Variablengruppe ####
# Fuehrt Referenzwahl, Regression, Tabellenzeilen und Plots
# (einzeln + Gruppe) fuer EINE Variablengruppe aus und speichert
# die PNGs nach scatter_<variable_group>/. Wird pro Variable aus
# dem jeweiligen Analyse-Skript aufgerufen (siehe dort).
#
# subset_label beschreibt, welcher Ausschnitt der Daten analysiert
# wird (z.B. "Ganzer Zeitraum", "Nur Nachtwerte (fester Zeitraum)",
# "Nur Tageswerte (Globalstrahlung >= 5 W/m^2)") und wird sowohl in
# der Konsolenausgabe als auch als Spalte "subset" in den
# Ergebnistabellen festgehalten - damit spaeter nachvollziehbar
# bleibt, worauf sich eine Zeile bezieht.
run_variable_analysis <- function(data, variable_group, subset_label = "Ganzer Zeitraum") {
  section(paste("ANALYSE:", variable_group))
  cat(subset_label, "\n")
  
  measurement_columns <- get_measurement_columns(data, variable_group)
  if (length(measurement_columns) == 0) {
    warning(paste("Keine Messvariablen fuer", variable_group, "gefunden."))
    return(NULL)
  }
  
  usable_columns <- measurement_columns[sapply(measurement_columns, function(col) {
    sum(is.finite(safe_numeric(data[[col]]))) > 0
  })]
  if (length(usable_columns) == 0) {
    warning(paste("Keine gueltigen Daten fuer", variable_group, "gefunden."))
    return(NULL)
  }
  
  reference_column <- find_best_reference(data, usable_columns)
  if (is.na(reference_column)) {
    warning(paste("Keine Referenzvariable fuer", variable_group, "bestimmbar."))
    return(NULL)
  }
  cat("Gewaehlte Referenzvariable:", reference_column, "\n\n")
  
  reference_row <- data.frame(
    variable_group = variable_group, reference_variable = reference_column,
    reference_station = get_station_from_name(reference_column),
    reference_height  = get_height_from_name(reference_column),
    subset = subset_label,
    stringsAsFactors = FALSE
  )
  
  comparison_columns <- usable_columns[usable_columns != reference_column]
  if (length(comparison_columns) == 0) {
    cat("Keine Vergleichsvariablen vorhanden.\n")
    return(list(stats_rows = NULL, reference_row = reference_row))
  }
  
  # Regression pro Vergleichsvariable EINMAL berechnen und dann
  # fuer Tabelle, Einzelplots und Gruppenplot wiederverwenden.
  stats_list <- setNames(
    lapply(comparison_columns, function(col) calculate_regression(data, reference_column, col)),
    comparison_columns
  )
  
  stats_rows <- do.call(rbind, lapply(comparison_columns, function(col) {
    s <- stats_list[[col]]
    data.frame(
      variable_group = variable_group, reference_variable = reference_column,
      reference_station = get_station_from_name(reference_column),
      reference_height  = get_height_from_name(reference_column),
      variable = col, station = get_station_from_name(col), height = get_height_from_name(col),
      subset = subset_label,
      n_pairs = s$n_pairs, r_squared = s$r_squared, slope = s$slope, offset = s$offset,
      residual_mean = s$residual_mean, residual_sd = s$residual_sd,
      equation = make_equation(s$slope, s$offset),
      stringsAsFactors = FALSE
    )
  }))
  
  output_directory <- paste0("scatter_", variable_group)
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
  
  cat("Erstelle einzelne Scatterplots...\n")
  for (col in comparison_columns) {
    single_plot <- create_single_scatter_plot(data, variable_group, reference_column, col, stats_list[[col]])
    if (is.null(single_plot)) {
      cat("  Keine gemeinsamen Daten:", col, "\n")
      next
    }
    print(single_plot)
    ggsave(
      filename = file.path(output_directory, paste0(variable_group, "_", col, "_vs_", reference_column, ".png")),
      plot = single_plot, width = single_plot_width, height = single_plot_height, units = "px", dpi = png_dpi
    )
  }
  
  cat("\nErstelle Gruppenplot...\n")
  group_plot <- create_group_scatter_plot(data, variable_group, reference_column, comparison_columns, stats_list)
  if (!is.null(group_plot)) {
    print(group_plot)
    # Hoehe skaliert automatisch mit der Anzahl Facetten (2 Spalten,
    # siehe facet_wrap(..., ncol = 2) in create_group_scatter_plot).
    group_plot_rows <- ceiling(length(comparison_columns) / 2)
    ggsave(
      filename = file.path(output_directory, paste0(variable_group, "_GROUP_", reference_column, ".png")),
      plot = group_plot, width = group_plot_width, height = group_plot_row_height * group_plot_rows,
      units = "px", dpi = png_dpi
    )
  }
  
  list(stats_rows = stats_rows, reference_row = reference_row)
}