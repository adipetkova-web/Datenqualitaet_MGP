# ============================
# Wind-arrow animation on OSM
# ============================

# ---- Packages ----
pkgs <- c("tidyverse","sf","geosphere","ggplot2","gganimate",
          "maptiles","terra","ggspatial","gifski","lubridate","transformr","av")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------
# 0) Station coordinates (lon, lat)
# -----------------------------
coords <- tribble(
  ~id,    ~lon,       ~lat,
  "AWS20", 12.805153, 53.390541,
  "AWS30", 12.784591, 53.399346,
  "AWS50", 12.790282, 53.401478,
 #"DWD_Rechlin", 12.7149,53.3311,
  "DWD_Trollenhagen",12.77212, 53.40593 # 13.3039,53.5998 real coords
)

# ----------------------------------------
# 1) Prepare data (NO aggregation)
#    Expected per df: t (POSIXct), wspeed (m/s), wdir (deg FROM)
#    Using your objects: aws20_30m, aws30_30m, aws50_30m
# ----------------------------------------
prep_station_df <- function(d, id_label) {
  d |>
    transmute(
      id   = id_label,
      # Tag existing times as fixed UTC+1 (no DST) without shifting the clock time
      time = lubridate::force_tz(t, "Etc/GMT-1"),
      wspeed   = as.numeric(wspeed),
      wdir   = as.numeric(wdir)   # degrees, meteorological FROM direction
    )
}

wind_raw <- bind_rows(
  prep_station_df(aws20_30m, "AWS20"),
  prep_station_df(aws30_30m, "AWS30"),
  prep_station_df(aws50_30m, "AWS50"),
  prep_station_df(dwd_rechlin, "DWD_Rechlin"),
  prep_station_df(dwd_trollenhagen, "DWD_Trollenhagen")
) |>
  left_join(coords, by = "id") |>
  filter(!is.na(time), !is.na(wspeed), !is.na(wdir), !is.na(lon), !is.na(lat))

# (Optional) restrict to a time window (still NO aggregation)
start_time <- NULL  # e.g., as.POSIXct("2023-08-15 00:00:00", tz="Etc/GMT-1")
end_time   <- NULL  # e.g., as.POSIXct("2023-08-16 00:00:00", tz="Etc/GMT-1")

wind_ds <- wind_raw
if (!is.null(start_time)) wind_ds <- dplyr::filter(wind_ds, time >= start_time)
if (!is.null(end_time))   wind_ds <- dplyr::filter(wind_ds, time <  end_time)
wind_ds <- dplyr::arrange(wind_ds, time)

# ---------------------------------------------------------
# 2) Arrow endpoints (wdir “from” → bearing “to”), length ∝ wspeed
# ---------------------------------------------------------
scale_m_per_ms <- 300  # 1 m/s → 300 m arrow (tune for map scale/zoom)

arrows <- wind_ds |>
  mutate(
    bearing_to = (wdir + 180) %% 360,   # flip FROM → TO
    dist_m     = wspeed * scale_m_per_ms
  )

dest <- geosphere::destPoint(
  p = cbind(arrows$lon, arrows$lat),
  b = arrows$bearing_to,
  d = arrows$dist_m
)
arrows <- arrows |>
  mutate(lon_end = dest[,1], lat_end = dest[,2])

# -----------------------------------------
# 3) BBox, frame times,labels, tiles
#    (ORDER MATTERS: build unique_times first; reuse everywhere)
# -----------------------------------------
# Frame times = all timestamps present in your data (no aggregation)
unique_times <- sort(unique(wind_ds$time))
nframes <- length(unique_times)
fps_set <- 5

# Timestamp overlay
x_text <- xrange[1] + 0.50 * diff(xrange)    # 2% from left
y_text <- yrange[2] - 0.03 * diff(yrange)    # 3% down from top
time_labels <- tibble(
  time  = unique_times,
  x     = x_text, y = y_text,
  label = format(unique_times, "%Y-%m-%d %H:%M (UTC+1)")
)
pad <- 0.01 # map size

xrange <- range(c(coords$lon, arrows$lon, arrows$lon_end), na.rm = TRUE)
yrange <- range(c(coords$lat, arrows$lat, arrows$lat_end), na.rm = TRUE)

bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = xrange[1]-pad, ymin = yrange[1]-pad,
    xmax = xrange[2]+pad, ymax = yrange[2]+pad),
  crs = 4326
))

# Basemap tiles
bbox_sf <- st_as_sfc(st_bbox(
  c(xmin = xrange[1]-pad, ymin = yrange[1]-pad,
    xmax = xrange[2]+pad, ymax = yrange[2]+pad),
  crs = 4326
))
# Basemap tiles — your {maptiles} version: use fixed zoom (no max_tiles arg)
tiles <- maptiles::get_tiles(
  bbox_sf,
  provider = "OpenStreetMap",
  zoom = 13,    # try 12–14 if you want less/more detail
  crop = TRUE
)
# -----------------------------------------
# 4) Plot + animate (linewidth legend for wind speed)
# -----------------------------------------
station_points <- arrows |> distinct(id, lon, lat)

p <- ggplot() +
  ggspatial::layer_spatial(tiles) +
  # Wind arrows: color = station, linewidth = wind speed (legend)
  geom_segment(
    data = arrows,
    aes(x = lon, y = lat, xend = lon_end, yend = lat_end,
        group = id, color = id, linewidth = wspeed),
    arrow   = arrow(length = grid::unit(3.5, "mm"), type = "closed"),
    lineend = "round",
    alpha   = 0.95
  ) +
  # Station markers
  geom_point(
    data = station_points,
    aes(lon, lat, fill = id),
    shape = 21, size = 3.2, stroke = 1.0, alpha = 0.95
  ) +
  # Timestamp overlay
  geom_label(
    data = time_labels,
    aes(x = x, y = y, label = label),
    size = 7,
    fontface = "bold",
    label.size = 0.6,
    label.padding = grid::unit(0.12, "lines"),
    fill = "white", alpha = 0.7,
    label.r = grid::unit(0.12, "lines")
  ) +
  coord_sf(xlim = c(xrange[1]-pad, xrange[2]+pad),
           ylim = c(yrange[1]-pad, yrange[2]+pad),
           expand = FALSE) +
  # Legends
  scale_linewidth_continuous(name = "Wind speed (m/s)", range = c(1.4, 3.6)) +
  guides(
    fill = guide_legend(title = "Station"),
    color = guide_legend(title = "Station"),
    linewidth = guide_legend(title = "Wind speed (m/s)")
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  transition_manual(time) +
  labs(title = NULL)

# -----------------------------------------
# 5) Render GIF
# -----------------------------------------
# if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
# 
# anim <- gganimate::animate(
#   p,
#   width = 900, height = 650,
#   nframes = nframes, fps = fps_set,
#   duration = nframes / fps_set,
#   end_pause = 10,
#   renderer = gganimate::gifski_renderer("plots/wind_arrows.gif")
# )


# install.packages("av")  # if needed

gganimate::animate(
  p,
  width = 900, height = 650,
  nframes = nframes, fps = fps_set,
  duration = nframes / fps_set, end_pause = 10,
  device = "png",   # IMPORTANT: pass a STRING, not a function
  renderer = gganimate::av_renderer("plots/wind_arrows.mp4", codec = "libx264")
)
