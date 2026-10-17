# ============================
# DWD wind near AWS20 → 30 min (no interpolation)
# ============================

suppressPackageStartupMessages({
  library(rdwd)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
})

# --- AWS20 location ---
aws20_lat <- 53.390541
aws20_lon <- 12.805153

# --- Find nearby hourly wind stations (handle old/new rdwd column names) ---
near <- rdwd::nearbyStations(
  lat = aws20_lat, lon = aws20_lon,
  radius = 60, res = "hourly", var = "wind", per = "recent"
) %>% as_tibble()

pick_col <- function(cands, df) cands[cands %in% names(df)][1]

id_col   <- pick_col(c("Stations_id","station_id","id","STATIONS_ID"), near)
name_col <- pick_col(c("Stationsname","name","Stationsname_Met","STATION_NAME"), near)
dist_col <- pick_col(c("dist","distance","Distanz_km"), near)

stopifnot(!is.na(id_col), !is.na(name_col), !is.na(dist_col))

# Prefer Rechlin/Lärz; else nearest
pref_idx <- which(str_detect(str_to_lower(near[[name_col]]), "rechlin"))[1]
if (is.na(pref_idx)) pref_idx <- which.min(near[[dist_col]])

station_id   <- near[[id_col]][pref_idx]
station_name <- near[[name_col]][pref_idx]
station_dist <- near[[dist_col]][pref_idx]
message(sprintf("Selected DWD station: %s (ID %s, %.1f km away)",
                station_name, station_id, station_dist))

# --- Download links: historical + recent (works across rdwd versions) ---
lnk_hist   <- rdwd::selectDWD(id = station_id, res = "hourly", var = "wind", per = "historical")
lnk_recent <- rdwd::selectDWD(id = station_id, res = "hourly", var = "wind", per = "recent")
lnks <- c(lnk_hist, lnk_recent)
if (!length(lnks)) stop("No DWD files found for this station.")

dl <- rdwd::dataDWD(lnks, dir = "data/dwd_wind", read = TRUE, quiet = TRUE)

# Bind all data.frames returned
dat_list <- if (is.list(dl)) dl[vapply(dl, is.data.frame, logical(1))] else list(dl)
if (!length(dat_list)) stop("No data frames read from DWD files.")
dat <- dplyr::bind_rows(dat_list)

# --- Identify time & wind columns (CDC: MESS_DATUM, F/FF, D/DD) ---
nn <- trimws(names(dat))
time_idx <- which(grepl("^MESS_DATUM(_BEGINN)?$", nn, ignore.case = TRUE))
ff_idx   <- which(toupper(trimws(nn)) %in% c("F","FF"))[1]
dd_idx   <- which(toupper(trimws(nn)) %in% c("D","DD"))[1]
if (!length(time_idx) || is.na(ff_idx) || is.na(dd_idx)) {
  stop("Could not find expected columns: MESS_DATUM / F(FF) / D(DD).")
}
time_col <- names(dat)[time_idx[1]]
ff_col   <- names(dat)[ff_idx]
dd_col   <- names(dat)[dd_idx]

# --- Parse time: DWD is UTC; convert to fixed UTC+1 (no DST) ---
t_utc <- as.POSIXct((dat[[time_col]]), "%Y%m%d%H", tz = "UTC")
if (all(is.na(t_utc))) stop("Failed to parse MESS_DATUM timestamps.")
t_loc <- with_tz(t_utc, "Etc/GMT-1")

# --- Clean hourly table (same structure as AWS: t, wspeed, wdir) ---
dwd_hourly <- tibble(
  t  = t_loc,
  wspeed = suppressWarnings(as.numeric(dat[[ff_col]])),
  wdir = suppressWarnings(as.numeric(dat[[dd_col]]))
) %>%
  mutate(
    wspeed = ifelse(!is.finite(wspeed) | wspeed < 0 | wspeed > 60, NA_real_, wspeed),
    wdir = ifelse(!is.finite(wdir) | wdir < 0 | wdir > 360, NA_real_, wdir)
  ) %>%
  distinct(t, .keep_all = TRUE) %>%
  arrange(t)

if (!nrow(dwd_hourly)) stop("No valid hourly rows after cleaning.")

attr(dwd_hourly$wspeed, "units") <- structure("m/s", names = "wspeed")
attr(dwd_hourly$wdir, "units") <- structure("deg",  names = "wdir")

# --- Compute clip window: prefer AWS20 / aws20_30m if present, else DWD span ---
clip_from <- floor_date(min(dwd_hourly$t, na.rm = TRUE), "30 minutes")
clip_to   <- ceiling_date(max(dwd_hourly$t, na.rm = TRUE), "30 minutes")

if (exists("AWS20", inherits = TRUE) && "t" %in% names(AWS20)) {
  AWS20 <- AWS20 %>% mutate(t = force_tz(t, "Etc/GMT-1"))
  clip_from <- max(clip_from, floor_date(min(AWS20$t, na.rm = TRUE), "30 minutes"))
  clip_to   <- min(clip_to,   ceiling_date(max(AWS20$t, na.rm = TRUE), "30 minutes"))
}
if (exists("aws20_30m", inherits = TRUE) && "t" %in% names(get("aws20_30m"))) {
  aws20_30m <- get("aws20_30m") %>% mutate(t = force_tz(t, "Etc/GMT-1"))
  clip_from <- max(clip_from, floor_date(min(aws20_30m$t, na.rm = TRUE), "30 minutes"))
  clip_to   <- min(clip_to,   ceiling_date(max(aws20_30m$t, na.rm = TRUE), "30 minutes"))
}

if (!is.finite(clip_from) || !is.finite(clip_to) || clip_from > clip_to) {
  stop("Clip window invalid. Check that DWD or AWS times parsed correctly.")
}

# --- Build 30-min grid & duplicate hourly values (NO interpolation) ---
grid_30 <- tibble(t = seq(from = clip_from, to = clip_to, by = "30 min"))

hourly_key <- dwd_hourly %>%
  mutate(t_hour = floor_date(t, "hour")) %>%
  distinct(t_hour, .keep_all = TRUE) %>%
  select(t_hour, wspeed, wdir)

dwd <- grid_30 %>%
  mutate(t_hour = floor_date(t, "hour")) %>%
  left_join(hourly_key, by = "t_hour") %>%
  select(t, wspeed, wdir)

attr(dwd$wspeed, "units") <- structure("m/s", names = "wspeed")
attr(dwd$wdir, "units") <- structure("deg",  names = "wdir")

message(sprintf("dwd: %d rows from %s to %s",
                nrow(dwd),
                format(min(dwd$t, na.rm = TRUE), "%Y-%m-%d %H:%M"),
                format(max(dwd$t, na.rm = TRUE), "%Y-%m-%d %H:%M")))
