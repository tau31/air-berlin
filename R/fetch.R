library(httr2)
library(data.table)
library(ggplot2)

my_key <- Sys.getenv("OPEN_WEATHER_API_KEY")
berlin_loc <- request("http://api.openweathermap.org/geo/1.0/direct?") |>
  req_url_query(q = "Berlin,,DE", appid = my_key) |>
  req_perform() |>
  resp_body_json() |>
  as.data.frame() |>
  dplyr::select(lat, lon)

air_data_resp <- request("http://api.openweathermap.org/data/2.5/air_pollution/history") |>
  req_url_query(
    lat = berlin_loc$lat,
    lon = berlin_loc$lon,
    start = as.numeric(as.POSIXct("2021-01-01 1:00:00 CET")),
    end = as.numeric(as.POSIXct("2025-01-01 0:00:00 CET")),
    appid = my_key
  ) |>
  req_perform() |>
  resp_body_json()

air_dt <- air_data_resp$list |>
  purrr::map_dfr(\(x) ({
    dt <- data.table(
      time = lubridate::as_datetime(x$dt),
      air_quality_index = x$main$aqi,
      other_polutants = list(as.data.table(x$components))
    )
    dt <- dt[, other_polutants[[1]], by = .(time, air_quality_index)]
  })) |>
  melt(id.vars = c("time", "air_quality_index"))

# remove outlier measurement
air_dt <- air_dt[value > 0, ]

air_pm <- air_dt[variable == "pm2_5"]

air_pm[, `:=`(
  year = year(time),
  date = as.Date(time),
  year_month = format(time, "%Y-%m"),
  month = month(time),
  week = week(time),
  weekday = wday(time),
  year_day = yday(time),
  month_time = format(time, "%m %H"),
  month_day_time = format(time, "%m-%d %H"),
  hour = hour(time)
)]

# Add WHO recommendation
# https://www.who.int/publications/i/item/9789240034228

openWeather_guideline <- c("Good", "Fair", "Moderate", "Poor_or_Very_Poor")
who_guideline <- c("meets_guideline", "2_times_higher", "3_times_higher", "5_times_higher", "7_times_higher", "10_times_higher", "over_10_times_higher")


rec <- data.table(
  qualitative = factor(
    c("Good", "Good", "Fair", "Fair", "Moderate", "Moderate", "Poor_or_Very_Poor"),
    ordered = TRUE,
    levels = openWeather_guideline
  ),
  who_guideline = factor(
    who_guideline,
    ordered = TRUE,
    levels = who_guideline
  ),
  min = c(0, 5, 10, 15, 25, 35, 50),
  max = c(5, 10, 15, 25, 35, 50, Inf)
)

# non-equi join
air_pm[rec, `:=`(qualitative = i.qualitative, who_guideline = i.who_guideline), on = .(value >= min, value < max)]

saveRDS(air_pm, "data/air_pm.rds")
