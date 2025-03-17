library(httr2)
library(data.table)
library(ggplot2)

my_key <- readLines(".api_key")

berlin_loc <- request("http://api.openweathermap.org/geo/1.0/direct?") |>
  req_url_query(q = "Berlin,,DE", appid = my_key) |>
  req_perform() |>
  resp_body_json() |>
  as.data.frame() |>
  dplyr::select(lat, lon)

# q=Berlin,,DE&limit=1&appid=4068918a8b685d4fe4a3ec34ec0e5c2b

air_data_resp <- request("http://api.openweathermap.org/data/2.5/air_pollution/history") |>
  req_url_query(
    lat = berlin_loc$lat,
    lon = berlin_loc$lon,
    start = as.numeric(as.POSIXct("2020-01-01 0:00:00 CET")),
    end = as.numeric(as.POSIXct("2025-03-01 0:00:00 CET")),
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
  year = format(time, "%Y"),
  month_day_time = format(time, "%m-%d %H"),
  hour = format(time, "%H")
)]

saveRDS(air_pm, "data/air_pm.rds")
