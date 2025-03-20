library(ggplot2)
library(data.table)

# https://www.c40knowledgehub.org/s/article/WHO-Air-Quality-Guidelines?language=en_US
# The WHO air quality guideline (AQG) states that annual average concentrations of PM2.5 should not exceed 5 µg/m3, while 24-hour average exposures should not exceed 15 µg/m3 more than 3 - 4 days per year.

# Interim targets have been set to support the planning of incremental milestones toward cleaner air, particularly for cities, regions and countries that are struggling with high air pollution levels. For PM2.5 these are:

#     35 µg/m3 annual mean, 75 µg/m3 24-hour mean.
#     25 µg/m3 annual mean, 50 µg/m3 24-hour mean.
#     15 µg/m3 annual mean, 37.5 µg/m3 24-hour mean.
#     10 µg/m3 annual mean, 25 µg/m3 24-hour mean.

air_pm <- readRDS("data/air_pm.rds")
air_pm[, year := year(time)]

daily <- air_pm[, .(year = year(date), mean = mean(value), sd = sd(value)), by = date]

ggplot(daily, aes(x = date, y = mean)) +
  geom_path() +
  facet_wrap(~year, ncol = 1, scales = "free_x")

ggplot(daily, aes(x = mean)) +
  geom_histogram() +
  facet_wrap(~year, ncol = 1) +
  geom_vline(xintercept = 5.39)

# average over year

year <- air_pm[, .(mean = mean(value)), year]

# average over month

year_month <- air_pm[, .(mean = mean(value)), by = .(year, month)]
month <- air_pm[, .(mean = mean(value)), by = month]

# extreme low-quality days
day <- air_pm[, .(year = year(date), mean = mean(value), sd = sd(value)), by = date]

# fluctuation over the day

# worst streaks over time
# (consecutive days in which the quality of the air was very poor (unhealthy))

# seasonality
# add all dates
dates <- data.table(date = seq(as.Date("2021-01-01"), as.Date("2024-12-31"), by = "day"))
day <- merge(day, dates, all.y = TRUE)

day[, `:=`(
  year = lubridate::year(date),
  wday = lubridate::wday(date, label = TRUE, abbr = TRUE, week_start = 1),
  week = lubridate::week(date),
  month = lubridate::month(date, label = TRUE, abbr = TRUE),
  exceeds = mean / 15 # daily recommended concentration
)]


# build x label ticks
x_labels <- day[, .(min_week = min(week)), by = month]

tile_plot <- ggplot(day, aes(week, wday, fill = exceeds)) +
  geom_tile(color = "#f5f2f2", linejoin = "bevel", linewidth = .3) + # "#dbdbdb"
  facet_wrap(
    ~year,
    ncol = 1,
    strip.position = "right"
  ) +
  scale_x_continuous(
    breaks = x_labels$min_week,
    labels = x_labels$month,
    position = "bottom",
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    breaks = c("Mon", "Wed", "Fri"),
    lim = rev
  ) +
  scale_fill_viridis_b(
    direction = 1,
    option = "plasma",
    na.value = "grey88",
    guide = guide_bins(
      title = "PM2.5 concentration exceeds x times guideline",
      position = "bottom"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(4, "cm"),
    legend.key.height = unit(.5, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  ) +
  labs(
    x = "",
    y = ""
  )

ggsave(tile_plot, device = "png", filename = "tile_plot.png")
