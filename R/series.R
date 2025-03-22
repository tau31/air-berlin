library(ggplot2)
library(data.table)
library(extrafont)
library(ggtext)
# font_import()
# font_import(pattern = 'Roboto')

# https://www.c40knowledgehub.org/s/article/WHO-Air-Quality-Guidelines?language=en_US
# The WHO air quality guideline (AQG) states that annual average concentrations of PM2.5 should not exceed 5 µg/m3, while 24-hour average exposures should not exceed 15 µg/m3 more than 3 - 4 days per year.

# Interim targets have been set to support the planning of incremental milestones toward cleaner air, particularly for cities, regions and countries that are struggling with high air pollution levels. For PM2.5 these are:

#     35 µg/m3 annual mean, 75 µg/m3 24-hour mean.
#     25 µg/m3 annual mean, 50 µg/m3 24-hour mean.
#     15 µg/m3 annual mean, 37.5 µg/m3 24-hour mean.
#     10 µg/m3 annual mean, 25 µg/m3 24-hour mean.

air_pm <- readRDS("data/air_pm.rds")
day <- air_pm[, .(year = year(date), mean = mean(value), sd = sd(value)), by = date]

day[, `:=`(
  year = lubridate::year(date),
  wday = lubridate::wday(date, label = TRUE, abbr = TRUE, week_start = 1),
  month = lubridate::month(date, label = TRUE, abbr = TRUE),
  yday = lubridate::yday(date),
  exceeds = mean / 15 # daily recommended concentration
)][
  , exceeds := round(exceeds)
]

# Week number starting at first day until Sunday
day[, week := ifelse(wday == "Mon" | yday == 1, 1, 0)][, week := cumsum(week), by = year]

## Consecutive streaks of bad air (at least 3 days consecutive with bad air)
# get bad days only (days where the average is at least 3 times above the guideline)
bad_days <- day[, is_bad := exceeds >= 2][(is_bad), ]
# get consecutive days
# code from https://stackoverflow.com/questions/5222061/create-grouping-variable-for-consecutive-sequences-and-split-vector

setorder(bad_days, date)
bad_days[, consecutive := cumsum(c(TRUE, diff(date) != 1))]

# Summarize for plotting
# severity is the the average of of how many times the gzuideline is exceeds for a particular streak of days
bad_days <- bad_days[, .(
  date,
  year,
  exceeds = factor(exceeds),
  n_days = .N,
  day_sequence = as.character(seq_len(.N)),
  severity = sum(exceeds) / .N,
  start = min(date),
  label = sprintf("%s to %s", min(date), max(date))
),
by = consecutive
][n_days > 2]

setorder(bad_days, start, date)

# plotting

ggplot(bad_days, aes(x = day_sequence, y = factor(start), fill = exceeds)) +
  geom_tile(color = "#f5f2f2", linejoin = "bevel", linewidth = .1) +
  coord_equal() +
  # facet_wrap(~year, ncol = 1)+
  # coord_fixed()
  scale_fill_viridis_d(
    option = "plasma",
    begin = 2 / 6,
    direction = 1,
    na.value = NA,
    guide = guide_legend(
      title = "How many times the PM<sub>2.5</sub> concentration exceeds the Air Quality Guideline (AQG)",
      position = "bottom",
      nrow = 1
    ),
    # limits = c(2,3,4,5),
    labels = c(
      "Exceeds 2-3 times",
      "Exceeds 3-4 times",
      "Exceeds 4-5 times",
      "Exceeds more than 5 times"
    )
  ) +
  scale_y_discrete(
    labels = rev(bad_days[, unique(label)]),
    lim = rev
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(
      size = 12
    ),
    legend.title = element_markdown(size = 14),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size = 14),
    text = element_text(family = "Lato"),
    plot.title = element_markdown(hjust = .5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = .5, size = 14)
  ) +
  labs(
    title = "Series of at least 3 consecutive days with PM<sub>2.5</sub> at least 2 times above recommended level (daily average below 15 (μ/m<sup>3</sup>))",
    subtitle = "Series of more than 2 days in a row. ",
    x = "Days in a row",
    y = ""
  )
