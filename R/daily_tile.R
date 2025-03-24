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

# build x label ticks
x_labels <- day[, .(min_week = min(week)), by = month]

tile_plot <- ggplot(day, aes(week, wday, fill = factor(exceeds))) +
  geom_tile(color = "#f5f2f2", linejoin = "bevel", linewidth = .1) + # "#dbdbdb"
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
  scale_fill_viridis_d(
    direction = 1,
    option = "plasma",
    na.value = NA,
    guide = guide_legend(
      title = "How many times the PM<sub>2.5</sub> concentration exceeds the Air Quality Guideline (AQG)",
      position = "bottom",
      nrow = 1
    ),
    labels = c(
      "Meets AQG",
      "Exceeds 1-2 times",
      "Exceeds 2-3 times",
      "Exceeds 3-4 times",
      "Exceeds 4-5 times",
      "Exceeds more than 5 times"
    )
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
    axis.text = element_text(size = 13),
    strip.text = element_text(size = 14),
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 20, face = "bold"),
    plot.subtitle = element_markdown(size = 14),
    plot.caption = element_markdown(face = "italic", size = 12)
  ) +
  labs(
    title = "Berlin 🇩🇪: Daily Average concentration of PM<sub>2.5</sub> compared to WHO Guidelines",
    subtitle = "WHO Air Quality Guideline: Average daily PM<sub>2.5</sub> concentration **below 15 (μ/m<sup>3</sup>**)",
    x = "",
    y = "",
    caption = "Data from the OpenWeather Air Pollution API (openweathermap.org/api/air-pollution)<br>By Tiago Cabaço (github.com/tau31)"
  )

ggsave(tile_plot, device = "png", filename = "plots/tile_plot.png", height = 7, width = 13, bg = "white")
