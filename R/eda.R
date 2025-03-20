library(data.table)
library(ggplot2)

air_pm <- readRDS("data/air_pm.rds")

ggplot(air_pm, aes(x = value)) +
  geom_histogram() +
  facet_grid(~who_guideline, scales = "free")

ggplot(air_pm, aes(x = time, y = who_guideline, group = 1)) +
  geom_step() +
  facet_wrap(~year, scales = "free", ncol = 1)

pm_hour <- air_pm[, .(
  mean = mean(value),
  sd = sd(value),
  q50 = quantile(value, probs = .5),
  q25 = quantile(value, probs = .25),
  q75 = quantile(value, probs = .75),
  q05 = quantile(value, probs = .05),
  q95 = quantile(value, probs = .95),
  N = .N
), by = hour]

setorder(pm_hour, hour)

ggplot(pm_hour, aes(x = hour, y = mean, group = 1)) +
  geom_ribbon(aes(ymin = max(mean - sd, 0), ymax = mean + sd), fill = "grey70") +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "orange") +
  geom_line()


# quality guideline per hour over all data
hour_pm <- air_pm[, .N, by = .(who_guideline, hour)][, prob := (N / sum(N)), by = hour]

ggplot(hour_pm, aes(x = hour, y = prob, fill = who_guideline)) +
  geom_area(
    position = position_fill(reverse = TRUE),
    color = "black",
    # size = .5,
    stat = "identity"
  ) +
  theme_bw() +
  guides(
    fill = guide_legend(reverse = TRUE),
  ) +
  labs(
    title = "Probability of Air Quality (pm25/m3) according to WHO guidelines for each Hour in the day",
    x = "Hour of the Day",
    y = "Probability"
  ) +
  scale_fill_viridis_d(option = "plasma")

# quality guideline per hour/month over all data
# https://www.accuweather.com/en/health-wellness/why-air-pollution-is-worse-in-winter/689434
hour_month_pm <- air_pm[, .N, by = .(who_guideline, month, hour)][, prob := (N / sum(N)), by = .(month, hour)]

ggplot(hour_month_pm, aes(x = hour, y = prob, fill = who_guideline)) +
  geom_area(
    position = position_fill(reverse = TRUE),
    color = "black",
    # size = .5,
    stat = "identity"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Probability of Air Quality (pm25/m3) according to WHO guidelines for each Hour in the day",
    x = "Hour of the Day",
    y = "Probability"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~month, nrow = 4)

# yearly distributions
# I am considering a log normal distribuition to model the yearly data
air_pm[, week := week(time)]

ggplot(air_pm, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~week, ncol = 5, scales = "free_x")

air_pm[year_day %between% c(1, 7), .N, year_day]

air_pm[, .N, week]


# quality guideline per hour/month over all data
# https://www.accuweather.com/en/health-wellness/why-air-pollution-is-worse-in-winter/689434
hour_week_pm <- air_pm[, .N, by = .(who_guideline, week, hour)][, prob := (N / sum(N)), by = .(week, hour)]

ggplot(hour_week_pm, aes(x = hour, y = prob, fill = who_guideline)) +
  geom_area(
    position = position_fill(reverse = TRUE),
    color = "black",
    # size = .5,
    stat = "identity"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Probability of Air Quality (pm25/m3) according to WHO guidelines for each Hour in the day",
    x = "Hour of the Day",
    y = "Probability"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~week, nrow = 12)

day_pm <- air_pm[, .N, by = .(who_guideline, week, hour)][, prob := (N / sum(N)), by = .(week, hour)]

ggplot(air_pm, aes(x = hour, y = value)) +
  geom_point()

air_pm[, .(mean = mean(value), sd = sd(value)), by = year]

# mix tables with plots
# yearly mean
# monthly mean
# daily means per year
# days in which it was really bad

ggplot(air_pm, aes(x = value)) +
  geom_histogram()

hist(exp(rnorm(1e4, log(5), 1)))

hist(rexp(1e4, 1 / .5))
