library(jsonlite)
library(ggplot2)

latitude <- 35.3737
longitude <- -83.2259
timezone <- "America/New_York"
year <- as.integer(format(Sys.Date(), "%Y"))

url <- paste0(
  "https://api.open-meteo.com/v1/forecast",
  "?latitude=", latitude,
  "&longitude=", longitude,
  "&daily=temperature_2m_max,temperature_2m_min",
  "&timezone=", URLencode(timezone, reserved = TRUE),
  "&start_date=", year, "-10-01",
  "&end_date=", year, "-10-31"
)

weather_data <- fromJSON(url, simplifyVector = TRUE)

if (!is.null(weather_data$error) && weather_data$error) {
  stop("Open-Meteo returned an error: ", weather_data$reason)
}

weather <- data.frame(
  date = as.Date(weather_data$daily$time),
  low_f = weather_data$daily$temperature_2m_min * 9 / 5 + 32,
  high_f = weather_data$daily$temperature_2m_max * 9 / 5 + 32
)

today <- Sys.Date()
last_date <- max(weather$date)
forecast_start <- today + 1
mountain_base <- floor((min(weather$low_f, na.rm = TRUE) - 15) / 5) * 5
scenery <- data.frame(
  date = weather$date,
  distant_ridge = mountain_base + 8 + 5 * abs(sin(seq(0, 3 * pi, length.out = nrow(weather)))),
  near_ridge = mountain_base + 4 + 7 * abs(sin(seq(0.8, 4.2 * pi, length.out = nrow(weather))))
)

weather_plot <- ggplot(weather, aes(x = date)) +
  annotate(
    "rect",
    xmin = forecast_start,
    xmax = last_date + 1L,
    ymin = -Inf,
    ymax = Inf,
    fill = "#E9DFC9",
    alpha = 0.6
  ) +
  geom_ribbon(
    data = scenery,
    aes(x = date, ymin = mountain_base, ymax = distant_ridge),
    inherit.aes = FALSE,
    fill = "#A9B8A0",
    alpha = 0.75
  ) +
  geom_ribbon(
    data = scenery,
    aes(x = date, ymin = mountain_base, ymax = near_ridge),
    inherit.aes = FALSE,
    fill = "#647F6B",
    alpha = 0.85
  ) +
  geom_ribbon(
    aes(ymin = low_f, ymax = high_f),
    fill = "#F2B544",
    alpha = 0.25
  ) +
  geom_line(
    aes(y = low_f, color = date),
    linewidth = 1.2,
    linetype = "solid"
  ) +
  geom_line(
    aes(y = high_f, color = date),
    linewidth = 1.5
  ) +
  geom_point(
    aes(y = low_f, color = date),
    size = 2.5
  ) +
  geom_point(
    aes(y = high_f, color = date),
    size = 2.8
  ) +
  geom_vline(
    xintercept = forecast_start,
    color = "#66584A",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = forecast_start + as.integer((last_date - forecast_start) / 2),
    y = Inf,
    label = "FORECAST",
    vjust = 1.5,
    color = "#51483F",
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
  scale_color_gradientn(
    colors = c("#9D3C36", "#D85B32", "#F29A38", "#E7B94E", "#76516A"),
    guide = "none"
  ) +
  labs(
    title = paste("October on the Blue Ridge", year),
    subtitle = "Sylva, North Carolina  |  Daily low and high temperatures",
    x = NULL,
    y = "Temperature (°F)",
    caption = "Open-Meteo · Shaded dates are forecast"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#FFF4DF", color = NA),
    panel.background = element_rect(fill = "#FFF4DF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#D8CDBA", linewidth = 0.4),
    axis.text = element_text(color = "#51483F"),
    axis.title.y = element_text(color = "#51483F"),
    plot.title = element_text(
      face = "bold",
      size = 24,
      color = "#7B392F"
    ),
    plot.subtitle = element_text(color = "#765A43", size = 13),
    plot.caption = element_text(color = "#765A43"),
    plot.margin = margin(18, 20, 14, 14)
  )

ggsave(
  "sylva-october-weather.png",
  plot = weather_plot,
  width = 10,
  height = 6,
  dpi = 300
)
