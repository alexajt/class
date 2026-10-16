library(ggplot2)

weather <- read.csv("hourly_weather.csv")
weather$time[nchar(weather$time) == 10] <- paste0(
  weather$time[nchar(weather$time) == 10],
  " 00:00:00"
)
weather$time <- as.POSIXct(
  weather$time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "America/New_York"
)
weather$date <- as.Date(weather$time, tz = "America/New_York")
weather$hour <- as.integer(format(weather$time, "%H", tz = "America/New_York"))

weather_plot <- ggplot(weather, aes(x = hour, y = date, fill = temperature_2m)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(0, 20, by = 4), limits = c(-0.5, 23.5)) +
  scale_fill_viridis_c(option = "magma", name = "Temperature (°C)") +
  labs(
    title = "Sarasota's Daily Temperature Rhythm",
    subtitle = "Hourly temperatures across the past week",
    x = "Hour of day",
    y = NULL,
    caption = "Source: Open-Meteo | Sarasota, Florida"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#17324D"),
    plot.subtitle = element_text(color = "#526779"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(color = "#17324D"),
    plot.caption = element_text(color = "#526779"),
    legend.position = "bottom"
  )

ggsave(
  "sarasota-weather-2.png",
  plot = weather_plot,
  width = 10,
  height = 5,
  dpi = 300
)
