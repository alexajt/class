library(jsonlite)

latitude <- 27.3233
longitude <- -82.5311
timezone <- "America/New_York"

url <- paste0(
  "https://api.open-meteo.com/v1/forecast",
  "?latitude=", latitude,
  "&longitude=", longitude,
  "&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m",
  "&past_days=7&forecast_days=1",
  "&timezone=", URLencode(timezone, reserved = TRUE)
)

weather <- fromJSON(url, simplifyVector = TRUE)

if (!is.null(weather$error) && weather$error) {
  stop("Open-Meteo returned an error: ", weather$reason)
}

hourly_weather <- data.frame(
  time = as.POSIXct(weather$hourly$time, format = "%Y-%m-%dT%H:%M", tz = timezone),
  temperature_2m = weather$hourly$temperature_2m,
  relative_humidity_2m = weather$hourly$relative_humidity_2m,
  precipitation = weather$hourly$precipitation,
  wind_speed_10m = weather$hourly$wind_speed_10m
)

now <- Sys.time()
hourly_weather <- hourly_weather[
  hourly_weather$time >= now - 7 * 24 * 60 * 60 &
    hourly_weather$time <= now,
]

write.csv(hourly_weather, "hourly_weather.csv", row.names = FALSE)
