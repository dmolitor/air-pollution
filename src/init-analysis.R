source(here::here("src/utils.R"))

cat("Loading raster pollution data ...\n")

# PM2.5 Raster data
surface_pm2.5_files <- list.files(
  path = here::here("data/monthly-pm2.5/"),
  full.names = TRUE,
  pattern = "*.nc"
)
surface_pm2.5 <- terra::rast(surface_pm2.5_files)
names(surface_pm2.5) <- unlist(
  lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
)

cat("Loading monitor pollution data ...\n")
all_monitors <- readRDS(here::here("data/monitor-pollution.Rds"))
all_pollution <- new_pollutionSeqCollection(all_monitors$pollution)

cat("Extracting raster layers to data.frame ...\n")

progressr::with_progress({
  pb <- progressr::progressor(along = 1:terra::nlyr(surface_pm2.5))
  pol_layers <- lapply(
    1:terra::nlyr(surface_pm2.5),
    function(layer) {
      mv <- terra::global(surface_pm2.5[[layer]], mean, na.rm = TRUE)
      pb()
      return(mv)
    }
  )
})

cat("Constructing national pollution data.frame ...\n")

all_pol_data <- dplyr::bind_cols(pol_layers) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "date",
    values_to = "pm"
  ) |>
  dplyr::mutate(
    date = lubridate::my(date),
    month = as.character(lubridate::month(date, TRUE, FALSE))
  )

cat("Applying Seasonal Decomposition by Loess to the pollution trend ...\n")

pm_ts <- ts(
  data = all_pol_data$pm,
  start = c(1998, 1),
  end = c(2021, 12),
  frequency = 12
)
pm_stl <- stl(pm_ts, s.window = "periodic", robust = TRUE)

cat("Adding trends to pollution data.frame ...\n")
all_pol_data <- all_pol_data |>
  dplyr::mutate(
    pm_seasonal = as.numeric(pm_stl$time.series[, 1]),
    pm_trend = as.numeric(pm_stl$time.series[, 2]),
    pm_remainder = as.numeric(pm_stl$time.series[, 3])
  )

readr::write_csv(all_pol_data, here::here("data/agg-monthly-us-pollution.csv"))

cat("Plotting pollution trends ...\n")

# Plot monthly, mean pm levels for all U.S. (January 1998 - December 2021)
ggplot2::ggplot(all_pol_data, ggplot2::aes(x = date, y = pm)) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pm_trend, linetype = "STL Trend"),
    # linetype = "dotted",
    color = "cornflowerblue"
  ) +
  ggplot2::geom_line(group = 1, color = "burlywood4") +
  # ggplot2::geom_point(color = "red", size = 0.3) +
  ggplot2::scale_x_date(date_breaks = "10 month", date_labels = "%B %Y") +
  ggplot2::labs(
    x = "",
    y = "Mean PM2.5 (\U03BCg/m\U00B3)",
    title = "U.S. Monthly PM2.5 Trends"
  ) +
  theme_minimal_light() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, face = "bold"),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.key = ggplot2::element_rect(fill = "white"),
    legend.text = ggplot2::element_text(face = "bold"),
    legend.title = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

# Plot the STL seasonal, trend, and remainder lines
all_pol_data |>
  tidyr::pivot_longer(
    cols = c(pm, pm_seasonal:pm_remainder),
    names_to = "trendline",
    values_to = "stl_pm"
  ) |>
  dplyr::mutate(
    trendline = dplyr::case_when(
      trendline == "pm_remainder" ~ "Remainder",
      trendline == "pm_seasonal" ~ "Seasonal",
      trendline == "pm_trend" ~ "Trend",
      trendline == "pm" ~ "Total PM2.5"
    ),
    trendline = factor(
      trendline,
      levels = c("Total PM2.5", "Seasonal", "Trend", "Remainder")
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = stl_pm)) +
  ggplot2::geom_line(group = 1) +
  ggplot2::facet_wrap(~ trendline, nrow = 4, scales = "free_y") +
  ggplot2::scale_x_date(date_breaks = "10 month", date_labels = "%B %Y") +
  ggplot2::labs(
    x = "",
    y = "Mean PM2.5 (\U03BCg/m\U00B3)",
    title = "U.S. Monthly PM2.5 STL Trends"
  ) +
  theme_minimal_light() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, face = "bold"),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.key = ggplot2::element_rect(fill = "white"),
    legend.text = ggplot2::element_text(face = "bold"),
    legend.title = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

# Check remainder normality
qqnorm(pm_stl$time.series[, 3], plot.it = TRUE)
qqline(pm_stl$time.series[, 3], col = 2, lwd = 2, lty = 2)

shapiro.test(pm_stl$time.series[, 3])

# Plot mean pm levels for all U.S. aggregated at the monthly level
# agg_monthly <- dplyr::group_by(all_pol_data, month) |>
#   dplyr::summarise(pm = mean(pm)) |>
#   dplyr::mutate(month = factor(month, levels = month_extract(1:12)))
# 
# ggplot2::ggplot(agg_monthly, ggplot2::aes(x = month, y = pm)) +
#   ggplot2::geom_line(group = 1) +
#   ggplot2::geom_point(color = "cornflowerblue") +
#   # ggplot2::scale_x_date(date_breaks = "10 month", date_labels = "%B %Y") +
#   ggplot2::labs(
#     x = "",
#     y = "Mean PM2.5 (\U03BCg/m\U00B3)",
#     title = "U.S. PM2.5 Seasonal Trends"
#   ) +
#   theme_minimal_light() +
#   ggplot2::theme(
#     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, face = "bold"),
#     axis.title = ggplot2::element_text(face = "bold"),
#     panel.grid.minor.x = ggplot2::element_blank()
#   )