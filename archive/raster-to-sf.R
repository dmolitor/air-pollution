source(here::here("src/utils.R"))

# Load Data ---------------------------------------------------------------

cat("Loading raster data ...\n")

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

cat("Loading monitor data ...\n")

# PM2.5 Monitor locations
pm2.5_active <- readr::read_csv(
  here::here("data/monitor-locations/active_pm2.5.csv"),
  show_col_types = FALSE
)
pm2.5_inactive <- readr::read_csv(
  here::here("data/monitor-locations/inactive_pm2.5.csv"),
  show_col_types = FALSE
)
# pm10_active <- read_csv(here("data/monitor-locations/active_pm10.csv"))
# pm10_inactive <- read_csv(here("data/monitor-locations/inactive_pm10.csv"))

cat("Constructing comprehensive monitor dataset ...\n")

all_monitors <- dplyr::bind_rows(
    dplyr::mutate(pm2.5_active, Active = TRUE, Pollutant = "PM2.5"),
    dplyr::mutate(pm2.5_inactive, Active = FALSE, Pollutant = "PM2.5")
  ) |>
  dplyr::mutate(
    Monitor_Start_Date = lubridate::mdy(Monitor_Start_Date),
    Last_Sample_Date = lubridate::mdy(Last_Sample_Date)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(map_popup = monitor_popup(City, State, Reporting_Agency)) |>
  dplyr::ungroup() |>
  dplyr::select(
    AQS_Site_ID,
    State:City,
    Address:Longitude,
    Elevation_meters_MSL:Sample_Collection_Frequency,
    Reporting_Agency:map_popup
  ) |>
  dplyr::rename_with(.fn = ~ tolower(.x)) |>
  dplyr::filter(
    dplyr::between(
      monitor_start_date,
      lubridate::mdy("1/1/1999"),
      lubridate::mdy("12/1/2020")
    ),
    state %in% states()
  )

# Cool MN plot area -------------------------------------------------------

cat("Making cool MN plot ...\n")

# Minnesota county lines 2021
mn_counties <- tigris::counties(state = "27", year = 2021) |>
  sf::st_transform(terra::crs(surface_pm2.5))

# Crop raster PM2.5
mn_pm2.5 <- terra::crop(rast_year(surface_pm2.5, year = 2021), mn_counties)

# Minnesota PM2.5 monitors 2021
mn_monitors_data <- plot_pollution(
  monitors = all_monitors,
  pm = mn_pm2.5,
  year = 2021,
  month = "January",
  state = "Minnesota",
  return_data = TRUE
)
mn_monitors <- sf::st_as_sf(
  mn_monitors_data$monitors,
  coords = c("longitude", "latitude"),
  agr = c(aqs_site_id = "identity", sf::NA_agr_),
  crs = 4326
)

# Extract raster values to polygons
mn_pm2.5_polys <- terra::extract(
  mn_pm2.5,
  mn_counties,
  exact = TRUE
)

mn_counties <- mn_counties |>
  dplyr::bind_cols(
    mn_pm2.5_polys |>
      dplyr::filter(!is.na(.data[[paste0("January", ".", 2021)]])) |>
      dplyr::group_by(ID) |>
      dplyr::summarise(
        dplyr::across(
          .cols = -fraction,
          .fns = \(.x) sum(.x * fraction)/sum(fraction)
        )
      ) |>
      dplyr::rename_with(.fn = ~ gsub(".2021", "", .x))
  )

# Plot it!!!
ggplot2::ggplot(
  data = dplyr::mutate(
    tidyr::pivot_longer(
      mn_counties,
      cols = January:December,
      names_to = "month",
      values_to = "pm_levels"
    ),
    month = factor(month, month_extract(1:12), ordered = TRUE)
  )
) +
  ggplot2::geom_sf(ggplot2::aes(fill = pm_levels)) +
  ggplot2::geom_sf(data = mn_monitors, color = "#3FBC73FF", size = 0.5) +
  ggplot2::labs(
    title = "Mean County-level PM2.5 Pollution (MN, 2021)",
    fill = "\U03BCg/m\U00B3"
  ) +
  viridis::scale_fill_viridis(option = "inferno") +
  ggplot2::facet_wrap(~ month, nrow = 3) +
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(face = "bold"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(color = "gray80"),
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
    strip.background = ggplot2::element_blank()
  )

# Uncomment to save plot
# ggplot2::ggsave("path/to/pic.png", width = 10, height = 10)

# Data cleaning -----------------------------------------------------------

cat("Convert monitors to simple features ...\n")

# Convert monitors to simple features
all_monitors <- sf::st_as_sf(
  all_monitors,
  coords = c("longitude", "latitude"),
  agr = c(aqs_site_id = "identity", sf::NA_agr_),
  crs = 4326
)

cat("Calculate pollution history for all monitors ...\n")

# Calculate pollution history for each monitor
future::plan("multicore", workers = parallel::detectCores())
progressr::with_progress({
  pb <- progressr::progressor(along = 1:nrow(all_monitors))
  monitors_pollution <- future.apply::future_lapply(
    1:nrow(all_monitors),
    \(.x) {
      monitor <- all_monitors[.x, ]
      pollution <- point_pollution_seq(
        x = monitor$geometry,
        pm = surface_pm2.5,
        meas_start_date = monitor$monitor_start_date
      )
      pb(paste0(.x, "/", nrow(all_monitors)))
      return(pollution)
    },
    future.packages = c("dplyr", "future", "future.apply", "Rcpp", "sf", "terra")
  )
}, enable = TRUE)
future::plan("sequential")

all_monitors <- all_monitors |>
  dplyr::mutate(pollution = monitors_pollution)

cat("Writing monitors with pollution to local file ...\n")
saveRDS(all_monitors, file = here::here("data/monitor-pollution.Rds"))
all_pollution <- readRDS(here::here("data/monitor-pollution.Rds"))
# Pollution analysis ------------------------------------------------------

cat("Running pollution analysis ...\n")

all_pollution <- new_pollutionSeqCollection(all_monitors$pollution)

# Try residualizing PM levels by month/year
# pol_data <- enframe(all_pollution)
# pol_data <- pol_data |>
#   dplyr::mutate(
#     year = factor(lubridate::year(dates)),
#     month = factor(as.character(lubridate::month(dates, TRUE, FALSE)))
#   )
# pol_model <- glmnet::cv.glmnet(
#   x = model.matrix(pm ~ year*month, data = pol_data)[, -1],
#   y = log(pol_data$pm),
#   nfolds = 10,
#   trace.it = 1
# )
# pol_fitted <- drop(
#   predict(
#     pol_model,
#     model.matrix(pm ~ year*month, data = pol_data)[, -1],
#     s = "lambda.min"
#   )
# )
# pol_resid <- pol_data$pm - pol_fitted

# Plot raw pollution pre/post trends
plot(all_pollution, trend = "lm", color = "seagreen4", se = TRUE)

# Plot residualized pollution pre/post trends
# pol_data_resid <- pol_data |>
#   dplyr::mutate(pm_resid = pol_resid) |>
#   dplyr::group_by(event_seq) |>
#   dplyr::summarise(pm_resid = mean(pm_resid), .groups = "drop")
# 
# ggplot2::ggplot(pol_data_resid, ggplot2::aes(x = event_seq, y = pm_resid)) +
#   ggplot2::geom_line(group = 1) +
#   ggplot2::geom_point(color = "red") +
#   ggplot2::geom_vline(
#     xintercept = 0,
#     color = "blue",
#     linetype = "dashed"
#   ) +
#   ggplot2::scale_x_continuous(
#     breaks = seq(-12, 12, by = 2)
#   ) +
#   ggplot2::geom_smooth(method = "lm", color = "seagreen4", se = TRUE)
