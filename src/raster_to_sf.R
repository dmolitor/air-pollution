library(dplyr)
library(ggplot2)
library(here)
library(leaflet)
library(lubridate)
library(readr)
library(raster)
library(stars)
library(terra)
library(tibble)
library(tidyr)
library(tigris)
library(viridis)

source(here("src/utils.R"))

# Load Data ---------------------------------------------------------------

# PM2.5 Raster data for January, 2021
surface_pm2.5_files <- list.files(here("data/monthly-pm2.5/"), full.names = TRUE)[-1]
surface_pm2.5 <- rast(surface_pm2.5_files)
names(surface_pm2.5) <- unlist(
  lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
)

# PM2.5 Monitor locations
pm2.5_active <- read_csv(here("data/monitor-locations/active_pm2.5.csv"))
pm2.5_inactive <- read_csv(here("data/monitor-locations/inactive_pm2.5.csv"))
pm10_active <- read_csv(here("data/monitor-locations/active_pm10.csv"))
pm10_inactive <- read_csv(here("data/monitor-locations/inactive_pm10.csv"))

all_monitors <- bind_rows(
  mutate(pm2.5_active, Active = TRUE, Pollutant = "PM2.5"),
  mutate(pm2.5_inactive, Active = FALSE, Pollutant = "PM2.5"),
  mutate(pm10_active, Active = TRUE, Pollutant = "PM10"),
  mutate(pm10_inactive, Active = FALSE, Pollutant = "PM10")
) |>
  mutate(
    Monitor_Start_Date = mdy(Monitor_Start_Date),
    Last_Sample_Date = mdy(Last_Sample_Date)
  ) |>
  rowwise() |>
  mutate(map_popup = monitor_popup(City, State, Reporting_Agency)) |>
  ungroup() |>
  dplyr::select(
    AQS_Site_ID,
    State:City,
    Address:Longitude,
    Elevation_meters_MSL:Sample_Collection_Frequency,
    Reporting_Agency:map_popup
  ) |>
  rename_with(.fn = ~ tolower(.x))

# Plot surface level PM2.5
# surface_pm2.5_df <- na.omit(as.data.frame(surface_pm2.5, xy = TRUE))
# ggplot(data = surface_pm2.5_df) +
#   geom_raster(aes(x = x, y = y, fill = January.2021)) +
#   scale_fill_viridis_c() +
#   theme_void() +
#   theme(legend.position = "bottom")

# Minnesota county lines 2021
mn_counties <- counties(state = "27", year = 2021) |>
  st_transform(crs(surface_pm2.5))

# Crop raster PM2.5
mn_pm2.5 <- crop(rast_year(surface_pm2.5, year = 2021), mn_counties)

# Minnesota PM2.5 monitors 2021
mn_monitors_data <- plot_pollution(
  monitors = all_monitors,
  pm = mn_pm2.5,
  year = 2021,
  month = "January",
  state = "Minnesota",
  return_data = TRUE
)
mn_monitors <- st_as_sf(
  mn_monitors_data$monitors,
  coords = c("longitude", "latitude"),
  agr = c(aqs_site_id = "identity", NA_agr_),
  crs = 4326
)

# Extract raster values to points
# mn_pm2.5_points <- extract(mn_pm2.5, mn_monitors)
# mn_monitors <- mn_monitors |>
#   bind_cols(
#     mn_pm2.5_points |> dplyr::select(-ID) |> rename_with(~ paste("PM2.5", .x))
#   )
# Plot it!!!
# ggplot(data = mn_counties) + 
#   geom_sf() + 
#   geom_sf(data = mn_monitors, aes(color = `PM2.5 January 2021`), size = 1) +
#   theme_minimal()

# Extract raster values to polygons
mn_pm2.5_polys <- terra::extract(
  mn_pm2.5,
  mn_counties,
  exact = TRUE
)

mn_counties <- mn_counties |>
  bind_cols(
    mn_pm2.5_polys |>
      filter(!is.na(.data[[paste0("January", ".", 2021)]])) |>
      group_by(ID) |>
      summarise(
        across(
          .cols = -fraction,
          .fns = \(.x) sum(.x * fraction)/sum(fraction)
        )
      ) |>
      rename_with(.fn = ~ gsub(".2021", "", .x))
  )

# Plot it!!!
ggplot(
  data = mutate(
    pivot_longer(
      mn_counties,
      cols = January:December,
      names_to = "month",
      values_to = "pm_levels"
    ),
    month = factor(month, month_extract(1:12), ordered = TRUE)
  )
) +
  geom_sf(aes(fill = pm_levels)) +
  geom_sf(data = mn_monitors, color = "#3FBC73FF", size = 0.5) +
  labs(
    title = "Mean County-level PM2.5 Pollution (MN, 2021)",
    fill = "\U03BCg/m\U00B3"
  ) +
  scale_fill_viridis(option = "inferno") +
  facet_wrap(~ month, nrow = 3) +
  theme(
    axis.ticks = element_blank(),
    legend.title = element_text(face = "bold"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_line(color = "gray80"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.background = element_blank()
  )

# Testing stuff -----------------------------------------------------------

monitors <- mn_monitors |>
  distinct(city, .keep_all = TRUE) |>
  slice_head(n = 2)

monitor_buffers <- st_buffer(monitors, dist = 10000)

# Plot points with buffer
monitors_buffer(monitors, mn_pm2.5, radius = 16)
