source(here::here("src/utils.R"))
library(sf)

# Cleaning monitor data ---------------------------------------------------

all_monitors <- readRDS(here::here("data/monitor-pollution.Rds"))
all_monitors_dedup_site <- all_monitors |>
  dplyr::group_by(geometry) |>
  dplyr::mutate(first_monitor_start_date = min(monitor_start_date)) |>
  dplyr::filter(monitor_start_date == first_monitor_start_date) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::mutate(row_id = dplyr::cur_group_rows())

# all_monitors_dedup_site_buffer <- all_monitors_dedup_site |>
#   bufferize_points(buffer = 5, unit = "mi")
# 
# all_monitors_dedup_site_buffer$intersects_geom <- (
#   lengths(sf::st_intersects(all_monitors_dedup_site_buffer)) > 1
# )
# all_monitors_dedup_site_buffer <- all_monitors_dedup_site_buffer |>
#   dplyr::filter(!intersects_geom, monitor_start_date >= as.Date("2001/01/01"))
# 
# all_monitors_dedup_site <- all_monitors_dedup_site |>
#   dplyr::filter(row_id %in% all_monitors_dedup_site_buffer$row_id)

saveRDS(all_monitors_dedup_site, here::here("data/pollution-monitors-dedup.Rds"))

# Demean pollution data ---------------------------------------------------

# if (!dir.exists(here::here("data", "demeaned-pm"))) {
#   dir.create(here::here("data", "demeaned-pm"))
# }
# for (target_month in 1:12) {
#   cat(paste0(month_extract(target_month), ":"), "\n")
#   target_month <- formatC(target_month, width = 2, flag = "0")
#   target_pattern <- vapply(
#     paste0(1998:2021, target_month),
#     \(.x) paste0(rep(.x, 2), collapse = "-"),
#     character(1)
#   )
#   target_files <- surface_pm2.5_files[
#     vapply(target_pattern, \(.x) grep(.x, surface_pm2.5_files), integer(1))
#   ]
#   cat("Calculating mean PM2.5 levels ...\n")
#   target_layers <- terra::rast(target_files)
#   mean_pm <- terra::app(target_layers, mean, na.rm = TRUE)
#   cat("De-meaning PM2.5 levels for all years ...\n")
#   for (i in 1:length(target_files)) {
#     cat(paste0(i, "/", length(target_files)), "\n")
#     layer_rast <- terra::rast(target_files[[i]])
#     layer_rast <- layer_rast - mean_pm
#     out_path <- here::here(
#       "data/demeaned-pm",
#       dplyr::last(strsplit(target_files[[i]], "/")[[1]])
#     )
#     terra::writeCDF(layer_rast, filename = out_path, overwrite = TRUE)
#   }
# }

# Load de-meaned pollution data
# surface_pm2.5_detrended_files <- list.files(
#   path = here::here("data/demeaned-pm/"),
#   full.names = TRUE,
#   pattern = "*.nc"
# )
# surface_pm2.5_detrended <- terra::rast(surface_pm2.5_detrended_files)
# names(surface_pm2.5_detrended) <- unlist(
#   lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
# )

# Calculate de-meaned pollution history for each monitor
# future::plan("multicore", workers = parallel::detectCores())
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1:nrow(all_monitors))
#   monitors_pollution <- future.apply::future_lapply(
#     1:nrow(all_monitors),
#     \(.x) {
#       monitor <- all_monitors[.x, ]
#       pollution <- point_pollution_seq(
#         x = monitor$geometry,
#         pm = surface_pm2.5_detrended,
#         meas_start_date = monitor$monitor_start_date,
#         summary_fns = c(
#           "Mean" = terra_weighted_mean,
#           "Median" = median,
#           "Top Quartile" = \(.x) quantile(.x, .75),
#           "Bottom Quartile" = \(.x) quantile(.x, .25)
#         )
#       )
#       pb()
#       return(pollution)
#     }
#   )
# }, enable = TRUE)
# future::plan("sequential")
# 
# all_monitors <- all_monitors |>
#   dplyr::mutate(pollution = monitors_pollution)
# 
# cat("Writing monitors with pollution to local file ...\n")
# saveRDS(all_monitors, file = here::here("data/demeaned-pm/monitor-pollution.Rds"))

# Calculate mean de-trended and raw monitor pollution ---------------------

# Deduplicated monitors
monitors <- readRDS(here::here("data/pollution-monitors-dedup.Rds"))

# Raw PM2.5 Raster data
surface_pm2.5_files <- list.files(
  path = here::here("data/monthly-pm2.5/"),
  full.names = TRUE,
  pattern = "*.nc"
)
surface_pm2.5 <- terra::rast(surface_pm2.5_files)
names(surface_pm2.5) <- unlist(
  lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
)

######## Calculate mean de-trended pollution within buffers

future::plan("multicore", workers = parallel::detectCores())
progressr::with_progress({
  pb <- progressr::progressor(along = 1:nrow(monitors))
  monitors_pollution <- future.apply::future_lapply(
    1:nrow(monitors),
    \(.x) {
      monitor <- monitors[.x, ]
      pollution <- point_pollution_seq(
        x = monitor$geometry,
        pm = surface_pm2.5,
        meas_start_date = monitor$monitor_start_date,
        summary_fns = c(
          "Mean" = terra_weighted_mean,
          "Median" = median,
          "Top Quartile" = \(.x) quantile(.x, .75),
          "Bottom Quartile" = \(.x) quantile(.x, .25)
        ),
        geo_radius = 5,
        geo_unit = "mi",
        detrend = TRUE,
        verbose = FALSE,
        seasonal_only = TRUE
      )
      pb()
      return(pollution)
    }
  )
}, enable = TRUE)
future::plan("sequential")

monitors <- monitors |>
  dplyr::mutate(pollution = monitors_pollution)

# Saving de-trended pollution
saveRDS(
  monitors,
  file = here::here("data/monthly-pm2.5/monitor-detrended-pollution.Rds")
)

######## Calculate mean pollution within buffers

future::plan("multicore", workers = parallel::detectCores())
progressr::with_progress({
  pb <- progressr::progressor(along = 1:nrow(monitors))
  monitors_pollution <- future.apply::future_lapply(
    1:nrow(monitors),
    \(.x) {
      monitor <- monitors[.x, ]
      pollution <- point_pollution_seq(
        x = monitor$geometry,
        pm = surface_pm2.5,
        meas_start_date = monitor$monitor_start_date,
        summary_fns = c(
          "Mean" = terra_weighted_mean,
          "Median" = median,
          "Top Quartile" = \(.x) quantile(.x, .75),
          "Bottom Quartile" = \(.x) quantile(.x, .25)
        ),
        geo_radius = 5,
        geo_unit = "mi",
        detrend = FALSE
      )
      pb()
      return(pollution)
    }
  )
}, enable = TRUE)
future::plan("sequential")

monitors <- monitors |>
  dplyr::mutate(pollution = monitors_pollution)

# Saving de-trended pollution
saveRDS(
  monitors,
  file = here::here("data/monthly-pm2.5/monitor-raw-pollution.Rds")
)

# Calculate mean de-trended and raw log(PM2.5) values ---------------------

# log PM2.5 Raster data
surface_log_pm2.5_files <- list.files(
  path = here::here("data/logged-pm/"),
  full.names = TRUE,
  pattern = "*.nc"
)
surface_log_pm2.5 <- terra::rast(surface_log_pm2.5_files)
names(surface_log_pm2.5) <- unlist(
  lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
)

######## Calculate mean de-trended pollution within buffers

future::plan("multicore", workers = parallel::detectCores())
progressr::with_progress({
  pb <- progressr::progressor(along = 1:nrow(monitors))
  monitors_pollution <- future.apply::future_lapply(
    1:nrow(monitors),
    \(.x) {
      monitor <- monitors[.x, ]
      pollution <- point_pollution_seq(
        x = monitor$geometry,
        pm = surface_log_pm2.5,
        meas_start_date = monitor$monitor_start_date,
        summary_fns = c(
          "Mean" = terra_weighted_mean,
          "Median" = median,
          "Top Quartile" = \(.x) quantile(.x, .75),
          "Bottom Quartile" = \(.x) quantile(.x, .25)
        ),
        geo_radius = 5,
        geo_unit = "mi",
        detrend = TRUE,
        verbose = FALSE,
        seasonal_only = TRUE
      )
      pb()
      return(pollution)
    }
  )
}, enable = TRUE)
future::plan("sequential")

monitors <- monitors |>
  dplyr::mutate(pollution = monitors_pollution)

# Saving de-trended pollution
saveRDS(
  monitors,
  file = here::here("data/logged-pm/monitor-detrended-pollution.Rds")
)

######## Calculate mean pollution within buffers

future::plan("multicore", workers = parallel::detectCores())
progressr::with_progress({
  pb <- progressr::progressor(along = 1:nrow(monitors))
  monitors_pollution <- future.apply::future_lapply(
    1:nrow(monitors),
    \(.x) {
      monitor <- monitors[.x, ]
      pollution <- point_pollution_seq(
        x = monitor$geometry,
        pm = surface_log_pm2.5,
        meas_start_date = monitor$monitor_start_date,
        summary_fns = c(
          "Mean" = terra_weighted_mean,
          "Median" = median,
          "Top Quartile" = \(.x) quantile(.x, .75),
          "Bottom Quartile" = \(.x) quantile(.x, .25)
        ),
        geo_radius = 5,
        geo_unit = "mi",
        detrend = FALSE
      )
      pb()
      return(pollution)
    }
  )
}, enable = TRUE)
future::plan("sequential")

monitors <- monitors |>
  dplyr::mutate(pollution = monitors_pollution)

# Saving de-trended pollution
saveRDS(
  monitors,
  file = here::here("data/logged-pm/monitor-raw-pollution.Rds")
)

# Log transform pollution data --------------------------------------------

# if (!dir.exists(here::here("data", "logged-pm"))) {
#   dir.create(here::here("data", "logged-pm"))
# }
# future::plan("multicore", workers = 2)
# progressr::with_progress({
#   pb <- progressr::progressor(along = surface_pm2.5_files)
#   logged_pm <- future.apply::future_vapply(
#     surface_pm2.5_files,
#     \(.x) {
#       pm <- terra::rast(.x)
#       log_pm <- terra::app(pm, \(.x) log(.x + .1))
#       out_path <- here::here(
#         "data/logged-pm",
#         dplyr::last(strsplit(.x, "/")[[1]])
#       )
#       terra::writeCDF(log_pm, filename = out_path, overwrite = TRUE)
#       pb()
#       return(TRUE)
#     },
#     FUN.VALUE = logical(1),
#     future.seed = TRUE
#   )
# }, enable = TRUE)
# future::plan("sequential")

# De-mean log-transformed PM2.5 data
# surface_log_pm2.5_files <- list.files(
#   path = here::here("data/logged-pm/"),
#   full.names = TRUE,
#   pattern = "*.nc"
# )
# if (!dir.exists(here::here("data", "demeaned-log-pm"))) {
#   dir.create(here::here("data", "demeaned-log-pm"))
# }
# for (target_month in 1:12) {
#   cat(paste0(month_extract(target_month), ":"), "\n")
#   target_month <- formatC(target_month, width = 2, flag = "0")
#   target_pattern <- vapply(
#     paste0(1998:2021, target_month),
#     \(.x) paste0(rep(.x, 2), collapse = "-"),
#     character(1)
#   )
#   target_files <- surface_log_pm2.5_files[
#     vapply(target_pattern, \(.x) grep(.x, surface_log_pm2.5_files), integer(1))
#   ]
#   cat("Calculating mean PM2.5 levels ...\n")
#   target_layers <- terra::rast(target_files)
#   mean_pm <- terra::app(target_layers, mean, na.rm = TRUE)
#   cat("De-meaning PM2.5 levels for all years ...\n")
#   for (i in 1:length(target_files)) {
#     cat(paste0(i, "/", length(target_files)), "\n")
#     layer_rast <- terra::rast(target_files[[i]])
#     layer_rast <- layer_rast - mean_pm
#     out_path <- here::here(
#       "data/demeaned-log-pm",
#       dplyr::last(strsplit(target_files[[i]], "/")[[1]])
#     )
#     terra::writeCDF(layer_rast, filename = out_path, overwrite = TRUE)
#   }
# }

# Load de-meaned log pollution data
# surface_log_pm2.5_detrend_files <- list.files(
#   path = here::here("data/demeaned-log-pm/"),
#   full.names = TRUE,
#   pattern = "*.nc"
# )
# surface_log_pm2.5_detrended <- terra::rast(surface_log_pm2.5_detrend_files)
# names(surface_log_pm2.5_detrended) <- unlist(
#   lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
# )

# Calculate de-meaned pollution history for each monitor
# future::plan("multicore", workers = parallel::detectCores())
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1:nrow(all_monitors))
#   monitors_pollution <- future.apply::future_lapply(
#     1:nrow(all_monitors),
#     \(.x) {
#       monitor <- all_monitors[.x, ]
#       pollution <- point_pollution_seq(
#         x = monitor$geometry,
#         pm = surface_log_pm2.5_detrended,
#         meas_start_date = monitor$monitor_start_date,
#         summary_fns = c(
#           "Mean" = terra_weighted_mean,
#           "Median" = median,
#           "Top Quartile" = \(.x) quantile(.x, .75),
#           "Bottom Quartile" = \(.x) quantile(.x, .25)
#         )
#       )
#       pb()
#       return(pollution)
#     },
#     future.seed = TRUE
#   )
# }, enable = TRUE)
# future::plan("sequential")
# 
# all_monitors <- all_monitors |>
#   dplyr::mutate(pollution = monitors_pollution)
# 
# cat("Writing monitors with pollution to local file ...\n")
# saveRDS(all_monitors, file = here::here("data/demeaned-log-pm/monitor-pollution.Rds"))

# De-trend (using STL) logged PM2.5 data ----------------------------------

# monitors <- readRDS(here::here("data/pollution-monitors-dedup.Rds"))
# surface_log_pm2.5_files <- list.files(
#   path = here::here("data/logged-pm/"),
#   full.names = TRUE,
#   pattern = "*.nc"
# )
# surface_log_pm2.5 <- terra::rast(surface_log_pm2.5_files)
# names(surface_log_pm2.5) <- unlist(
#   lapply(1998:2021, \(.x) paste0(month_extract(1:12), ".", .x))
# )

# Calculate de-trended pollution history for each monitor
# future::plan("multicore", workers = parallel::detectCores())
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1:nrow(monitors))
#   monitors_pollution <- future.apply::future_lapply(
#     1:nrow(monitors),
#     \(.x) {
#       monitor <- monitors[.x, ]
#       pollution <- point_pollution_seq(
#         x = monitor$geometry,
#         pm = surface_log_pm2.5,
#         meas_start_date = monitor$monitor_start_date,
#         # summary_fns = c(
#         #   "Mean" = terra_weighted_mean,
#         #   "Median" = median,
#         #   "Top Quartile" = \(.x) quantile(.x, .75),
#         #   "Bottom Quartile" = \(.x) quantile(.x, .25)
#         # ),
#         detrend = TRUE,
#         seasonal_only = TRUE,
#         verbose = TRUE
#       )
#       pb()
#       return(pollution)
#     },
#     future.seed = TRUE
#   )
# }, enable = TRUE)
# future::plan("sequential")
# 
# monitors <- monitors |>
#   dplyr::mutate(pollution = monitors_pollution)
# 
# cat("Writing monitors with pollution to local file ...\n")
# saveRDS(monitors, file = here::here("data/logged-pm/monitor-pollution.Rds"))


# if (!dir.exists(here::here("data", "stl-detrended-log-pm"))) {
#   dir.create(here::here("data", "stl-detrended-log-pm"))
# }
# surface_log_pm2.5_stl <- terra::app(
#   x = surface_log_pm2.5,
#   fun = \(.x) stl_remainder(.x)
#   # cores = parallel::detectCores() - 1
# )
# terra::writeRaster(
#   x = surface_log_pm2.5_stl,
#   filename = here::here("data/stl-detrended-log-pm/stl-detrended-pm.tif")
# )

# Extract PM2.5 annual values ---------------------------------------------

# cat("Extracting PM2.5 values for all years ...\n")
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1998:2021)
#   for (yr in 1998:2021) {
#     layers <- paste0(month_extract(1:12), ".", yr)
#     vals <- as.vector(terra::values(surface_pm2.5[[layers]]))
#     saveRDS(vals, here::here(paste0("data/pm-", yr, "-vals.Rds")))
#     pb()
#   }
# }, enable = TRUE)

# cat("Constructing PM2.5 histograms for all months ...\n")
# future::plan("multicore", workers = parallel::detectCores())
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1:terra::nlyr(surface_pm2.5))
#   pm_hists <- future.apply::future_lapply(
#     1:(terra::nlyr(surface_pm2.5)),
#     \(.x) {
#       histogram <- terra::global(
#         surface_pm2.5[[.x]],
#         \(.y) list(hist(.y, breaks = 100))
#       )
#       histogram <- histogram[["global"]][[1]]
#       pb()
#       return(histogram)
#     },
#     future.seed = TRUE
#   )
# }, enable = TRUE)
# names(pm_hists) <- names(surface_pm2.5)
# future::plan("sequential")
# saveRDS(pm_hists, here::here("data/pm-dist.Rds"))
# 
# cat("Constructing log(PM2.5) histograms for all months ...\n")
# future::plan("multicore", workers = parallel::detectCores())
# progressr::with_progress({
#   pb <- progressr::progressor(along = 1:terra::nlyr(surface_pm2.5))
#   pm_hists <- future.apply::future_lapply(
#     1:(terra::nlyr(surface_pm2.5)),
#     \(.x) {
#       histogram <- terra::global(
#         surface_pm2.5[[.x]],
#         \(.y) list(hist(log(.y), breaks = 100))
#       )
#       histogram <- histogram[["global"]][[1]]
#       pb()
#       return(histogram)
#     },
#     future.seed = TRUE
#   )
# }, enable = TRUE)
# names(pm_hists) <- names(surface_pm2.5)
# future::plan("sequential")
# saveRDS(pm_hists, here::here("data/pm-log-dist.Rds"))