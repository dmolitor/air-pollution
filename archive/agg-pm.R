source(here::here("src/utils.R"))

pm_files <- list.files(
  here::here("data/monthly-pm2.5"),
  full.names = TRUE,
  pattern = "*.nc"
)
pm <- suppressWarnings({ terra::rast(pm_files) })
names(pm) <- unlist(
  lapply(
    paste0(".", 1998:2021),
    \(.x) paste0(month_extract(1:12), .x)
  )
)
pm_agg <- terra::app(pm, fun = mean, na.rm = TRUE)

terra::writeCDF(pm_agg, here::here("data/monthly-pm2.5/agg-pm-1998-2021.nc"))