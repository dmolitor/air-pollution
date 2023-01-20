# Create buffers around point geometries and return the corresponding
# polygons
bufferize_points <- function(points, buffer = 10, unit = c("m", "km", "mi")) {
  unit <- match.arg(unit)
  buffer <- switch(
    unit,
    "m" = buffer,
    "km" = buffer * 1000,
    "mi" = buffer * 1609.34
  )
  orig_crs <- sf::st_crs(points)
  # Transform points to North America Albers Equal Area Conic projected CRS
  points <- sf::st_transform(points, "ESRI:102008")
  points <- sf::st_buffer(points, dist = buffer)
  # Transform bufferized points back to original CRS
  points <- sf::st_transform(points, orig_crs)
  return(points)
}

# Create pop-up text for air monitor icons
monitor_popup <- function(city, state, reporting_agency) {
  paste(
    c(
      paste0(
        "<b>",
        paste(
          c(city, state)[!is.na(c(city, state))],
          collapse = ", "
        ),
        "</b>"
      ),
      reporting_agency
    ),
    collapse = "<br>"
  )
}

# Get the month(s) from a numeric/Date vector as a string
month_extract <- function(x, ...) {
  as.character(lubridate::month(x, label = TRUE, abbr = FALSE, ...))
}

# Function that creates a leaflet plot of PM monitors and surface-level PM
plot_pollution <- function(monitors,
                           pm,
                           year, 
                           month,
                           pollutants = c("PM2.5", "PM10", "both"),
                           state = "All",
                           return_data = FALSE) {
  year_month <- lubridate::ymd(paste(year, month, "01"))
  state_list <- state
  if (length(state_list) == 1 && state_list == "All") {
    state_list <- states()
  }
  pollutants <- match.arg(pollutants)
  monitors <- dplyr::mutate(
    monitors,
    active_flag = was_active(
      year,
      month,
      monitor_start_date,
      last_sample_date,
      active
    )
  )
  monitors <- dplyr::filter(
    monitors,
    pollutant %in% pollutants,
    state %in% state_list,
    active_flag
  )
  pm <- raster::raster(pm[[paste0(month_extract(year_month), ".", year)]])
  pal <- leaflet::colorNumeric(
    viridis::inferno(100),
    terra::values(pm),
    na.color = "transparent"
  )
  if (return_data) {
    return(list("monitors" = monitors, "pm2.5" = pm))
  }
  leaf_plot <- leaflet::leaflet(data = monitors) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(lng = ~ longitude, lat = ~ latitude, popup = ~ map_popup) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLines) |>
    leafem::addGeoRaster(
      x = pm,
      colorOptions = leafem::colorOptions(palette = viridis::inferno(100)),
      resolution = 70,
      opacity = 0.7,
      autozoom = FALSE
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = terra::values(pm),
      title = "\U03BCg/m\U00B3"
    )
  return(leaf_plot)
}

# Plot method for pollutionSeq
plot.pollutionSeq <- function(x, trend = NULL, ...) {
  event_date <- lubridate::dmy(paste("1", attr(x, "event_month")))
  pollution_dat <- data.frame(
    "dates" = lubridate::dmy(paste("1", names(x))),
    "pm" = unname(unlist(unclass(x)))
  )
  p <- ggplot2::ggplot(pollution_dat, ggplot2::aes(x = dates, y = pm)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_vline(
      xintercept = event_date,
      color = "blue",
      linetype = "dashed"
    ) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  if (!is.null(trend)) {
    p <- p + ggplot2::geom_smooth(method = trend, ...)
  }
  return(p)
}

# Given a point, return as a polygon with pollution time trend
point_pollution_seq <- function(x,
                                pm,
                                meas_start_date,
                                time_window = 12,
                                geo_radius = 10,
                                geo_unit = "mi") {
  stopifnot(inherits(x, "sfc_POINT"), length(x) == 1)
  geo_loc <- sf::st_coordinates(x)
  x <- sf::st_as_sf(bufferize_points(x, buffer = geo_radius, unit = geo_unit))
  date_seq <- seq_time(meas_start_date, window = time_window)
  pm <- seq_pollution(pm, date = meas_start_date, window = time_window)
  pm <- terra::extract(pm, x, exact = TRUE)
  pm <- dplyr::summarise(
    .data = dplyr::group_by(dplyr::filter(pm, !is.na(pm[[2]])), ID),
    dplyr::across(
      .cols = -fraction,
      .fns = \(.x) sum(.x * fraction)/sum(fraction)
    )
  )
  pm <- as.list(dplyr::select(pm, -ID))
  pollutionSeq <- structure(
    as.list(pm),
    class = "pollutionSeq",
    event_month = names(pm)[[median(1:length(pm))]],
    pre_event = names(pm)[1:(median(1:length(pm)) - 1)],
    post_event = names(pm)[(median(1:length(pm)) + 1):length(pm)],
    geo_radius = geo_radius,
    geo_unit = geo_unit,
    geo_location = geo_loc,
    geo_crs = sf::st_crs(x)
  )
  return(pollutionSeq)
}

# Print method for pollutionSeq
print.pollutionSeq <- function(x, ...) {
  cat("Event month     :", gsub("\\.", " ", attr(x, "event_month")), "\n")
  cat("Data span       :", gsub("\\.", " ", attr(x, "pre_event")[[1]]), "- ")
  post_event <- attr(x, "post_event")
  cat(gsub("\\.", " ", post_event[[length(post_event)]]), "\n")
  cat("Location (X, Y) : (")
  geo_loc <- attr(x, "geo_location")
  cat(paste0(round(geo_loc[[1]], 5), ","), round(geo_loc[[2]], 5))
  cat(")\n")
  cat("CRS             :", attr(x, "geo_crs")[["input"]], "\n")
  return(invisible(x))
}

# Get all raster levels in a certain year
rast_year <- function(x, year) {
  rast_levels <- paste0(month_extract(1:12), ".", year)
  x[[rast_levels]]
}

# Extract pollution raster layers for a user-specified time window around a date
seq_pollution <- function(pm, date, window = 12) {
  date_seq <- seq_time(date = date, window = window)
  pm_seq <- pm[[date_seq]]
  return(pm_seq)
}

# Create a time window sequence around a specified date
seq_time <- function(date, window = 12) {
  prior_month_start <- lubridate::rollback(
    lubridate::rollback(date, roll_to_first = FALSE),
    roll_to_first = TRUE
  )
  next_month_start <- lubridate::rollforward(date, roll_to_first = TRUE)
  window_start <- lubridate::add_with_rollback(
    e1 = prior_month_start,
    e2 = -months(window - 1)
  )
  window_end <- lubridate::add_with_rollback(
    e1 = next_month_start,
    e2 = months(window - 1)
  )
  date_seq <- seq(window_start, window_end, by = "month")
  month_seq <- month_extract(date_seq)
  year_seq <- vapply(date_seq, year, numeric(1))
  paste0(month_seq, ".", year_seq)
}

# List of contiguous 48 states/District of Columbia
states <- function() {
  c(
    "Alabama",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "District Of Columbia",
    "Florida",
    "Georgia",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
  )
}

# Parallel Vectorize function
# future_vectorize <- function(FUN,
#                              vectorize.args = arg.names,
#                              SIMPLIFY = TRUE,
#                              USE.NAMES = TRUE) {
#   arg.names <- as.list(formals(FUN))
#   arg.names[["..."]] <- NULL
#   arg.names <- names(arg.names)
#   vectorize.args <- as.character(vectorize.args)
#   if (!length(vectorize.args)) 
#     return(FUN)
#   if (!all(vectorize.args %in% arg.names)) 
#     stop("must specify names of formal arguments for 'vectorize'")
#   collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", 
#                                  "vectorize.args")
#   if (any(collisions)) 
#     stop(sQuote("FUN"), " may not have argument(s) named ", 
#          paste(sQuote(arg.names[collisions]), collapse = ", "))
#   rm(arg.names, collisions)
#   (function() {
#     FUNV <- function() {
#       args <- lapply(as.list(match.call())[-1L], eval, 
#                      parent.frame())
#       names <- if (is.null(names(args))) 
#         character(length(args))
#       else names(args)
#       dovec <- names %in% vectorize.args
#       do.call(
#         future_mapply,
#         c(
#           FUN = FUN,
#           args[dovec],
#           MoreArgs = list(args[!dovec]), 
#           SIMPLIFY = SIMPLIFY,
#           USE.NAMES = USE.NAMES
#         )
#       )
#     }
#     formals(FUNV) <- formals(FUN)
#     environment(FUNV) <- parent.env(environment())
#     FUNV
#   })()
# }
 
# Get pollution for sets of points
# point_pollution <- future_vectorize(
#   FUN = point_pollution_seq,
#   vectorize.args = c("x", "meas_start_date")
# )

# Was a monitor active in a given Year-Month?
was_active <- function(year, month, monitor_start, monitor_end, is_active) {
  year_month <- lubridate::dmy(paste("15", month, year))
  year_month_start <- lubridate::rollback(year_month, roll_to_first = TRUE)
  year_month_end <- lubridate::rollforward(year_month)
  activity_status <- (
    monitor_start <= year_month_start
    | lubridate::`%within%`(
        monitor_start,
        lubridate::interval(year_month_start, year_month_end)
      )
  ) &
    (
      is_active
      | monitor_end >= year_month_end
      | lubridate::`%within%`(
          monitor_end,
          lubridate::interval(year_month_start, year_month_end)
        )
    )
  return(activity_status)
}
