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

# Generic method for data extraction from pollutionSeq and
# pollutionSeqCollection classes
enframe <- function(x, ...) {
  UseMethod("enframe")
}

# Enframe data from pollutionSeq object
enframe.pollutionSeq <- function(x, ...) {
  event_date <- lubridate::dmy(paste("1", attr(x, "event_month")))
  pollution_dat <- tibble::enframe(unclass(x), "dates", "pm")
  pollution_dat <- cbind(
    pollution_dat[, "dates"],
    do.call(rbind, pollution_dat[["pm"]])
  )
  if (!is.null(attr(x, "summary_names"))) {
    addtl_names <- attr(x, "summary_names")
  } else {
    addtl_names <- "pm"
  }
  names(pollution_dat) <- c("dates", addtl_names)
  pollution_dat$dates <- lubridate::my(pollution_dat$dates)
  return(tibble::tibble(pollution_dat))
}

# Enframe data from pollutionSeqCollection object
enframe.pollutionSeqCollection <- function(x, ...) {
  pol_data <- lapply(
    1:length(x),
    \(i) {
      dplyr::mutate(
        enframe(x[[i]]),
        id = i,
        event_seq = 1:dplyr::n(),
        event_seq = event_seq - median(event_seq)
      )
    }
  )
  pol_data <- dplyr::bind_rows(pol_data)
  return(pol_data)
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

# Check pollutionSeqCollection object
check_pollutionSeqCollection <- function(x) {
  vapply(
    x,
    \(.x) any(
      vapply(.x, \(.y) identical(.y, numeric(0)), logical(1))
    ),
    logical(1)
  )
}

# Constructor for pollutionSeqCollection object
new_pollutionSeqCollection <- function(x, summary_names = NULL) {
  contains_missing_data <- check_pollutionSeqCollection(x)
  if (any(contains_missing_data)) {
    missing_idx <- which(contains_missing_data)
    stop(
      "Object is missing data at the following indices: ",
      paste0(missing_idx, collapse = ", "),
      call. = FALSE
    )
  }
  event_dates <- c()
  for(.x in x) {
    event_dates <- append(event_dates, lubridate::my(attr(.x, "event_month")))
  }
  date_range <- range(event_dates)
  pollutionSeqCollection <- structure(
    x,
    class = "pollutionSeqCollection",
    date_min = date_range[[1]],
    date_max = date_range[[2]],
    n_collection = length(x),
    summary_names = summary_names
  )
  return(pollutionSeqCollection)
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
  multi_summary <- ifelse(!is.null(attr(x, "summary_names")), TRUE, FALSE)
  pollution_dat <- enframe(x)
  if (multi_summary) {
    pollution_dat <- tidyr::pivot_longer(
      data = pollution_dat,
      cols = -dates,
      names_to = "pm_summary",
      values_to = "pm"
    )
  }
  event_date <- lubridate::my(attr(x, "event_month"))
  if (multi_summary) {
    p <- ggplot2::ggplot(
      pollution_dat,
      ggplot2::aes(x = dates, y = pm, group = pm_summary, color = pm_summary)
    )
  } else {
    p <- ggplot2::ggplot(
      pollution_dat,
      ggplot2::aes(x = dates, y = pm, group = 1)
    )
  }
  p <- p +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(
      xintercept = event_date,
      color = "thistle4",
      linetype = "dashed"
    ) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B") +
    theme_minimal_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      legend.key = ggplot2::element_rect(fill = "white")
    )
  if (!is.null(trend)) {
    p <- p + ggplot2::geom_smooth(method = trend, ...)
  }
  return(p)
}

# Plot method for pollutionSeqCollection
plot.pollutionSeqCollection <- function(x, trend = NULL, summary_fn = mean, ...) {
  multi_summary <- ifelse(!is.null(attr(x, "summary_names")), TRUE, FALSE)
  pollution_dat <- enframe(x)
  pollution_dat <- dplyr::summarise(
    dplyr::group_by(pollution_dat, event_seq),
    dplyr::across(.fns = summary_fn),
    .groups = "drop"
  )
  if (multi_summary) {
    pollution_dat <- tidyr::pivot_longer(
      data = pollution_dat,
      cols = attr(x, "summary_names"),
      names_to = "pm_summary",
      values_to = "pm"
    )
  }
  event_seq_range <- range(pollution_dat$event_seq)
  if (multi_summary) {
    p <- ggplot2::ggplot(
      pollution_dat,
      ggplot2::aes(event_seq, pm, group = pm_summary, color = pm_summary)
    )
  } else {
    p <- ggplot2::ggplot(
      pollution_dat,
      ggplot2::aes(x = event_seq, y = pm, group = 1)
    )
  }
  p <- p +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "thistle4",
      linetype = "dashed"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(event_seq_range[[1]], event_seq_range[[2]], by = 2)
    ) +
    theme_minimal_light()
  if (!is.null(trend)) {
    p <- p + ggplot2::geom_smooth(method = trend, ...)
  }
  return(p)
}

# Given a point, return as a polygon with pollution time trend
point_pollution_seq <- function(x,
                                pm,
                                meas_start_date,
                                summary_fns = terra_weighted_mean,
                                time_window = 12,
                                geo_radius = 10,
                                geo_unit = "mi",
                                detrend = FALSE,
                                ...) {
  stopifnot(inherits(x, "sfc_POINT"), length(x) == 1)
  multi_summary <- ifelse(
    length(summary_fns) > 1 || is.list(summary_fns),
    yes = TRUE,
    no = FALSE
  )
  geo_loc <- sf::st_coordinates(x)
  x <- sf::st_as_sf(bufferize_points(x, buffer = geo_radius, unit = geo_unit))
  date_seq <- seq_time(meas_start_date, window = time_window)
  pm <- seq_pollution(pm, date = meas_start_date, window = time_window)
  pm <- terra::extract(pm, x, exact = TRUE)
  pm <- dplyr::summarise(
    .data = dplyr::group_by(dplyr::filter(pm, !is.na(pm[[2]])), ID),
    dplyr::across(
      .cols = -fraction,
      .fns = summary_fns
    )
  )
  if (multi_summary) {
    pm <- tidyr::pivot_longer(
      data = pm,
      cols = -ID,
      names_to = c(".value", "summary_name"),
      names_pattern = "([^_]*)_(.*)", values_drop_na = TRUE
    )
  }
  if (multi_summary) {
    summary_names <- pm[["summary_name"]]
    pm <- as.list(dplyr::select(pm, -c(ID, summary_name)))
    if (detrend) {
      dates <- lubridate::my(date_seq)
      seq_start <- c(lubridate::year(min(dates)), lubridate::month(min(dates)))
      seq_end <- c(lubridate::year(max(dates)), lubridate::month(max(dates)))
      pm_rem_vals <- lapply(1:length(summary_names), function(i) {
        pm_ts <- ts(
          data = vapply(pm, \(.x) .x[[i]], numeric(1)),
          start = seq_start,
          end = seq_end,
          frequency = 12
        )
        pm_rem <- stl_remainder(
          x = pm_ts,
          start = seq_start,
          end = seq_end,
          robust = TRUE,
          ...
        )
        return(pm_rem)
      })
      pm <- setNames(
        as.list(as.data.frame(t(do.call(cbind, pm_rem_vals)))),
        names(pm)
      )
    }
  } else {
    summary_names <- NULL
    pm <- as.list(dplyr::select(pm, -ID))
    if (detrend) {
      dates <- lubridate::my(date_seq)
      seq_start <- c(lubridate::year(min(dates)), lubridate::month(min(dates)))
      seq_end <- c(lubridate::year(max(dates)), lubridate::month(max(dates)))
      pm_ts <- ts(unlist(pm), start = seq_start, end = seq_end, frequency = 12)
      pm_rem <- stl_remainder(
        x = pm_ts,
        start = seq_start,
        end = seq_end,
        robust = TRUE,
        ...
      )
      pm <- setNames(as.list(pm_rem), names(pm))
    }
  }
  pollutionSeq <- structure(
    as.list(pm),
    class = "pollutionSeq",
    event_month = names(pm)[[median(1:length(pm))]],
    pre_event = names(pm)[1:(median(1:length(pm)) - 1)],
    post_event = names(pm)[(median(1:length(pm)) + 1):length(pm)],
    geo_radius = geo_radius,
    geo_unit = geo_unit,
    geo_location = geo_loc,
    geo_crs = sf::st_crs(x),
    summary_names = summary_names,
    detrended = detrend
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

# Print method for pollutionSeqCollection
print.pollutionSeqCollection <- function(x, ...) {
  cat("Collection of", attr(x, "n_collection"), "pollutionSeq objects\n")
  cat(
    "Date range:",
    as.character(attr(x, "date_min")), "to",
    as.character(attr(x, "date_max")), "\n"
  )
  invisible(x)
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
  year_seq <- vapply(date_seq, lubridate::year, numeric(1))
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

# For a numeric vector (assumed to be monthly values), decompose using STL
# and return the remainder values
stl_remainder <- function(x,
                          start = c(1998, 1),
                          end = c(2021, 12),
                          verbose = FALSE,
                          seasonal_only = FALSE,
                          ...) {
  pm_ts <- ts(
    data = x,
    start = start,
    end = end,
    frequency = 12
  )
  pm_stl <- stl(pm_ts, s.window = "periodic")
  if (seasonal_only) {
    stl_rem <- as.numeric(pm_stl$time.series[, "remainder"])
               + as.numeric(pm_stl$time.series[, "trend"])
  } else {
    stl_rem <- as.numeric(pm_stl$time.series[, "remainder"])
  }
  if (verbose) {
    plot(pm_stl)
    cat("% of total variance explained:\n")
    print(apply(pm_stl$time.series, 2, var) / var(pm_ts))
  }
  return(stl_rem)
}

# Calculate a weighted mean within a terra dataframe (respects NSE)
terra_weighted_mean <- function(x) {
  num <- sum(x * get("fraction", envir = parent.frame()))
  denom <- sum(get("fraction", envir = parent.frame()))
  return(num/denom)
}

# Cross between ggplot2's theme_minimal and theme_light
theme_minimal_light <- function(...) {
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(face = "bold"),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(color = "gray90"),
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
    strip.background = ggplot2::element_blank(),
    ...
  )
}

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
