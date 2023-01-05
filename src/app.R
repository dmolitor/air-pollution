library(bslib)
library(dplyr)
library(ggplot2)
library(here)
library(leafem)
library(leaflet)
library(lubridate)
library(raster)
library(readr)
library(shiny)
library(terra)
library(viridis)

# Functions ---------------------------------------------------------------

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

# Function that creates a leaflet plot of PM monitors and surface-level PM
plot_pollution <- function(monitors,
                           pm,
                           year, 
                           month,
                           pollutants = c("PM2.5", "PM10", "both"),
                           state = "All") {
  year_month <- ymd(paste(year, month, "01"))
  state_list <- state
  if (length(state_list) == 1 && state_list == "All") {
    state_list <- states()
  }
  pollutants <- match.arg(pollutants)
  monitors <- mutate(
    monitors,
    active_flag = was_active(
      year,
      month,
      monitor_start_date,
      last_sample_date,
      active
    )
  )
  monitors <- filter(
    monitors,
    pollutant %in% pollutants,
    state %in% state_list,
    active_flag
  )
  pm <- raster(
    pm[[
      paste(as.character(lubridate::month(year_month, TRUE, FALSE)), year)
    ]]
  )
  pal <- colorNumeric(inferno(100), values(pm), na.color = "transparent")
  leaf_plot <- leaflet(data = monitors) |>
    addTiles() |>
    addMarkers(lng = ~ longitude, lat = ~ latitude, popup = ~ map_popup) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addProviderTiles(providers$Stamen.TonerLines) |>
    addGeoRaster(
      x = pm,
      colorOptions = colorOptions(palette = inferno(100)),
      resolution = 70,
      opacity = 0.7,
      autozoom = FALSE
    ) |>
    addLegend(
      pal = pal,
      values = values(pm),
      title = "\U03BCg/m\U00B3"
    )
  return(leaf_plot)
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

# Was a monitor active in a given Year-Month?
was_active <- function(year, month, monitor_start, monitor_end, is_active) {
  year_month <- dmy(paste("15", month, year))
  year_month_start <- rollback(year_month, TRUE)
  year_month_end <- rollforward(year_month)
  activity_status <- (
    monitor_start <= year_month_start
    | monitor_start %within% interval(year_month_start, year_month_end)
  ) &
    (
      is_active
      | monitor_end >= year_month_end
      | monitor_end %within% interval(year_month_start, year_month_end)
    )
  return(activity_status)
}

# Data --------------------------------------------------------------------

# Load monitor locations
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

# Load surface-level pollution
surface_pm2.5_files <- list.files(here("data/monthly-pm2.5/"), full.names = TRUE)
surface_pm2.5 <- rast(surface_pm2.5_files[26:length(surface_pm2.5_files)])
names(surface_pm2.5) <- unlist(
  lapply(
    2000:2021,
    function(year) {
      paste(
        as.character(lubridate::month(1:12, label = TRUE, abbr = FALSE)),
        year
      )
    }
  )
)

# Shiny App ---------------------------------------------------------------

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "litera"),
  titlePanel(
    title = "PM2.5 Monitors and Ground-level PM2.5 Levels",
    windowTitle = "PM2.5 Pollution"
  ),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("selectYear", "Year", 2000:2021, 2021),
      selectInput(
        inputId = "selectMonth",
        label = "Month",
        choices = as.character(month(1:12, TRUE, FALSE)),
        selected = "January"
      ),
      selectInput(
        inputId = "selectStates",
        label = "States",
        choices = c("All", states()),
        selected = "New York",
        multiple = TRUE,
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      leafletOutput(outputId = "leafPlot", height = 600)
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  output$leafPlot <- renderLeaflet({
    req(input$selectStates)
    plot_pollution(
      monitors = all_monitors,
      pm = surface_pm2.5,
      year = input$selectYear,
      month = input$selectMonth,
      state = input$selectStates
    )
  })
}

# Deploy app
shinyApp(ui, server, options = list("launch.browser" = TRUE))
