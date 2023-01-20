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

source(here("src/utils.R"))

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
