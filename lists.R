#' List all states/territories/regions with corresponding FIPS codes
#' 
#' @return A data.frame of corresponding regions and codes
#' @examples
#' \dontrun{
#'   list_states()
#' }
#' @export
list_states <- function() {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/list/states?email=",
    email,
    "&key=",
    key
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  return(data$Data)
}

#' List counties within a state/territory/region with corresponding FIPs
#' 
#' @param state A state/territory/region's FIPs code
#' @return A data.frame with corresponding counties and codes
#' @examples
#' \dontrun{
#'   list_state_counties("37")
#' }
#' @export
list_state_counties <- function(state) {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/list/countiesByState?email=",
    email,
    "&key=",
    key,
    "&state=",
    state
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  return(data$Data)
}

#' List pollution monitoring sites within a county
#' 
#' @param state A state/territory/region's FIPs code
#' @param county A state/territory/region's county FIPs code
#' @return A data.frame with corresponding site names and codes
#' @examples
#' \dontrun{
#'   list_county_sites("37", "089")
#' }
#' @export
list_county_sites <- function(state, county) {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/list/sitesByCounty?email=",
    email,
    "&key=",
    key,
    "&state=",
    state,
    "&county=",
    county
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  return(data$Data)
}

#' List parameter classes
#' 
#' @return A data.frame of parameter classes
#' @examples
#' \dontrun{
#'   list_parameter_classes()
#' }
#' @export
list_parameter_classes <- function() {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/list/classes?email=",
    email,
    "&key=",
    key
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  return(data$Data)
}

#' List parameters within a class
#' 
#' @param param_class Parameter class to return all parameters from
#' @return A data.frame of class parameters
#' @examples
#' \dontrun{
#'   list_class_parameters("PM COARSE")
#' }
#' @export
list_class_parameters <- function(param_class) {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/list/parametersByClass?email=",
    email,
    "&key=",
    key,
    "&pc=",
    URLencode(param_class)
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  return(data$Data)
}

#' List monitors by site location
#' 
#' @param pollutants 5-digit AQS pollutant code, or vector of up to 5 pollutant
#'   codes.
#' @param begin_date Begin date in YYYYMMDD format
#' @param end_date End date in YYYYMMDD format
#' @param state 2-digit state FIPS code
#' @param county 3-digit county FIPS code
#' @param site 4-digit AQS site number
#' @return A data.frame of relevant site information
#' @examples
#' \dontrun{
#'   list_monitors_site(
#'     pollutants = c(81104, 88101, 88500, 88501),
#'     begin_date = "20211201",
#'     end_date = "20221201",
#'     state = "27",
#'     county = "123",
#'     site = "6030"
#'   )
#' }
#' @export
list_monitors_site <- function(pollutants,
                               begin_date,
                               end_date,
                               state,
                               county,
                               site) {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/monitors/bySite?email=",
    email,
    "&key=",
    key,
    "&param=",
    paste0(pollutants, collapse = ","),
    "&bdate=",
    begin_date,
    "&edate=",
    end_date,
    "&state=",
    state,
    "&county=",
    county,
    "&site=",
    site
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  if (identical(data$Data, list())) data$Data <- NULL
  return(data$Data)
}

#' List monitors by state
#' 
#' @param pollutants 5-digit AQS pollutant code, or vector of up to 5 pollutant
#'   codes.
#' @param begin_date Begin date in YYYYMMDD format
#' @param end_date End date in YYYYMMDD format
#' @param state 2-digit state FIPS code
#' @return A data.frame of relevant site information
#' @examples
#' \dontrun{
#'   list_monitors_state(
#'     pollutants = c(81104, 88101, 88500, 88501),
#'     begin_date = "20211201",
#'     end_date = "20221201",
#'     state = "27"
#'   )
#' }
#' @export
list_monitors_state <- function(pollutants, begin_date, end_date, state) {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/monitors/byState?email=",
    email,
    "&key=",
    key,
    "&param=",
    paste0(pollutants, collapse = ","),
    "&bdate=",
    begin_date,
    "&edate=",
    end_date,
    "&state=",
    state
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- jsonlite::fromJSON(
    httr::content(req, as = "text"),
    simplifyVector = TRUE
  )
  if (identical(data$Data, list())) data$Data <- NULL
  return(data$Data)
}

#' List monitors across states
#' 
#' @param pollutants 5-digit AQS pollutant code, or vector of up to 5 pollutant
#'   codes.
#' @param begin_date Begin date in YYYYMMDD format
#' @param end_date End date in YYYYMMDD format
#' @param states Vector of 2-digit state FIPS code
#' @return A data.frame of relevant site information
#' @examples
#' \dontrun{
#'   list_monitors_states(
#'     pollutants = c(81104, 88101, 88500, 88501),
#'     begin_date = "20211201",
#'     end_date = "20221201",
#'     states = c("27", "28")
#'   )
#' }
#' @export
list_monitors_states <- function(pollutants, begin_date, end_date, states) {
  pb <- txtProgressBar(min = 0, max = length(states), style = 3)
  states_data <- lapply(
    1:length(states),
    function(i) {
      out <- list_monitors_state(pollutants, begin_date, end_date, states[[i]])
      setTxtProgressBar(pb, i)
      return(out)
    }
  )
  return(do.call(rbind, states_data))
}
