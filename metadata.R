#' Query API Metadata endpoint.
#' 
#' @return `TRUE`, invisibly.
#' @examples
#' \dontrun{
#'   is_available()
#' }
#' @export
is_available <- function() {
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  url <- paste0(
    "https://aqs.epa.gov/data/api/metaData/isAvailable?email=",
    email,
    "&key=",
    key
  )
  req <- httr::stop_for_status(httr::GET(url))
  data <- httr::content(req, as = "parsed")
  cat(data$Header[[1]]$status, "\n")
  return(invisible(TRUE))
}
