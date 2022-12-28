#' Sign up for AQS API
#' 
#' @param email User's email address.
#' @return `TRUE`, invisibly.
#' @examples
#' \dontrun{
#'   sign_up("john.doe@gmail.com")
#' }
#' @export
sign_up <- function(email) {
  url <- paste0("https://aqs.epa.gov/data/api/signup?email=", email)
  req <- httr::stop_for_status(httr::GET(url))
  message <- httr::content(req, as = "parsed")
  cat(message$Data[[1]], "\n")
  return(invisible(TRUE))
}
