#' @title TPL timestamp
#' @description Creates a timestamp for date/time of last data update
#' @param version the version of the dashboard
#' @export
timestamp <- function() {
  
  date <- date()
  
  formatted_date <- format(strptime(date, format = "%A %B %d %H:%M:%S %Y", tz = "America/Chicago"), "%A, %B %d %I:%M:%S %p %Z")
   
}