#' @title TPL timestamp
#' @description Creates a timestamp for date/time of last data update
#' @param version the version of the dashboard
#' @export
timestamp <- function() {
  
  date <- date()
  
  formatted_date <- format(strptime(date, format = "%A %B %d %H:%M:%S %Y"), "%A, %B %d %I:%M:%S %p")
  
  # htmltools::tags$div(class = "timestamp_container",
  # htmltools::tags$div(class = "timestamp"
  #                     ,htmltools::tags$p("Last updated:", htmltools::br(), formatted_date)
  #                     )
  # )
  
   
}