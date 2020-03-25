#' @title TPL timestamp
#' @description Creates a timestamp for date/time of last data update
#' @param version the version of the dashboard
#' @export
timestamp <- function() {
  
  date <- date()
  tokens <- strsplit(date, " ")
  weekday <- tokens[[1]][1]
  month <- tokens[[1]][2]
  day <- tokens[[1]][3]
  time <- tokens[[1]][4]
  year <- tokens[[1]][5]
  
  formatted_time <- format(strptime(time, format='%H:%M:%S'), '%I:%M:%S %p')
  
  formatted_weekday <- dplyr::case_when(weekday == "Mon" ~ "Monday",
                                    weekday == "Tues" ~ "Tuesday",
                                    weekday == "Wed" ~ "Wednesday",
                                    weekday == "Thurs" ~ "Thursday",
                                    weekday == "Fri" ~ "Friday",
                                    weekday == "Sat" ~ "Saturday",
                                    weekday == "Sun" ~ "Sunday"
  )
  
  formatted_date <- paste(formatted_weekday, month, day, year, formatted_time)
 
  htmltools::tags$div(class = "timestamp_container",
  htmltools::tags$div(class = "timestamp"
                      ,htmltools::tags$p("Last updated:", htmltools::br(), formatted_date)
                      )
  )
  
   
}