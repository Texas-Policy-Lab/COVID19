#' @title CSSE confirmed cases data
csse_data.confirmed <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") {
 
  df <- read.csv(url) %>% 
    tidyr::gather(Date, confirmed, -c(Province.State, Country.Region, Lat, Long)) %>% 
    dplyr::group_by(Date, Country.Region) %>% 
    dplyr::summarise(confirmed = sum(confirmed))
}

#' @title CSSE deaths data
csse_data.deaths <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") {
  
  df <- read.csv(url) %>% 
    tidyr::gather(Date, deaths, -c(Province.State, Country.Region, Lat, Long)) %>% 
    dplyr::group_by(Date, Country.Region) %>% 
    dplyr::summarise(deaths = sum(deaths))
}