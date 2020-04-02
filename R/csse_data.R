#' @title CSSE confirmed cases data
csse_data.confirmed <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") {
 
  df <- readr::read_csv(url) %>% 
    tidyr::gather(Date, confirmed, -c(`Province/State`, `Country/Region`, Lat, Long)) %>% 
    dplyr::group_by(Date, `Country/Region`) %>% 
    dplyr::summarise(confirmed = sum(confirmed))
}

#' @title CSSE deaths data
csse_data.deaths <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") {
  
  df <- readr::read_csv(url) %>% 
    tidyr::gather(Date, deaths, -c(`Province/State`, `Country/Region`, Lat, Long)) %>% 
    dplyr::group_by(Date, `Country/Region`) %>% 
    dplyr::summarise(deaths = sum(deaths))
}

#' @title CSSE recovered data
csse_data.recovered <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") {
  
  df <- readr::read_csv(url) %>% 
    tidyr::gather(Date, recovered, -c(`Province/State`, `Country/Region`, Lat, Long)) %>% 
    dplyr::group_by(Date, `Country/Region`) %>% 
    dplyr::summarise(recovered = sum(recovered))
}

#' @title CSSE daily data
csse_data.daily <- function(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
                            start = "2020-01-22") {

  dates <- format(seq(as.Date(start), Sys.Date() -1, "days"), "%m-%d-%Y")

  dfs <- lapply(dates, function(date) {
    
    df <- readr::read_csv(paste0(url, date, ".csv"))
    
    names(df)[grep("Province/State", names(df))] <- "Province_State"
    names(df)[grep("Country/Region", names(df))] <- "Country_Region"
    names(df)[grep("Last Update", names(df))] <- "Last_Update"
    names(df)[grep("Lat", names(df))] <- "Latitude"
    names(df)[grep("Long_", names(df))] <- "Longitude"
    
    df <- df %>% 
      mutate(Date = date)
  })
  
  dfs2 <- do.call("rbind.fill", dfs)
  
  return(dfs2)
  
}

