update_timeline <- function() useMethod("update_timeline")

update_timeline.country <- function(world,
                                    pth = "./data/timeline/country.csv") {
  
  df <- read.csv(here::here(pth), stringsAsFactors = FALSE) %>% 
    dplyr::mutate(Date = lubridate::mdy(Date))
  
  first_deaths <- world %>% 
    dplyr::select(deaths, Date, countryName) %>%
    dplyr::filter(deaths > 0) %>% 
    dplyr::arrange(Date, countryName)  %>% 
    dplyr::group_by(countryName) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(Date, countryName) %>% 
    dplyr::mutate(event = "First death", label = "First death reported", link = NA)
  
  first_confirmed <- world %>% 
    dplyr::select(confirmed, Date, countryName) %>%
    dplyr::filter(confirmed > 0) %>%
    dplyr::arrange(Date, countryName) %>%
    dplyr::group_by(countryName) %>%
    dplyr::slice(1) %>%
    dplyr::select(Date, countryName) %>%
    dplyr::mutate(event = "First confirmed", label = "First confirmed case reported", link = NA)
  
  df <- df %>%
    dplyr::bind_rows(first_deaths) %>%
    dplyr::bind_rows(first_confirmed)
  
  return(df)
}


update_timeline.state <- function(state,
                                  pth = "./data/timeline/state.csv") {

  df <- read.csv(here::here(pth), stringsAsFactors = FALSE) %>% 
    dplyr::mutate(Date = lubridate::mdy(Date))

  first_deaths <- state %>% 
    dplyr::select(deaths, Date, stateName) %>%
    dplyr::filter(deaths > 0) %>% 
    dplyr::arrange(Date, stateName)  %>% 
    dplyr::group_by(stateName) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(Date, stateName) %>% 
    dplyr::mutate(event = "First death", label = "First death reported", link = NA)

  first_confirmed <- state %>% 
    dplyr::select(confirmed, Date, stateName) %>%
    dplyr::filter(confirmed > 0) %>%
    dplyr::arrange(Date, stateName) %>%
    dplyr::group_by(stateName) %>%
    dplyr::slice(1) %>%
    dplyr::select(Date, stateName) %>%
    dplyr::mutate(event = "First confirmed", label = "First confirmed case reported", link = NA)

  df <- df %>%
    dplyr::bind_rows(first_deaths) %>%
    dplyr::bind_rows(first_confirmed)

  return(df)
}

update_timeline.county <- function(county,
                                   pth = "./data/timeline/county.csv") {

  df <- read.csv(here::here(pth), stringsAsFactors = FALSE) %>% 
    dplyr::mutate(Date = lubridate::mdy(Date))

  first_deaths <- county %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Date, stateName, countyName, deaths) %>% 
    dplyr::filter(deaths > 0) %>% 
    dplyr::arrange(Date, stateName, countyName) %>% 
    dplyr::group_by(stateName, countyName) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(Date, countyName, stateName) %>% 
    dplyr::mutate(event = "First death", label = "First death reported", link = NA)

  first_confirmed <- county %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Date, stateName, countyName, confirmed) %>%
    dplyr::filter(confirmed > 0) %>%
    dplyr::arrange(Date, stateName, countyName) %>%
    dplyr::group_by(stateName, countyName) %>%
    dplyr::slice(1) %>%
    dplyr::select(Date, countyName, stateName) %>%
    dplyr::mutate(event = "First confirmed", label = "First confirmed case reported", link = NA)

  df <- df %>% 
    dplyr::bind_rows(first_deaths) %>% 
    dplyr::bind_rows(first_confirmed)

  return(df)
}