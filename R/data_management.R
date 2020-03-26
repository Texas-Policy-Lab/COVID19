#' @title Creates data
#' @export
create_data <- function(...) UseMethod("create_data")

create_data.default <- function(...) {

  confirmed_df <- usa_facts_data.confirmed() %>% 
    tidyr::gather(Date, confirmed, -c(countyFIPS, `County Name`, State, stateFIPS)) %>%
    dplyr::mutate(Date = gsub("X", "", Date)
                 ,Date = gsub("[.]", "/", Date)
                 ,Date = lubridate::mdy(Date))

  deaths_df <- usa_facts_data.deaths() %>% 
    tidyr::gather(Date, deaths, -c(countyFIPS, `County Name`, State, stateFIPS)) %>% 
    dplyr::mutate(Date = gsub("X", "", Date)
                 ,Date = gsub("[.]", "/", Date)
                 ,Date = lubridate::mdy(Date))
  
  df <- confirmed_df %>%
    dplyr::left_join(deaths_df)
  
}

#' @title Create county level data
#' @export
create_data.county <- function(write = FALSE, day100 = as.Date("2020-1-18"),
                               pth = "./data/xwalk_state_stateAbb.csv", ...) {
  
  county <- create_data.default() %>%
    dplyr::group_by(countyFIPS, State, Date) %>%
    dplyr::slice(dplyr::n()) %>% 
    dplyr::arrange(Date, State) %>%
    dplyr::left_join(census.county_pop()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                  ,death_per100k = (deaths/pop)*100000) %>% 
    dplyr::arrange(countyFIPS, State, Date) %>% 
    dplyr::group_by(countyFIPS, State) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                 ,deaths_lag = dplyr::lag(deaths)
                 ,dy_confirmed = confirmed - confirmed_lag
                 ,dy_deaths = deaths - deaths_lag
                 ,pct_chg_confirmed = (dy_confirmed/confirmed)*100
                 ,pct_chg_deaths = (dy_deaths/deaths)*100
                 ,ndayssince100 = Date - day100
                 ,ndays = seq(1, dplyr::n())
    ) %>% 
    dplyr::arrange(desc(Date)) %>% 
    dplyr::rename(countyName = `County Name`,
                  stateAbb = State) %>% 
    dplyr::left_join(read.csv(here::here(pth), stringsAsFactors = FALSE))
  
  if(write) {
    write.csv(county, file.path("data", "county.csv"), row.names = FALSE)
  }
  
  return(county)
}

#' @title Create state level data
#' @export
create_data.state <- function(write = FALSE, day100 = as.Date("2020-1-18"), ...) {
  
  state <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date, State, stateFIPS) %>%
    dplyr::summarise(deaths = sum(deaths)
                     ,confirmed = sum(confirmed)) %>%
    dplyr::left_join(fips_xwalk.state()) %>%
    dplyr::left_join(census.state_pop()) %>%
    dplyr::left_join(testing()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                  ,deaths_per100k = (deaths/pop)*100000
                  ,tests_per_100k = (total_tests/pop)*100000
                  ,positive_per100k = (positive/pop)*100000
                  ,negative_per100k = (negative/pop)*100000
                  ,pending_per100k = (pending/pop)*100000
    ) %>% 
    dplyr::arrange(State, Date) %>% 
    dplyr::group_by(State) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                  ,deaths_lag = dplyr::lag(deaths)
                  ,tests_lag = dplyr::lag(total_tests)
                  ,positive_lag = dplyr::lag(positive)
                  ,negative_lag = dplyr::lag(negative)
                  ,pending_lag = dplyr::lag(pending)
                  ,dy_confirmed = confirmed - confirmed_lag
                  ,dy_deaths = deaths - deaths_lag
                  ,dy_tests = total_tests - tests_lag
                  ,dy_positive = positive - positive_lag
                  ,dy_negative = negative - negative_lag
                  ,pct_chg_confirmed = (dy_confirmed/confirmed)*100
                  ,pct_chg_deaths = (dy_deaths/deaths)*100
                  ,pct_chg_tests = (dy_tests/total_tests)*100
                  ,pct_chg_postive = (dy_positive/positive)*100
                  ,pct_chg_negative = (dy_negative/negative)*100
                  ,ndayssince100 = Date - day100
    ) %>%
    dplyr::arrange(desc(Date)) %>% 
    dplyr::mutate(ndays = seq(1, dplyr::n())
    ) %>%
    dplyr::rename(stateAbb = State)
  
  if(write) {
    write.csv(state, file.path("data", "state.csv"), row.names = FALSE)
  }
  
  return(state)
}

#' @title Create usa-level data
#' @export
create_data.usa <- function(write = FALSE, day100 = as.Date("2020-1-18"), ...) {
  
  usa <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(deaths = sum(deaths)
                     ,confirmed = sum(confirmed)) %>% 
    dplyr::arrange(Date) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                  ,deaths_lag = dplyr::lag(deaths)
                  ,dy_confirmed = confirmed - confirmed_lag
                  ,dy_deaths = deaths - deaths_lag
                  ,pct_chg_confirmed = (dy_confirmed/confirmed)*100
                  ,pct_chg_deaths = (dy_deaths/deaths)*100
                  ,ndayssince100 = Date - day100) %>% 
    dplyr::arrange(desc(Date)) %>% 
    dplyr::mutate(ndays = seq(1, dplyr::n())
    )
  
  if(write) {
    write.csv(usa, file.path("data", "usa.csv"), row.names = FALSE)
  }
  return(usa)
}

#' @title Create data nation-level data
#' @export
create_data.world <- function(write = FALSE, day100 = as.Date("2020-1-18"), ...) {
  
  world <- csse_data.confirmed() %>%
    dplyr::left_join(csse_data.deaths()) %>%
    dplyr::ungroup() %>% 
    dplyr::rename(countryName = Country.Region) %>% 
    dplyr::mutate(Date = gsub("X", "", Date)
                  ,Date = gsub("[.]", "/", Date)
                 ,Date = lubridate::mdy(Date)
                 ,countryName = gsub("[^[:alnum:] ]", "", countryName)) %>% 
    dplyr::arrange(Date, countryName) %>% 
    dplyr::group_by(countryName) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                 ,deaths_lag = dplyr::lag(deaths)
                 ,dy_confirmed = confirmed - confirmed_lag
                 ,dy_deaths = (deaths - deaths_lag)
                 ,pct_chg_confirmed = (dy_confirmed/confirmed)*100
                 ,pct_chg_deaths = (dy_deaths/deaths)*100
                 ,ndayssince100 = Date - day100
    ) %>% 
    dplyr::arrange(desc(Date)) %>% 
    dplyr::mutate(ndays = seq(1, dplyr::n())
                  )

  if(write) {
    write.csv(world, file.path("data", "world.csv"), row.names = FALSE)
  }
  
  return(world)
}

#' @title Create fips xwalk
#' @export
fips_xwalk <- function(geocodes) UseMethod("fips_xwalk")

#' @title Creates FIPS crosswalk
#' @description Creates the FIPS crosswalk between FIPS codes and state names
#' @param geocodes_pth string. The path to the geocodes data.
fips_xwalk.state <- function(geocodes_pth = "./data/census/geocodes.csv") {

  geocodes <- read.csv(here::here(geocodes_pth),
                       fileEncoding="latin1",
                       stringsAsFactors = FALSE)

  geocodes %>%
    dplyr::filter(SummaryLevel == 40) %>% 
    dplyr::select(stateFIPS, label) %>% 
    dplyr::rename(stateName = label)
}