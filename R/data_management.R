#' @title Creates data
#' @export
create_data <- function(...) UseMethod("create_data")

create_data.default <- function(...) {

  confirmed_df <- usa_facts_data.confirmed() %>% 
    tidyr::gather(Date, confirmed, -c(countyFIPS, `County Name`, State, stateFIPS))

  deaths_df <- usa_facts_data.deaths() %>% 
    tidyr::gather(Date, deaths, -c(countyFIPS, `County Name`, State, stateFIPS))

  df <- confirmed_df %>%
    dplyr::left_join(deaths_df) %>%
    dplyr::mutate(Date = gsub("X", "", Date)
                  ,Date = gsub("[.]", "/", Date)
                  ,Date = lubridate::mdy(Date))

}

#' @title Create county level data
#' @export
create_data.county <- function(write = FALSE, ...) {

  county <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::left_join(census.county_pop()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                 ,death_per100k = (deaths/pop)*100000) %>% 
    dplyr::arrange(State, countyFIPS, Date) %>% 
    dplyr::group_by(State, countyFIPS) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed),
                  deaths_lag = dplyr::lag(deaths),
                  pct_chg_confirmed = ((confirmed - confirmed_lag)/confirmed)*100,
                  pct_chg_deaths = ((deaths - deaths_lag)/deaths)*100
    )

  if(write) {
    write.csv(county, file.path("data", "county.csv"), row.names = FALSE)
  }
  
  return(county)
}

#' @title Create state level data
#' @export
create_data.state <- function(write = FALSE, ...) {

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
                 ,pct_chg_confirmed = ((confirmed - confirmed_lag)/confirmed)*100
                 ,pct_chg_deaths = ((deaths - deaths_lag)/deaths)*100
                 ,pct_chg_tests = ((total_tests - tests_lag)/total_tests)*100
                 ,pct_chg_postive = ((positive - positive_lag)/positive)*100
                 ,pct_chg_negative = ((negative - negative_lag)/negative)*100
    ) %>% 
    dplyr::left_join(timeline_data())

  # state[is.na(state)] <- 0

  if(write) {
    write.csv(state, file.path("data", "state.csv"), row.names = FALSE)
  }
  
  return(state)
}

#' @title Create usa-level data
#' @export
create_data.usa <- function(write = FALSE, ...) {

  usa <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(deaths = sum(deaths)
                    ,confirmed = sum(confirmed)) %>% 
    dplyr::arrange(Date) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                 ,deaths_lag = dplyr::lag(deaths)
                 ,pct_chg_confirmed = ((confirmed - confirmed_lag)/confirmed)*100
                 ,pct_chg_deaths = ((deaths - deaths_lag)/deaths)*100)
    
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
    dplyr::rename(country = Country.Region) %>% 
    dplyr::mutate(Date = gsub("X", "", Date)
                 ,Date = gsub("[.]", "/", Date)
                 ,Date = lubridate::mdy(Date)) %>% 
    dplyr::arrange(Date, country) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(confirmed_lag = dplyr::lag(confirmed)
                 ,deaths_lag = dplyr::lag(deaths)
                 ,pct_chg_confirmed = ((confirmed - confirmed_lag)/confirmed)*100
                 ,pct_chg_deaths = ((deaths - deaths_lag)/deaths)*100
                 ,ndayssince100 = Date - day100) %>%
    dplyr::left_join(timeline_data())

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

timeline_data <- function(pth = here::here("./data/covid_timeline.csv")) {

  readr::read_csv(pth) %>% 
    dplyr::mutate(Date = lubridate::mdy(Date),
                  label = paste(lubridate::month(Date, label = TRUE, abbr = TRUE),
                                " ", lubridate::day(Date), ", ", lubridate::year(Date), ": ", label, sep = ""),
                  label = stringi::stri_enc_toutf8(label))

}