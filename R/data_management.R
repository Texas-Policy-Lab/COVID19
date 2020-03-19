create_data <- function(...) UseMethod("create_data")

create_data.default <- function(confirmed_df, deaths_df, ...) {

  df <- confirmed_df %>%
    dplyr::left_join(deaths_df) %>%
    dplyr::mutate(Date = gsub("X", "", Date)
                  ,Date = gsub("[.]", "/", Date)
                  ,Date = lubridate::mdy(Date))
  
}
create_data.county <- function(confirmed_df, deaths_df) {
  
  county <- create_data.default(confirmed_df, deaths_df) %>%
    dplyr::arrange(Date, State) %>%
    dplyr::left_join(census.county_pop()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                  ,death_per100k = (deaths/pop)*100000)
  
  return(county)
}

create_data.state <- function(confirmed_df, deaths_df, geocodes) {

  state <- create_data.default(confirmed_df, deaths_df) %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date, State, stateFIPS) %>%
    dplyr::summarise(deaths = sum(deaths)
                     ,confirmed = sum(confirmed)) %>%
    dplyr::left_join(fips_xwalk.state(geocodes)) %>%
    dplyr::left_join(census.state_pop()) %>%
    dplyr::left_join(testing()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                  ,deaths_per100k = (deaths/pop)*100000
                  ,tests_per_100k = (total_tests/pop)*100000
    )

  state[is.na(state)] <- 0

  return(state)
}

create_data.usa <- function(confirmed_df, deaths_df) {

  usa <- state %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(deaths = sum(deaths)
                     ,confirmed = sum(confirmed))
  return(usa)
}

fips_xwalk <- function(geocodes) UseMethod("fips_xwalk")

fips_xwalk.state <- function(geocodes) {
  
  geocodes %>%
    dplyr::filter(SummaryLevel == 40) %>% 
    dplyr::select(stateFIPS, label) %>% 
    dplyr::rename(stateName = label)
}
