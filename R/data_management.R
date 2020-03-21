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
create_data.county <- function(...) {

  county <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::left_join(census.county_pop()) %>%
    dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
                 ,death_per100k = (deaths/pop)*100000) %>% 
    dplyr::group_by(stateFIPS, countyFIPS, Date)

  write.csv(county, file.path("data", "county.csv"), row.names = FALSE)
  
  return(county)
}

#' @title Create state level data
#' @export
create_data.state <- function(...) {

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
    )

  state[is.na(state)] <- 0

  write.csv(state, file.path("data", "state.csv"), row.names = FALSE)
  
  return(state)
}

#' @title Create usa-level data
#' @export
create_data.usa <- function(...) {

  usa <- create_data.default() %>%
    dplyr::arrange(Date, State) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(deaths = sum(deaths)
                     ,confirmed = sum(confirmed))
  
  write.csv(usa, file.path("data", "usa.csv"), row.names = FALSE)
  
  return(usa)
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
