#' @title Download testing
#' @description Downloads the testing data from the Covid Tracking Project (https://covidtracking.com/)
#' @param url the url to get the data from
#' @export
testing <- function(url = "https://covidtracking.com/api/states/daily") {

  df <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE) %>% 
    dplyr::rename(State = state,
                  total_tests = total) %>% 
    dplyr::mutate(Date = lubridate::ymd(paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep = "/"))) %>% 
    dplyr::select(-date)
}
