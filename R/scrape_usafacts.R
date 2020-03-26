usa_facts_data <- function(...) UseMethod("usa_facts_data")

#' @title Get USAFacts
#' @description Get USAFacts and download the data
#' @param url string. The url to the usafacts.org data default ("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/")
#' @param confirmed string. The name of the confirmed deaths csv file which will be downloaded.
#' @param deaths string. The name of the confirmed deaths csv file which will be downloaded.
#' @export
usa_facts_data.default <- function(url = "https://static.usafacts.org/public/data/covid-19/",
                                   csv_name) {

  df <- readr::read_csv(paste0(url, csv_name))

  utils::write.csv(df, here::here("./data/usafacts/", csv_name))

  return(df)
}

#' @title Get USAFacts: confirmed data
#' @description Calls the usa_facts_data.default functions, downloads and writes data to data/usafacts 
#' @param csv_name string. Default is "covid_confirmed_usafacts.csv"
#' @export
usa_facts_data.confirmed <- function(csv_name = "covid_confirmed_usafacts.csv") {

  usa_facts_data(csv_name = csv_name)
}

#' @title Get USAFacts: deaths data
#' @description Calls the usa_facts_data.default functions, downloads and writes data to data/usafacts 
#' @param csv_name string. Default is "covid_deaths_usafacts.csv"
#' @export
usa_facts_data.deaths <- function(csv_name = "covid_deaths_usafacts.csv") {

  usa_facts_data(csv_name = csv_name)
}
