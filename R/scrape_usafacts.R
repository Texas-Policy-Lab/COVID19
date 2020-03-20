usa_facts_data <- function(...) UseMethod("usa_facts_data")

#' @title Get USAFacts
#' @description Get USAFacts and download the data
#' @param url string. The url to the usafacts.org data default ("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/")
#' @param confirmed string. The name of the confirmed deaths csv file which will be downloaded.
#' @param deaths string. The name of the confirmed deaths csv file which will be downloaded.
#' @export
usa_facts_data.default <- function(url = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/",
                                   csv_name = NULL) {
  
  site <- xml2::read_html(url)

  links <- site %>%
    rvest::html_nodes(".ts-rich-text a") %>%
    rvest::html_attr("href")

  csvs <- links[stringr::str_detect(links, ".csv")]

  url <- csvs[stringr::str_detect(csvs, csv_name)]

  df <- readr::read_csv(url)
  
  utils::write.csv(df, here::here("./data/usafacts/", csv_name))
  
  return(df)

}

#' @title Get USAFacts: confirmed data
#' @description Calls the usa_facts_data.default functions, downloads and writes data to data/usafacts 
#' @param csv_name string. Default is "covid_confirmed_usafacts.csv"
#' @export
usa_facts_data.confirmed <- function(csv_name = "covid_confirmed_usafacts.csv") {

  usa_facts_data.default(csv_name = csv_name)
}

#' @title Get USAFacts: deaths data
#' @description Calls the usa_facts_data.default functions, downloads and writes data to data/usafacts 
#' @param csv_name string. Default is "covid_deaths_usafacts.csv"
#' @export
usa_facts_data.deaths <- function(csv_name = "covid_deaths_usafacts.csv") {

  usa_facts_data.default(csv_name = csv_name)
}
