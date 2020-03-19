parse_site <- function() {
  
  usa_facts_url <-
    "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"
  
  site <- xml2::read_html(usa_facts_url)
  
  links <- site %>%
    rvest::html_nodes(".ts-rich-text a") %>%
    rvest::html_attr("href")
  
  csvs <- links[stringr::str_detect(links, ".csv")]
  
  confirmed <-
    csvs[stringr::str_detect(csvs, "covid_confirmed_usafacts.csv")]
  deaths <-
    csvs[stringr::str_detect(csvs, "covid_deaths_usafacts.csv")]
  
}

## CSV urls
# covid_confirmed_url <- "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv?_ga=2.78308380.902150832.1584655257-679521212.1584655257"
# covid_deaths_url <- "https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv?_ga=2.74786458.902150832.1584655257-679521212.1584655257"

create_confirmed_data <- function(url) {
  
  covid_confirmed <- readr::read_csv(url)
  
  utils::write.csv(covid_confirmed, here::here("./data/usafacts/covid_confirmed_TEST.csv"))
  
}

create_deaths_data <- function(url) {
  
  covid_deaths <- readr::read_csv(url)
  
  utils::write.csv(covid_deaths, here::here("./data/usafacts/covid_deaths_TEST.csv"))
  
}

parse_site()
create_deaths_data(deaths)
create_confirmed_data(confirmed)
