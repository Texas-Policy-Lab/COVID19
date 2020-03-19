# library(ggplot2)
# library(magrittr)
# 
# confirmed <- read.csv("~/Projects/covid19/data/usafacts/covid_confirmed.csv",
#                fileEncoding="UTF-8-BOM",
#                stringsAsFactors = FALSE) %>% 
#   tidyr::gather(Date, confirmed, -c(countyFIPS, County.Name, State, stateFIPS))
# 
# deaths <- read.csv("~/Projects/covid19/data/usafacts/covid_deaths.csv",
#                       fileEncoding="UTF-8-BOM",
#                       stringsAsFactors = FALSE) %>% 
#   tidyr::gather(Date, deaths, -c(countyFIPS, County.Name, State, stateFIPS))
# 
# df <- confirmed %>% 
#   dplyr::left_join(deaths) %>% 
#   dplyr::mutate(Date = gsub("X", "", Date)
#                ,Date = gsub("[.]", "/", Date)
#                ,Date = lubridate::mdy(Date))
# 
# county <- df %>%
#   dplyr::arrange(Date, State) %>% 
#   dplyr::left_join(census.county_pop()) %>% 
#   dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
#                ,death_per100k = (deaths/pop)*100000)
# 
# state <- df %>%
#   dplyr::arrange(Date, State) %>%
#   dplyr::group_by(Date, State, stateFIPS) %>%
#   dplyr::summarise(deaths = sum(deaths)
#                   ,confirmed = sum(confirmed)) %>%
#   dplyr::left_join(census.state_pop()) %>%
#   dplyr::left_join(testing()) %>% 
#   dplyr::mutate(confirm_per_100k = (confirmed/pop)*100000
#                ,deaths_per100k = (deaths/pop)*100000
#                ,tests_per_100k = (total_tests/pop)*100000
#                )
# 
# state[is.na(state)] <- 0
# 
# usa <- state %>%
#   dplyr::arrange(Date, State) %>% 
#   dplyr::group_by(Date) %>%
#   dplyr::summarise(deaths = sum(deaths)
#                   ,confirmed = sum(confirmed))
# 
# 
# ggplot(state %>% 
#          dplyr::filter(State == "TX"),
#        aes(x = Date, y = confirmed, fill = as.factor(State))) +
#   geom_point()
# 
# ggplot(usa,
#        aes(x = Date, y = confirmed)) +
#   geom_point() %>% 
# 
# ggplot(state %>% 
#          dplyr::filter(State == "TX"),
#        aes(x = Date, y = deaths, fill = as.factor(State))) +
#   geom_point()
# 
# ggplot(usa,
#        aes(x = Date, y = deaths)) +
#   geom_point()
# 
# ggplot(state %>% 
#          dplyr::filter(State %in% c("WA", "CA", "TX", "NY", "FL")),
#        aes(x = Date, y = tests_per_100k, color = as.factor(State))) +
#   geom_point() + 
#   geom_line()
