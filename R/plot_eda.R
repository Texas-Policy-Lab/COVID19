# library(tidyverse)
# 
# # test1 <- county %>%
# #   filter(State == "TX")
# # 
# # test2 <- county %>%
# #   filter(stateFIPS == 48)
# # 
# # setdiff(test1,test2)
# 
# # get_date_params <- function(plot_start_date = "2020-03-03") {
#   
#   max_date <- max(county$Date)
#   min_date <- min(county$Date)
#   plot_start_date <- as.Date("2020-03-03")
# 
# # }
# 
# county %>%
#   filter(State == "TX") %>%
#   group_by(Date) %>% 
#   summarise(confirmed = sum(confirmed)) %>% 
#   ggplot(aes(x = Date, y = confirmed)) +
#   # geom_point() +
#   geom_line() +
#   scale_x_date(limits = c(plot_start_date, max_date))
# 
# county %>%
#   filter(State == "TX") %>%
#   group_by(Date) %>% 
#   summarise(confirmed = sum(confirmed)) %>% 
#   ggplot(aes(x = Date, y = confirmed)) +
#   geom_col() +
#   scale_x_date(limits = c(plot_start_date, max_date))
# 
# ## top counties: used to make a line chart with less clutter
# ## change top_n to the specified number of lines you'd like to show
# ## we can also make this a dropdown/slider for users so they can specify how many lines they see
# 
# top_counties <- county %>% 
#   filter(State == "TX") %>% 
#   group_by(County.Name) %>% 
#   summarise(confirmed = sum(confirmed)) %>% 
#   top_n(6, confirmed) %>% 
#   ungroup()
#   
# county %>%   
#   filter(State == "TX",
#          County.Name %in% top_counties$County.Name) %>%
#   group_by(Date, County.Name) %>% 
#   summarise(confirmed = sum(confirmed)) %>% 
#   ggplot(aes(x = Date, y = confirmed, color = County.Name)) +
#   geom_line() +
#   scale_x_date(limits = c(plot_start_date, max_date))
# 
# county %>%
#   filter(State == "TX",
#          County.Name %in% top_counties$County.Name) %>%
#   group_by(Date, County.Name) %>% 
#   summarise(confirmed = sum(confirmed)) %>% 
#   ggplot(aes(x = Date, y = confirmed, fill = County.Name)) +
#   geom_col(position = "stack") +
#   scale_x_date(limits = c(plot_start_date, max_date))
