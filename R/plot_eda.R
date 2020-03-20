library(tidyverse)

# test1 <- county %>%
#   filter(State == "TX")
# 
# test2 <- county %>%
#   filter(stateFIPS == 48)
# 
# setdiff(test1,test2)

# get_date_params <- function(plot_start_date = "2020-03-03") {
  
  max_date <- max(county$Date)
  min_date <- min(county$Date)
  plot_start_date <- as.Date("2020-03-03")

# }

county %>%
  filter(State == "TX") %>%
  group_by(Date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = Date, y = confirmed)) +
  # geom_point() +
  geom_line() +
  scale_x_date(limits = c(plot_start_date, max_date))

county %>%
  filter(State == "TX") %>%
  group_by(Date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = Date, y = confirmed)) +
  geom_col() +
  scale_x_date(limits = c(plot_start_date, max_date))

## top counties: used to make a line chart with less clutter
## change top_n to the specified number of lines you'd like to show
## we can also make this a dropdown/slider for users so they can specify how many lines they see

top_counties <- county %>% 
  filter(State == "TX") %>% 
  group_by(County.Name) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  top_n(6, confirmed) %>% 
  ungroup()
  
county %>%   
  filter(State == "TX",
         County.Name %in% top_counties$County.Name) %>%
  group_by(Date, County.Name) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = Date, y = confirmed, color = County.Name)) +
  geom_line() +
  scale_x_date(limits = c(plot_start_date, max_date))

county %>%
  filter(State == "TX",
         County.Name %in% top_counties$County.Name) %>%
  group_by(Date, County.Name) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = Date, y = confirmed, fill = County.Name)) +
  geom_col(position = "stack") +
  scale_x_date(limits = c(plot_start_date, max_date))

tx_counties <- map_data("county") %>% filter(region == "texas") %>% mutate(subregion = str_to_title(subregion))
tx_county_stats <- county %>% 
  filter(State == "TX") %>% 
  group_by(County.Name) %>% 
  summarise(confirmed = sum(confirmed), deaths = sum(deaths)) %>% 
  mutate(County.Name = gsub("County", "", County.Name),
         County.Name = str_trim(County.Name))

merged <- full_join(tx_county_stats, tx_counties, by = c("County.Name" = "subregion"))

ggplot(data = merged, mapping = aes(x = long, y = lat, group = County.Name, fill = confirmed)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black") +
  labs(fill = "Confirmed Cases") +
  # ggmap::theme_nothing(legend = TRUE) +
  theme(legend.title = element_text(),
        legend.key.height = unit(.1, "in"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Proxima Nova"),
        legend.position = "bottom") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Confirmed COVID-19 Cases in Texas") +
  theme(
  axis.text = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),
  legend.margin = ggplot2::margin(t = 6L, r = 6L, b = 6L)
)

