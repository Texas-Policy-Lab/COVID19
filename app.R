library(magrittr)
library(ggplot2)

rm(list = ls())
options(encoding = "UTF-8")
tpltheme::set_tpl_theme(style = "print", font = "lato")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

county <- create_data.county()

state <- create_data.state()

usa <- create_data.usa()

world <- create_data.world()

timeline_tx_cls <- structure(list(df = state,
                                  state = "Texas",
                                  title = "Confirmed cases in the state of {} over time",
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "tx")

timeline_ca_cls <- structure(list(df = state,
                                  state = "California",
                                  title = "Confirmed cases in the state of {} over time",
                                  str_width = 100,
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "ca")

timeline_ny_cls <- structure(list(df = state,
                                  state = "New York",
                                  title = "Confirmed cases in the state of {} over time",
                                  str_width = 100,
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "ny")

timeline_us_cls <- structure(list(df = world %>% 
                                    dplyr::filter(is.na(stateName)),
                                  country = "US",
                                  title = "Confirmed cases in the United States over time",
                                  hjust = 1,
                                  nudge_x = 20),
                             class = "us")

timeline_china_cls <- structure(list(df = world,
                                     country = "China",
                                     title = "Confirmed cases in {} over time"),
                                class = "china")

timeline_italy_cls <- structure(list(df = world,
                                     country = "Italy",
                                     title = "Confirmed cases in {} over time",
                                     str_width = 100,
                                     hjust = 1,
                                     nudge_x = 20,
                                     box_padding = 1),
                                class = "italy")

timeline_cls_list <- list(tx = timeline_tx_cls,
                          ca = timeline_ca_cls,
                          ny = timeline_ny_cls,
                          us = timeline_us_cls,
                          china = timeline_china_cls,
                          italy = timeline_italy_cls)

ui <- tpl_ui(title = config$ui$dashboardtitle$title
            ,tabs = config$ui$tabs
            ,js_pth = list.files(config$js, full.names = TRUE)
            ,css_pth = list.files(config$css, full.names = TRUE)
            ,favicon_pth = config$favicon)

shiny::shinyApp(ui, server)
