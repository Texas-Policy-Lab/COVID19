library(magrittr)
library(ggplot2)

rm(list = ls())
options(encoding = "UTF-8")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

county <- create_data.county(confirmed_df = confirmed,
                             deaths_df = deaths)

state <- create_data.state(confirmed_df = confirmed,
                           deaths_df = deaths,
                           geocodes = geocodes)

usa <- create_data.usa(confirmed_df = confirmed,
                       deaths_df = deaths)

ui <- tpl_ui(title = config$ui$dashboardtitle$title
            ,tabs = config$ui$tabs
            ,js_pth = list.files(config$js, full.names = TRUE)
            ,css_pth = list.files(config$css, full.names = TRUE)
            ,favicon_pth = config$favicon)

shiny::shinyApp(ui, server)
