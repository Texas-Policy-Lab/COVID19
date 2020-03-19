rm(list = ls())
options(encoding = "UTF-8")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

ui <- tpl_ui(title = config$ui$dashboardtitle$title
            ,tabs = config$ui$tabs
            ,js_pth = list.files(config$js, full.names = TRUE)
            ,css_pth = list.files(config$css, full.names = TRUE)
            ,favicon_pth = config$favicon)

shiny::shinyApp(ui, server)
