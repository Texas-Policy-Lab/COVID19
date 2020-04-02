

ui <- tpl_ui(title = config$ui$dashboardtitle$title
             ,tabs = config$ui$tabs
             ,js_pth = list.files(config$js, full.names = TRUE)
             ,css_pth = list.files(config$css, full.names = TRUE)
             ,favicon_pth = config$favicon
             ,world = world
             ,state = state
             ,county = county)
