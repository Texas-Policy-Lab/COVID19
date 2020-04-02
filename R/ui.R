#' @title TPL logo
#' @param tpl_lolo_pth the path the TPL logo to use
#' @export
logo <- function(pth) {
  shiny::img(class = "logo", src = pth)
}

#' @title Create tab ids
#' @description Creates ids for the tabs
#' @param tabs the tab list
#' @export
create_tab_ids <- function(tabs) {

  test_list_item_not_null(tabs, "icon")
  test_list_item_not_null(tabs, "menu_title")
  test_list_item_not_null(tabs, "page_title")

  tabs2 <- lapply(1:length(tabs), function(i, tabs) {

    tabs[[i]][["id"]] <- paste0("tab", i)
    tabs_setup <- structure(tabs[[i]], class = names(tabs)[[i]])

    return(tabs_setup)
  }, tabs = tabs)

  names(tabs2) <- names(tabs)
  return(tabs2)
}

#' @title TPL header
#' @description Creates the header for the TPL dashboard
#' @param title the dashboard title
#' @export
header <- function(title) {
    header <- shinydashboard::dashboardHeader(title = title)
}

#' @title TPL sidebar panel
#' @description Uses the configuration file to automatically create the sidebar panel
#' @inheritParams create_tab_ids
#' @param tab_icons a vector of icon names the same length as the tab_names (default is NULL)
#' @export
sidebar_panel <- function(tabs) {

  tab_id_html <- lapply(tabs, function(tab) {
    shinydashboard::menuItem(text = tab$menu_title,
                             tabName = tab$id,
                             icon = shiny::icon(tab$icon))
  })

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
    tab_id_html
    )
  )

  return(sidebar)
}

#' @title Main Panel
#' @description Creates basic structure for main panel
#' @inheritParams create_tab_ids
#' @export
main_panel <- function(tabs, world, state, county) {

  x <- lapply(tabs, function(tab, world, state, county)
    shinydashboard::tabItem(
      tabName = tab$id
      ,shiny::div(shiny::span(tab$page_title)
                  ,class = "page-title")
      ,ui_element(tab = tab, world = world, state = state, county = county)
    ), world = world, state = state, county = county)

  body <- shinydashboard::dashboardBody(
    shiny::div(class= "tab-content", x)
  )

  return(body)
}

#' @title TPL User Inferface
#' @param title the title of the dashboard
#' @param tabs a list with tab information
#' @param css_pth path the CSS styles file
#' @param js_pth path the JS files
#' @param favicon_pth path the favicon file
#' @inheritParams logo
#' @export
tpl_ui <- function(title, tabs, css_pth, js_pth, favicon_pth,
                   world, state, county) {

  tabs <- create_tab_ids(tabs = tabs)

  header <- header(title = title)

  sidebar <- sidebar_panel(tabs = tabs)

  body <- main_panel(tabs = tabs, world, state, county) 

  ui <- shiny::shinyUI(
          shiny::fluidPage(
            lapply(js_pth, shiny::includeScript)
            ,lapply(css_pth, shiny::includeCSS)
            ,shiny::tags$head(
              shiny::tags$link(rel = "shortcut icon"
                              ,type = "image/png"
                              ,href = favicon_pth))
            ,shinydashboard::dashboardPage(header, sidebar, body)
            ,footer()
            )
          )

  return(ui)
}
