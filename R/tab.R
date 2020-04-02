#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, world, state, county, ...) UseMethod("ui_element")

ui_element.default <- function(tab, world, state, county, ...) {
  return(NULL)
}

# This tab corresponds to tab 1 in the `mainDashboard.yaml` file
ui_element.tab1 <- function(tab, world, ...) {
  country_stats.ui(world)
}

# This tab corresponds to tab 2 in the `mainDashboard.yaml` file
ui_element.tab2 <- function(tab, state, ...) {
  state_stats.ui(state)
}

# This tab corresponds to tab 3 in the `mainDashboard.yaml` file
ui_element.tab3 <- function(tab, county, ...) {
  county_stats.ui(county)
}

# Uncomment these addtional tabx functions or add more to match the tabs layout in the mainDashboard.yaml file

# ui_element.tab4 <- function(tab, ...) {
#   return(NULL)
# }

# ui_element.tab5 <- function(tab, ...) {
#   return(NULL)
# }

# ui_element.tab6 <- function(tab, ...) {
#   return(NULL)
# }
