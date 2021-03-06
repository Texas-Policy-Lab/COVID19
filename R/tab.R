#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, ...) UseMethod("ui_element")

ui_element.default <- function(tab, ...) {
  return(NULL)
}

# This tab corresponds to tab 1 in the `mainDashboard.yaml` file
ui_element.tab1 <- function(tab, ...) {
  return(NULL)
}

# This tab corresponds to tab 2 in the `mainDashboard.yaml` file
ui_element.tab2 <- function(tab, ...) {
  return(NULL)
}

# This tab corresponds to tab 3 in the `mainDashboard.yaml` file
ui_element.tab3 <- function(tab, ...) {
  return(NULL)
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
