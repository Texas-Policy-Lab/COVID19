#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, ...) UseMethod("ui_element")

ui_element.default <- function(tab, ...) {
  return(NULL)
}

ui_element.tab1 <- function(tab, ...) {
  timeline_world.ui()
}

ui_element.tab2 <- function(tab, ...) {
  timeline_state.ui()
  state_stats.ui()
}

ui_element.tab3 <- function(tab, ...) {
  return(NULL)
}

ui_element.tab4 <- function(tab, ...) {
  terms.ui()
}

ui_element.tab5 <- function(tab, ...) {
  return(NULL)
}

# ui_element.tab6 <- function(tab, ...) {
#   return(NULL)
# }
