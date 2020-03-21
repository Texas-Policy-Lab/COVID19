#' #' @title Create data tables
#' #' @description Create data tables for shiny app
#' render_state_table <- function() {
#'   state %>%
#'     dplyr::filter(Date == max(Date)) %>%
#'     dplyr::select(stateName, deaths, confirmed, positive, negative, total_tests) %>%
#'     reactable::reactable(searchable = TRUE)
#' }
#' 
#' render_txcounty_table <- function() {
#'   county %>%
#'     dplyr::filter(State == "TX",
#'                   Date == max(Date)) %>%
#'     dplyr::select(`County Name`, deaths, confirmed) %>%
#'     reactable::reactable(searchable = TRUE)
#' }
#' 
#' render_country_table <- function() {
#'   world %>%
#'     dplyr::filter(Date == max(Date)) %>%
#'     dplyr::select(country,
#'                   deaths,
#'                   confirmed) %>%
#'     reactable::reactable(searchable = TRUE)
#' }