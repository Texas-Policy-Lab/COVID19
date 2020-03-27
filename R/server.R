#' @title TPL Server
server <- function(input, output, session) {

  country_stats.server(input, output, session, world)
  state_stats.server(input, output, session, state)
  county_stats.server(input, output, session, county)
}
