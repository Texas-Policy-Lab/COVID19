#' @title TPL Server
server <- function(input, output, session) {
  country_stats.server(input, output, session)
  state_stats.server(input, output, session)
}
