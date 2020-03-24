timeline_tx_cls <- structure(list(df = state,
                                  stateName = "Texas",
                                  title = "Confirmed cases in the state of {} over time",
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "tx")

timeline_ca_cls <- structure(list(df = state,
                                  stateName = "California",
                                  title = "Confirmed cases in the state of {} over time",
                                  str_width = 100,
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "ca")

timeline_ny_cls <- structure(list(df = state,
                                  stateName = "New York",
                                  title = "Confirmed cases in the state of {} over time",
                                  str_width = 100,
                                  hjust = 1,
                                  nudge_x = 20,
                                  box_padding = 1),
                             class = "ny")

state_cls_list <- list(tx = timeline_tx_cls,
                       ca = timeline_ca_cls,
                       ny = timeline_ny_cls)

timeline_state.ui <- function() {
  
  shiny::fluidRow(
    shiny::column(width = 3,
                  shiny::radioButtons("choose_state",
                                      label = shiny::h3("State"),
                                      width = "110px",
                                      choices = list("Texas" = "tx",
                                                     "California" = "ca",
                                                     "New York" = "ny"),
                                      selected = "tx")),
    shiny::column(width = 9,
                  shiny::plotOutput("timeline_state_plot"))
                  
  )
}

timeline_state.server <- function(input, output, session) {

  output$timeline_state_plot <- shiny::renderPlot({

    timeline_cls <- state_cls_list[[input$choose_state]]
    do.call(timeline, timeline_cls)
  })

}
