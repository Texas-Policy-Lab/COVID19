timeline_state.ui <- function() {
  shiny::fluidRow(
    shiny::column(width = 2,
                  shiny::radioButtons("choose_state",
                                      label = shiny::h3("State"),
                                      width = "110px",
                                      choices = list("Texas" = "tx",
                                                     "California" = "ca",
                                                     "New York" = "ny"),
                                      selected = "tx")),
    shiny::column(width = 8,
                  shiny::plotOutput("timeline_state_plot"))
                  
  )
}

timeline_state.server <- function(input, output, session) {

  output$timeline_state_plot <- shiny::renderPlot({
    timeline_cls <- timeline_cls_list[[input$choose_state]]
    do.call(timeline, timeline_cls)
  })

}
