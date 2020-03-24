timeline_world.ui <- function() {
  shiny::fluidRow(
    shiny::column(width = 2,
                  shiny::radioButtons("choose_country",
                                      label = shiny::h3("Country"),
                                      width = "100px",
                                      choices = list("US" = "us",
                                                     "China" = "china",
                                                     "Italy" = "italy"),
                                      selected = "us")),
    shiny::column(width = 8, 
                  shiny::plotOutput("timeline_world_plot"))
  )
}

timeline_world.server <- function(input, output, session) {
  
  output$timeline_world_plot <- shiny::renderPlot({
    timeline_cls <- timeline_cls_list[[input$choose_country]]
    do.call(timeline, timeline_cls)
  })
  
}
