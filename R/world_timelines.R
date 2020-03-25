timeline_world.ui <- function() {
  shiny::fluidRow(
    shiny::column(
      width = 2,
      shiny::radioButtons(
        "choose_country",
        label = shiny::h3("Country"),
        width = "100px",
        choices = list(
          "US" = "us",
          "China" = "china",
          "Italy" = "italy"
        ),
        selected = "us"
      ),
                    shiny::h3("Show timeline"),
                    shinyWidgets::switchInput(
                      inputId = "show_timeline"
                      # label = "Show timeline",
                      # onStatus = "#e54e4d"
                    )
  ),
    
    shiny::column(width = 8,
                  shiny::plotOutput("timeline_world_plot"))
  )

}

timeline_world.server <- function(input, output, session) {
  
  df <- shiny::reactive({

    timeline.default(alpha = ifelse(input$show_timeline, 1, 0))
    
  })
  
  output$timeline_world_plot <- shiny::renderPlot({
    timeline_cls <- timeline_cls_list[[input$choose_country]]
    do.call(timeline, timeline_cls)
  })
  
}
