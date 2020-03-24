timeline_world.ui <- function() {
  shiny::fluidRow(
    shiny::column(
      width = 2,
      shiny::fluidRow(
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
      )
    ),
    
    shiny::fluidRow(
    shinyWidgets::switchInput(
      inputId = "showEvents",
      label = "Show Events",
      # handleWidth = "1000px",
      # width = "100%",
      size = "mini"
      )
    )
  ),
    
    shiny::column(width = 8,
                  shiny::plotOutput("timeline_world_plot"))
  )
}

timeline_world.server <- function(input, output, session) {
  
  df <- shiny::reactive({
    
    world %>% 
      dplyr::mutate(alpha = ifelse(input$showEvents == TRUE, 1, 0))
    
  })
  
  output$timeline_world_plot <- shiny::renderPlot({
    timeline_cls <- timeline_cls_list[[input$choose_country]]
    do.call(timeline, timeline_cls)
  })
  
}
