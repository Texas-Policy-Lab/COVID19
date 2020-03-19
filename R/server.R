#' @title TPL Server
server <- function(input, output) {

  output$progressBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = shiny::icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = shiny::icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
}
