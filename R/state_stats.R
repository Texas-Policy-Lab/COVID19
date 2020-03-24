state_stats.ui <- function() {

  shiny::fluidRow(
    shiny::column(width = 3,
                  # shiny::h3("Show timeline"),
                  # shinyWidgets::switchInput(
                  #   inputId = "show_timeline",
                  #   # label = "Show timeline",
                  #   onStatus = "#e54e4d"
                  # ),
                  shiny::sliderInput(inputId = "last_x_days",
                                     label = "# most recent days",
                                     min = min(state$ndays),
                                     max = max(state$ndays),
                                     step = 1,
                                     value = max(state$ndays)),
                  shinyWidgets::pickerInput(
                    inputId = "statesGroup", 
                    label = "States",
                    choices = sapply(unique(state$stateName),
                                     FUN = function(x) x,
                                     USE.NAMES = TRUE, simplify = FALSE), 
                    options = list(
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = TRUE
                    ), 
                    multiple = TRUE,
                    selected = c("Texas", "New York", "California", "Washington")
                  )),
    shiny::column(width = 9,
                  shinydashboard::tabBox(side = "left",
                                         selected = "Tab1",
                                         width = 9,
                                         shiny::tabPanel(value = "Tab1",
                                                         title = "Confirmed cases",
                                                         shiny::plotOutput("confirmed_state_plot")),
                                         shiny::tabPanel(value = "Tab2",
                                                         title = "Deaths",
                                                         shiny::plotOutput("deaths_state_plot")),
                                         shiny::tabPanel(value = "Tab3",
                                                         title = "Tests",
                                                         shiny::plotOutput("tests_state_plot"))))
    )
}


state_stats.server <- function(input, output, session) {

  df <- shiny::reactive({

    state %>% 
      dplyr::filter(stateName %in% input$statesGroup) %>% 
      dplyr::filter(ndays <= input$last_x_days)

  })

  output$confirmed_state_plot <- shiny::renderPlot({
   state_stats.confirmed(df = df())
  })

  output$deaths_state_plot <- shiny::renderPlot({
    state_stats.deaths(df = df())
  })

  output$tests_state_plot <- shiny::renderPlot({
    state_stats.tests(df = df())
  })

}



