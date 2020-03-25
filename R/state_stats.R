state_stats.confirmed <- function(df, alpha) {
  
  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed",
              color = "stateName",
              df = df,
              alpha = alpha)

  do.call(stats, cls)
}

state_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths",
              color = "stateName",
              df = df,
              alpha = alpha)
  
  do.call(stats, cls)
}

state_stats.tests <- function(df, alpha) {
  
  cls <- list(y_lab = "# tests",
              y = "total_tests",
              color = "stateName",
              df = df,
              alpha = alpha)

  do.call(stats, cls)
}

widget.state_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "state_show_timeline"
  )
}

widget.state_ndays_slider <- function() {

  shiny::sliderInput(inputId = "state_last_x_days",
                     label = "# most recent days",
                     min = min(state$ndays),
                     max = max(state$ndays),
                     step = 1,
                     value = max(state$ndays))

}

widget.state_state_picker <- function() {
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
  )
}

tabBox.state <- function() {
  shinydashboard::tabBox(side = "left",
                         selected = "Tab1",
                         width = 9,
                         shiny::tabPanel(value = "state_tab1",
                                         title = "Confirmed cases",
                                         shiny::plotOutput("confirmed_state_plot")),
                         shiny::tabPanel(value = "state_tab2",
                                         title = "Deaths",
                                         shiny::plotOutput("deaths_state_plot")),
                         shiny::tabPanel(value = "stage_tab3",
                                         title = "Tests",
                                         shiny::plotOutput("tests_state_plot")))
}

state_stats.ui <- function() {

  shiny::fluidRow(
    shiny::column(width = 3,
                  shiny::h3("Show timeline"),
                  widget.state_timeline_switch(),
                  widget.state_ndays_slider(),
                  widget.state_state_picker()),
    shiny::column(width = 9,
                  tabBox.state()
                  )
    )
}

state_stats.server <- function(input, output, session) {

  state_sub <- shiny::reactive({

    state %>% 
      dplyr::filter(stateName %in% input$statesGroup) %>% 
      dplyr::filter(ndays <= input$state_last_x_days)

  })

  state_alpha <- shiny::reactive({
    alpha <- ifelse(input$state_show_timeline, 1, 0)
  })

  output$confirmed_state_plot <- shiny::renderPlot({

   state_stats.confirmed(df = state_sub(),
                         alpha = state_alpha())
  })

  output$deaths_state_plot <- shiny::renderPlot({

    state_stats.deaths(df = state_sub(),
                       alpha = state_alpha())
  })

  output$tests_state_plot <- shiny::renderPlot({

    state_stats.tests(df = state_sub(),
                      alpha = state_alpha())
  })

}
