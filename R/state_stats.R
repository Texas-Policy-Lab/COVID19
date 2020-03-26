state_stats <- function(...) UseMethod("state_stats")

state_stats.default <- function(df, alpha, ...) {

  list(df = df,
       alpha = alpha,
       color = "stateName",
       source = "Confirmed COVID-19 cases and deaths: USAFacts Data",
       url = "URL: https://usafacts.org/")
}

state_stats.confirmed <- function(df, alpha) {
  
  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed")
  cls <- append(cls, state_stats(df, alpha))

  do.call(stats, cls)
}

state_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths")
  cls <- append(cls, state_stats(df, alpha))

  do.call(stats, cls)
}

state_stats.tests <- function(df, alpha) {
  
  cls <- list(y_lab = "# tests",
              y = "total_tests")
  cls <- append(cls, state_stats(df, alpha))

  do.call(stats, cls)
}

widget.state_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "state_show_timeline"
  )
}

widget.state_event_picker <- function() {
  
  shinyWidgets::pickerInput(
    inputId = "stateEvent",
    label = NULL, 
    choices = update_timeline.state(state) %>%
      dplyr::distinct(event) %>% 
      dplyr::pull(event),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    )
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

widget.state_picker <- function() {
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
                         selected = "state_tab1",
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
                  shiny::tags$div(
                    class = "timeline-container",
                    shiny::h3("Timeline"),
                    shiny::h4("On/Off"),
                    widget.state_timeline_switch(),
                    shiny::h4("Events"),
                    widget.state_event_picker()
                  ),
                  widget.state_ndays_slider(),
                  widget.state_picker()),
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
  
  timeline_sub <- shiny::reactive({
    
    timeline <- update_timeline.state(state_sub()) %>% 
      dplyr::filter(event %in% input$stateEvent)
    
    if (nrow(timeline) > 0) {
    
      timeline <- aggregate(timeline$label, list(timeline$stateName,
                                                 timeline$Date), paste, collapse="; ")
      names(timeline) <- c("stateName", "Date", "label")
      
      timeline <- timeline %>% 
        dplyr::right_join(state_sub())
      
    } else {
      state_sub() %>% 
        dplyr::mutate(label = NA)
    }
    
    })

  state_alpha <- shiny::reactive({
    alpha <- ifelse(input$state_show_timeline, 1, 0)
  })

  output$confirmed_state_plot <- shiny::renderPlot({

   state_stats.confirmed(df = timeline_sub(),
                         alpha = state_alpha())
  })

  output$deaths_state_plot <- shiny::renderPlot({

    state_stats.deaths(df = timeline_sub(),
                       alpha = state_alpha())
  })

  output$tests_state_plot <- shiny::renderPlot({

    state_stats.tests(df = timeline_sub(),
                      alpha = state_alpha())
  })

}
