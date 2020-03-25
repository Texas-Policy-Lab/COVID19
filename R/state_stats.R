state_stats.ui <- function() {

  shiny::fluidRow(
    shiny::column(width = 2,
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
    shiny::column(width = 7,
                  shinydashboard::tabBox(side = "left",
                                         selected = "Tab1",
                                         width = 12,
                                         shiny::tabPanel(value = "Tab1",
                                                         title = "Confirmed cases",
                                                         shiny::plotOutput("confirmed_state_plot")),
                                         shiny::tabPanel(value = "Tab2",
                                                         title = "Deaths",
                                                         shiny::plotOutput("deaths_state_plot")),
                                         shiny::tabPanel(value = "Tab3",
                                                         title = "Tests",
                                                         shiny::plotOutput("tests_state_plot")),
                                         shiny::tabPanel(value = "Tab4",
                                                         title = "Attack rate",
                                                         shiny::plotOutput("attack_rate_state_plot"))
                                         )
                  ),
      shiny::column(width = 3, shinydashboard::valueBoxOutput(outputId = "confirmed_state")),
      shiny::column(width = 3, shinydashboard::valueBoxOutput(outputId = "deaths_state")),
      shiny::column(width = 3, shinydashboard::valueBoxOutput(outputId = "tests_state")),
      shiny::column(width = 3, shinydashboard::valueBoxOutput(outputId = "attack_rate_state"))
    )
}

state_stats.server <- function(input, output, session) {

  df <- shiny::reactive({

    state %>% 
      dplyr::filter(stateName %in% input$statesGroup) %>% 
      dplyr::filter(ndays <= input$last_x_days)

  })

  totals <- shiny::reactive({
    df() %>% 
      dplyr::arrange(desc(Date)) %>% 
      dplyr::group_by(stateName) %>% 
      dplyr::slice(1) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(total_confirmed = sum(confirmed),
                       total_deaths = sum(deaths),
                       total_tests = sum(total_tests),
                       mean_attack_rate = round(mean(attack_rate, na.rm = TRUE), 0))
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
  
  output$attack_rate_state_plot <- shiny::renderPlot({
    state_stats.attack_rate(df = df())
  })

  output$confirmed_state <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = totals() %>% dplyr::select(total_confirmed),
      subtitle = "Confirmed Cases",
      color = "red",
      width = NULL
    )
  })

  output$deaths_state <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = totals() %>% dplyr::select(total_deaths),
      subtitle = "Deaths",
      color = "red",
      width = NULL
    )
  })

  output$tests_state <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = totals() %>% dplyr::select(total_tests),
      subtitle = "Tests",
      color = "red",
      width = NULL
    )
  })

  output$attack_rate_state <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = totals() %>% dplyr::select(mean_attack_rate),
      subtitle = "Attack rate",
      color = "red",
      width = NULL
    )
  })

}
