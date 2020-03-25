state_stats <- function(...) UseMethod("state_stats")

state_stats.default <- function(df,
                                alpha = 1,
                                x = "Date",
                                y = NULL,
                                color = NULL,
                                y_lab = NULL,
                                x_lab = "Date",
                                legend_lab = "States",
                                size = 3, 
                                hjust = 0, 
                                nudge_x = 8,
                                box_padding = .5,
                                point.padding = .2,
                                direction = "y",
                                str_width = 65,
                                usafacts_source = "Confirmed COVID-19 cases and deaths: USAFacts Data (https://usafacts.org/)") {

  x_vec <- df[[x]]
  y_vec <- df[[y]]
  color_vec <- df[[color]]

  gg <- ggplot(df, aes(x = x_vec, y = y_vec, color = color_vec,
                       label = stringr::str_wrap(label,
                                                 width = str_width))) +
    geom_line() +
    geom_point() +
    scale_color_manual(legend_lab,
                       values = as.vector(tpltheme::tpl_palettes$categorical)) + 
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = x_lab,
         y = y_lab,
         caption = paste("Source:", usafacts_source))

  gg <- text_format(gg = gg,
                    size = size, 
                    hjust = hjust, 
                    nudge_x = nudge_x,
                    box_padding = box_padding,
                    point.padding = point.padding,
                    direction = direction,
                    alpha = alpha)

  gg <- pandemic_declared(gg = gg, df = df, y = y)
  print(gg)
} 

state_stats.confirmed <- function(df, alpha) {
  
  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed",
              color = "stateName",
              df = df,
              alpha = alpha)

  do.call(state_stats, cls)
}

state_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths",
              color = "stateName",
              df = df,
              alpha = alpha)
  
  do.call(state_stats, cls)
}

state_stats.tests <- function(df, alpha) {
  
  cls <- list(y_lab = "# tests",
              y = "total_tests",
              color = "stateName",
              df = df,
              alpha = alpha)

  do.call(state_stats, cls)
}

widget.state_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "show_timeline2"
  )
}

widget.state_ndays_slider <- function() {

  shiny::sliderInput(inputId = "last_x_days",
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

  df <- shiny::reactive({

    state %>% 
      dplyr::filter(stateName %in% input$statesGroup) %>% 
      dplyr::filter(ndays <= input$last_x_days)

  })

  alpha <- shiny::reactive({
    alpha <- ifelse(input$show_timeline2, 1, 0)
  })

  output$confirmed_state_plot <- shiny::renderPlot({

   state_stats.confirmed(df = df(),
                         alpha = alpha())
  })

  output$deaths_state_plot <- shiny::renderPlot({

    state_stats.deaths(df = df(),
                          alpha = alpha())
  })

  output$tests_state_plot <- shiny::renderPlot({

    state_stats.tests(df = df(),
                          alpha = alpha())
  })

}
