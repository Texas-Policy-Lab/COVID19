timeline <- function(...) UseMethod ("timeline")

timeline.default <- function(df,
                             country = NULL,
                             state = NULL,
                             color = "#e54e4d",
                             y_lab = "Confirmed cases",
                             x_lab = "Date",
                             title = NULL,
                             size = 3, 
                             hjust = 0, 
                             nudge_x = 8,
                             box_padding = .5,
                             point.padding = .2,
                             direction = "y",
                             alpha = 1,
                             str_width = 65,
                             usafacts_source = "Confirmed COVID-19 cases and deaths: USAFacts Data (https://usafacts.org/)",
                             census_source = "Population data: 2018 American Community Survey 5-year Estimates",
                             covid_tracking_source = "Testing data: The Covid Tracking Project (https://covidtracking.com/)",
                             world_data_source ="Johns Hopkins Center for Systems Science and Engineering  (https://github.com/CSSEGISandData/COVID-19)") {
  
  if(!is.null(country)) {
    df <- df %>% 
      dplyr::filter(countryName == country)
    
    title <- glue::glue(title)
  }
  
  if(!is.null(state)) {
    df <- df %>%
      dplyr::filter(stateName == state)
    
    title <- glue::glue(title)
  }
  
  gg <- ggplot2::ggplot(df,
                        ggplot2::aes(x = Date,
                                     y = confirmed,
                                     label = stringr::str_wrap(label,
                                                               width = str_width))) + 
    ggplot2::geom_point(color = color) +
    ggplot2::geom_line() +
    ggplot2::labs(y = y_lab,
                  x = x_lab,
                  caption = paste("Source:", world_data_source),
                  title = title) +
    ggplot2::scale_y_continuous(labels = scales::comma_format())
  
  gg <- text_format(gg = gg,
                    size = size, 
                    hjust = hjust, 
                    nudge_x = nudge_x,
                    box_padding = box_padding,
                    point.padding = point.padding,
                    direction = direction,
                    alpha = alpha)
  
  gg <- pandemic_declared(gg = gg, df = df)
  
  print(gg)
}

timeline.tx <- function(df, alpha) {
  
  cls <- list(df = df,
              state = "Texas",
              title = "Confirmed cases in the state of {state} over time",
              hjust = 1,
              nudge_x = 20,
              box_padding = 1,
              alpha = alpha)
  
  do.call(timeline, cls)
}

timeline.ca <- function(df, alpha) {
  
  cls <- list(df = df,
              state = "California",
              title = "Confirmed cases in the state of {state} over time",
              str_width = 100,
              hjust = 1,
              nudge_x = 20,
              box_padding = 1,
              alpha = alpha)
  
  do.call(timeline, cls) 
}

timeline.ny <- function(df, alpha) {
  
  cls <- list(df = df,
              state = "New York",
              title = "Confirmed cases in the state of {state} over time",
              str_width = 100,
              hjust = 1,
              nudge_x = 20,
              box_padding = 1,
              alpha = alpha)
  
  do.call(timeline, cls)
}

timeline.us <- function(df, alpha) {
  
  cls <- list(df = df,
              country = "US",
              title = "Confirmed cases in the United States over time",
              hjust = 1,
              nudge_x = 20,
              alpha = alpha)
  
  do.call(timeline, cls)
}

timeline.china <- function(df, alpha) {
  
  cls <- list(df = df,
              country = "China",
              title = "Confirmed cases in {country} over time",
              alpha = alpha)
  
  do.call(timeline, cls)
}

timeline.italy <- function(df, alpha) {
  
  cls <- list(df = df,
              country = "Italy",
              title = "Confirmed cases in {country} over time",
              str_width = 100,
              hjust = 1,
              nudge_x = 20,
              box_padding = 1,
              alpha = alpha)
  
  do.call(timeline, cls)
}

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

  df2 <- shiny::reactive({

    world %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(countryName = tolower(countryName)) %>% 
      dplyr::filter(countryName %in% input$choose_country)

  })

  output$timeline_world_plot <- shiny::renderPlot({

    alpha <- ifelse(input$show_timeline, 1, 0)

    cls <- structure(list(df = df2(),
                          alpha = alpha),
                     class = input$choose_country)

    do.call(timeline, cls)
  
  })
  
}

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

