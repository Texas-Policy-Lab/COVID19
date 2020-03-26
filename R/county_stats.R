county_stats <- function(...) UseMethod("county_stats")

county_stats.default <- function(df, alpha, ...) {
  
  list(df = df,
       alpha = alpha,
       color = "countyName",
       source = "Confirmed COVID-19 cases and deaths: USAFacts Data",
       url = "URL: https://usafacts.org/",
       tt_place = "County")
}

county_stats.confirmed <- function(df, alpha) {
  
  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed",
              tt_name = "Cases")
  cls <- append(cls, county_stats(df, alpha))
  
  do.call(stats, cls)
}

county_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths",
              tt_name = "Deaths")
  cls <- append(cls, county_stats(df, alpha))
  
  do.call(stats, cls)
}

widget.county_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "county_show_timeline"
  )
}

widget.county_event_picker <- function() {
  
  shinyWidgets::pickerInput(
    inputId = "countyEvent",
    label = NULL, 
    choices = update_timeline.county(county) %>%
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


widget.county_ndays_slider <- function() {
  
  shiny::sliderInput(inputId = "county_last_x_days",
                     label = "# most recent days",
                     min = min(county$ndays),
                     max = max(county$ndays),
                     step = 1,
                     value = max(county$ndays))
  
}

widget.county_picker <- function() {
  shinyWidgets::pickerInput(
    inputId = "countysGroup", 
    label = "Counties",
    choices = sapply(unique(county$countyName),
                     FUN = function(x) x,
                     USE.NAMES = TRUE, simplify = FALSE), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    ), 
    multiple = TRUE,
    selected = c("Harris County", "Bexar County", "Travis County", "Dallas County")
  )
}


widget.state_picker2 <- function() {
  shinyWidgets::pickerInput(
    inputId = "stateGroup2", 
    label = "States",
    choices = sapply(unique(county$stateName),
                     FUN = function(x) x,
                     USE.NAMES = TRUE, simplify = FALSE), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    ), 
    multiple = TRUE,
    selected = c("Texas")
  )
}

tabBox.county <- function() {
  shinydashboard::tabBox(side = "left",
                         selected = "county_tab1",
                         width = 12,
                         shiny::tabPanel(value = "county_tab1",
                                         title = "Confirmed cases",
                                         ggiraph::girafeOutput("confirmed_county_plot")),
                         shiny::tabPanel(value = "county_tab2",
                                         title = "Deaths",
                                         ggiraph::girafeOutput("deaths_county_plot"))
  )
}

county_stats.ui <- function() {
  
  shiny::fluidRow(
    shiny::column(width = 2,
                  shiny::tags$div(
                    class = "timeline-container",
                    shiny::h3("Timeline"),
                    shiny::h4("On/Off"),
                    widget.county_timeline_switch(),
                    shiny::h4("Events"),
                    widget.county_event_picker()
                  ),
                  widget.county_ndays_slider(),
                  widget.state_picker2(),
                  widget.county_picker()),
    shiny::column(width = 7,
                  tabBox.county()
    ),
    shiny::column(width = 3)
  )
}

county_stats.server <- function(input, output, session) {
  
  county_sub <- shiny::reactive({
    
    county %>%
      dplyr::filter(stateName %in% input$statesGroup2) %>%
      dplyr::filter(countyName %in% input$countysGroup) %>% 
      dplyr::filter(ndays <= input$county_last_x_days) %>% 
      dplyr::filter(!is.na(Date))
  })

  timeline_sub <- shiny::reactive({

    timeline <- update_timeline.county(county_sub()) %>%
      dplyr::filter(event %in% input$countyEvent)

    if (nrow(timeline) > 0) {

      timeline <- aggregate(timeline$label, list(timeline$countyName,
                                                 timeline$Date), paste, collapse="; ")
      names(timeline) <- c("countyName", "Date", "label")

      timeline <- timeline %>% 
        dplyr::right_join(county_sub())

    } else {
      county_sub() %>% 
        dplyr::mutate(label = NA)
    }

  })

  county_alpha <- shiny::reactive({
    alpha <- ifelse(input$county_show_timeline, 1, 0)
  })

  output$confirmed_county_plot <- ggiraph::renderGirafe({

    county_stats.confirmed(df = timeline_sub(),
                           alpha = county_alpha())
  })

  output$deaths_county_plot <- ggiraph::renderGirafe({
  
    county_stats.deaths(df = timeline_sub(),
                        alpha = county_alpha())
  })

}
