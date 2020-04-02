country_stats <- function(...) UseMethod("country_stats")

country_stats.default <- function(df, alpha, ...) {

  list(df = df,
       alpha = alpha,
       color = "countryName",
       source = "Johns Hopkins Center for Systems Science and Engineering",
       url = "URL: https://github.com/CSSEGISandData/COVID-19",
       tt_place = "Country")
}

country_stats.confirmed <- function(df, alpha) {

  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed",
              tt_name = "Cases")

  cls <- append(cls, country_stats(df = df, alpha = alpha))

  do.call(stats, cls)
}

country_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths",
              tt_name = "Deaths")
  
  cls <- append(cls, country_stats(df = df, alpha = alpha))
  
  do.call(stats, cls)
}

country_stats.recovered <- function(df, alpha) {
  
  cls <- list(y_lab = "# recovered",
              y = "recovered",
              tt_name = "Recovered")
  
  cls <- append(cls, country_stats(df = df, alpha = alpha))
  
  do.call(stats, cls)
}

widget.country_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "country_show_timeline"
  )
}

widget.country_ndays_slider <- function(world) {

  shiny::sliderInput(inputId = "country_last_x_days",
                     label = "# most recent days",
                     min = min(world$ndays),
                     max = max(world$ndays),
                     step = 1,
                     value = max(world$ndays))

}

widget.country_picker <- function(world) {
  shinyWidgets::pickerInput(
    inputId = "countriesGroup", 
    label = "Country",
    choices = sapply(unique(world$countryName),
                     FUN = function(x) x,
                     USE.NAMES = TRUE, simplify = FALSE), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    ), 
    multiple = TRUE,
    selected = c("US", "China", "Italy", "Korea, South", "Iran")
  )
}

widget.country_event_picker <- function(world) {
  
  shinyWidgets::pickerInput(
    inputId = "countryEvent",
    label = NULL, 
    choices = update_timeline.country(world) %>%
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

tabBox.country <- function() {
  shinydashboard::tabBox(side = "left",
                         selected = "country_tab1",
                         width = 12,
                         shiny::tabPanel(value = "country_tab1",
                                         title = "Confirmed cases",
                                         ggiraph::girafeOutput("confirmed_country_plot")),
                         shiny::tabPanel(value = "country_tab2",
                                         title = "Deaths",
                                         ggiraph::girafeOutput("deaths_country_plot")),
                         shiny::tabPanel(value = "country_tab3",
                                         title = "Recovered",
                                         ggiraph::girafeOutput("recovered_country_plot"))
                         
  )
}

country_stats.ui <- function(world) {
  
  shiny::fluidRow(
    shiny::column(width = 2,
                  shiny::tags$div(
                    class = "timeline-container",
                    shiny::h3("Timeline"),
                    shiny::h4("On/Off"),
                    widget.country_timeline_switch(),
                    shiny::h4("Events"),
                    widget.country_event_picker(world)
                  ),
                  widget.country_ndays_slider(world),
                  widget.country_picker(world)),
    shiny::column(width = 7,
                  tabBox.country()
    ),
    shiny::column(width = 3)
  )
}

country_stats.server <- function(input, output, session, world) {

  country_sub <- shiny::reactive({

    world %>% 
      dplyr::filter(countryName %in% input$countriesGroup) %>% 
      dplyr::filter(ndays <= input$country_last_x_days)

  })

  timeline_sub <- shiny::reactive({

    timeline <- update_timeline.country(country_sub()) %>%
      dplyr::filter(event %in% input$countryEvent) %>%
      dplyr::filter(!is.na(Date))

    if (nrow(timeline) > 0) {
      timeline <- aggregate(timeline$label, list(timeline$countryName,
                                                 timeline$Date), paste, collapse="; ")

      names(timeline) <- c("countryName", "Date", "label")

      timeline <- timeline %>%
        dplyr::right_join(country_sub())

    } else {

      country_sub() %>% 
        dplyr::mutate(label = NA)
    }

  })

  country_alpha <- shiny::reactive({
    alpha <- ifelse(input$country_show_timeline, 1, 0)
  })

  output$confirmed_country_plot <- ggiraph::renderGirafe({

    country_stats.confirmed(df = timeline_sub(),
                            alpha = country_alpha())
    
  })

  output$deaths_country_plot <- ggiraph::renderGirafe({

    country_stats.deaths(df = timeline_sub(),
                         alpha = country_alpha())
  })

  output$recovered_country_plot <- ggiraph::renderGirafe({

    country_stats.recovered(df = timeline_sub(),
                            alpha = country_alpha())
  })
  
}

