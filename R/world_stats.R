country_stats.confirmed <- function(df, alpha) {
  
  cls <- list(y_lab = "# confirmed cases",
              y = "confirmed",
              color = "countryName",
              df = df,
              alpha = alpha)
  
  do.call(stats, cls)
}

country_stats.deaths <- function(df, alpha) {
  
  cls <- list(y_lab = "# deaths",
              y = "deaths",
              color = "countryName",
              df = df,
              alpha = alpha)
  
  do.call(stats, cls)
}

widget.country_timeline_switch <- function() {
  shinyWidgets::switchInput(
    inputId = "country_show_timeline"
  )
}

widget.country_ndays_slider <- function() {
  
  shiny::sliderInput(inputId = "country_last_x_days",
                     label = "# most recent days",
                     min = min(world$ndays),
                     max = max(world$ndays),
                     step = 1,
                     value = max(world$ndays))
  
}

widget.country_picker <- function() {
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
    selected = c("US", "China", "Italy", "South Korea")
  )
}

tabBox.country <- function() {
  shinydashboard::tabBox(side = "left",
                         selected = "country_tab1",
                         width = 9,
                         shiny::tabPanel(value = "country_tab1",
                                         title = "Confirmed cases",
                                         ggiraph::girafeOutput("confirmed_country_plot")),
                         shiny::tabPanel(value = "country_tab2",
                                         title = "Deaths",
                                         ggiraph::girafeOutput("deaths_country_plot"))
                         )
}

country_stats.ui <- function() {

  shiny::fluidRow(
    shiny::column(width = 3,
                  shiny::h3("Show timeline"),
                  widget.country_timeline_switch(),
                  widget.country_ndays_slider(),
                  widget.country_picker()),
    shiny::column(width = 9,
                  tabBox.country()
    )
  )
}

country_stats.server <- function(input, output, session) {

  country_sub <- shiny::reactive({

    world %>% 
      dplyr::filter(countryName %in% input$countriesGroup) %>% 
      dplyr::filter(ndays <= input$country_last_x_days)
    
  })
  
  timeline_sub <- shiny::reactive({
    
    timeline <- update_timeline.country(country_sub())
    
    timeline <- aggregate(timeline$label, list(timeline$countryName,
                                               timeline$Date), paste, collapse="; ")
    
    names(timeline) <- c("countryName", "Date", "label")
    
    timeline <- timeline %>% 
      dplyr::right_join(country_sub())
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

  output$tests_country_plot <- ggiraph::renderGirafe({

    country_stats.tests(df = timeline_sub(),
                        alpha = country_alpha())
  })
  
}

