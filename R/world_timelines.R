timeline_us_cls <- structure(list(df = world %>% 
                                    dplyr::filter(is.na(stateName)),
                                  countryName = "US",
                                  title = "Confirmed cases in the United States over time",
                                  hjust = 1,
                                  nudge_x = 20),
                             class = "us")

timeline_china_cls <- structure(list(df = world,
                                     countryName = "China",
                                     title = "Confirmed cases in {} over time"),
                                class = "china")

timeline_italy_cls <- structure(list(df = world,
                                     countryName = "Italy",
                                     title = "Confirmed cases in {} over time",
                                     str_width = 100,
                                     hjust = 1,
                                     nudge_x = 20,
                                     box_padding = 1),
                                class = "italy")

world_cls_list <- list(us = timeline_us_cls,
                       china = timeline_china_cls,
                       italy = timeline_italy_cls)

timeline_world.ui <- function() {

  shiny::fluidRow(
    shiny::column(width = 3,
                  shiny::radioButtons("choose_country",
                                      label = shiny::h3("Country"),
                                      width = "100px",
                                      choices = list("US" = "us",
                                                     "China" = "china",
                                                     "Italy" = "italy"),
                                      selected = "us")),
    shiny::column(width = 9, 
                  shiny::plotOutput("timeline_world_plot"))
  )
}

timeline_world.server <- function(input, output, session) {
  
  output$timeline_world_plot <- shiny::renderPlot({
    timeline_cls <- world_cls_list[[input$choose_country]]
    do.call(timeline, timeline_cls)
  })
  
}
