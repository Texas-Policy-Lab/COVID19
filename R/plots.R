line_chart <- function(...) UseMethod ("graph")

#' @param df dataframe. The dataframe use
#' @param x string. The name of the x variable.
#' @param y string. The name of the y variable.
#' @param color string. The name of the color variable.
#' @param title string. The title for the plot
#' @param x_lab string. The label for the x-axis.
#' @param y_lab string. The label for the y-axis.
#' @param legend_lab string. The label for the legend.
#' @param annotation_text_size integer. The size for the text.
#' @param label string. The name of the label.
line_chart.default <- function(df = NULL,
                               x = NULL,
                               y = NULL,
                               color = NULL,
                               title = NULL,
                               x_lab = NULL,
                               y_lab = NULL,
                               legend_lab = NULL,
                               annotation_text_size = NULL,
                               label = NULL,
                               usafacts_source = "Confirmed COVID-19 cases and deaths: USAFacts Data (https://usafacts.org/)",
                               census_source = "Population data: 2018 American Community Survey 5-year Estimates",
                               covid_tracking_source = "Testing data: The Covid Tracking Project (https://covidtracking.com/)",
                          ...) {
# browser()
  x_vec <- df[[x]]
  y_vec <- df[[y]]

  if(!is.null(color)) color_vec <- df[[color]]
  if(!is.null(label)) label_vec <- df[[label]]

  if(is.null(color)) {

    if(is.null(label)) {
      gg <- ggplot(df, aes(x = x_vec, y = y_vec))
        geom_point(color = "#e54e4d")

    } else{
      
      gg <- ggplot(df, aes(x = x, y = y, label = str_wrap(label_vec, width = 65))) +
        geom_point(color = "#e54e4d") +
        geom_text_repel(size = 3,
                        hjust = 0,
                        nudge_x = 2)
    }

  } else {

    if(is.null(label)) {
      
      gg <- ggplot(df,
                   aes(x = x_vec, y = y_vec, color = color_vec)) +
        scale_color_manual(values = as.vector(tpltheme::palette_tpl_main))

    } else {
      
      gg <- ggplot(df,
                   aes(x = x_vec, y = y_vec, color = color_vec,
                       label = str_wrap(label_vec, width = 65))) +
        scale_color_manual(values = as.vector(tpltheme::palette_tpl_main)) +
        geom_text_repel(size = 3,
                        hjust = 0,
                        nudge_x = 2)
      

    }
  }

  pandemic <- df %>%
    dplyr::filter(Date == as.Date("2020-01-30"))

  gg <- gg +
    geom_point() +
    geom_line() +
    # scale_color_manual(values = as.vector(tpltheme::palette_tpl_main)) +
    labs(title = title,
         x = x_lab,
         y = y_lab,
         color = legend_lab,
         caption = paste(usafacts_source, census_source, covid_tracking_source, sep = "/n")) +
    geom_vline(xintercept = pandemic$Date, linetype="dashed", 
               color = "grey", size = 1) +
    annotate(geom="text", x = pandemic$Date, y = max(df$confirmed) - 1000,
             label = "WHO declares pandemic", size = annotation_text_size)
  
}

line_chart.state.confirmed_cases <- function(state,
                                             x = "Date",
                                             y = "confirmed",
                                             color = "stateName",
                                             title = "Total # confirmed cases in each state",
                                             x_lab = "Date",
                                             y_lab = "Total # cases",
                                             legend_lab = "State") {

  gg <- line_chart.default(df = state, x = x, y = y, color = color, title = title, x_lab = x_lab, y_lab = y_lab, legend_lab = legend_lab)
  print(gg)
}

line_chart.world.confirmed_cases <- function(world,
                                             x = "Date",
                                             y = "confirmed",
                                             color = "county",
                                             title = "Total # confirmed cases in each state",
                                             x_lab = "Date",
                                             y_lab = "Total # cases",
                                             legend_lab = "State") {
  
  gg <- line_chart.default(df = world, x = x, y = y, color = color, title = title, x_lab = x_lab, y_lab = y_lab, legend_lab = legend_lab)
  print(gg)
}


line_chart.china <- function(china,
                             x = "Date",
                             y = "confirmed",
                             color = NULL,
                             title = "Total # confirmed cases in China",
                             x_lab = "Date",
                             y_lab = "Total # cases",
                             label = "label",
                             legend_lab = NULL,
                             annotation_text_size = 3) {
  
  gg <- line_chart.default(df = china, x = x, y = y, color = color, title = title,
                           x_lab = x_lab, y_lab = y_lab, legend_lab = legend_lab,
                           annotation_text_size = annotation_text_size,
                           label = label)
  
 
  # gg <- gg +
  #   annotate(geom="text", x = quarantineWuhan$Date, y = quarantineWuhan$confirmed, 
  #            label="Wuhan and three other cities placed under quarantine", size = annotation_text_size) +
  #   annotate(geom="point", x = quarantineWuhan$Date, y = quarantineWuhan$confirmed,
  #            size=1, shape=21, fill="transparent") +
  #   annotate(geom="text", x = quarantineHubei$Date, y = quarantineHubei$confirmed, 
  #            label="Wuhan and three other cities placed under quarantine", size = annotation_text_size) +
  #   annotate(geom="point", x = quarantineHubei$Date, y = quarantineHubei$confirmed,
  #            size=1, shape=21, fill="transparent")
  # 
  return(gg)
}

pandemic_declared <- function(gg, df) {
  
  pandemic <- df %>%
    dplyr::filter(Date == as.Date("2020-01-30"))
  
  gg <- gg + 

    geom_vline(xintercept = pandemic$Date, linetype="dashed", 
               color = "grey", size = 1) +
      annotate(geom = "text", x = pandemic$Date, y = max(df$confirmed) - 1000,
               label = "WHO declares pandemic", size = 3)
  return(gg)
}

text_format <- function(gg, hjust = 0, nudge_x = 8, box_padding = .5) {
  
  gg <- gg +
    geom_text_repel(size = 3,
                    hjust = hjust,
                    nudge_x = nudge_x,
                    box.padding = box_padding,
                    na.rm = TRUE,
                    point.padding = .2,
                    direction = "y")
  
  return(gg)
}
