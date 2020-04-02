stats <- function(...) UseMethod("stats")

stats.default <- function(df,
                          alpha = 1,
                          x = "Date",
                          y = NULL,
                          color = NULL,
                          y_lab = NULL,
                          x_lab = "Date",
                          legend_lab = NULL,
                          size = 3, 
                          hjust = 0, 
                          nudge_x = 8,
                          box_padding = .5,
                          point.padding = .2,
                          direction = "y",
                          str_width = 65,
                          source = NULL,
                          url = NULL,
                          tt_name = NULL,
                          tt_place = NULL) {
  
  x_vec <- df[[x]]
  y_vec <- df[[y]]
  color_vec <- df[[color]]
  
  pandemic <- data.frame(Date = as.Date(c("2020-03-11")),
                         labelText = c("WHO declares pandemic"))

  gg <- ggplot(df, aes(x = x_vec, y = y_vec, color = color_vec,
                       label = stringr::str_wrap(label,
                                                 width = str_width))) +
    geom_vline(aes(xintercept = Date, 
                   linetype = stringr::str_wrap(labelText, 8)), color = "#CCCCCC",
               data = pandemic, show.legend = NA) +
    geom_line() +
    ggiraph::geom_point_interactive(tooltip = glue::glue("{tt_place}: {color_vec}<br>Date: {format(x_vec, '%B %d, %Y')}<br>{tt_name}: {comma(y_vec)}"),
                                    alpha = .75) +
    scale_color_manual("",
                       values = as.vector(tpltheme::tpl_palettes$categorical)) + 
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = x_lab,
         y = y_lab,
         caption = paste(stringr::str_wrap(paste("Source:", source), width = 100),
                         url,
                         paste("Data last updated:", timestamp(), "(Central time)"),
                         sep ="\n"),
         linetype = ""
    ) +
      theme(plot.caption = element_text(hjust = 0, vjust = 0, size = 10),
          axis.title = element_text(size = 16,
                                    margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
          axis.title.y = element_text(margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
          axis.text = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.line = element_line(colour = "grey"),
          text = element_text(family="Arial")
    )

  gg <- ggiraph::girafe(ggobj = gg)
} 
