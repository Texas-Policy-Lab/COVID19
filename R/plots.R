pandemic_declared <- function(gg, df, y,
                              text1 = "WHO announces that the new coronavirus disease will be called 'COVID-19'",
                              text2 = "WHO declares outbreak a pandemic",
                              text3 = "Federal plan leaked which warns the new coronavirus pandemic may last up to 18 months or longer and come in multiple waves") {

  
  pandemic <- df %>%
    dplyr::filter(Date == as.Date("2020-03-11"))
  
  gg <- gg + 
    geom_vline(xintercept = pandemic$Date, linetype="dashed",
               color = "grey", size = 1) +
      annotate(geom = "label", x = pandemic$Date, y = (max(df[[y]], na.rm = T)*.95),
               label = "WHO declares pandemic", size = 3)
  
  
  # pandemic <- data.frame(Date = as.Date(c("2020-02-11", "2020-03-11", "2020-03-17")),
  #                        # labels = c("A", "B", "C"),
  #                        labelText = c(text1,
  #                                      text2,
  #                                      text3))
  # gg <- gg + 
  #   geom_vline(aes(xintercept = Date, color = labelText),
  #              data = pandemic, linetype = "dashed", size = 1, show.legend = TRUE) +
  #   scale_color_manual("", values = c(text1 = "#151248",
  #                                     text2 = "#605F5E",
  #                                     text3 = "#5393EA"))

  return(gg)
}

stats <- function(...) UseMethod("stats")

stats.default <- function(df,
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
                          source = NULL,
                          url = NULL,
                          tt_name = NULL,
                          tt_place = NULL) {
  
  x_vec <- df[[x]]
  y_vec <- df[[y]]
  color_vec <- df[[color]]

  gg <- ggplot(df, aes(x = x_vec, y = y_vec, color = color_vec, label = stringr::str_wrap(label,
                                                                                          width = str_width))) +
    geom_line() +
    ggiraph::geom_point_interactive(tooltip = glue::glue("{tt_place}: {color_vec}<br>Date: {format(x_vec, '%B %d, %Y')}<br>{tt_name}: {comma(y_vec)}")) +
    scale_color_manual(legend_lab,
                       values = as.vector(tpltheme::tpl_palettes$categorical)) + 
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = x_lab,
         y = y_lab,
         caption = paste(stringr::str_wrap(paste("Source:", source), width = 100),
                         url,
                         paste("Data last updated:", timestamp()),
                         sep ="\n")
    ) +
    theme(plot.caption = element_text(hjust = 0, vjust = 0, size = 10),
          axis.title = element_text(size = 16,
                                    margin(t = 0, r = 10, b = 0, l = 10, unit = "pt")),
          axis.text = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
    #       panel.border = element_rect(color = "white"),
          axis.line = element_line(colour = "grey")
    )

  gg <- gg +
    ggrepel::geom_label_repel(size = size,
                              hjust = hjust,
                              nudge_x = nudge_x,
                              box.padding = box_padding,
                              na.rm = TRUE,
                              point.padding = point.padding,
                              direction = direction,
                              alpha = alpha)

  gg <- pandemic_declared(gg = gg, df = df, y = y)

  gg <- ggiraph::girafe(ggobj = gg)
} 
