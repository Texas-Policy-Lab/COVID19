# # devtools::install_github("shosaco/vistime")
# # library(vistime)

# country_test <- update_timeline.country(world)
# 
# china <- country_test %>%
#   dplyr::filter(countryName == "China") %>%
#   dplyr::mutate(test = strwrap(label,30, simplify = FALSE))
# 
# china$text <- sapply(china$test, paste, collapse="<br>")
# 
# china <- china %>%
#   dplyr::mutate(label_line_breaks = glue::glue("{countryName} on {Date}:<br>{text}"))
# 
# # vistime(china, start = "Date", tooltips = "label_line_breaks", show_labels = FALSE)
# 
# timeline <- ggplot(china, aes(x = Date, y = 1, label = label, tooltip = glue::glue("{countryName} on {format(Date, '%B %d, %Y')}:<br>{label}"))) +
#   ggiraph::geom_point_interactive(size = 3) +
#   labs(y = element_blank(),
#        x = element_blank()) +
#   theme(
#     axis.line.y.left = element_blank(),
#     axis.text.y.left = element_blank(),
#     axis.line.x.bottom = element_blank(),
#     axis.ticks.x = element_blank()
#   )
# 
# ggiraph::girafe(ggobj = timeline)
