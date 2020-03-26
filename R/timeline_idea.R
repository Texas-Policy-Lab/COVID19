#devtools::install_github("shosaco/vistime")
# library(vistime)
# 
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
# vistime(china, start = "Date", tooltips = "label_line_breaks", show_labels = FALSE)
