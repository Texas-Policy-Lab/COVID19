#' @title TPL footer
#' @description Creates the TPL footer to add to dashboards and reports
#' @param version the version of the dashboard
#' @export
footer <- function() {

  htmltools::tags$footer(
    htmltools::tags$div(class = "star"
      ,htmltools::tags$object(type="image/svg+xml"
                             ,data="svg/tpl_star_red.svg")
    ),
    htmltools::tags$div(class="first-row"
      ,htmltools::tags$div(class = "g-container"
         ,htmltools::tags$div(class = "g-row"
            ,htmltools::tags$div(class = "footer_logo"
               ,htmltools::tags$object(class="footer_logo-tpl"
                                       ,type="image/svg+xml"
                                       ,data="svg/tpl_text_logo_color.svg")
            )
            ,htmltools::tags$div(class="footer_copyright"
               ,htmltools::tags$p(htmltools::tags$strong(
                 paste("Copyright"
                       ,format(Sys.Date(), "%Y")
                       ,"Texas Policy Lab", sep = " ")
               )
               )
            )
            ,htmltools::tags$div(class = "footer_contact"
               ,htmltools::tags$p(
                 htmltools::tags$a("Contact Us"
                                   ,class = "btn"
                                   ,target = ""
                                   ,href = "mailto:texaspolicylab@rice.edu")
               )
            )
         )
      )
    )
  )
}
