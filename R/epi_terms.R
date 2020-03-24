terms.ui <- function() {
  shiny::tableOutput("epi_table")  
}

terms.server <- function(input, output, session, epi_pth = "data/epi_terms.csv") {

  output$epi_table <- shiny::renderTable({
    read.csv(here::here(epi_pth),
             stringsAsFactors = FALSE)
  })

}