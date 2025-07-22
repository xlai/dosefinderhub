library(shiny)
library(shiny.semantic)

sim_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Simulation"),
        p("This section allows you to run simulations based on the trial design parameters specified."),
        p("You can adjust the simulation settings and view the results."),
        ),
    sidebar = sidebar(
        h4("Simulation Settings"),
        p("Adjust the parameters for your simulation below:")
      
      )
  )
}

sim_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}