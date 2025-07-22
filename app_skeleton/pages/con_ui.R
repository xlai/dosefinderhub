library(shiny)
library(shiny.semantic)


con_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Conduct"),
        p("This section allows you to conduct the trial based on the design parameters specified."),
        p("You can monitor the trial progress, view results, and make adjustments as necessary.")
    ),
    sidebar = sidebar(
        h4("Conduct Trial"),
        p("Monitor the trial progress and view results below:")
    )
  )
}

con_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}