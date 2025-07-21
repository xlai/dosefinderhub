library(shiny)
library(shiny.semantic)


con_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebarPanel(
      h4("Conduct"),
      p("This section provides guidance on how to conduct the trial based on the selected design."),
      actionButton(ns("finalize_button"), "Finalize Design")
    ),
    main = mainPanel(
      h3("Conducting the Trial"),
      p("Here you will find detailed instructions and best practices for conducting your trial."),
      p("Click 'Finalize Design' to complete the process.")
    )
  )
}

con_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}