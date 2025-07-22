library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Intro"),
        p("This hub provides a user-friendly interface to guide you through the process of determining the appropriate trial design based on your inputs."),
      ),
      sidebar = sidebar(
        h4("Table of Contents"),
        accordion_filters <- accordion(
        id = "nav_id",
        accordion_panel(
            "Section 1", icon = icon("menu-app"),
        ),
        accordion_panel(
            "Section 2", icon = icon("menu-app"),
        )
        
    )
   )
  )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}