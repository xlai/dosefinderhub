library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
 page_sidebar(
  sidebar = sidebar(
    h4("Welcome"),
    p("This application is designed to help you determine the appropriate trial design based on your inputs."),
    actionButton(ns("get_started"), "Get Started")
  ),
  main = div(
    h3("Intro"),
    p("This hub provides a user-friendly interface to guide you through the process of determining the appropriate trial design based on your inputs."),
    p("Click 'Get Started' to begin.")
  )
 )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}