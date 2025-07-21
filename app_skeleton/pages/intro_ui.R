library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebarPanel(
      h4("Welcome to the Dose Finder Hub"),
      p("This application helps you find the right trial design for your needs."),
      actionButton(ns("start_button"), "Get Started")
    ),
    main = mainPanel(
      h3("Intro"),
      p("This hub provides a user-friendly interface to guide you through the process of determining the appropriate trial design based on your inputs."),
      p("Click 'Get Started' to begin.")
    )
  )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$start_button, {
      updateTabsetPanel(session, "nav", selected = "Questionnaire")
    })
  })
}