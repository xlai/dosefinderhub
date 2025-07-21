library(shiny)
library(shiny.semantic)

question_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    nav_panel(
      h3("Questionnaire"),
      p("Please answer the following questions to help us determine the best trial design for you."),
      p("Click 'Submit' to proceed with your answers.")
    ),
    sidebar = sidebar(
      h4("Questionnaire"),
      p("Please answer the following questions to help us determine the best trial design for you."),
      actionButton(ns("submit_button"), "Submit")
    )
  )
}

question_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}