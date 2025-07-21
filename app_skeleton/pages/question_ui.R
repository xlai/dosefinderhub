library(shiny)
library(shiny.semantic)

question_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
   main = div(
      h3("Trial Design Questionnaire"),
      p("This section will guide you through a series of questions to identify your needs and preferences for trial design."),
      p("Click 'Submit' when you are ready to proceed.")
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