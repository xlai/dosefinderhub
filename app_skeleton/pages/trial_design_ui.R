library(shiny)
library(shiny.semantic)


trial_design_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Trial Design"),
        p("This section allows you to design your trial based on the inputs provided in the questionnaire."),
        p("You can specify the trial parameters, including dose levels, sample size, and other relevant details."),
      ),
    sidebar = sidebar(
        h4("Trial Design Parameters"),
        p("Adjust the parameters for your trial design below:")

    )
  )
}

trial_design_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}