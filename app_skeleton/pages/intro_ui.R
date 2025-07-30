#library(shiny)
#library(shiny.semantic)

library(rmarkdown)
library(shiny)
library(bslib)


intro_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      style = "max-width: 800px; margin-left: auto; margin-right: auto;",
      navset_tab(
        nav_panel(
          "Introduction",
          includeMarkdown("app_skeleton/pages/rmd/intro_introduction.Rmd"),
          div(
            style = "text-align: center; margin: 30px 0;",
            actionButton(
              ns("navigate_to_questionnaire"),
              "Which Method for Me?",
              class = "btn-primary btn-lg",
              style = "padding: 15px 30px; font-size: 18px; font-weight: bold;"
            )
          )
        ),
        nav_panel(
          "Dose-Finding Methods",
          includeMarkdown("app_skeleton/pages/rmd/intro_three_models.Rmd")
        ),
        nav_panel(
          "Team",
          includeMarkdown("app_skeleton/pages/rmd/intro_team.Rmd")
        )
      )  
    )
  )
}

intro_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    # Handle navigation button clicks
    observeEvent(input$navigate_to_questionnaire, {
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "nav", selected = "Questionnaire")
      }
    })
  })
}