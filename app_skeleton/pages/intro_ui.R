#library(shiny)
#library(shiny.semantic)

library(rmarkdown)
library(shiny)
library(bslib)


intro_ui <- function(id) {
  ns <- NS(id)
  tab_info <- list(
    "Introduction"         = "app_skeleton/pages/rmd/intro_introduction.Rmd",
    "Workflow"             = "app_skeleton/pages/rmd/intro_workflow.Rmd",
    "The Three Models"     = "app_skeleton/pages/rmd/intro_three_models.Rmd",
    "The Team"             = "app_skeleton/pages/rmd/intro_team.Rmd",
    "Guidance on Importing"= "app_skeleton/pages/rmd/intro_import_guidance.Rmd"
  )

  fluidPage(
    div(
      style = "max-width: 800px; margin-left: auto; margin-right: auto;",
      navset_tab(
        nav_panel(
          "Introduction",
          includeMarkdown("app_skeleton/pages/rmd/intro_introduction.Rmd")
        ),
        nav_panel(
          "The Three Models",
          includeMarkdown("app_skeleton/pages/rmd/intro_three_models.Rmd")
        ),
        nav_panel(
          "Workflow",
          includeMarkdown("app_skeleton/pages/rmd/intro_workflow.Rmd")
        ),
        nav_panel(
            "Team",
            includeMarkdown("app_skeleton/pages/rmd/intro_team.Rmd")
          ),
          nav_panel(
            "Guide to Importing",
            includeMarkdown("app_skeleton/pages/rmd/intro_import_guidance.Rmd")    
          )
        )
      )  
    )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for the intro module can be added here if needed
  })
}