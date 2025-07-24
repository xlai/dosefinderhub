#library(shiny)
#library(shiny.semantic)

library(rmarkdown)
library(shiny)
library(bslib)


intro_ui <- function(id) {
  ns <- NS(id)
  # Tab names and associated Rmd files
  tab_info <- list(
    "Introduction"         = "app_skeleton/pages/rmd/intro_introduction.Rmd",
    "Workflow"             = "app_skeleton/pages/rmd/intro_workflow.Rmd",
    "The Three Models"     = "app_skeleton/pages/rmd/intro_three_models.Rmd",
    "The Team"             = "app_skeleton/pages/rmd/intro_team.Rmd",
    "Guidance on Importing"= "app_skeleton/pages/rmd/intro_import_guidance.Rmd"
  )
  page_sidebar(
    mainPanel(
          navset_tab(
          nav_panel(
            "Intro",
            includeMarkdown("app_skeleton/pages/rmd/intro_introduction.Rmd")
          ),
          nav_panel(
            "Workflow",
            includeMarkdown("app_skeleton/pages/rmd/intro_workflow.Rmd")
          ),
          nav_panel(
            "The Three Models",
            includeMarkdown("app_skeleton/pages/rmd/intro_three_models.Rmd")
          ),
          nav_panel(
            "Team",
            includeMarkdown("app_skeleton/pages/rmd/intro_team.Rmd")
          ),
          nav_panel(
            "Guide to Importing",
            includeMarkdown("app_skeleton/pages/rmd/intro_import_guidance.Rmd")
       
          )
    )),
    sidebar = sidebar(
      h4("Contents"),
      p("Insert contents page here.")
    )
  )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    tab_info <- list(
      "Introduction"         = "app_skeleton/pages/rmd/intro_introduction.Rmd",
      "Workflow"             = "app_skeleton/pages/rmd/intro_workflow.Rmd",
      "The Three Models"     = "app_skeleton/pages/rmd/intro_three_models.Rmd",
      "The Team"             = "app_skeleton/pages/rmd/intro_team.Rmd",
      "Guidance on Importing"= "app_skeleton/pages/rmd/intro_import_guidance.Rmd"
    )
    
    # Helper to render Rmd to HTML
    render_rmd_to_html <- function(file) {
      html_file <- tempfile(fileext = ".html")
      rmarkdown::render(file, output_file = html_file, quiet = TRUE)
      html_file
    }
    
    # For each tab, render Rmd file to HTML
    lapply(names(tab_info), function(tabname) {
      output[[paste0("tab_content_", gsub(" ", "_", tabname))]] <- renderUI({
        html_file <- render_rmd_to_html(tab_info[[tabname]])
        includeHTML(html_file)
      })
    })
    
    # Dynamic card content for each tab
    output$dynamic_card <- renderUI({
      req(input$main_tabs)
      tab <- input$main_tabs
      card(
        card_header(paste(tab, "Card")),
        card_body(paste("This card is for the", tab, "tab. Add dynamic content here as needed."))
      )
    })
  })
}