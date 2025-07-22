library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Intro"),
        p("This hub provides a user-friendly interface to guide you through the process of determining the appropriate trial design based on your inputs."),
      tags$head(
        tags$style(HTML("
        html {
        scroll-behaviour: smooth;
        }
        h2 {
        margin-top; 100px;     /* for spacing when jumped to  */
        }
        "))
      ),

      tags$div(id = "section1", h2("Why the app was created"), p("Content of section 1......")),
      tags$div(id = "section2", h2("Background to Trial Designs and Statistics"), p("Content of section 2.....")),
      tags$div(id = "section3", h3("How DoseFinderHub can help you"), p("Content of section 3....."))
     ),

      sidebar = sidebar(
        h4("Table of Contents"),
        accordion_filters <- accordion(
        id = "nav_id",
        accordion_panel(
            "Introduction",
            tags$ul(
                tags$li(a(href = "#section1", "Why the app was created")),
                tags$li(a(href = "#section2", "Background to Trial Designs and Statistics")),
                tags$li(a(href = "#section3", "How DoseFinderHub can help you"))
            )
        ),
        accordion_panel(
            "WorkFlow", icon = icon("menu-app"),
        ),
        accordion_panel(
            "Team/Contact", icon = icon("menu-app"),
        ),
        accordion_panel(
            "Guide to Importing", icon = icon("menu-app"),
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