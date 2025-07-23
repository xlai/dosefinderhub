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
      #intro ui section
      tags$div(id = "Introsection1", h2("Why the app was created"), p("Content of section 1......")),
      tags$div(id = "Introsection2", h2("Background to Trial Designs and Statistics"), p("Content of section 2.....")),
      tags$div(id = "Introsection3", h3("How DoseFinderHub can help you"), p("Content of section 3.....")),
     #workflow questionnaire ui section
      tags$div(id = "WFquestionnaire1", h2("The Meaning Behind the Questionnaire"), p("Content of section 1......")),
      tags$div(id = "WFquestionnaire2", h2("The Trial Design Recommendation"), p("Content of section 2.....")),
      tags$div(id = "WFquestionnaire3", h3("The Optional Methods Questionnaire to Tailor even Further"), p("Content of section 3.....")),
     #workflow trial design ui section
      tags$div(id = "WFtrial", h2("For the Statiscian"), p("Content of section 1......"))
     ),
    
    sidebar = sidebar(
        h4("Table of Contents"),
        accordion_filters <- accordion(
        id = "nav_id",
        accordion_panel(
            "Introduction",
            tags$ul(
                tags$li(a(href = "#Introsection1", "Why the app was created")),
                tags$li(a(href = "#Introsection2", "Background to Trial Designs and Statistics")),
                tags$li(a(href = "#Introsection3", "How DoseFinderHub can help you"))
            
        ),
        accordion_panel(
          "WorkFlow",
            accordion_panel(
              "Questionnaire",
              tags$ul(
                tags$li(a(href = "#WFquestionnaire1", "The Meaning Behind the Questionnaire")),
                tags$li(a(href = "#WFquestionnaire2", "The Trial Design Recommendation")),
                tags$li(a(href = "#WFquestionnaire3", "The Optional Methods Questionnaire to Tailor even Further"))
            )),
            accordion_panel(
              "Trial Design",
              tags$ul(
                tags$li(a(href = "#WFtrial1", "For the Statiscian"))
            ))
        )),
        accordion_panel(
            "Team/Contact", icon = icon("menu-app"),
        ),
        accordion_panel(
          "Guide to Importing", icon = icon("menu-app"),
      ))
    ))
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}