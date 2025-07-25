library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
      tags$head(
        tags$style(HTML("
          html {
            scroll-behavior: smooth;
          }
          h2 {
            margin-top: 100px; /* for spacing when jumped to */
          }
        "))
      ),
      
      # Intro UI section
      h2("Introduction"),
      tags$div(id = "Introsection1", h4("Why the app was created"), p("Content of section 1......")),
      tags$div(id = "Introsection2", h4("Background to Trial Designs and Statistics"), p("Content of section 2.....")),
      tags$div(id = "Introsection3", h4("How DoseFinderHub can help you"), p("Content of section 3.....")),

      # Workflow questionnaire UI section
      h1("Work Flow"),
      h3("Questionnaire"),
      tags$div(id = "WFquestionnaire1", h4("The Meaning Behind the Questionnaire"), p("Content of section 1......")),
      tags$div(id = "WFquestionnaire2", h4("The Trial Design Recommendation"), p("Content of section 2.....")),
      tags$div(id = "WFquestionnaire3", h4("The Optional Methods Questionnaire to Tailor even Further"), p("Content of section 3.....")),

      # Workflow trial design UI section
      h3("Trial Design"),
      tags$div(id = "WFtrial1", h4("For the Statistician"), p("Content of section 1......"))
    ),
    
    sidebar = sidebar(
      h4("Table of Contents"),
      accordion(
        id = "nav_id",
        accordion_panel(
          "Introduction",
          tags$ul(
            tags$li(a(href = "#Introsection1", "Why the app was created")),
            tags$li(a(href = "#Introsection2", "Background to Trial Designs and Statistics")),
            tags$li(a(href = "#Introsection3", "How DoseFinderHub can help you"))
          )
        ),
        accordion_panel(
          "Work Flow",
          tags$ul(
            tags$li(a(href = "#WFquestionnaire1", "The Meaning Behind the Questionnaire")),
            tags$li(a(href = "#WFquestionnaire2", "The Trial Design Recommendation")),
            tags$li(a(href = "#WFquestionnaire3", "The Optional Methods Questionnaire to Tailor even Further")),
            tags$li(a(href = "#WFtrial1", "For the Statistician"))
          )
        ),
        accordion_panel(
          "Team/Contact", icon = icon("menu-app")
        ),
        accordion_panel(
          "Guide to Importing", icon = icon("menu-app")
        )
      )
    )
  )
}