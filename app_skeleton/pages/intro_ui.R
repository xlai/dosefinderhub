library(shiny)
library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tabPanel(
        h3("Intro"),
        p("This hub provides a user-friendly interface to guide you through the process of determining the appropriate trial design based on your inputs."),
      ),
      sidebar = sidebar(
        h4("Welcome"),
        navset_card_tab(
        title = "Table of Contents",
        id = "nav_id",
        nav_panel(
            "Section 1",
            value = "section1",
            h3("Introduction to Dose Finder Hub"),
            p("Section 1 info")
        ),
        nav_panel(
            "Section 2",
            value = "section2",
            h3("How to Use the Hub"),
            p("Section 2 info")
        ),
        nav_panel(
            "Section 3",
            value = "section3",
            h3("Understanding Trial Design"),
            p("Section 3 info")
        ))
    )
      
  )
}

intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}