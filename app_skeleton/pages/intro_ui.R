#library(shiny)
#library(shiny.semantic)

intro_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    mainPanel(
          navset_card_tab(
          full_screen = TRUE,
          nav_panel(
            "Intro",
            card_title("Introduction to dosefinder"),
            p("la la la")

          ),
          nav_panel(
            "Workflow",
            card_title("In what order to use DoseFinderHub"),
            p("serious stuff")
          ),
          nav_panel(
            "The Three Models",
            card_title("Breakdown of the three models DoseFinder focuses on"),
            p("statsie stats")
          ),
          nav_panel(
            "Team",
            card_title("The Legends behind it all"),
            p("People")
          ),
          nav_panel(
            "Guide to Importing",
            card_title("How to import/export with DoseFinderHub"),
            p("Blah Blah again")
          )
        ),
        card(
        card_header(
          class = "bg-dark",
          "graphics"),
          card_body(
            markdown("some text")
          )
        )
      ),
    sidebar = sidebar(
      h4("Contents"),
      p("Insert contents page here.")
    )
  )
}

intro_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}