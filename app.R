#app.R#
# Load necessary libraries
library(shiny)
library(bslib)
library(htmltools)

# Define UI for the application
ui <- page_navbar(
    title = "Dose Finder Hub",
    navbar_options = navbar_options(
        bg = "#0062cc",
        underline = TRUE,   #definnig the nav bar
    ),
    nav_panel(title ="Introduction", p("first tab content")),
    nav_panel(title = "Questionnaire", p("second tab content")),
    nav_panel(title = "Trial Design", p("third tab content")),
    nav_panel(title = "Simulation", p("fourth tab content")),
    nav_panel(title = "Conduct", p("fifth tab content")),
    nav_spacer(),
)

# Define server logic
server <- function(input, output){}

shinyApp(ui, server)