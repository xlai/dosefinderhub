library(shiny)

ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  shiny::radioButtons("tpt_allow_deesc",
                      label = "Would you like to be able to skip doses when
                      deescalating?",
                      choices = list("Yes", "No")),

  shiny::numericInput("tpt_n_sims",
                      label = "How many simulations would you like to run?",
                      value = 20, min = 1, max = 10000),

  shiny::fluidRow(
    actionButton("get_rating",
                 label = "Simulate!",
                 class = "btn-block")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
