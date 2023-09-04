library(shiny)

ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  shiny::titlePanel("3+3 Design Parameters"),

  shiny::mainPanel(

    htmltools::h4("Please answer the questions below to get 
       comparisons between trial designs,
       or upload previously saved responses."),

    shiny::fileInput("file_upload", "Upload Previous Responses:",
                     accept = c(".csv", ".rds")),

    shiny::uiOutput("questionsUI"),

    shiny::downloadButton("save_button", "Save Responses"),

    shiny::fluidRow(
      actionButton("get_rating",
                   label = "Simulate!",
                   class = "btn-block")
    )

  )
)

shinyApp(ui, server)
