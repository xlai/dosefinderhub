library(shiny)

ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  titlePanel("CRM Design Parameters"),

  mainPanel(

    h4("Please answer the questions below to get 
       comparisons between trial designs,
       or upload previously saved responses."),

    shiny::fileInput("file_upload", "Upload Previous Responses:",
                     accept = c(".csv", ".rds")),

    uiOutput("questionsUI"),

    shiny::fluidRow(
      actionButton("get_rating",
                   label = "Generate recommendations!",
                   width = 500)
    ),

    # Simple text output giving a sentence describing singular
    # recommended method, for now.
    shiny::textOutput("recommendations"),

    shiny::downloadButton("save_button", "Save Responses"),

  ),

  shiny::fluidRow(actionButton("get_rating",
                               label = "Simulate!",
                               class = "btn-block"))
)

shinyApp(ui, server)
