library(shiny)

ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  titlePanel("Welcome to the Dose Finder Hub!"),

  mainPanel(

    h4("Please answer the questions below to get 
       recommended trial designs tailored to your
       needs, or upload previously saved responses."),

    shiny::fileInput("file_upload", "Upload Previous Responses:",
                     accept = c(".csv", ".rds")),

    uiOutput("questionsUI"),

    shiny::fluidRow(
      actionButton("get_rating",
                   label = "Generate recommendations!",
                   width = 500)
    )
  ),

  # Simple text output giving a sentence describing singular
  # recommended method, for now.
  shiny::textOutput("recommendations"),

  shiny::downloadButton("save_button", "Save Responses")

)

shinyApp(ui, server)
