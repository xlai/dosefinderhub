ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  shiny::titlePanel("Welcome to the Dose Finder Hub!"),

  shiny::mainPanel(

    shiny::h4("Please answer the questions below to get 
       recommended trial designs tailored to your
       needs, or upload previously saved responses."),

    shiny::fileInput("file_upload", "Upload Previous Responses:",
                     accept = c(".csv", ".rds")),

    shiny::uiOutput("questionsUI"),

    shiny::tags$hr(),

    shiny::textOutput("progress"),
    # Previous, Next, Reset button
    shiny::actionButton("previous", "Previous"),

    shiny::actionButton("next_button", "Next"),

    shiny::actionButton("reset", "Reset"),

    shiny::fluidRow(
      shiny::downloadButton("save_button", "Save Responses"),
    )
  ),

  shiny::sidebarPanel(
    shiny::fluidRow(
      shiny::actionButton("get_rating",
                          label = "Generate recommendations!")
    ),
    # Simple text output giving a sentence describing singular
    # recommended method, for now.
    shiny::textOutput("recommendations")
  )
)

shiny::shinyApp(ui, server)
