ui <- shiny::fluidPage(
  tags$head(
    tags$style(HTML("
      .center-contents {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 20vh;  # This uses 80% of the viewport height; adjust as needed
      }
    "))
  ),
  theme = bslib::bs_theme(bootswatch = "darkly"),

  shiny::titlePanel("Welcome to the Dose Finder Hub!"),

  shiny::mainPanel(

    shiny::h4("Please answer the questions below to get 
       recommended trial designs tailored to your
       needs, or upload previously saved responses."),

    div(style = "display: inline-block; width: 48%; vertical-align: top;", 
      shiny::fileInput("file_upload", "Upload Previous Responses:",
                     accept = c(".csv", ".rds"))
    ),
    div(style = "display: inline-block; width: 40%; vertical-align: top;", 
      shiny::downloadButton("save_button", "Save Responses"),
    ),

    div(class = "center-contents text-center",    
      shiny::uiOutput("questionsUI", style = "font-size: 18px;")
    ),
    
    shiny::tags$hr(),

    fluidRow(
      column(8, uiOutput("progress_bar")),  # Progress bar taking up 8/12 of the width
      column(4, 
            div(style="display: flex; align-items: center;",  # CSS to vertically center the content
                actionButton("prev_button", "Previous"), 
                uiOutput("next_or_recommend_button"),
                actionButton("reset_button", "Reset")
            )
      )
    )


  ),

  shiny::conditionalPanel(
      condition = "!!output.showRecommendation",
      fluidRow(
        textOutput("recommendationText")
      )
  )
)

#shiny::shinyApp(ui, server)
