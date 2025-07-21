library(shiny)
library(shiny.semantic)
questions_df <- read.csv("q_database.csv")

#######UI######################################################
question_ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    sidebar = sidebar(
      h4("Navigation"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
        actionButton(ns("prev_button"), "Previous"),
        uiOutput(ns("next_or_recommend_button")),
        actionButton(ns("reset_button"), "Reset")
      )
    ),
    
 # Main content
    mainPanel(
      h4("Please answer the questions below to get 
         recommended trial designs tailored to your
         needs, or upload previously saved responses."),

      fluidRow(
        column(6,
          fileInput(ns("file_upload"), "Upload Previous Responses:",
                    accept = c(".csv", ".rds"))
        ),
        column(6,
          downloadButton(ns("save_button"), "Save Responses")
        )
      ),

      div(class = "center-contents text-center",
          uiOutput(ns("questionsUI"), style = "font-size: 18px;")
      ),

      tags$hr(),

      fluidRow(
        column(8, uiOutput(ns("progress_bar")))
      ),

      conditionalPanel(
        condition = "output.showRecommendation === true",
        fluidRow(
          textOutput(ns("recommendationText"))
        )
      )
    )
  )
}


######SERVER########################################################################
question_server <- function(id, questions_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_index <- reactiveVal(1)
    showRecommendation <- reactiveVal(FALSE)

    # Load uploaded responses
    observe({
      in_file <- input$file_upload
      if (is.null(in_file)) return(NULL)

      ext <- tools::file_ext(in_file$datapath)
      user_responses <- switch(ext,
        csv = read.csv(in_file$datapath),
        rds = readRDS(in_file$datapath),
        NULL
      )

      if (!is.null(user_responses)) {
        for (i in seq_len(nrow(user_responses))) {
          updateNumericInput(session,
            inputId = user_responses$inputId[i],
            value = user_responses$value[i]
          )
        }
      }
    })

    # Render current question
    output$questionsUI <- renderUI({
      current_question <- questions_df[current_index(), ]
      generate_UI(current_question)
    })

    # Progress bar
    output$progress_bar <- renderUI({
      progress_value <- (current_index() / nrow(questions_df)) * 100
      div(style = "width: 100%; display: flex; align-items: center;",
        div(id = "progressBar", class = "progress", style = "height: 20px; width: 300px;",
          div(class = "progress-bar", role = "progressbar",
              `aria-valuenow` = progress_value, `aria-valuemin` = "0",
              `aria-valuemax` = "100", style = sprintf("width: %.0f%%;", progress_value))
        ),
        tags$span(style = "margin-left: 10px;",
                  sprintf("%s/%s questions answered", current_index(), nrow(questions_df)))
      )
    })

    # Navigation
    observeEvent(input$prev_button, {
      if (current_index() > 1) current_index(current_index() - 1)
    })

    observeEvent(input$next_button, {
      if (current_index() < nrow(questions_df)) current_index(current_index() + 1)
    })

    observeEvent(input$reset_button, {
      current_index(1)
      showRecommendation(FALSE)
    })

    # Conditional button rendering
    output$next_or_recommend_button <- renderUI({
      if (current_index() < nrow(questions_df)) {
        actionButton(ns("next_button"), "Next")
      } else if (!showRecommendation()) {
        actionButton(ns("generate_recommendation"), "Generate!")
      } else {
        NULL
      }
    })

    # Recommendation logic
    observeEvent(input$generate_recommendation, {
      showRecommendation(TRUE)
    })

    output$recommendationText <- renderText({
      if (showRecommendation()) {
        # Collect numeric inputs
        numeric_vars <- questions_df$q_variable[questions_df$q_type %in% c("numeric", "slider")]
        numeric_values <- sapply(numeric_vars, function(x) as.numeric(input[[x]]))
        numeric_values <- numeric_values[!is.na(numeric_values)]

        # Compute score (mean of numeric inputs)
        score <- if (length(numeric_values) > 0) mean(numeric_values) else 0.5

        # Generate recommendation
        generate_recommendation(score)
      } else {
        NULL
      }
    })

    output$showRecommendation <- reactive({
      showRecommendation()
    })
    outputOptions(output, "showRecommendation", suspendWhenHidden = FALSE)

    # Save responses
    output$save_button <- downloadHandler(
      filename = function() {
        paste0("user_responses-", Sys.Date(), ".csv")
      },
      content = function(file) {
        inputs_to_save <- questions_df$q_variable
        inputs <- sapply(inputs_to_save, function(x) input[[x]])
        inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
        write.csv(inputs_data_frame, file, row.names = FALSE)
      }
    )
  })
}