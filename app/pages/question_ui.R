#library(shiny)
#library(bslib)
#library(shinyjs)

#################### Helper Functions#################################### 
parse_params <- function(params_str) {
  if (is.null(params_str) || is.na(params_str) || params_str == "") return(list())
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, function(x) if (length(x) > 1) x[2] else NA)
}

generate_questions_UI <- function(current_question) {
  params <- parse_params(current_question$params)

  switch(current_question$q_type,
         radioButtons = shiny::radioButtons(inputId = current_question$q_variable,
                                            label = current_question$q_text,
                                            choices = strsplit(params[["choices"]], ",")[[1]],
                                            width = "500"),
         numeric = shiny::numericInput(inputId = current_question$q_variable,
                                       label = current_question$q_text,
                                       min = as.numeric(params[["min"]]),
                                       value = 0,
                                       width = "500"),
         slider = shiny::sliderInput(inputId = current_question$q_variable,
                                     label = current_question$q_text,
                                     min = as.numeric(params[["min"]]),
                                     max = as.numeric(params[["max"]]),
                                     value = as.numeric(params[["min"]]),
                                     width = "500"),
         text = shiny::textInput(inputId = current_question$q_variable,
                                 label = current_question$q_text,
                                 placeholder = "e.g. 0.05, 0.15, 0.3, 0.7",
                                 value = "", width = "500")
  )
}

generate_recommendation <- function(x) {
  if (x > 0 & x < 1 / 3) {
    "First choice is CRM, second choice is 3+3, third choice is BOIN."
  } else if (x > 1 / 3 & x < 2 / 3) {
    "First choice is 3+3, second choice is CRM, third choice is BOIN."
  } else {
    "First choice is BOIN, second choice is 3+3, third choice is CRM."
  }
}

# Read questions
input_directory <- here::here('app_skeleton','Inputs')
questions <- read.csv(here::here(input_directory,"questions.csv"))

############################### UI ##################################
question_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      useShinyjs(),
      fileInput(ns("file_upload"), "Upload Previous Responses:", accept = c(".csv", ".rds")),
      downloadButton(ns("save_button"), "Save Responses"),
      actionButton(ns("reset_button"), "Reset"),
      br(), br(),
      uiOutput(ns("progress_bar"))
    ),
    tabPanel(
      h4("Please answer the questions below:"),
      uiOutput(ns("questions_ui")),
      br(),
      fluidRow(
        column(4, actionButton(ns("prev_button"), "Previous")),
        column(4, actionButton(ns("next_button"), "Next"))
      )
    )
  )
}

################################## SERVER ########################################
question_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    current_question <- reactiveVal(1)

    # Render question UI
    output$questions_ui <- renderUI({
      req(current_question() <= nrow(questions))
      generate_questions_UI(questions[current_question(), ])
    })

    # Progress bar
    output$progress_bar <- renderUI({
      progress_value <- round((current_question() / nrow(questions)) * 100)
      tagList(
        p(sprintf("Progress: %d%%", progress_value)),
        div(style = "background-color: #e0e0e0; height: 20px; width: 100%;",
            div(style = sprintf("background-color: #28ce8c; width: %s%%; height: 100%%;", progress_value)))
      )
    })

    # File upload
    observeEvent(input$file_upload, {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$datapath)
      user_responses <- if (ext == "csv") read.csv(input$file_upload$datapath) else readRDS(input$file_upload$datapath)

      for (i in seq_len(nrow(user_responses))) {
        inputId <- user_responses$inputId[i]
        value <- user_responses$value[i]
        updateTextInput(session, inputId, value = value)
        updateNumericInput(session, inputId, value = as.numeric(value))
        updateSliderInput(session, inputId, value = as.numeric(value))
        updateRadioButtons(session, inputId, selected = value)
      }
    })

    # Save responses
    output$save_button <- downloadHandler(
      filename = function() paste0("responses-", Sys.Date(), ".csv"),
      content = function(file) {
        input_ids <- questions$q_variable
        values <- sapply(input_ids, function(x) input[[x]])
        df <- data.frame(inputId = input_ids, value = values)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Reset
    observeEvent(input$reset_button, {
      current_question(1)
    })

    # Next button logic
    observeEvent(input$next_button, {
      if (current_question() < nrow(questions)) {
        current_question(current_question() + 1)
      } else {
        # At last question: show modal
        numeric_ids <- questions$q_variable[questions$q_type == "numeric"]
        values <- sapply(numeric_ids, function(var) as.numeric(input[[var]]))
        x <- mean(values, na.rm = TRUE) / 10

        showModal(modalDialog(
          title = "Your Recommendation",
          tabsetPanel(
            tabPanel("Summary", p(generate_recommendation(x))),
            tabPanel("CRM", p("CRM Pros: Adaptive, efficient", br(), "CRM Cons: Complex, prior-dependent")),
            tabPanel("3+3", p("3+3 Pros: Simple, standard", br(), "3+3 Cons: Statistically inefficient, rigid")),
            tabPanel("BOIN", p("BOIN Pros: Balanced, less prior-dependent", br(), "BOIN Cons: Less intuitive to clinicians"))
          ),
          easyClose = TRUE,
          size = "l"
        ))
      }
    })

    # Previous button logic
    observeEvent(input$prev_button, {
      if (current_question() > 1) {
        current_question(current_question() - 1)
      }
    })

    # Dynamically update Next button label
    observe({
      if (current_question() < nrow(questions)) {
        updateActionButton(session, "next_button", label = "Next")
      } else {
        updateActionButton(session, "next_button", label = "Generate Recommendation")
      }
    })
  })
}