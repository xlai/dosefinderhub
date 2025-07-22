library(shiny)
library(shiny.semantic)
library(bslib)
library(shinyjs)

############HELPER FUNCTIONS########################################################
parse_params <- function(params_str) {
    if(is.null(params_str) || is.na(params_str) || params_str =="") return(list())
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
                            placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
                            value = "", width = "500"))
}

generate_recommendation <- function(x){
    if (x > 0 & x < 1 / 3) {
        "First choice is CRM, second choice is 3+3, third choice is BOIN."
    } else if (x > 1 / 3 & x < 2 / 3) {
        "First choice is 3+3, second choice is CRM, third choice is BOIN."
    } else {
        "First choice is BOIN, second choice is 3+3, third choice is CRM."
    }
}

questions <- read.csv("app_skeleton/Inputs/questions.csv", stringsAsFactors = FALSE)

#######UI######################################################
question_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        fileInput(ns("file_upload"), "Upload Previous Responses:", accept = c(".csv", ".rds")),
        downloadButton(ns("save_button"), "Save Responses"),
        actionButton(ns("reset"), "Reset"),
        uiOutput(ns("progress_bar"))
    ),
 # Main content
    mainPanel(
        h4("Please answer the following questions:"),
        uiOutput(ns("questions_ui")),
        actionButton(ns("next_or_recommendation_button"), "Next/Generate Recommendation")
    )
  )
}


######SERVER########################################################################
question_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    current_question <- reactiveVal(1)

    observeEvent(input$file_upload, {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$datapath)
      user_responses <- if (ext == "csv") read.csv(input$file_upload$datapath) else readRDS(input$file_upload$datapath)
      for (i in seq_len(nrow(user_responses))) {
        updateTextInput(session, user_responses$inputId[i], value = user_responses$value[i])
      }
    })

    output$questionsUI <- renderUI({
      req(current_question() <= nrow(questions))
      generate_questions_UI(questions[current_question(), ])
    })

    output$progress_bar <- renderUI({
      progress_value <- (current_question() / nrow(questions)) * 100
      tagList(
        p(sprintf("Progress: %.0f%%", progress_value)),
        div(style = "background-color:rgb(255, 255, 255); height: 20px; width: 100%;",
            div(style = sprintf("background-color:rgb(40, 206, 140); width: %s%%; height: 100%%;", progress_value)))
      )
    })

    observeEvent(input$reset_button, { current_question(1) })
    observeEvent(input$next_button, {
      if (current_question() < nrow(questions)) current_question(current_question() + 1)
    })

    output$next_or_recommend_button <- renderUI({
      if (current_question() < nrow(questions)) {
        actionButton(session$ns("next_button"), "Next")
      } else {
        actionButton(session$ns("generate_recommendation"), "Generate Recommendation")
      }
    })

    output$save_button <- downloadHandler(
      filename = function() paste("responses-", Sys.Date(), ".csv", sep=""),
      content = function(file) {
        input_ids <- questions$q_variable
        values <- sapply(input_ids, function(x) input[[x]])
        df <- data.frame(inputId = input_ids, value = values)
        write.csv(df, file, row.names = FALSE)
      }
    )

    observeEvent(input$generate_recommendation, {
      showModal(modalDialog(
        title = "Your Recommendation",
        tabsetPanel(
          tabPanel("Summary", p({
            numeric_ids <- questions$q_variable[questions$q_type == "numeric"]
            values <- sapply(numeric_ids, function(var) as.numeric(input[[var]]))
            x <- mean(values, na.rm = TRUE) / 10
            generate_recommendation(x)
          })),
          tabPanel("CRM", p("CRM Pros: Adaptive, efficient\nCRM Cons: Complex, prior-dependent")),
          tabPanel("3+3", p("3+3 Pros: Simple, standard\n3+3 Cons: Statistically inefficient, rigid")),
          tabPanel("BOIN", p("BOIN Pros: Balanced, less prior-dependent\nBOIN Cons: Less intuitive to clinicians"))
        ),
        easyClose = TRUE, size = "l"
      ))
    })
  })
}