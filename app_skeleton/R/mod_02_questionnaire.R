source('app_skeleton/R/utils_questionnaire.R')

# File: app_skeleton/R/mod_questionnaire.R
# Questionnaire Module

#' Questionnaire UI Function
#'
#' @description A shiny Module for questionnaire interface
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_questionnaire_ui <- function(id) {
  ns <- NS(id)
  
  bslib::page_sidebar(
    title = "Questionnaire",
    
    # Sidebar with controls
    sidebar = bslib::sidebar(
      width = 300,
      
      # Use shinyjs
      shinyjs::useShinyjs(),
      
      # File upload
      shiny::fileInput(
        ns("file_upload"), 
        "Upload Previous Responses:",
        accept = c(".csv", ".rds"),
        width = "100%"
      ),
      
      # Action buttons
      shiny::downloadButton(
        ns("save_button"), 
        "Save Responses",
        class = "btn-primary",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      shiny::actionButton(
        ns("reset_button"), 
        "Reset",
        class = "btn-warning",
        style = "width: 100%; margin-bottom: 20px;"
      ),
      
      # Progress section
      htmltools::div(
        style = "margin-top: 20px;",
        htmltools::h5("Progress"),
        shiny::uiOutput(ns("progress_bar"))
      )
    ),
    
    # Main content area
    htmltools::div(
      class = "main-content",
      style = "padding: 20px;",
      
      # Question display area
      htmltools::div(
        id = ns("question_container"),
        htmltools::h4("Please answer the questions below:", 
                     style = "margin-bottom: 30px;"),
        shiny::uiOutput(ns("questions_ui"))
      ),
      
      # Navigation buttons
      htmltools::div(
        class = "navigation-buttons",
        style = "margin-top: 40px;",
        
        shiny::fluidRow(
          shiny::column(
            4, 
            shiny::actionButton(
              ns("prev_button"),
              "Previous",
              class = "btn-secondary",
              style = "width: 100%;"
            )
          ),
          shiny::column(
            4,
            htmltools::div(
              style = "text-align: center; padding-top: 8px;",
              shiny::textOutput(ns("question_counter"))
            )
          ),
          shiny::column(
            4, 
            shiny::actionButton(
              ns("next_button"),
              "Next", 
              class = "btn-primary",
              style = "width: 100%;"
            )
          )
        )
      )
    )
  )
}

#' Questionnaire Server Function
#'
#' @description Server logic for questionnaire module
#'
#' @param id Module id
#' 
#' @noRd 
mod_questionnaire_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load questions data 
    questions <- tryCatch({
      input_directory <- here::here('app_skeleton', 'Inputs')
      read.csv(file.path(input_directory, "questions.csv"))
    }, error = function(e) {
      # Fallback data for testing
      shiny::showNotification(
        paste("Could not load questions.csv:", e$message), 
        duration = 5,
        closeButton = TRUE,
        type = "warning"
      )
      data.frame(
        q_variable = c("q1", "q2", "q3"),
        q_text = c("Question 1", "Question 2", "Question 3"),
        q_type = c("radioButtons", "numeric", "slider"),
        params = c("choices=Yes,No", "min=0", "min=1;max=10"),
        stringsAsFactors = FALSE
      )
    })
    
    # Reactive values
    current_question <- reactiveVal(1)
    user_responses <- reactiveValues()
    questions_shown <- reactiveVal(c())  # Track which questions have been displayed
    
    # Render question UI using conditional logic
    output$questions_ui <- renderUI({
      current_q_num <- current_question()
      if (current_q_num <= nrow(questions)) {
        # Find question by q_number (your approach)
        question <- questions[questions$q_number == current_q_num, ]
        
        if (nrow(question) > 0) {
          # Track that this question has been shown
          shown <- questions_shown()
          if (!(current_q_num %in% shown)) {
            questions_shown(c(shown, current_q_num))
          }
          
          # Create namespaced version for UI generation
          question_ns <- question
          question_ns$q_variable <- ns(question$q_variable)
          
          # Use your existing renderUI logic
          params <- parse_params(question$params)
          switch(question$q_type,
            radioButtons = shiny::radioButtons(
              inputId = ns(question$q_variable),
              label = question$q_text,
              choices = strsplit(params[["choices"]], ",")[[1]],
              width = 500
            ),
            numeric = shiny::numericInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              min = as.numeric(params[["min"]]),
              value = 0,
              width = 500
            ),
            slider = shiny::sliderInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              min = as.numeric(params[["min"]]),
              max = as.numeric(params[["max"]]),
              value = as.numeric(params[["min"]]),
              width = 500
            ),
            text = shiny::textInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
              value = "", 
              width = 500
            )
          )
        }
      }
    })
    
    # Progress bar with Bootstrap styling
    output$progress_bar <- renderUI({
      progress_value <- round((current_question() / nrow(questions)) * 100)
      
      htmltools::tagList(
        htmltools::p(
          sprintf("Question %d of %d", current_question(), nrow(questions)),
          style = "margin-bottom: 10px; font-weight: bold;"
        ),
        htmltools::div(
          class = "progress",
          style = "height: 20px; margin-bottom: 10px;",
          htmltools::div(
            class = "progress-bar bg-success",
            role = "progressbar", 
            style = sprintf("width: %s%%;", progress_value),
            sprintf("%d%%", progress_value)
          )
        )
      )
    })
    
    # Question counter
    output$question_counter <- renderText({
      sprintf("Question %d of %d", current_question(), nrow(questions))
    })
    
    # Helper function to save current response
    save_current_response <- function() {
      current_q_num <- current_question()
      if (current_q_num <= nrow(questions)) {
        # Find question by q_number
        question <- questions[questions$q_number == current_q_num, ]
        if (nrow(question) > 0) {
          # Get the original variable name (without namespace)
          original_var <- question$q_variable
          # Get the namespaced variable name
          namespaced_var <- ns(original_var)
          
          # Save the response if it exists
          if (!is.null(input[[namespaced_var]])) {
            user_responses[[original_var]] <- input[[namespaced_var]]
            cat("Saved response for", original_var, ":", input[[namespaced_var]], "\n")
          } else {
            cat("No input found for", namespaced_var, "\n")
          }
        }
      }
    }
    
    # Alternative helper to save ALL current responses (only for shown questions)
    save_all_responses <- function() {
      shown_q_numbers <- questions_shown()
      cat("Questions shown so far:", paste(shown_q_numbers, collapse = ", "), "\n")
      
      for (q_num in shown_q_numbers) {
        question <- questions[questions$q_number == q_num, ]
        if (nrow(question) > 0) {
          original_var <- question$q_variable
          namespaced_var <- ns(original_var)
          
          # Only try to save if the input exists
          if (namespaced_var %in% names(input) && !is.null(input[[namespaced_var]])) {
            user_responses[[original_var]] <- input[[namespaced_var]]
            cat("Saved response for", original_var, ":", input[[namespaced_var]], "\n")
          } else {
            cat("Input", namespaced_var, "not found or is NULL\n")
          }
        }
      }
    }
    
    # Previous button logic
    observeEvent(input$prev_button, {
      if (current_question() > 1) {
        # Save current response before moving
        save_current_response()
        current_question(current_question() - 1)
      }
    })
    
    # Next button logic with conditional navigation
    observeEvent(input$next_button, {
      # Save current response
      save_current_response()
      
      current_q_num <- current_question()
      
      if (current_q_num < nrow(questions)) {
        # Get current question data
        question <- questions[questions$q_number == current_q_num, ]
        
        if (nrow(question) > 0 && !is.na(question$condition) && question$condition != "") {
          # Parse the condition using your existing function
          condition <- parse_conditions(question$condition)
          cond_operator <- condition[1]
          cond_value <- condition[2]
          
          # Get the current input value for comparison
          current_input_value <- user_responses[[question$q_variable]]
          
          # Determine next question based on condition
          if (is.na(cond_operator)) {
            # No condition, go to next question
            current_question(current_q_num + 1)
          } else {
            # Evaluate condition (simplified - you may need to expand this)
            condition_met <- FALSE
            
            if (!is.null(current_input_value)) {
              if (cond_operator == "==" && current_input_value == cond_value) {
                condition_met <- TRUE
              } else if (cond_operator == "!=" && current_input_value != cond_value) {
                condition_met <- TRUE
              }
              # Add more operators as needed (>, <, >=, <=, etc.)
            }
            
            if (condition_met) {
              # Condition met, go to next question
              current_question(current_q_num + 1)
            } else {
              # Condition not met, skip a question
              current_question(current_q_num + 2)
            }
          }
        } else {
          # No condition, simple increment
          current_question(current_q_num + 1)
        }
      } else {
        # At last question: show modal using your existing function
        show_recommendation_modal()
      }
    })

    # Helper function to show recommendation modal using your existing function
    show_recommendation_modal <- function() {
      # Calculate recommendation using your existing logic
      numeric_ids <- questions$q_variable[questions$q_type == "numeric"]
      values <- sapply(numeric_ids, function(var) {
        val <- user_responses[[var]]
        if (is.null(val)) return(0)
        as.numeric(val)
      })
      
      x <- mean(values, na.rm = TRUE) / 10
      
      # Use your existing generate_recommendation function
      recommendation_text <- generate_recommendation(x)
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Your Recommendation",
          size = "l",
          easyClose = TRUE,
          
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::p(recommendation_text, style = "font-size: 16px;")
              )
            ),
            shiny::tabPanel(
              "CRM", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::p("CRM Pros: Adaptive, efficient"),
                htmltools::br(),
                htmltools::p("CRM Cons: Complex, prior-dependent")
              )
            ),
            shiny::tabPanel(
              "3+3", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::p("3+3 Pros: Simple, standard"), 
                htmltools::br(),
                htmltools::p("3+3 Cons: Statistically inefficient, rigid")
              )
            ),
            shiny::tabPanel(
              "BOIN", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::p("BOIN Pros: Balanced, less prior-dependent"),
                htmltools::br(), 
                htmltools::p("BOIN Cons: Less intuitive to clinicians")
              )
            )
          )
        )
      )
    }
    
    # Continuously save responses as they change (only for shown questions)
    observe({
      current_q_num <- current_question()
      if (current_q_num <= nrow(questions)) {
        question <- questions[questions$q_number == current_q_num, ]
        if (nrow(question) > 0) {
          original_var <- question$q_variable
          namespaced_var <- ns(original_var)
          
          # Only watch for changes if the input actually exists
          if (namespaced_var %in% names(input) && !is.null(input[[namespaced_var]])) {
            user_responses[[original_var]] <- input[[namespaced_var]]
          }
        }
      }
    })
    
    # Dynamic button states
    observe({
      # Update previous button state
      if (current_question() <= 1) {
        shinyjs::disable("prev_button")
      } else {
        shinyjs::enable("prev_button")
      }
      
      # Update next button label
      if (current_question() >= nrow(questions)) {
        shiny::updateActionButton(
          session, 
          "next_button", 
          label = "Generate Recommendation"
        )
      } else {
        shiny::updateActionButton(
          session, 
          "next_button", 
          label = "Next"
        )
      }
    })
    
    # File upload handler
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      tryCatch({
        ext <- tools::file_ext(input$file_upload$datapath)
        uploaded_responses <- if (ext == "csv") {
          read.csv(input$file_upload$datapath)
        } else {
          readRDS(input$file_upload$datapath)
        }
        
        # Load responses using existing approach but with namespace handling
        for (i in seq_len(nrow(uploaded_responses))) {
          original_input_id <- uploaded_responses$inputId[i]
          namespaced_input_id <- ns(original_input_id)
          value <- uploaded_responses$value[i]
          
          # Store in user_responses
          user_responses[[original_input_id]] <- value
          
          # Update UI inputs using existing approach
          shiny::updateTextInput(session, original_input_id, value = value)
          shiny::updateNumericInput(session, original_input_id, value = as.numeric(value))
          shiny::updateSliderInput(session, original_input_id, value = as.numeric(value))
          shiny::updateRadioButtons(session, original_input_id, selected = value)
        }
        
        shiny::showNotification(
          "Responses loaded successfully!", 
          duration = 3,
          closeButton = TRUE,
          type = "default"
        )
        
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading file:", e$message), 
          duration = 5,
          closeButton = TRUE,
          type = "error"
        )
      })
    })
    
    # Save responses download handler (only saves responses from shown questions)
    output$save_button <- shiny::downloadHandler(
      filename = function() {
        paste0("responses-", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Save all responses from shown questions before downloading
        save_all_responses()
        
        # Only get variables from questions that were actually shown
        shown_q_numbers <- questions_shown()
        shown_questions <- questions[questions$q_number %in% shown_q_numbers, ]
        input_ids <- shown_questions$q_variable
        
        # Collect responses only from shown questions
        values <- sapply(input_ids, function(x) {
          response <- user_responses[[x]]
          if (is.null(response)) return("")
          as.character(response)
        })
        
        # Create dataframe
        df <- data.frame(
          inputId = input_ids, 
          value = values,
          question_number = shown_questions$q_number,
          question_text = shown_questions$q_text,
          stringsAsFactors = FALSE
        )
        
        # Filter out empty responses for cleaner output
        df_filtered <- df[df$value != "", ]
        
        # Write to file
        write.csv(df_filtered, file, row.names = FALSE)
        
        # Show notification
        showNotification(
          paste("Saved", nrow(df_filtered), "responses from", length(shown_q_numbers), "questions shown!"), 
          type = "success"
        )
      }
    )
    
    # Reset button handler
    observeEvent(input$reset_button, {
      # Reset to first question
      current_question(1)
      
      # Clear user responses
      for (var in questions$q_variable) {
        user_responses[[var]] <- NULL
      }
      
      showNotification("Responses have been reset.", type = "info")
    })
    
    # Return reactive values for use by parent modules
    return(
      list(
        responses = reactive(reactiveValuesToList(user_responses)),
        current_question = current_question,
        is_complete = reactive(current_question() > nrow(questions))
      )
    )
  })
}

# Wrapper functions to use existing helper functions

#' Modified wrapper for existing generate_questions_UI function
#' This handles the namespace issue while using existing logic
generate_questions_UI_wrapper <- function(current_question, ns) {
  # Create a modified version that handles namespacing
  current_question_ns <- current_question
  current_question_ns$q_variable <- ns(current_question$q_variable)
  
  # Call existing function
  generate_questions_UI(current_question_ns)
}