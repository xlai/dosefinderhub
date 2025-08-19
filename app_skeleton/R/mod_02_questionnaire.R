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
                     style = "margin-bottom: 30px; color: #495057;"),
        shiny::uiOutput(ns("questions_ui"))
      ),
      
      # Navigation buttons
      htmltools::div(
        style = "max-width: 800px; margin: 20px auto 0 auto;",
        bslib::card(
          bslib::card_body(
            style = "padding: 15px;",
            shiny::fluidRow(
              shiny::column(
                4, 
                shiny::actionButton(
                  ns("prev_button"),
                  htmltools::tagList(
                    htmltools::tags$i(class = "fas fa-chevron-left", style = "margin-right: 8px;"),
                    "Previous"
                  ),
                  class = "btn-outline-secondary",
                  style = "width: 100%; border-radius: 8px;"
                )
              ),
              shiny::column(
                4,
                htmltools::div(
                  style = "text-align: center; padding-top: 8px; color: #6c757d; font-weight: 500;",
                  shiny::textOutput(ns("question_counter"))
                )
              ),
              shiny::column(
                4, 
                shiny::actionButton(
                  ns("next_button"),
                  htmltools::tagList(
                    "Next",
                    htmltools::tags$i(class = "fas fa-chevron-right", style = "margin-left: 8px;")
                  ),
                  class = "btn-primary",
                  style = "width: 100%; border-radius: 8px;"
                )
              )
            )
          ),
          style = "box-shadow: 0 1px 3px rgba(0,0,0,0.1);"
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
mod_questionnaire_server <- function(id, shared, move_data, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
   # Initialise reactive values
   shared$q_n_doses <- reactiveVal(numeric(0))
   shared$q_start_dose <- reactiveVal(numeric(0))
   shared$q_ttl <- reactiveVal(numeric(0))
   shared$q_cohort <- reactiveVal(numeric(0))
   shared$q_max_size <- reactiveVal(numeric(0))

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
          
          # Create question input wrapped in bslib card
          params <- parse_params(question$params)
          question_input <- switch(question$q_type,
            radioButtons = shiny::radioButtons(
              inputId = ns(question$q_variable),
              label = question$q_text,
              choices = strsplit(params[["choices"]], ",")[[1]],
              width = "100%"
            ),
            numeric = shiny::numericInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              min = as.numeric(params[["min"]]),
              value = as.numeric(params[["min"]]),
              width = "100%"
            ),
            slider = shiny::sliderInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              min = as.numeric(params[["min"]]),
              max = as.numeric(params[["max"]]),
              value = as.numeric(params[["min"]]),
              width = "100%"
            ),
            text = shiny::textInput(
              inputId = ns(question$q_variable),
              label = question$q_text,
              placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
              value = "", 
              width = "100%"
            )
          )
          
          # Wrap question in bslib card with max width
          htmltools::div(
            style = "max-width: 800px; margin: 0 auto;",
            bslib::card(
              bslib::card_header(
                htmltools::div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  htmltools::h5(sprintf("Question %d of %d", current_q_num, nrow(questions)), 
                               style = "margin: 0; color: #495057;"),
                  htmltools::span(
                    class = "badge bg-primary",
                    sprintf("%d%%", round((current_q_num / nrow(questions)) * 100))
                  )
                )
              ),
              bslib::card_body(
                htmltools::div(
                  style = "padding: 10px 0;",
                  question_input
                )
              ),
              height = "auto",
              style = "box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px;"
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
          original_var <- question$q_variable
          
          # Save response using non-namespaced input name
          if (original_var %in% names(input) && !is.null(input[[original_var]])) {
            user_responses[[original_var]] <- input[[original_var]]
          }
        }
      }
    }
    
    # Alternative helper to save ALL current responses (only for shown questions)
    save_all_responses <- function() {
      shown_q_numbers <- questions_shown()
      
      for (q_num in shown_q_numbers) {
        question <- questions[questions$q_number == q_num, ]
        if (nrow(question) > 0) {
          original_var <- question$q_variable
          
          # Save response using non-namespaced input name
          if (original_var %in% names(input) && !is.null(input[[original_var]])) {
            user_responses[[original_var]] <- input[[original_var]]
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
    
    question_responses <- reactiveValues(list = vector("list", length = nrow(questions)))

    # Next button logic with conditional navigation
    observeEvent(input$next_button, {
      
      # Save current response
      if(!is.null(current_question())) {
      current_q_num <- current_question()

      question_response <- questions$q_variable[questions$q_number == current_q_num]
      question_responses$list[[current_q_num]] <- input[[question_response]]

      #print(question_responses$list)
    }
      
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

    # Helper function to show recommendation modal using intelligent recommendation
    show_recommendation_modal <- function() {
      # Save all responses before generating recommendation
      save_all_responses()
      
      # Use the new intelligent recommendation function
      recommendation_result <- generate_intelligent_recommendation(reactiveValuesToList(user_responses))
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Your Method Recommendation",
          size = "l",
          easyClose = TRUE,
          
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Recommendation", 
              htmltools::div(
                style = "padding: 20px;",
                
                # Ranking display with badges
                htmltools::div(
                  style = "margin-bottom: 20px;",
                  htmltools::h4("Recommended Ranking", style = "color: #495057; margin-bottom: 15px;"),
                  
                  # First choice
                  htmltools::div(
                    style = "display: flex; align-items: center; margin-bottom: 10px;",
                    htmltools::span("1st Choice:", style = "font-weight: bold; margin-right: 10px; min-width: 100px;"),
                    htmltools::span(
                      class = "badge bg-success fs-6",
                      style = "margin-right: 10px; padding: 8px 12px;",
                      paste0(recommendation_result$ranked_methods[1], " (Score: ", recommendation_result$scores[recommendation_result$ranked_methods[1]], ")")
                    )
                  ),
                  
                  # Second choice
                  htmltools::div(
                    style = "display: flex; align-items: center; margin-bottom: 10px;",
                    htmltools::span("2nd Choice:", style = "font-weight: bold; margin-right: 10px; min-width: 100px;"),
                    htmltools::span(
                      class = "badge bg-warning fs-6",
                      style = "margin-right: 10px; padding: 8px 12px;",
                      paste0(recommendation_result$ranked_methods[2], " (Score: ", recommendation_result$scores[recommendation_result$ranked_methods[2]], ")")
                    )
                  ),
                  
                  # Third choice
                  htmltools::div(
                    style = "display: flex; align-items: center; margin-bottom: 15px;",
                    htmltools::span("3rd Choice:", style = "font-weight: bold; margin-right: 10px; min-width: 100px;"),
                    htmltools::span(
                      class = "badge bg-secondary fs-6",
                      style = "margin-right: 10px; padding: 8px 12px;",
                      paste0(recommendation_result$ranked_methods[3], " (Score: ", recommendation_result$scores[recommendation_result$ranked_methods[3]], ")")
                    )
                  ),
                  
                  # Confidence level
                  htmltools::div(
                    style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                    htmltools::span("Confidence Level: ", style = "font-weight: bold;"),
                    htmltools::span(
                      class = if (recommendation_result$confidence == "High") "badge bg-success" else if (recommendation_result$confidence == "Medium") "badge bg-warning" else "badge bg-secondary",
                      style = "font-size: 14px; padding: 6px 10px;",
                      recommendation_result$confidence
                    )
                  )
                ),
                
                # Rationale section
                htmltools::div(
                  htmltools::h5("Why This Recommendation?", style = "color: #495057; margin-bottom: 10px;"),
                  htmltools::p(
                    recommendation_result$rationale,
                    style = "font-size: 15px; line-height: 1.5; text-align: justify;"
                  )
                )
              )
            ),
            shiny::tabPanel(
              "CRM Details", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::h5("Continual Reassessment Method (CRM)", style = "color: #495057; margin-bottom: 15px;"),
                htmltools::div(
                  style = "margin-bottom: 15px;",
                  htmltools::h6("Advantages:", style = "color: #28a745; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Most efficient at finding the MTD"),
                    htmltools::tags$li("Continuously learns from data"),
                    htmltools::tags$li("Flexible adaptation to new information"),
                    htmltools::tags$li("Can incorporate prior knowledge effectively")
                  )
                ),
                htmltools::div(
                  htmltools::h6("Disadvantages:", style = "color: #dc3545; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Requires statistical expertise"),
                    htmltools::tags$li("More complex to implement"),
                    htmltools::tags$li("Results depend on prior specification"),
                    htmltools::tags$li("Less transparent decision-making")
                  )
                )
              )
            ),
            shiny::tabPanel(
              "BOIN Details", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::h5("Bayesian Optimal Interval (BOIN)", style = "color: #495057; margin-bottom: 15px;"),
                htmltools::div(
                  style = "margin-bottom: 15px;",
                  htmltools::h6("Advantages:", style = "color: #28a745; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Balances efficiency with simplicity"),
                    htmltools::tags$li("Pre-specified transparent decision rules"),
                    htmltools::tags$li("Good statistical properties"),
                    htmltools::tags$li("Suitable for regulatory submissions")
                  )
                ),
                htmltools::div(
                  htmltools::h6("Disadvantages:", style = "color: #dc3545; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Less familiar to many clinicians"),
                    htmltools::tags$li("Requires some statistical understanding"),
                    htmltools::tags$li("Less efficient than CRM"),
                    htmltools::tags$li("Fixed boundaries may not suit all scenarios")
                  )
                )
              )
            ),
            shiny::tabPanel(
              "3+3 Details", 
              htmltools::div(
                style = "padding: 20px;",
                htmltools::h5("Traditional 3+3 Design", style = "color: #495057; margin-bottom: 15px;"),
                htmltools::div(
                  style = "margin-bottom: 15px;",
                  htmltools::h6("Advantages:", style = "color: #28a745; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Simple and well-understood"),
                    htmltools::tags$li("No statistical expertise required"),
                    htmltools::tags$li("Widely accepted by regulators"),
                    htmltools::tags$li("Easy to implement and explain")
                  )
                ),
                htmltools::div(
                  htmltools::h6("Disadvantages:", style = "color: #dc3545; font-weight: bold;"),
                  htmltools::tags$ul(
                    htmltools::tags$li("Statistically inefficient"),
                    htmltools::tags$li("Often treats patients at suboptimal doses"),
                    htmltools::tags$li("Rigid escalation rules"),
                    htmltools::tags$li("Higher risk of under-dosing")
                  )
                )
              )
            )
          ),
          
          # Navigation buttons in modal footer
          footer = htmltools::div(
            style = "display: flex; justify-content: center; gap: 20px; padding: 15px;",
            shiny::actionButton(
              ns("go_to_simulation"),
              htmltools::tagList(
                htmltools::tags$i(class = "fas fa-play", style = "margin-right: 8px;"),
                "Go to Simulation"
              ),
              class = "btn-success btn-lg",
              style = "border-radius: 8px; font-weight: 500;"
            ),
            shiny::actionButton(
              ns("advanced_settings"), 
              htmltools::tagList(
                htmltools::tags$i(class = "fas fa-cog", style = "margin-right: 8px;"),
                "Advanced Settings"
              ),
              class = "btn-primary btn-lg", 
              style = "border-radius: 8px; font-weight: 500;"
            )
          )
        )
      )
    }
    
    # Transfer questionnaire values to shared variables for Trial Design/Simulation
    transfer_questionnaire_values <- function() {
      # Only map essential questionnaire values - advanced parameters handled in trial design
      shared$n_dosess <- shared$q_n_doses
      shared$ttl <- shared$q_ttl  
      shared$max_size <- shared$q_max_size
      shared$start_dose <- shared$q_start_dose
      shared$cohort_size <- shared$q_cohort
    }
    
    # Handler for "Go to Simulation" button
    observeEvent(input$go_to_simulation, {
      # Transfer questionnaire values
      transfer_questionnaire_values()
      
      # Trigger the transfer function in the Trial Design Module
      move_data(TRUE)

      # Navigate to Simulation tab
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "nav", selected = "Simulation")
      }
      
      # Close modal
      removeModal()
      
      # Show success notification
      showNotification(
        "Questionnaire values transferred! Navigating to Simulation...",
        type = "default",
        duration = 3
      )
    })
    
    # Handler for "Advanced Settings" button  
    observeEvent(input$advanced_settings, {
      # Transfer questionnaire values
      transfer_questionnaire_values()

      # Trigger the transfer function in the Trial Design Module
      move_data(TRUE)

      # Navigate to Trial Design tab
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "nav", selected = "Trial Design")
      }
      
      # Close modal
      removeModal()
      
      # Show success notification
      showNotification(
        "Questionnaire values transferred! Navigating to Trial Design...",
        type = "default", 
        duration = 3
      )
    })
    
    # Continuously save responses as they change (only for shown questions)
    observe({
      current_q_num <- current_question()
      if (current_q_num <= nrow(questions)) {
        question <- questions[questions$q_number == current_q_num, ]
        if (nrow(question) > 0) {
          original_var <- question$q_variable
          
          # Save response using non-namespaced input name
          if (original_var %in% names(input) && !is.null(input[[original_var]])) {
            user_responses[[original_var]] <- input[[original_var]]
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
        all_question_responses <- question_responses$list # saving set of responses

        # Defining shared variables to move to trial design
        shared$q_n_doses <- reactive(as.numeric(all_question_responses[[1]]))
        shared$q_start_dose <- reactive(as.numeric(all_question_responses[[4]]))
        shared$q_ttl <- reactive(as.numeric(all_question_responses[[2]]))
        shared$q_cohort <- reactive(as.numeric(all_question_responses[[5]]))
        shared$q_max_size <- reactive(as.numeric(all_question_responses[[3]]))
        #print(shared$q_start_dose())
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