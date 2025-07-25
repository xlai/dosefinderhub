here::i_am("app/modules/questionnaire/server.R")
data_directory <- here::here('app','data','questionnaire_inputs') #define relative path to your questionnaire app directory
questions_df <- read.csv(here::here(data_directory,"q_database.csv"))

check_validation <- function(current_index, input) {
  if (current_index <= nrow(questions_df)) {
    current_question <- questions_df[questions_df$q_number == current_index, ]
    validation_expr <- current_question$condition
    print(current_question)
    print(input)
    shiny::observe({
      print(input$know_doses)
    })
    # If the validation expression is not empty, evaluate it
    if (nchar(validation_expr) > 0) {
      # Create a new environment for evaluation
      eval_env <- new.env()
      # Assign values from input to this environment
      list2env(x = input, envir = eval_env)
      # Parent environment should be the server environment to have access to other necessary objects
      parent.env(eval_env) <- environment()
      # Evaluate the expression within the new environment
      condition_met <- eval(parse(text = validation_expr), envir = eval_env)
      if (condition_met) {
        return(current_index + 1)
      }
    }
  }
}

server <- function(input, output, session) {

  # Load button
  shiny::observe({
    in_file <- input$file_upload

    if (is.null(in_file)) {
      return(NULL)
    }

    ext <- tools::file_ext(in_file$datapath)

    if (ext == "csv") {
      user_responses <- read.csv(in_file$datapath)
    } else if (ext == "rds") {
      user_responses <- readRDS(in_file$datapath)
    }
    for (i in seq_len(nrow(user_responses))){
      shiny::updateNumericInput(session,
                                inputId = user_responses$inputId[i],
                                value = user_responses$value[i])
    }
  })

  current_index <- shiny::reactiveVal(1)

  # Render the UI for the current question
  output$questionsUI <- shiny::renderUI({
    current_question <- questions_df[current_index(), ]
    generate_questions_UI(current_question)
  })

  # Update the progress bar
  output$progress_bar <- renderUI({
    
    progress_value <- (current_index() / nrow(questions_df)) * 100
    
    div(style = "width: 100%; display: flex; align-items: center;",  # CSS to vertically center the content
      div(id = "progressBar", class = "progress", style="height: 20px; width: 300px;", 
        div(class = "progress-bar", role = "progressbar", 
            `aria-valuenow` = progress_value, `aria-valuemin` = "0", 
            `aria-valuemax` = "100", style = sprintf("width: %s%%;", progress_value)
        )
      ),
      tags$span(style="margin-left: 10px;", 
                sprintf("%s/%s questions answered", current_index(), nrow(questions_df)))
    )
    
  })

  # Update the current question when the next button is clicked
  shiny::observeEvent(input$next_button, {
    if (current_index() < nrow(questions_df)) {
      new_index <- current_index() + 1
      current_index(new_index)
    }
    }
  )
  shiny::observeEvent(
    input$previous_button, {
      if (current_index() > 1) {
        current_index(current_index() - 1)
      }
    }
  )
  shiny::observeEvent(
    input$reset_button, {
      current_index(1)
    }
  )
  
  # Reactive value to control visibility of recommendation
  showRecommendation <- reactiveVal(FALSE)

 output$next_or_recommend_button <- renderUI({
  if (current_index() < nrow(questions_df)) {
    actionButton("next_button", "Next")
  } else if (!showRecommendation()) {
    actionButton("generate_recommendation", "Generate!")
  } else {
    actionButton("continue_to_methods", "Continue to Methods Questionnaire")
  }
 })



  # Observe the "Generate!" button click
  observeEvent(input$generate_recommendation, {
    showRecommendation(TRUE)  # Set to TRUE to show the recommendation
  })

  # Random number generation creating recommendation
  rand <- shiny::eventReactive(input$generate_recommendation, {
    runif(1)
  })

  output$recommendationText <- shiny::renderText({
    if (showRecommendation()) {
         # Compute and return your recommendation text here
         return(generate_recommendation(0.3))
    }
    return(NULL)

  })
  

   # This output will provide the condition for the conditionalPanel in the UI
  output$showRecommendation <- reactive({
      showRecommendation()
  })

  outputOptions(output, "showRecommendation", suspendWhenHidden = FALSE)
  
   #button to move onto the methods questionnaire
  observeEvent(input$continue_to_methods, {
    output$dynamicUI <- renderUI({
      questionnaireUI("methods")
    })
    session$userData$currentUI <- "methods"
  })

  # Save button
  output$save_button <- shiny::downloadHandler(
    filename = function() {
      paste("user_responses-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      inputs_to_save <- questions_df$q_variable
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for (input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      write.csv(inputs_data_frame, file)
    }
  )
}
