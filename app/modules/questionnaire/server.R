questions_df <- read.csv("app/data/questionnaire_inputs/q_database.csv")

parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}

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

generate_UI <- function(current_question) {
  params <- parse_params(current_question$params)

  switch(current_question$q_type,
    radioButtons = shiny::radioButtons(inputId = current_question$q_variable,
                                       label = current_question$q_text,
                                       choices = strsplit(params[["choices"]], ",")[[1]],
                                       width = 500),
    numeric = shiny::numericInput(inputId = current_question$q_variable,
                                  label = current_question$q_text,
                                  min = as.numeric(params[["min"]]),
                                  value = 0,
                                  width = 500),
    slider = shiny::sliderInput(inputId = current_question$q_variable,
                                label = current_question$q_text,
                                min = as.numeric(params[["min"]]),
                                max = as.numeric(params[["max"]]),
                                value = as.numeric(params[["min"]]),
                                width = 500),
    text = shiny::textInput(inputId = current_question$q_variable,
                            label = current_question$q_text,
                            placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
                            value = "", width = 500)
  )
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
    generate_UI(current_question)
  })

  # Update the progress bar
  output$progress <- shiny::renderText({
    paste("Progress:", current_index(), "/", nrow(questions_df))
  })

  # Update the current question when the next button is clicked
  shiny::observeEvent(
    input$next_button, {
      new_index <- current_index() + 1
      #new_index <- check_validation(new_index, input)
      current_index(new_index)
    }
  )

  shiny::observeEvent(
    input$previous, {
      if (current_index() > 1) {
        current_index(current_index() - 1)
      }
    }
  )

  shiny::observeEvent(
    input$reset, {
      current_index(1)
    }
  )

  # Random number generation creating recommendation
  rand <- shiny::eventReactive(input$get_rating, {
    runif(1)
  })

  output$recommendations <- shiny::renderText({
    if (rand() > 0 & rand() < 1 / 3) {

      rating <- c("crm", "tpt", "boin")
      "First choice is CRM, second choice is 3+3, third choice is BOIN."

    } else if (rand() > 1 / 3 & rand() < 2 / 3) {

      rating <- c("tpt", "crm", "boin")
      "First choice is 3+3, second choice is CRM, third choice is BOIN."

    } else {

      rating <- c("boin", "tpt", "crm")
      "First choice is BOIN, second choice is 3+3, third choice is CRM."

    }
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
