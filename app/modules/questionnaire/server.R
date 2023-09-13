questions <- read.csv("app/data/questionnaire_inputs/q_database.csv")

parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}

check_validation <- function(current_index) {
  if (current_index <= nrow(questions)) {
    current_question <- questions[questions$q_number == current_index, ]
    validation_expr <- current_question$condition

    # If the validation expression is not empty, evaluate it
    if (nchar(validation_expr) > 0) {
      condition_met <- eval(parse(text = validation_expr),
                            envir = list2env(input))
      if (condition_met) {
        return(current_index + 1)
      }
    }
  }
  return(current_index)
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

  # Update the current question in the UI
  output$questionUI <- shiny::renderUI({
    current_q <- current_index()
    if (current_q <= nrow(questions)) {
      question <- questions[questions$q_number == current_q, ]
      params <- parse_params(question$params, question$q_type)

      switch(question$q_type,
        radioButtons = shiny::radioButtons(inputId = question$q_variable,
                                          label = question$q_text,
                                          choices = strsplit(params[["choices"]], ",")[[1]],
                                          width = 500),
        numeric = shiny::numericInput(inputId = question$q_variable,
                                      label = question$q_text,
                                      min = as.numeric(params[["min"]]),
                                      value = 0,
                                      width = 500),
        slider = shiny::sliderInput(inputId = question$q_variable,
                                    label = question$q_text,
                                    min = as.numeric(params[["min"]]),
                                    max = as.numeric(params[["max"]]),
                                    value = as.numeric(params[["min"]]),
                                    width = 500),
        text = shiny::textInput(inputId = question$q_variable,
                                label = question$q_text,
                                placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
                                value = "", width = 500)
      )
    }
  })

  # Update the progress bar
  output$progress <- shiny::renderText({
    paste("Progress:", current_index(), "/", nrow(questions))
  })

  # Update the current question when the next button is clicked
  shiny::observeEvent(
    input$next_button, {
      new_index <- current_index() + 1
      new_index <- check_validation(new_index)
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
      inputs_to_save <- questions$q_variable
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