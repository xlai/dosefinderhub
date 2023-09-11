parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}
questions <- read.csv("app/data/questionnaire_inputs/q_database.csv")

server <- function(input, output, session) {

  # Load button
  observe({
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

  output$questionsUI <- renderUI({
    tagList(
      lapply(seq_len(nrow(questions)), function(i) {
        question <- questions[questions$q_number == i, ]
        params <- parse_params(question$params)

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
                                  placeholder = "Enter your hint here",
                                  value = "", width = 500)
          )
      })
    )
  })

  # Random number generation creating recommendation
  rand <- shiny::eventReactive(input$get_rating, {
    runif(1)
  })

  output$recommendations <- shiny::renderText({
    if (rand() > 0 & rand() < 1 / 3) {

      rating <- c("crm", "tpt", "other")
      "First choice is CRM, second choice is 3+3, third choice is other."

    } else if (rand() > 1 / 3 & rand() < 2 / 3) {

      rating <- c("tpt", "crm", "other")
      "First choice is 3+3, second choice is CRM, third choice is other."

    } else {

      rating <- c("other", "tpt", "crm")
      "First choice is other, second choice is 3+3, third choice is CRM."

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
