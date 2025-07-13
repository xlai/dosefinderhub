
parse_conditions <- function(condition_str) {
  conditions <- strsplit(condition_str, ";")[[1]]
  cond_var <- conditions[1]
  cond_val <- conditions[2]
  cbind(cond_var, cond_val)
}

current_question <- reactiveVal(1)

output$questionsUI <- renderUI({
  current questoin <- questions_df[current_index(), ]
  generate_questions_UI(current_question)
})  

output$questionsUI <- shiny::renderUI({
    current_q <- current_question()
    if (current_q <= nrow(questions)) {
      question <- questions[questions$q_number == current_q, ]
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
                                placeholder = "i.e. 0.05, 0.15, 0.3, 0.7",
                                value = "", width = 500)
      )
    }
  })


    shiny::observeEvent(
    input$next_button, {
      question <- questions[questions$q_number == current_question(), ]
      condition <- parse_conditions(question$condition)
      inputcond <- noquote(paste(question$q_variable, condition[1], sep = ""))
      if (current_question() < nrow(questions)) {
        if (is.na(condition[1])) {
          current_question(current_question() + 1)
        } else if (condition[1] == condition[2]) {
          current_question(current_question() + 1)
        } else {
          current_question(current_question() + 2)
        }
      }
    }
  )




  
generate_UI <- function(current_question) {
  params <- parse_params(current_question$params, current_question$q_type)

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