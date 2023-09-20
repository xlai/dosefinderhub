generate_questions_UI <- function(current_question) {
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