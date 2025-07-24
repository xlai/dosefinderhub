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


parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}

generate_recommendation <- function(x){
  if (x > 0 & x < 1 / 3) {
      rating <- c("crm", "tpt", "boin")
      "First choice is CRM, second choice is 3+3, third choice is BOIN."
  } else if (x > 1 / 3 & x < 2 / 3) {
      rating <- c("tpt", "crm", "boin")
      "First choice is 3+3, second choice is CRM, third choice is BOIN."
  } else {
      rating <- c("boin", "tpt", "crm")
      "First choice is BOIN, second choice is 3+3, third choice is CRM."
  }    
}