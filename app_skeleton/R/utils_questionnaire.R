# File: R/utils_questionnaire.R
# Helper functions for questionnaire module

#' Parse conditions string for conditional question navigation
#' @param condition_str String containing condition in format "operator;value"
#' @return Matrix with operator and value
parse_conditions <- function(condition_str) {
  if (is.null(condition_str) || is.na(condition_str) || condition_str == "") {
    return(cbind(NA, NA))
  }
  conditions <- strsplit(condition_str, ";")[[1]]
  cond_var <- conditions[1]
  cond_val <- conditions[2]
  cbind(cond_var, cond_val)
}

#' Parse parameter string into list (your existing function)
#' @param params_str String containing parameters separated by semicolons
#' @return Named list of parameters
parse_params <- function(params_str) {
  if (is.null(params_str) || is.na(params_str) || params_str == "") return(list())
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, function(x) if (length(x) > 1) x[2] else NA)
}

#' Generate questions UI (your existing function)
#' @param current_question Single row data frame containing question info
#' @return Shiny input UI element
generate_questions_UI <- function(current_question) {
  params <- parse_params(current_question$params)

  switch(current_question$q_type,
         radioButtons = shiny::radioButtons(
           inputId = current_question$q_variable,
           label = current_question$q_text,
           choices = strsplit(params[["choices"]], ",")[[1]],
           width = "500"
         ),
         numeric = shiny::numericInput(
           inputId = current_question$q_variable,
           label = current_question$q_text,
           min = as.numeric(params[["min"]]),
           value = 0,
           width = "500"
         ),
         slider = shiny::sliderInput(
           inputId = current_question$q_variable,
           label = current_question$q_text,
           min = as.numeric(params[["min"]]),
           max = as.numeric(params[["max"]]),
           value = as.numeric(params[["min"]]),
           width = "500"
         ),
         text = shiny::textInput(
           inputId = current_question$q_variable,
           label = current_question$q_text,
           placeholder = "e.g. 0.05, 0.15, 0.3, 0.7",
           value = "", 
           width = "500"
         )
  )
}

#' Generate recommendation (your existing function)
#' @param x Numeric score for recommendation
#' @return Character string with recommendation
generate_recommendation <- function(x) {
  if (x > 0 & x < 1 / 3) {
    "First choice is CRM, second choice is 3+3, third choice is BOIN."
  } else if (x > 1 / 3 & x < 2 / 3) {
    "First choice is 3+3, second choice is CRM, third choice is BOIN."
  } else {
    "First choice is BOIN, second choice is 3+3, third choice is CRM."
  }
}