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

#' Generate intelligent method recommendation based on user responses
#' @param user_responses Named list containing user responses to recommendation questions
#' @return List with ranked methods, scores, confidence, rationale, and display text
generate_intelligent_recommendation <- function(user_responses) {
  
  # Initialize scores for each method
  scores <- c(CRM = 0, BOIN = 0, "3+3" = 0)
  
  # Decision matrix for toxicity_confidence
  if (!is.null(user_responses$toxicity_confidence)) {
    toxicity_scores <- switch(user_responses$toxicity_confidence,
      "Very confident (good historical data)" = c(CRM = 3, BOIN = 1, "3+3" = 0),
      "Somewhat confident" = c(CRM = 1, BOIN = 3, "3+3" = 1),
      "Not confident/limited data" = c(CRM = 0, BOIN = 1, "3+3" = 3),
      c(CRM = 0, BOIN = 0, "3+3" = 0)  # fallback
    )
    scores <- scores + toxicity_scores
  }
  
  # Decision matrix for trial_priorities
  if (!is.null(user_responses$trial_priorities)) {
    priority_scores <- switch(user_responses$trial_priorities,
      "Getting started quickly with simple rules" = c(CRM = 0, BOIN = 1, "3+3" = 3),
      "Finding the best dose efficiently" = c(CRM = 3, BOIN = 2, "3+3" = 0),
      "Balance of both" = c(CRM = 1, BOIN = 3, "3+3" = 1),
      c(CRM = 0, BOIN = 0, "3+3" = 0)  # fallback
    )
    scores <- scores + priority_scores
  }
  
  # Decision matrix for statistical_support
  if (!is.null(user_responses$statistical_support)) {
    support_scores <- switch(user_responses$statistical_support,
      "Yes experienced with complex modeling" = c(CRM = 3, BOIN = 1, "3+3" = 0),
      "Yes but prefer simpler approaches" = c(CRM = 1, BOIN = 3, "3+3" = 1),
      "Limited statistical support" = c(CRM = 0, BOIN = 1, "3+3" = 3),
      c(CRM = 0, BOIN = 0, "3+3" = 0)  # fallback
    )
    scores <- scores + support_scores
  }
  
  # Decision matrix for decision_transparency
  if (!is.null(user_responses$decision_transparency)) {
    transparency_scores <- switch(user_responses$decision_transparency,
      "Very important (regulatory/reproducibility)" = c(CRM = 0, BOIN = 3, "3+3" = 1),
      "Flexible adaptation preferred" = c(CRM = 3, BOIN = 1, "3+3" = 0),
      "Simple fixed rules fine" = c(CRM = 0, BOIN = 1, "3+3" = 3),
      c(CRM = 0, BOIN = 0, "3+3" = 0)  # fallback
    )
    scores <- scores + transparency_scores
  }
  
  # Rank methods by score (highest first)
  ranked_methods <- names(sort(scores, decreasing = TRUE))
  
  # Calculate confidence based on score separation
  max_score <- max(scores)
  second_score <- sort(scores, decreasing = TRUE)[2]
  score_gap <- max_score - second_score
  
  confidence <- if (score_gap >= 3) {
    "High"
  } else if (score_gap >= 2) {
    "Medium"
  } else {
    "Low"
  }
  
  # Generate rationale based on responses and dominant factors
  rationale <- generate_rationale(user_responses, ranked_methods[1], scores)
  
  # Create formatted recommendation text
  recommendation_text <- sprintf(
    "Based on your responses, we recommend:\n\n1st choice: %s (score: %d)\n2nd choice: %s (score: %d)\n3rd choice: %s (score: %d)\n\nConfidence: %s\n\n%s",
    ranked_methods[1], scores[ranked_methods[1]],
    ranked_methods[2], scores[ranked_methods[2]], 
    ranked_methods[3], scores[ranked_methods[3]],
    confidence, rationale
  )
  
  return(list(
    ranked_methods = ranked_methods,
    scores = scores,
    confidence = confidence,
    rationale = rationale,
    recommendation_text = recommendation_text
  ))
}

#' Generate explanatory rationale for recommendation
#' @param user_responses User responses to recommendation questions
#' @param top_method The highest-scoring method
#' @param scores Named vector of method scores
#' @return Character string explaining the recommendation
generate_rationale <- function(user_responses, top_method, scores) {
  
  # Start building rationale based on top method
  rationale_parts <- c()
  
  # Method-specific opening
  method_intro <- switch(top_method,
    "CRM" = "CRM (Continual Reassessment Method) is recommended because",
    "BOIN" = "BOIN (Bayesian Optimal Interval) is recommended because", 
    "3+3" = "The 3+3 design is recommended because"
  )
  
  rationale_parts <- c(rationale_parts, method_intro)
  
  # Add reasoning based on responses
  reasons <- c()
  
  # Toxicity confidence reasoning
  if (!is.null(user_responses$toxicity_confidence)) {
    if (user_responses$toxicity_confidence == "Very confident (good historical data)" && top_method == "CRM") {
      reasons <- c(reasons, "you have strong historical toxicity data that CRM can leverage effectively")
    } else if (user_responses$toxicity_confidence == "Not confident/limited data" && top_method == "3+3") {
      reasons <- c(reasons, "with limited toxicity data, the 3+3 design provides a simple, well-understood approach")
    } else if (user_responses$toxicity_confidence == "Somewhat confident" && top_method == "BOIN") {
      reasons <- c(reasons, "BOIN provides a good balance when toxicity confidence is moderate")
    }
  }
  
  # Trial priorities reasoning
  if (!is.null(user_responses$trial_priorities)) {
    if (user_responses$trial_priorities == "Finding the best dose efficiently" && top_method == "CRM") {
      reasons <- c(reasons, "CRM is most efficient at finding the maximum tolerated dose")
    } else if (user_responses$trial_priorities == "Getting started quickly with simple rules" && top_method == "3+3") {
      reasons <- c(reasons, "the 3+3 design offers the simplest implementation with fixed decision rules")
    } else if (user_responses$trial_priorities == "Balance of both" && top_method == "BOIN") {
      reasons <- c(reasons, "BOIN balances efficiency with simplicity")
    }
  }
  
  # Statistical support reasoning
  if (!is.null(user_responses$statistical_support)) {
    if (user_responses$statistical_support == "Yes experienced with complex modeling" && top_method == "CRM") {
      reasons <- c(reasons, "your statistical expertise enables effective use of CRM's adaptive modeling")
    } else if (user_responses$statistical_support == "Limited statistical support" && top_method == "3+3") {
      reasons <- c(reasons, "the 3+3 design requires minimal statistical expertise to implement")
    } else if (user_responses$statistical_support == "Yes but prefer simpler approaches" && top_method == "BOIN") {
      reasons <- c(reasons, "BOIN provides statistical rigor without excessive complexity")
    }
  }
  
  # Transparency reasoning
  if (!is.null(user_responses$decision_transparency)) {
    if (user_responses$decision_transparency == "Very important (regulatory/reproducibility)" && top_method == "BOIN") {
      reasons <- c(reasons, "BOIN provides transparent, pre-specified decision boundaries")
    } else if (user_responses$decision_transparency == "Flexible adaptation preferred" && top_method == "CRM") {
      reasons <- c(reasons, "CRM allows flexible adaptation based on accumulating data")
    } else if (user_responses$decision_transparency == "Simple fixed rules fine" && top_method == "3+3") {
      reasons <- c(reasons, "the 3+3 design uses simple, fixed escalation rules")
    }
  }
  
  # Combine reasons
  if (length(reasons) > 0) {
    reason_text <- paste(reasons, collapse = ", and ")
    rationale_parts <- c(rationale_parts, reason_text)
  }
  
  # Add method characteristics
  method_desc <- switch(top_method,
    "CRM" = "CRM continuously updates dose-toxicity estimates using Bayesian methods, making it highly efficient but requiring more statistical expertise.",
    "BOIN" = "BOIN uses pre-specified decision boundaries that balance efficiency with transparency, making it suitable for many regulatory contexts.",
    "3+3" = "The 3+3 design follows simple escalation rules (treat 3, escalate if 0/3 toxicities), making it the most straightforward to implement."
  )
  
  # Check for close scores and add caveats
  max_score <- max(scores)
  second_score <- sort(scores, decreasing = TRUE)[2]
  
  caveat <- if (max_score - second_score <= 1) {
    sprintf("\n\nNote: The scores are quite close (difference of %d), so %s could also be a reasonable choice depending on your specific context.", 
            max_score - second_score, names(sort(scores, decreasing = TRUE))[2])
  } else {
    ""
  }
  
  # Combine all parts
  parts <- c()
  if (length(rationale_parts) > 0) {
    parts <- c(parts, paste(rationale_parts, collapse = " "))
  }
  parts <- c(parts, method_desc)
  if (nzchar(caveat)) {
    parts <- c(parts, caveat)
  }
  full_rationale <- paste(parts, collapse = ". ")
  
  return(full_rationale)
}